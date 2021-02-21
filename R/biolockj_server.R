#' BioLockJ server.R
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return server function
#'
biolockj_server <- function(input, output, session){
    
    envType = detect_deployment()
    
    
    #############################      initial JAVA calls      #########################################
    # Run these calls before starting so these bulk values are available.
    # IFF the user chooses to change the BioLockJ jar file, (which will a rare thing)
    # then the reactive values that store these are updated.
    
    tryCatch({
        BioLockR::get_BLJ_JAR()
    }, error=function(...){
        BioLockR::set_BLJ_JAR(file.path(getwd(), "BioLockJ", "dist", "BioLockJ.jar"), remember=TRUE, doublecheck = FALSE)
    })
    tryCatch({
        BioLockR::get_BLJ_PROJ()
    }, error=function(...){
        BioLockR::set_BLJ_PROJ(file.path(getwd(), "temp"), remember=TRUE, doublecheck = FALSE)
    })
    
    initbljVer = BioLockR::biolockjVersion()
    initpropInfo <- propInfoSansSpecials()
    initmoduleInfo <- BioLockR::moduleInfo()
    initGenDefaults <- lapply(initpropInfo, function(prop){ prop$default })
    initFilePathType <- propsInfoForType(initpropInfo, "file path")
    initFileListType <- propsInfoForType(initpropInfo, "list of file paths")
    
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    
    #############################            SERVER            #########################################
    
    server <- function(input, output, session) {
        
        #############################         Core Objects         #########################################
        
        ## Use reactive objects as the single source of truth.
        values <- reactiveValues(defaultProps=NA,
                                 moduleList=vector("character"), 
                                 customProps=list(), 
                                 removedModules=vector("character"),
                                 ### IMPORTANT !
                                 # GET the property values through this object; the pipelineProperties reactiveValues object
                                 # SET the property values through the input$[propName] object
                                 # an observeEvent ensures the flow of info from the input$ to the reactiveValues
                                 pipelineProperties=initGenDefaults # TODO: maybe change name to backboneProperties
                                 # moduleProperties=modulePerProp(initmoduleInfo)
        )
        
        defaults <- reactiveValues(
            defaultPropsChain = list(),
            defaultPropsList = list(standard=initGenDefaults),
            values = initGenDefaults,
            uploadedFiles = data.frame(name=c(), size=c(), type=c(), datapath=c()),
            activeFiles = c()
        )
        
        projectDirPath <- reactiveVal("")
        
        jarFilePath <- reactiveVal( BioLockR::get_BLJ_JAR() )
        
        extModsDir <- reactiveVal( "" )
        
        bljVer <- reactiveVal(initbljVer)
        
        allModuleInfo <- reactiveVal(initmoduleInfo)
        
        # map module short name to run line syntax
        moduleRunLines <- reactiveVal(getModuleRunLines(initmoduleInfo))
        
        genPropInfo <- reactiveVal(initpropInfo)
        
        # break out properties into categories
        groupedProps <- reactiveVal( groupPropsByCategory(initpropInfo) )
        
        # TODO: make this update if jar file is updated
        filePathProps <- reactiveVal( initFilePathType )
        fileListProps <- reactiveVal( initFileListType )
        
        # 
        fileListUpdates <- reactiveValues()
        
        
        #############################         Dynamic UI           #########################################
        # Defining the UI.  This would be in the ui function... but its dynamic.
        
        # Home ####
        
        output$showProjectDir <- renderPrint(cat(projectDirPath()))
        
        observeEvent(input$projectRootDir,{
            if (is.integer(input$projectRootDir)) {
                message("No directory has been selected.")
            } else {
                message("setting new value for project root...")
                projectDirPath( shinyFiles::parseDirPath(volumes, input$projectRootDir) )
            }
        })
        
        output$configText <- renderPrint({
            cat(paste0(configLines(), collapse = "\n"))
        })
        
        output$examples <- renderUI({
            examples = findExampleConfigs()
            select = "BioLockJ/templates/myFirstPipeline/myFirstPipeline.config"
            if (examples=="") select = ""
            tagList(
                h2("Pull from an example config file"),
                selectInput("selectExample", "Select an example", choices = examples, 
                            selected=select),
                actionButton("populateExampleConfig", "pull values")
            )
        })
        
        # Defaults ####
        output$chainedFiles <- renderTable({
            req(input$selectDefaultProps)
            shinyFeedback::hideFeedback("selectDefaultProps")
            chainInfo = orderDefaultPropFiles(start=input$selectDefaultProps, chain=defaults$defaultPropsChain)
            if (length(chainInfo$dangling) > 0){
                shinyjs::disable("loadDefaultProps")
                shinyFeedback::showFeedbackWarning("selectDefaultProps", paste("Missing file:", BioLockR::printListProp(chainInfo$missing)) )
            }else{
                shinyjs::enable("loadDefaultProps")
                shinyFeedback::showFeedbackSuccess("selectDefaultProps")
            }
            chainLinks = intersect(chainInfo$chained, names(defaults$defaultPropsChain))
            df = stack(defaults$defaultPropsChain[chainLinks])
            names(df) = c("linksTo", "file")
            df[,c("file", "linksTo")]
        })
        
        output$chainableFiles <- renderTable({
            message("defaults[['defaultPropsChain']] : ", defaults[["defaultPropsChain"]])
            message("defaults[['defaultPropsChain']] : ", str(defaults[["defaultPropsChain"]]))
            if( length( defaults$defaultPropsChain ) > 0 ){
                from = defaults$defaultPropsChain
            }else{
                validate("If there is a chain of default properties, the links are listed here.")
            }
            df = stack(from)
            names(df) = c("linksTo", "file")
            df[,c("file", "linksTo")]
        })
        
        output$currentDefaultProps <- renderPrint({
            if(!is.null(values$defaultProps) && !is.na(values$defaultProps)){
                cat( writeConfigProp("pipeline.defaultProps", values$defaultProps, "list") )
            }else{
                cat("# no pipeline.defaultProps")
            }
        })
        
        # Modules ####
        output$selectModule <- renderUI({
            selectInput("AddBioModule",
                        "Select new BioModule", 
                        names(allModuleInfo()), 
                        selected = "GenMod",
                        width = '100%')
        })
        
        output$manageModules <- renderUI({
            sortable::bucket_list(
                header="BioModule Run Order",
                sortable::add_rank_list(
                    text = "Drag and drop to re-order",
                    labels = values$moduleList,
                    input_id = "orderModules",
                    options = sortable::sortable_options(multiDrag = TRUE)),
                sortable::add_rank_list(
                    text="Trash",
                    labels = values$removedModules,
                    input_id = "trashModules",
                    options = sortable::sortable_options(multiDrag = TRUE)),
                orientation="vertical")
        })
        
        
        # Properties - backbone ####
        output$genProps <- renderUI({
            message("Rendering ui for slot genProps...")
            argsList =  lapply(as.list(names(groupedProps())), function(groupName){
                group = groupedProps()[[groupName]]
                tabPanel(groupName,
                         p(paste("See the user guide for more info about", groupName, "properties.", collaps=" ")),
                         lapply(group, function(propName){
                             propUI <- renderPropUi(propName, 
                                                    genPropInfo()[[propName]], 
                                                    defaults$values[[propName]], 
                                                    isolate(values$pipelineProperties[[propName]]),
                                                    isolate(defaults))
                             observeEvent(input[[propUiName(propName)]],{
                                 values$pipelineProperties[[propName]] <- input[[propUiName(propName)]]
                                 req(input$checkLiveFeedback)
                                 req(genPropInfo()[[propName]]$type != "boolean")
                                 shinyFeedback::hideFeedback( propUiName(propName) )
                                 req(input[[propUiName(propName)]] != "")
                                 isGood = isolate(BioLockR::isValidProp(propName, input[[propUiName(propName)]]))
                                 message("isGood: ", isGood)
                                 if (is.na(isGood)){
                                     shinyFeedback::hideFeedback( propUiName(propName) )
                                 }else if(isGood){
                                     shinyFeedback::showFeedbackSuccess( propUiName(propName) )
                                 }else{
                                     shinyFeedback::showFeedbackWarning( propUiName(propName), "not good" )
                                 }
                             })
                             propUI
                         }))
            })
            refreshFileChoosers()
            argsList$selected = "input"
            argsList$id = "genPropsTabSet"
            do.call(tabsetPanel, argsList)
        })
        
        # myVolumes = reactiveVal(volumes)
        ## This was effective at making the file/dir chooser window start at the project dir
        ## But it had the terrible side-effect that if you set the project dir, and then opened 
        ## the dir chooser, and selected something in the project root, the whole system would
        ## crash with error:
        # Warning: Error in : `path` must not have missing values
        # * NAs found at 1 locations: 1
        # 51: <Anonymous>
        #
        # So... we don't do this anymore.
        # myVolumes = reactiveVal(c(Project=volumes["Home"], volumes))
        # observeEvent(projectDirPath(), {
        #     if ( projectDirPath() == "" ){
        #         myVolumes( c(Project=volumes["Home"], volumes) )
        #     }else{
        #         myVolumes( c(Project=projectDirPath(), volumes) )
        #     }
        #     refreshFileChoosers()
        # })
        
        refreshFileChoosers <- reactive({
            for (prop in filePathProps()){
                buildFilePathPropObservers(session, input, output, prop$property, volumes, values) #myVolumes()
            }
            for (prop in fileListProps()){
                buildFileListPropObservers(session, input, output, prop$property, volumes, values, fileListUpdates) #myVolumes()
            }
        })
        
        
        # Properties - custom ####
        indexRmButtons = reactiveVal(0) # each time this ui is rendered, the buttons are all new buttons; otherwise, if you remove property "a", and later try to add it, you can't.
        output$showCustomProps <- renderUI({
            if (length(values$customProps) > 0 ){
                lapply(names(values$customProps), function(cp){
                    index=isolate(indexRmButtons()) + 1
                    indexRmButtons(index)
                    buttonId=paste0("REMOVE", propUiName(cp), index)
                    message("creating option [", buttonId, "] to remove property: ", cp)
                    customPropUi <- fluidRow(
                        column(9, renderPrint(cat(paste(cp, "=", values$customProps[[cp]])))),
                        column(3, actionButton(buttonId, "remove")))
                    observeEvent(input[[buttonId]],{
                        values$customProps[[cp]] <- NULL
                    })
                    customPropUi
                })
            }else{
                em("none")
            }
        })
        
        output$showDefaultCustomProps <- renderUI({
            if (length(customDefaults()) > 0 ){
                lapply(names(customDefaults()), function(cp){
                    buttonId=paste0("rm-", cp)
                    customPropUi <- fluidRow(
                        column(9, renderPrint(cat(paste(cp, "=", customDefaults()[[cp]])))),
                        column(3, renderPrint(cat(""))))
                    customPropUi
                })
            }else{
                list(br(),em("none"))
            }
        })
        
        customDefaults <- reactive({
            df = stack( sapply(names(defaults$values), function(p){strsplit(p, ".", fixed=TRUE)[[1]][1]}), stringsAsFactors=FALSE)
            names(df) = c("prefix", "propName")
            df$prefix = as.character(df$prefix)
            df$propName = as.character(df$propName)
            df$isCustom = TRUE
            message("df has ", nrow(df), " rows.")
            #
            # general property defaults are reflected in general property ui
            df[df$propName %in% names(values$pipelineProperties), "isCustom"] <- FALSE
            # df = df[! df$propName %in% names(values$pipelineProperties),]
            message("After removing general props, df has ", sum(df$isCustom), " rows.")
            #
            # mdoule property defaults are reflected in ui when that module is added
            df[df$propName %in% names( modulePerProp( allModuleInfo() )), "isCustom"] <- FALSE
            # df = df[! df$propName %in% names( modulePerProp( allModuleInfo() )),]
            message("After removing module props, df has ", sum(df$isCustom), " rows.")
            #
            # module overrides that use the module class name are reflected in module property ui
            df[df$prefix %in% names( allModuleInfo() ), "isCustom"] <- FALSE
            # df = df[! df$prefix %in% names( allModuleInfo() ),]
            message("After removing module-override props, df has ", sum(df$isCustom), " rows.")
            #
            # module overrides that use the module's alias are shown in the module ui with the matching alias 
            # IFF the alias is an alias in the current pipline
            df[df$prefix %in% allActiveAliases(), "isCustom"] <- FALSE
            # df = df[! df$prefix %in% allActiveAliases(),]
            message("after removing alias-overrides, df has ", sum(df$isCustom), " rows.")
            #
            # module overrides that use the module's class name where the modules class name
            # in this pipeline is covered by an alias...should be shown.
            # hidden = 
            # df[df$prefix %in% hidden, "isCustom"] <- TRUE
            #
            # not included in the current config's custom props
            df[df$prefix %in% names(values$customProps), "isCustom"] <- FALSE
            # df = df[! df$prefix %in% names(values$customProps),]
            message("after removing current custom props, df has ", sum(df$isCustom), " rows.")
            #
            df = df[df$isCustom,]
            defaults$values[df$propName]
        })
        
        output$modulePropsHeader <- renderText("The properties for a given module include the properties that are specific to that module, as well as any general properties that the module is known to reference.")
        
        # Precheck
        
        
        # Settings ####
        
        
        #############################       Button Actions         #########################################
        # define event handlers for buttons
        
        # Home - save config ####
        
        shinyFiles::shinyDirChoose(input, "projectRootDir", roots = volumes, restrictions = system.file(package = "base"))
        
        output$downloadData <- downloadHandler(
            filename = function() {
                configFileName()
            },
            content = function(file) {
                writeLines( configLines(), file)
            }
        )
        
        output$saveButton <- renderUI({
            useIcon = icon("save")
            id = "saveConfigBtn"
            #create, replace, update, cannot
            styleCase = savingFileWill()
            message("rendering save button for case: ", styleCase)
            if ( styleCase == "create" ){
                shinyBS::tipify(actionButton(id, "create config file in Project Root Directory", icon = useIcon, class="btn-success"), basename(saveAsPath()), placement = 'right')
            }else if ( styleCase == "update" ){
                actionButton(id, "update config file in Project Root Directory", icon = useIcon, class="btn-success")
            }else if ( styleCase == "replace" ){
                shinyBS::tipify(actionButton(id, "replace config file in Project Root Directory", icon = useIcon, class="btn-danger"), basename(saveAsPath()), placement = 'right')
            }else if ( styleCase == "cannot" && projectDirPath() != ""){
                shinyjs::disabled(actionButton(id, "Save to Project Root Directory (non writable location)", icon = useIcon))
            }else{
                shinyjs::disabled(actionButton(id, "Save to Project Root Directory (requires Project Root)", icon = useIcon))
            }
        })
        
        saveAsPath <- reactive( file.path(projectDirPath(), configFileName()) )
        
        sessionHasSavedAs <- reactiveVal(c())
        
        savingFileWill <- reactive({
            sessionHasSavedAs()
            saveAsPath()
            if ( file.exists(saveAsPath()) ){
                if ( file.access(dirname(saveAsPath()), mode=2)!=0 ) {
                    val = "cannot"
                }else if ( saveAsPath() %in% sessionHasSavedAs() ){
                    val = "update"
                }else{
                    val = "replace"
                }
            }else{
                if ( file.access(dirname(saveAsPath()), mode=2)==0 ) {
                    message("saveAsPath: ", saveAsPath())
                    message("Does that path exist: ", file.exists(saveAsPath()))
                    val = "create"
                }else{
                    val = "cannot"
                }
            }
            message("new value: ", val)
            val
        })
        
        observeEvent(input$saveConfigBtn, {
            message("Saving file to: ", saveAsPath())
            writeLines(configLines(), saveAsPath())
            sessionHasSavedAs( c(isolate( sessionHasSavedAs() ), saveAsPath()))
        })
        
        # Home - load config ####
        output$LocalExistingConfig <- renderUI({
            if (input$radioServerType == "local"){
                shinyFiles::shinyFileChoose(input, "LocalExistingConfig", roots = volumes, session = session)
                tagList(
                    hr(),
                    h2("Load an existing config file"),
                    shinyFiles::shinyFilesButton("LocalExistingConfig", "Choose a local config file", "Please select a file", multiple = FALSE, viewtype = "list"),
                    p(),
                    verbatimTextOutput("showLocalFile", placeholder=TRUE),
                    shinyjs::disabled(actionButton("populateFromLocalConfig", "pull values"))
                )
                
            }
        })
        observeEvent( localFilePath(), {
            if ( isTruthy(localFilePath()) && file.exists(localFilePath())){
                shinyjs::enable("populateFromLocalConfig")
            }else{
                shinyjs::disable("populateFromLocalConfig")
            }
        })
        localFilePath <- reactive({
            prsd = shinyFiles::parseFilePaths(volumes, input$LocalExistingConfig)
            prsd$datapath
        })
        
        output$showLocalFile <- renderPrint(cat(localFilePath()))
        
        observeEvent(input$populateFromLocalConfig, {
            message("The button got pushed: populateFromLocalConfig")
            req( file.exists(localFilePath()) )
            existingLines( readLines( localFilePath() ) )
            if (input$radioServerType == "local") projectDirPath( dirname(localFilePath()) )
            else projectDirPath( "" )
            populateModules()
            populateProps()
            populateProjectName( basename( localFilePath() ) )
            updateTabsetPanel(session, "HomeTabs", selected="Save to file")
        })
        
        observeEvent(input$populateExistingConfig, {
            message("The button got pushed: populateExistingConfig")
            req( input$uploadExistingConfig )
            existingLines( readLines( input$uploadExistingConfig$datapath ) )
            projectDirPath( "" )
            populateModules()
            populateProps()
            populateProjectName(input$uploadExistingConfig$name)
            updateTabsetPanel(session, "HomeTabs", selected="Save to file")
        })
        
        observeEvent(input$populateExampleConfig, {
            message("The button got pushed, button: populateExampleConfig")
            existingLines( readLines( input$selectExample ) )
            projectDirPath( dirname(input$selectExample) )
            populateModules()
            populateProps()
            populateProjectName( basename( input$selectExample ) )
            updateTabsetPanel(session, "HomeTabs", selected="Save to file")
        })
        
        # Defaults ####
        observeEvent(input$defaultPropsFiles,{
            message("Event: input$defaultPropsFiles")
            req(input$defaultPropsFiles)
            for (i in 1:nrow(input$defaultPropsFiles)){
                allProps = readBljProps(readLines(input$defaultPropsFiles$datapath[i]))
                if ( length(allProps$defaultProps)==0 || input$ignoreChain){
                    defaults$defaultPropsChain[input$defaultPropsFiles$name[i]] = NA
                }else{
                    defaults$defaultPropsChain[[input$defaultPropsFiles$name[i]]] = allProps$defaultProps
                }
                defaults$defaultPropsList[[input$defaultPropsFiles$name[i]]] = allProps$properties
            }
            wasSelected = input$selectDefaultProps
            defaults$uploadedFiles <- rbind(defaults$uploadedFiles, input$defaultPropsFiles)
            updateSelectInput(session, "selectDefaultProps", choices = defaults$uploadedFiles$name, selected=wasSelected)
            shinyjs::enable("selectDefaultProps")
            updateCheckboxInput(session, "ignoreChain", value=FALSE)
        })
        
        observe({
            if (length(input$selectDefaultProps)==0){ 
                shinyFeedback::hideFeedback("selectDefaultProps") 
            }
        })
        
        observeEvent(input$loadDefaultProps, {
            message("Pushed button: loadDefaultProps")
            chainInfo = orderDefaultPropFiles(start=input$selectDefaultProps, chain=defaults$defaultPropsChain)
            if (length(chainInfo$dangling) > 0){
                shinyjs::disable("loadDefaultProps")
                shinyFeedback::showFeedbackDanger("selectDefaultProps", c("Missing file: ", BioLockR::printListProp(chainInfo$missing)))
            }else{
                # save state
                prevCheckBox = input$include_standard_defaults
                updateCheckboxInput(session, "include_standard_defaults", value=FALSE)
                tempFile = tempfile()
                writeLines(configLines(), tempFile)
                # modify underlying state
                defaults$values = c()
                defaults$values = lapply(genPropInfo(), function(prop){ prop$default })
                defaults$activeFiles = chainInfo$chained
                for (file in defaults$activeFiles){
                    newProps = defaults$defaultPropsList[[file]]
                    defaults$values[names(newProps)] = newProps
                }
                values$pipelineProperties = defaults$values[names(values$pipelineProperties)]
                # restore state
                existingLines( readLines( tempFile ) )
                populateProps()
                values$defaultProps = input$selectDefaultProps
                updateCheckboxInput(session, "include_standard_defaults", value=prevCheckBox)
                shinyFeedback::hideFeedback("selectDefaultProps")
            }
        })
        
        # Module ####
        observeEvent(input$AddModuleButton, {
            runLine = makeRunLine(input$AddBioModule, moduleRunLines(), input$newAlias)
            msg = capture.output({
                goodAlias = isValidAlias(aliasFromRunline(runLine), allActiveAliases())
            }, type="message")
            shinyFeedback::feedbackDanger("newAlias", !goodAlias, msg)
            req(goodAlias)
            values$moduleList <- c(isolate(input$orderModules), runLine)
            updateTextInput(session, "newAlias", value = "")
        })
        
        observeEvent(input$emptyModuleTrash, {
            values$removedModules <- list()
        })
        
        # Properties ####
        observeEvent(input$addCostomPropBtn, {
            message("The button was pushed! button: addCostomPropBtn")
            req(input$customPropName)
            req(trimws(input$customPropName) != "")
            req(input$customPropVal)
            values$customProps[[input$customPropName]] <- input$customPropVal
            updateTextInput(session, "customPropName", value = "")
            updateTextInput(session, "customPropVal", value = "")
        })
        
        # Precheck ####
        observeEvent(input$runPrecheckBtn, {
            message("Running command:")
            message(precheckCommand())
            
            if ( length(values$moduleList) == 0 ) validate("You need to include at least one module")
            if ( isWritableValue(projectDirPath()) && input$radioServerType =="local" ){
                message("Running locally with a configured project root dir; using project root dir")
                tempFileLoc = tempfile(pattern = input$projectName, tmpdir = projectDirPath(), fileext = ".config")
            }else{
                tempFileLoc = file.path(getwd(),"temp", configFileName())
            }
            
            message("writing to file: ", tempFileLoc)
            writeLines(configLines(), tempFileLoc)
            
            # short term cheats - TODO
            command = gsub(pattern=configFileName(), replacement = tempFileLoc, precheckCommand())
            defaultBljProj=paste("--precheck-only", "--blj_proj", file.path(getwd(), "BioLockJ", "pipelines"))
            command = gsub("--precheck-only", defaultBljProj, command)
            message("...actually running command:")
            message(command)
            #
            precheckRestultText(system2("exec", args = command, stdout = TRUE, stderr = TRUE))
            #
            file.remove(tempFileLoc)
        })
        
        # Settings ####
        output$coreSettings <- renderUI({
            tagList(
                shinyFiles::shinyFilesButton("biolockjJarFile", "BioLockJ Jar File Location", "Choose jar file", multiple=FALSE),
                shinyjs::disabled(actionButton("updateJar", "update jar location")),
                verbatimTextOutput("showNewJarFile"),
                br(),
                shinyFiles::shinyDirButton("extMods", "External Modules Folder", "Choose directory", multiple=FALSE),
                shinyjs::disabled(actionButton("updateMods", "update modules location")),
                verbatimTextOutput("showNewExtMods"))
        })
        
        # Settings - jar file ####
        newJar = reactiveVal("")
        shinyFiles::shinyFileChoose(input, "biolockjJarFile", roots = volumes)
        output$showNewJarFile <- renderPrint( cat(newJar()) )
        
        observeEvent(input$biolockjJarFile, {
            req(input$biolockjJarFile)
            if (!is.integer(input$biolockjJarFile)){
                newJar( shinyFiles::parseFilePaths(volumes, input$biolockjJarFile)$datapath[[1]] )
                shinyjs::enable("updateJar")
            }else{
                newJar( "" )
                shinyjs::disable("updateJar")
            }
        })
        
        modal_confirm_jar <- modalDialog(
            "Switching to a different jar file may have unforeseen effects.",
            title = "Change to a new BioLockJ.jar location?",
            footer = tagList(
                actionButton("cancelJar", "Cancel"),
                actionButton("setNewJar", "Use new path (this time)"),
                actionButton("rememberSetNewJar", "Always use this new path", class = "btn btn-danger")
            )
        )
        
        observeEvent(input$updateJar, {
            showModal(modal_confirm_jar)
        })
        
        observeEvent(input$cancelJar, {
            message("JK, I don't want to change the jar file.")
            removeModal()
        })
        
        observeEvent(input$setNewJar, {
            removeModal()
            message("Switching to newJar path, for this session.")
            good = BioLockR::set_BLJ_JAR( newJar(), remember = FALSE )
            if (good) respondToUpdateJar()
        })
        
        observeEvent(input$rememberSetNewJar, {
            removeModal()
            message("Switching to newJar path, for this and future sessions.")
            good = BioLockR::set_BLJ_JAR( newJar(), remember = TRUE, doublecheck = FALSE )
            if (good) respondToUpdateJar()
        })
        
        respondToUpdateJar <- reactive({
            # TODO: show spinner or progress bar or something to let the user know that a delay is expected.
            # update objects from java
            jarFilePath( BioLockR::get_BLJ_JAR() )
            bljVer(BioLockR::biolockjVersion())
            allModuleInfo(BioLockR::moduleInfo()) 
            moduleRunLines(getModuleRunLines(allModuleInfo()))
            genPropInfo(propInfoSansSpecials())
            #TODO: update modules properties
        })
        
        # Settings - ext mods dir ####
        
        newModsDir = reactiveVal("")
        shinyFiles::shinyDirChoose(input, "extMods", roots = volumes)
        output$showNewExtMods <- renderPrint( cat(newModsDir()) )
        
        observeEvent(input$extMods, {
            req(input$extMods)
            if (!is.integer(input$extMods)){
                newModsDir( shinyFiles::parseDirPath(volumes, input$extMods) )
                shinyjs::enable("updateMods")
            }else{
                newModsDir( "" )
                shinyjs::disable("updateMods")
            }
        })
        
        modal_confirm_mods <- modalDialog(
            "Althought much safer than changing the BioLockJ.jar path, this too may have unforseen effects.",
            title = "Change the external modules directory ?",
            footer = tagList(
                actionButton("cancelMods", "Cancel"),
                actionButton("setNewMods", "Use new path (this time)"),
                shinyjs::disabled(actionButton("rememberSetNewMods", "Always use this new path", class = "btn btn-danger"))
            )
        )
        
        observeEvent(input$updateMods, {
            showModal(modal_confirm_mods)
        })
        
        observeEvent(input$cancelMods, {
            message("JK, I don't want to change the mods dir.")
            removeModal()
        })
        
        observeEvent(input$setNewMods, {
            removeModal()
            message("Switching to mods dir, for this session.")
            extModsDir( newModsDir() )
            respondToUpdateMods()
        })
        
        observeEvent(input$rememberSetNewMods, {
            removeModal()
            message("Switching to new mods dir, for this and future sessions.---JK I have no way of remembering!")
            # TODO: set up way to remember this preference
            extModsDir( newModsDir() )
            respondToUpdateMods()
        })
        
        respondToUpdateMods <- reactive({
            # TODO: show spinner or progress bar or something to let the user know that a delay is expected.
            # update objects from java
            allModuleInfo( BioLockR::moduleInfo( externalModules=extModsDir() ) ) 
            moduleRunLines( getModuleRunLines( allModuleInfo() ) )
            message("There are now ", length( moduleRunLines() ), " modules available.")
            updateSelectInput(session, "AddBioModule", choices=names(allModuleInfo()) )
            #TODO: update modules properties
        })
        
        #############################           Actions            #########################################
        # reactive expressions that are intuitively like functions
        
        existingLines <- reactiveVal()
        
        configFileName <- reactive( paste0(input$projectName, ".config") )
        
        populateProjectName <- function(path){
            newName = tools::file_path_sans_ext(path)
            updateTextInput(session, "projectName", value=newName)
        }
        
        populateModules <- reactive({
            existingLines <- existingLines()
            exModOrder = existingLines[ grep("^#BioModule ", existingLines) ]
            if (length(exModOrder) > 0){
                values$moduleList <- exModOrder
                values$removedModules <- list()
            }
        })
        
        populateProps <- reactive({
            newProps = readBljProps( existingLines() )
            values$defaultProps = newProps$defaultProps
            vals = newProps$properties
            if (length(vals) > 0 ){
                for( propName in names(vals) ){
                    message("updating property value: ", propName, " = ", vals[propName])
                    # If propName is a genProp, set genProp
                    if ( propName %in% names(genPropInfo()) ){
                        # if input object is established, set through that
                        # otherwise, set value in pipelineProperties object Directly
                        if( !is.null(input[[propSelectFromId(propName)]]) ){
                            # if rendered, file path list props go this way
                            fileListUpdates[[propName]] = vals[propName]
                        }else if (!is.null(input[[propUiName(propName)]])){
                            # most props, if rendered, go this way
                            updateTextInput(session, propName, value = paste(vals[propName]))
                        }else{ 
                            # file path props got this way, rendered or not
                            # all props, if NOT rendered, go this way
                            values$pipelineProperties[[propName]] <- vals[propName]
                        }
                    }else{
                        values$customProps[[propName]] <- vals[propName]
                    }
                }
            }
        })
        
        configLines <- reactive({
            lines = vector("character")
            if (input$include_biolockj_version){
                lines = c(lines, paste("# Updated using BioLockJ version:", bljVer()))
            }
            #
            lines = c(lines, "", values$moduleList)
            #
            if ( !is.na(values$defaultProps) && length(values$defaultProps) > 0 ){
                lines = c(lines, "", writeConfigProp("pipeline.defaultProps", values$defaultProps, "list of file paths", projectDirPath(), input$checkRelPaths))
            }
            if ( length(values$customProps) > 0 ){
                lines = c(lines, "", "# Custom Properties")
                for(cp in names(values$customProps)){
                    line = writeConfigProp(cp, values$customProps[[cp]])
                    lines = c(lines, line)
                }
            }
            lines = c(lines, "", "# General Properties")
            for(p in names(genPropInfo())){
                value = values$pipelineProperties[[p]]
                if ( doIncludeProp(p, value) ){
                    line = writeConfigProp(p, value, genPropInfo()[[p]]$type, projectDirPath(), input$checkRelPaths)
                    lines = c(lines, line)
                }
            }
            for (runline in values$moduleList){
                al = aliasFromRunline(runline)
                alsProps = list()
                if ( length(alsProps) > 0 ){
                    lines = c(lines, "", paste("#", al))
                    # TODO - add module props, module override props for this module
                }
            }
            if (input$include_mid_progress){
                if ( length(values$removedModules) > 0){
                    lines = c(lines, "", "",  "### Modules that might be added to the pipeline:")
                    for (runline in values$removedModules){
                        lines = c(lines, paste0("#", runline))
                        # TODO - add module props, module override props for this module ---all to have '#' in front!
                    }
                }
            }
            lines
        })
        
        doIncludeProp <- function(p, value){
            if (isWritableValue(value)){
                notTheDefault = !is.null(defaults$values[[p]]) && value != defaults$values[[p]]
                # message("value of property ", p, "=", value, " is ", ifelse(notTheDefault, "NOT", ""), " the same as the default value: ", defaults$values[[p]])
                return(is.null(defaults$values[[p]]) || notTheDefault || input$include_standard_defaults)
            }else{
                return(FALSE)
            }
        }
        
        # Precheck
        precheckCommand <- reactive({
            command = ifelse(input$callJavaDirectly, paste("java -jar", BioLockR::get_BLJ_JAR()), "biolockj")
            if (input$checkPrecheck) command = paste(command, "--precheck-only")
            if (input$checkUnused) command = paste(command, "--unused-props")
            if (input$checkDocker) command = paste(command, "--docker")
            # if (input$checkAws) command = paste(command, "--aws")
            if (input$checkForground) command = paste(command, "--foreground")
            if (input$checkVerbose) command = paste(command, "--verbose")
            if (input$checkMapBlj) command = paste(command, "--blj")
            # if (input$extModsDir) command = paste(command, "--external-modules", input$extModsDir$name) #TODO this should use a path/to/dir
            # if (input$bljProjDir) command = paste(command, "--blj_proj", input$bljProjDir$name) #TODO this should use a path/to/dir
            command = paste0(command, " ", input$projectName, ".config")
            # command = paste(command, "--help") #TODO
            command
        })
        
        
        
        
        #############################           Synchrony          #########################################
        # Reactive espressions and observers that serve to keep things smooth and synchronized.
        # These make the app nice, but they are not fundamental to the understanding of the layout and workings.
        
        # Home ####
        observeEvent(projectDirPath(), {
            if( projectDirPath() != "" ) {
                shinyjs::enable("checkRelPaths")
                shinyBS::removeTooltip(session, "checkRelPaths")
            }else{
                shinyjs::disable("checkRelPaths")
                updateCheckboxInput(session, "checkRelPaths", value = FALSE)
                shinyBS::addTooltip(session, "checkRelPaths", title="requires Project Root Directory")
            }
        })
        
        observeEvent(input$uploadExistingConfig, {
            if (file.exists(input$uploadExistingConfig$datapath)){
                shinyjs::enable("populateExistingConfig")
            }else{
                shinyjs:disable("populateExistingConfig")
            }
        })
        
        observeEvent(input$selectExample, {
            if (file.exists(input$selectExample)){
                shinyjs::enable("populateExampleConfig")
            }else{
                shinyjs:disable("populateExampleConfig")
            }
        })
        
        # Modules ####
        observeEvent(input$orderModules, {
            values$moduleList <- input$orderModules
        })
        
        observeEvent(input$trashModules, {
            values$removedModules <- input$trashModules
        })
        
        
        # placeholder of new alias shows the current implied alias
        showDefaultAlias <- reactive({
            possibleLine = makeRunLine(input$AddBioModule, moduleRunLines(), input$newAlias)
            derivedAlias = aliasFromRunline( possibleLine )
        })
        setDefaultAliasPlaceholder <- reactive({
            updateTextInput(session, "newAlias", placeholder = showDefaultAlias() )
        })
        observeEvent(input$AddBioModule, {
            setDefaultAliasPlaceholder()
        })
        observeEvent(input$newAlias, {
            setDefaultAliasPlaceholder()
        })
        
        pipelineAliases <- reactive({
            sapply(values$moduleList, aliasFromRunline)
        })
        
        allActiveAliases <- reactive({
            removedAliases <- sapply(values$removedModules, aliasFromRunline)
            c(pipelineAliases(), removedAliases)
        })
        
        # green for valid alias
        observeEvent(showDefaultAlias(), {
            shinyFeedback::hideFeedback("newAlias")
            runLine = makeRunLine(input$AddBioModule, moduleRunLines(), input$newAlias)
            msg = capture.output({
                goodAlias = isValidAlias(aliasFromRunline(runLine), allActiveAliases())
            }, type="message")
            shinyFeedback::feedbackSuccess("newAlias", goodAlias)
        })
        
        # Properties ####
        # The essential observers that keep the pipelineProperties synchronized are defined within 
        # the renderUI function that creates the inputs.
        
        # Precheck ####
        output$bljVersion <- renderText( bljVer() )
        
        observe({
            # input$AddBioModule
            shinyjs::disable("checkPrecheck")
            shinyjs::disable("updateJar")
            if ( !hasDockerCmd() ) {
                updateCheckboxInput(session, "checkDocker", value = FALSE)
                shinyjs::disable("checkDocker")
                if (isInDocker()){
                    shinyjs::disable("biolockjJarFile")
                }
            }
        })
        
        observeEvent(input$extModsDir, {
            shinyjs::enable("updateJar")
        })
        
        output$precheckCommand <- renderText(precheckCommand())
        
        precheckRestultText <- reactiveVal(c("Precheck results will appear here."))
        output$precheckOutput <- renderUI({
            do.call(pre, as.list( precheckRestultText() ))
        })
        
        observeEvent(input$checkDocker, {
            if ( ! input$checkDocker) {
                updateCheckboxInput(session, "checkMapBlj", value = FALSE)
                shinyjs::disable("checkMapBlj")
            }else{
                shinyjs::enable("checkMapBlj")
            }
        })
        
    } # end of Server ####
    return(server)
}