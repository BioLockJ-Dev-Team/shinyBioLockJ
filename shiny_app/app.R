# Header ####
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#############################          Libraries          #########################################
library(shiny)
library(shinyjs)
# install.packages("shinythemes")
library(shinythemes)
# install.packages("shinyBS")
library(shinyBS)
# install.packages("sortable")
library(sortable)
# install.packages('shinyFiles')
library(shinyFiles)
library(shinyFeedback)
source('biolockj.R')
source('biolockj_gui_bridge.R')
# source('propertiesDynamicUI.R')


#############################      initial JAVA calls      #########################################
# Run these calls before starting so these bulk values are available.
# IFF the user chooses to change the BioLockJ jar file, (which will a rare thing)
# then the reactive values that store these are updated.

initbljVer = biolockjVersion()
initpropInfo <- propInfoSansSpecials()
initmoduleInfo <- moduleInfo()
initGenDefaults <- lapply(initpropInfo, function(prop){ prop$default })
initFilePathType <- propsInfoForType(initpropInfo, "file path")
initFileListType <- propsInfoForType(initpropInfo, "list of file paths")

volumes <- c(Home = fs::path_home())

#############################              UI              #########################################

ui <-  fluidPage( 
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    navbarPage(
        position = "fixed-top",
        theme = shinythemes::shinytheme("cerulean"),
        "BioLockJ",
        # Home ####
        tabPanel("Home",
                 tags$style(".shiny-input-container {margin-bottom: 0px} #existingConfig_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 tags$style(".shiny-input-container {margin-bottom: 0px} #defaultPropsFiles_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 tags$style(".shiny-input-container {margin-bottom: 0px} #projectRootDir_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 p("navbar"),p("spacer"),
                 h1("BioLockJ Pipeline Builder"),
                 tabsetPanel(id="HomeTabs",
                     tabPanel("Save to file",
                              br(),
                              textInput("projectName", "Project name", value="myPipeline", placeholder = "new project name"),
                              p(),
                              checkboxInput("include_standard_defaults", "include values that match defaults"),
                              checkboxInput("include_biolockj_version", "include BioLockJ version"),
                              shinyBS::tipify(checkboxInput("include_mid_progress", "include work-in-progress"), "include commented list of modules in Trash and their properties, See Modules", placement='right'),
                              shinyBS::tipify(shinyjs::disabled(checkboxInput("checkRelPaths", "write relative file paths")), "requires project root directory", placement='right'),
                              #
                              fluidRow(
                                  column(3, shinyFiles::shinyDirButton("projectRootDir", "Set Project Root Directory", "Select Project Root Directory")),
                                  column(9, verbatimTextOutput("showProjectDir", placeholder=TRUE))
                              ),
                              fluidRow(
                                  column(3, downloadButton("downloadData", "Download", width='80%')),
                                  column(9, uiOutput("saveButton"))
                              ),
                              p(),
                              verbatimTextOutput("configText", placeholder = TRUE)),
                     tabPanel("Load from file",
                              h2("Upoad an existing config file"),
                              em("(optional)"),
                              p(em("When you pull values from an existing file, the values from the file will replace anything configured here.")),
                              fileInput("uploadExistingConfig", label="Upload an existing config file", accept = c(".properties", ".config")),
                              shinyjs::disabled(actionButton("populateExistingConfig", "pull values")),
                              uiOutput("LocalExistingConfig"),
                              hr(),
                              uiOutput("examples")
                              )
                 )
        ),
        # Modules ####
        tabPanel("Modules",
                 p("navbar"),p("spacer"),
                 fluidPage(    
                     h2("BioModule Run Order"),
                     fluidRow(
                         column(5, uiOutput("selectModule")),
                         column(1, "AS", style = "margin-top: 30px;"),
                         column(4, textInput("newAlias", "", 
                                             placeholder = "alternative alias",
                                             width = '100%')),
                         column(2, actionButton("AddModuleButton", style = "margin-top: 20px;", "add to pipeline", class = "btn-success"))),
                     uiOutput("manageModules"),
                     actionButton("emptyModuleTrash", "Empty Trash"))),
        # Properties ####
        tabPanel("Properties",
                 splitLayout(
                     cellArgs = list(style='white-space: normal;'),
                     fluidPage(p("navbar"),p("spacer"),
                               h2("General Properties"),
                               checkboxInput("checkLiveFeedback", "show live feedback", value=FALSE),
                               p("General properties are not specific to any one module."),
                               tabsetPanel(
                                   tabPanel(
                                       "Backbone Properties",
                                       uiOutput("genProps")),
                                   tabPanel("Custom Properties",
                                            p("Add custom properties here. All property names must be unique."),
                                            p("Any property can reference the exact value of any other property, for example:"),
                                            p("prop2 = build on ${prop1}"),
                                            fluidRow(
                                                column(4, textInput("customPropName","property name")),
                                                column(5, textInput("customPropVal", "= value")),
                                                column(3, actionButton("addCostomPropBtn", "add", style = "margin-top: 25px;"))),
                                            h5("Current custom properties:"),
                                            uiOutput("showCustomProps"),
                                            br(),
                                            h5("Custom values in default properties files:"),
                                            em("These are not included as part of the current config file."),
                                            uiOutput("showDefaultCustomProps"))
                               )),
                     fluidPage(p("navbar"),p("spacer"),
                               h2("Module Properties"),
                               # uiOutput("modProps"),
                               p(),
                               textOutput("modulePropsHeader"))
                 )),
        # Precheck ####
        tabPanel("Precheck",
                 p("navbar"),p("spacer"),
                 sidebarLayout(
                     sidebarPanel(
                         # width=5,
                         h3("command options"),
                         checkboxInput("callJavaDirectly", "direct call to java", value=TRUE),
                         h4("flags"),
                         checkboxInput("checkPrecheck", "--precheck-only", value=TRUE),
                         checkboxInput("checkUnused", "--unused-props", value=FALSE),
                         checkboxInput("checkDocker", "--docker", value=FALSE),
                         # checkboxInput("checkAws", "--aws", value=FALSE),
                         checkboxInput("checkForground", "--foreground", value=FALSE),
                         checkboxInput("checkVerbose", "--verbose", value=FALSE),
                         checkboxInput("checkMapBlj", "--blj", value=FALSE),
                         h4("arguments"),
                         fileInput("extModsDir", "External Modules", placeholder = "optional"),
                         # render text "contains X additional jar files
                         fileInput("bljProjDir", "Projects (Output) Directory", placeholder = "$BLJ_PROJ")
                     ),
                     mainPanel(
                         # width=7,
                         h3("Test current configuration"),
                         p("Pass this config file to BioLockJ to build the pipeline and check dependencies. This does not actually execute the pipeline (because we include the --precheck-only flag), but it rus the initial phases to look for potential problems."),
                         h4("biolockj command:"),
                         verbatimTextOutput("precheckCommand"), 
                         actionButton("runPrecheckBtn", "Run Precheck", class = "btn-success"),
                         h4("command output:"),
                         uiOutput("precheckOutput")
                     )
                 )),
        # Defaults ####
        tabPanel("Defaults",
                 p("navbar"),p("spacer"),
                 fluidPage(
                     h2("Chain default properties"),
                     em("(optional)"),
                     p(em("The property 'pipeline.defaultProps=[file]' allows you to use property values from another file. That file may also include a reference to another file creating a chain.  When you run the pipeline, BioLockJ puts all the properties together. Properties defined in multiple files are set according the most recent value in the chain.")),
                     h4("Locate defaults"),
                     em("This does not affect your configuration."),
                     br(),br(),
                     fileInput("defaultPropsFiles", label="upload default properties files", multiple = TRUE, accept = c(".properties", ".config")),
                     checkboxInput("ignoreChain", "ignore pipeline.defaultProps in this file", value=FALSE),
                     tableOutput("chainableFiles"),
                     h4("Select defaults"),
                     em("This is what sets the pipeline.defaultProps for your current pipeline, and the defaults that are reflected throughout this configuration."),
                     shinyjs::disabled(selectInput("selectDefaultProps", "pipeline.defaultProps", choices = c(none=""), multiple=TRUE)),
                     tableOutput("chainedFiles"),
                     shinyjs::disabled(actionButton("loadDefaultProps", "set as defaults")),
                     h4("Current defaults"),
                     verbatimTextOutput("currentDefaultProps")
                 )),
        # Settings ####
        tabPanel("Settings",
                 p("navbar"),p("spacer"),
                 fluidPage(
                     h2("Settings"),
                     p("Control aspects of this user interface."),
                     h4("File access"),
                     shinyjs::disabled(radioButtons("radioServerType", "How should this interface access files?", 
                                  choiceNames = list("remote server", "local virtual server", "local machine"), 
                                  choiceValues = c("remote", "virtual", "local"), inline=TRUE, 
                                  selected = ifelse(isInDocker(), "remote", "local")) ),
                     br(), 
                     h4("Core"),
                     strong("BioLockJ version: "),
                     # p("Currently referencing BioLockJ version:"),
                     textOutput("bljVersion"),
                     br(),
                     fluidRow(
                         column(6, fileInput("biolockjJarFile", "BioLockJ Jar File Location", accept=c(".jar"), width='100%')),
                         column(6, actionButton("updateJar", "update jar location", class="btn-danger", style = "margin-top: 25px;"))),
                     textOutput("textWarningOnUpdateJar")
                 )),
        # Help ####
        tabPanel("Help",
                 p("navbar"),p("spacer"),
                 includeMarkdown("HelpPage.md"))
    )
) # end of UI ####


#############################            SERVER            #########################################

server <- function(input, output, session) {
    
    source('propertiesDynamicUI.R')
    
    #############################         Core Objects         #########################################

    ## Use reactive objects as the single source of truth.
    values <- reactiveValues(defaultProps=NA,
                             moduleList=list(), 
                             customProps=list(), 
                             removedModules=list(),
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

    jarFilePath <- reactiveVal("BioLockJ/dist/BioLockJ.jar")
    
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
    fileBins <- reactiveValues()
    

    #############################         Dynamic UI           #########################################
    # Defining the UI.  This would be in the ui function... but its dynamic.
    
    # Home ####
    output$configText <- renderPrint({
        cat(paste0(configLines(), collapse = "\n"))
    })
    
    # shinyFileSave(input, "saveConfigBtn", roots = volumes, session = session, restrictions = system.file(package = "base"))
    
    output$examples <- renderUI({
        examples = findExampleConfigs()
        tagList(
            h2("Pull from an example config file"),
            selectInput("selectExample", "Select an example", choices = examples, 
                        selected="BioLockJ/templates/myFirstPipeline/myFirstPipeline.config"),
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
            shinyFeedback::showFeedbackWarning("selectDefaultProps", paste("Missing file:", printListProp(chainInfo$missing)) )
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
        bucket_list(
            header="BioModule Run Order",
            add_rank_list(
                text = "Drag and drop to re-order",
                labels = values$moduleList,
                input_id = "orderModules",
                options = sortable_options(multiDrag = TRUE)),
            add_rank_list(
                text="Trash",
                labels = values$removedModules,
                input_id = "trashModules",
                options = sortable_options(multiDrag = TRUE)),
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
                             isGood = isolate(isValidProp(propName, input[[propUiName(propName)]]))
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
        argsList$selected = "input"
        argsList$id = "genPropsTabSet"
        do.call(tabsetPanel, argsList)
    })
    
    observeEvent(projectDirPath(), {
        if ( projectDirPath() == "" ){ myVolumes = volumes
        }else{ myVolumes = c(project=projectDirPath(), volumes) }
        for (prop in filePathProps()){
            buildFilePathPropObservers(session, input, output, prop$property, myVolumes, values)
        }
        for (prop in fileListProps()){
            buildFileListPropObservers(session, input, output, prop$property, myVolumes, values, fileBins)
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
                    updateTabsetPanel(session, "genPropsTabSet", selected = "ADD MORE")
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
    

    
    output$showProjectDir <- renderPrint(cat(projectDirPath()))
    
    observeEvent(input$projectRootDir,{
        if (is.integer(input$projectRootDir)) {
            message("No directory has been selected (shinyDirChoose)")
        } else {
            message("setting new value for project root...")
            projectDirPath( parseDirPath(volumes, input$projectRootDir) )
        }
    })
    

    #############################       Button Actions         #########################################
    # define event handlers for buttons
    
    # Home - save config ####
    
    shinyFiles::shinyDirChoose(input, "projectRootDir", roots = volumes, restrictions = system.file(package = "base"))

    output$downloadData <- downloadHandler(
        filename = function() {
            configFileName()
        },
        content = function(file) {
            writeLines(as.character(configLines()), file)
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
        writeLines(text = as.character(configLines()), saveAsPath())
        sessionHasSavedAs( c(isolate( sessionHasSavedAs() ), saveAsPath()))
    })

    # Home - load config ####
    output$LocalExistingConfig <- renderUI({
        if (input$radioServerType == "local"){
            shinyFileChoose(input, "LocalExistingConfig", roots = volumes, session = session)
            tagList(
                hr(),
                h2("Load an existing config file"),
                shinyFilesButton("LocalExistingConfig", "Choose a local config file", "Please select a file", multiple = FALSE, viewtype = "list"),
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
        prsd = parseFilePaths(volumes, input$LocalExistingConfig)
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
        projectDirPath( file.path(getwd(), dirname(input$selectExample) ) )
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
            shinyFeedback::showFeedbackDanger("selectDefaultProps", c("Missing file: ", printListProp(chainInfo$missing)))
        }else{
            # save state
            prevCheckBox = input$include_standard_defaults
            updateCheckboxInput(session, "include_standard_defaults", value=FALSE)
            tempFile = tempfile()
            writeLines(as.character(configLines()), tempFile)
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
        runLine = makeRunLine(input$AddBioModule, input$newAlias)
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
    observeEvent(input$updateJar, {
        req(input$biolockjJarFile)
        # TODO: show spinner or progress bar or something to let the user know that a delay is expected.
        # update objects from java
        jarFilePath(input$biolockjJarFile$datapath) #TODO: gather external path
        bljVer(biolockjVersion())
        allModuleInfo(moduleInfo()) 
        moduleRunLines(getModuleRunLines(allModuleInfo()))
        genPropInfo(propInfoSansSpecials())
        # TODO update defaults, including values of defaults$*[["standard"]]
        # restore button to disabled
        shinyjs::disable("updateJar")
        output$textWarningOnUpdateJar <- renderText("")
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
                    if (!is.null(input[[propName]])){
                        updateTextInput(session, propName, value = paste(vals[propName]))
                    }else{
                        values$pipelineProperties[[propName]] <- vals[propName]
                    }
                }else{
                    values$customProps[[propName]] <- vals[propName]
                }
            }
        }
    })
    
    configLines <- reactive({
        lines = c()
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
        command = ifelse(input$callJavaDirectly, paste("java -jar", jarFilePath()), "biolockj")#"biolockj"
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
        possibleLine = makeRunLine(input$AddBioModule, input$newAlias)
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
        runLine = makeRunLine(input$AddBioModule, input$newAlias)
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
        output$textWarningOnUpdateJar <- renderText("This could cause major changes.")
    })
    
    observeEvent(input$biolockjJarFile, {
        output$textWarningOnUpdateJar <- renderText("")
        req(input$biolockjJarFile)
        shinyjs::enable("updateJar")
        output$textWarningOnUpdateJar <- renderText("This could cause major changes.")
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

#############################            Notes            #########################################

# Collapse all sections: cmd + alt + O
# Expand all sections: shift + cmd + alt + O

# Run the application ####
shinyApp(ui = ui, server = server)
