#' BioLockJ server.R
#'
#' @param input input=input
#' @param output output=output
#' @param session session=session
#'
#' @return server function
#'
#
#  PLEASE NOTE: this document makes use of code-folding separators.  
#  Any line that matches: "# * #### *" can be used to 'fold' the code.
#
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
    
    initbljVer =  tryCatch({
        BioLockR::biolockjVersion()
    }, error=function(...){""})
   
    initpropInfo <- tryCatch({
        propInfoSansSpecials()
    }, error=function(...){""})
    
    initmoduleInfo <- tryCatch({
        BioLockR::moduleInfo()
    }, error=function(...){""})
    
    initGenDefaults <- tryCatch({
        unlist( sapply(initpropInfo, function(prop){ prop$default }) )
    }, error=function(...){""})
    
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    
    #############################            SERVER            #########################################
    
    server <- function(input, output, session) {
        
        #############################         Core Objects         #########################################
        
        ## Use reactive objects as the single source of truth
        values <- reactiveValues(defaultProps=NA,
                                 # The module run order. character vector. Values are the BioModule run line; 
                                 # NOT NAMED.  Use sapply with aliasFromRunLine to stand-in for names
                                 moduleList=vector("character"), 
                                 # Like moduleList, but these modules are in the "trash", not currently part of the pipeline.
                                 # names may not be maintained
                                 removedModules=vector("character"),
                                 ### IMPORTANT !
                                 # GET the property values through this object; the generalProps reactiveValues object
                                 # SET the property values through the input$[propName] object
                                 # an observeEvent ensures the flow of info from the input$ to the reactiveValues
                                 generalProps=initGenDefaults, 
                                 # a named list, each element a length=1 character vector
                                 moduleProps=list(),
                                 # named list of length=1 character vectors of property values
                                 customProps=list()
        )
        
        defaults <- reactiveValues(
            # named character vector, names are property names
            values = initGenDefaults,
            # cummulative data frame of uploaded default props files
            uploadedFiles = data.frame(name=c(), size=c(), type=c(), datapath=c()),
            # named list; names are file basenames.
            # Each named element is a named vector of property names and values
            defaultPropsList = list(standard=initGenDefaults),
            # named list where names are the basename of an uploaded defaultProps file
            # and each element is a charactor vector giving the file path of any/all defaultProps 
            # files referenced by that file.
            defaultPropsChain = list(),
            # vector of defaultProps file names in the order in which they are loaded as defaults;
            # values correspond to names of defaults$defaultPropsList
            activeFiles = c()
        )
        
        projectDirPath <- reactiveVal("")
        
        jarFilePath <- reactiveVal( tryCatch({
            BioLockR::get_BLJ_JAR()
        }, error=function(...){""}) )
        
        extModsDir <- reactiveVal( NULL )
        
        bljProjDir <- reactiveVal( tryCatch({
            BioLockR::get_BLJ_PROJ()
        }, error=function(...){""}) )
        
        bljVer <- reactiveVal(initbljVer)
        
        allModuleInfo <- reactiveVal(initmoduleInfo)
        
        genPropInfo <- reactiveVal(initpropInfo)
        
        # Event Reactives around core ####
        
        moduleRunLines <- eventReactive(allModuleInfo(), {
            # map module short name to run line syntax
            getModuleRunLines(allModuleInfo())
        })
        
        activeModules <- eventReactive(list(values$moduleList, values$removedModules), {
            c(values$moduleList, values$removedModules)
        })
        
        pipelineModsPerProp <- eventReactive( list(allModuleInfo(), activeModules()),{
            moduleClasses = unlist(sapply(activeModules(), classFromRunline))
            names(moduleClasses) = unlist(sapply(activeModules(), aliasFromRunline))
            obj = modulePerProp( allModuleInfo(), moduleClasses )
        })
        
        pipelineModuleInfo <- eventReactive( list(allModuleInfo(), values$moduleList, values$removedModules, sharedModuleProps()), {
            # an object similar to BioLockR::moduleInfo(), BUT with a few differences
            # - the names of the elements are modules alias, rather than the class names 
            #   (thus, a single class may be represented many times)
            # - Each element of $properties has additional elements: ownership and override
            #   - override is the name of the override form of a property; example: Part2.scriptPath
            #   - ownership is the form of ownership this module has within this pipeline over this property
            #     (see applyPropOwnership), one of: general, shared, single.
            #     As other modules are removed, some properties may alternate between shared and single
            if ( BioLockR::hasReadableValue(activeModules())){
                moduleClasses = unlist(sapply(activeModules(), classFromRunline))
                pipelineModules = allModuleInfo()[moduleClasses]
                names(pipelineModules) = sapply(activeModules(), aliasFromRunline)
                applyPropOwnership(pipelineModules, sharedModuleProps(), genPropInfo() )
            }else{
                list()
            }
        })
        
        sharedModuleProps <- eventReactive(pipelineModsPerProp(), {
            if ( BioLockR::hasReadableValue(pipelineModsPerProp())){
                isShared = sapply(pipelineModsPerProp(), function(usedby){length(usedby)>1})
                multimoduleProps = names(pipelineModsPerProp())[isShared]
                shared = setdiff(multimoduleProps, names(values$generalProps))
                message("shared props include: ", shared)
                shared
            }else{
                vector("character")
            }
        })
        
        pipelineModuleProps <- eventReactive(pipelineModuleInfo(), {
            names = unlist(sapply(pipelineModuleInfo(), function(module){
                unlist(sapply(module$properties, function(prop){
                    c(prop$property, prop$override)
                }))
            }))
            names = unique(names[order(names)])
        })
        
        filePathProps <- eventReactive(genPropInfo(), {
            propsInfoForType(genPropInfo(), "file path")
            })

        fileListProps <- eventReactive( genPropInfo(), {
            propsInfoForType(genPropInfo(), "list of file paths")
        })
        
        groupedProps <- eventReactive(genPropInfo(), {
            # break out properties into categories
            groupPropsByCategory( genPropInfo() )
        })
        
        
        #############################         Dynamic UI           #########################################
        # Defining the UI.  This would be in the ui function... but its dynamic.
        
        # Modal - Please wait ####
        modal_please_wait <- modalDialog(title="Just a moment", 
                                         htmlOutput("updating"), 
                                         fade=FALSE, 
                                         easyClose = TRUE,
                                         footer = NULL)
        output$updating <- renderUI(HTML(paste(updatingText(), collapse="<br/>")))
        updatingText <- reactiveVal("Updating...")
        appendUpdatingText <- function(old, new){
            text = c(old, new)
            updatingText(text)
            return(text)
        }
        
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
        
        exampleConfigs <- eventReactive( jarFilePath(), findExampleConfigs())
        
        output$examples <- renderUI({
            examples = exampleConfigs()
            select = "BioLockJ/templates/myFirstPipeline/myFirstPipeline.config"
            if (examples=="") select = ""
            selectInput("selectExample", "Select an example", 
                        choices = examples, 
                        selected=select)
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
            message("chainInfo$chained: ", chainInfo$chained)
            message("basename(chainInfo$chained): ", basename(chainInfo$chained))
            message("names(defaults$defaultPropsChain): ", names(defaults$defaultPropsChain))
            message("chainLinks: ", chainLinks)
            df = utils::stack(defaults$defaultPropsChain[chainLinks])
            names(df) = c("linksTo", "file")
            df[,c("file", "linksTo")]
        })
        
        output$chainableFiles <- renderTable({
            message("defaults[['defaultPropsChain']] : ", defaults[["defaultPropsChain"]])
            message("defaults[['defaultPropsChain']] : ", utils::str(defaults[["defaultPropsChain"]]))
            if( length( defaults$defaultPropsChain ) > 0 ){
                from = defaults$defaultPropsChain
            }else{
                validate("If there is a chain of default properties, the links are listed here.")
            }
            df = utils::stack(from)
            names(df) = c("linksTo", "file")
            df[,c("file", "linksTo")]
        })
        
        output$currentDefaultProps <- renderPrint({
            if( BioLockR::hasReadableValue(values$defaultProps) ){
                cat( writeConfigProp("pipeline.defaultProps", values$defaultProps, "list") )
            }else{
                cat("# no pipeline.defaultProps")
            }
        })
        
        # Modules ####
        output$selectModule <- renderUI({
            selectInput("AddBioModule",
                        "Select new BioModule", 
                        names(moduleRunLines()), 
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
        
        output$moduleSummary <- renderUI({
            selMod = classFromRunline( moduleRunLines()[input$AddBioModule] )
            tagList(
                h3(input$AddBioModule),
                strong(markdownTextToHTML(allModuleInfo()[[selMod]]$description)),
                p(a("user guide", href=module_userguide_url(selMod))),
                p("class: ", selMod),
                h5("Details"),
                markdownTextToHTML(allModuleInfo()[[selMod]]$details)
            )
        })
        
        
        # Properties - backbone ####
        output$genProps <- renderUI({
            message("Rendering ui for slot genProps...")
            totalSteps = length( groupedProps() ) + 2
            withProgress({
                argsList =  lapply(as.list(names(groupedProps())), function(groupName){
                    message("Building property group: ", groupName)
                    incProgress(1 / totalSteps)
                    group = groupedProps()[[groupName]]
                    tabPanel(groupName,
                             div(style = "height:1000px; overflow-y: scroll", 
                             p(paste("See the user guide for more info about", groupName, "properties.", collaps=" ")),
                             lapply(group, function(propName){
                                 propUI <- renderPropUi(propName, 
                                                        genPropInfo()[[propName]], 
                                                        isolate(values$generalProps[propName]),
                                                        isolate(defaults))
                                 applyShinyFeedback(inputId=propUiName(propName), 
                                                    prop=genPropInfo()[[propName]], 
                                                    projPath=projectDirPath(),
                                                    input=input)
                                 propUI
                             })))
                }) 
                incProgress(1 / totalSteps)
                refreshFileChoosers()
                argsList$selected = "input"
                argsList$id = "genPropsTabSet"
                incProgress(1 / totalSteps)
                result = do.call(tabsetPanel, argsList)
                div(style="padding-left: 5px; padding-right: 0px;", result)
            }, message = "Rendering general properties...")
            result
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
                buildFileListPropObservers(session, input, output, prop$property, volumes, values) #myVolumes()
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
            df = utils::stack( sapply(names(defaults$values), function(p){strsplit(p, ".", fixed=TRUE)[[1]][1]}), stringsAsFactors=FALSE)
            names(df) = c("prefix", "propName")
            df$prefix = as.character(df$prefix)
            df$propName = as.character(df$propName)
            df$isCustom = TRUE
            message("df has ", nrow(df), " rows.")
            #
            # general property defaults are reflected in general property ui
            df[df$propName %in% names(values$generalProps), "isCustom"] <- FALSE
            # df = df[! df$propName %in% names(values$generalProps),]
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
        
        # Properties - modules ####
        
        output$modProps <- renderUI({
            message("Rendering ui for slot modProps...")
            totalSteps = length( values$moduleList ) + 2
            
            
            
            if ( length(values$moduleList) >0 ) {
                withProgress({
                    modules = isolate(values$moduleList)
                    names(modules) = sapply(modules, aliasFromRunline) #TODO: is this already like this?
                    
                    argsList =  lapply(as.list(names(modules)), function(moduleId){
                        message("Building properties for module: ", moduleId)
                        incProgress(1 / totalSteps)
                        thisModule = pipelineModuleInfo()[[moduleId]]
                        props = thisModule$properties
                        message("This module has ", length(props), " properties.")
                        lastKeyPropName = Find(f=function(p){p$isKey}, x=thisModule$properties, right=TRUE)$property
                        firstHeader = "Key Properties"
                        if (!BioLockR::isReadableValue(lastKeyPropName)) {
                            lastKeyPropName = ""
                            firstHeader = "Properties"
                        }
                        message("Determined the last key prop to be named: ", lastKeyPropName)
                        midBorderFun = function(){return(tagList( br(), br(), h3("Other Properties"), 
                                                                  em("General properties that apply to this module.  "),
                                                                  em("In some cases it is beneficial to override one or more of these to give specific values to use for just this module."), 
                                                                  br(), br() ))}
                        standardBorderFun = function(){tagList( hr() )}
                        tabPanel(moduleId,
                                 div(style = "height:1300px; overflow-y: scroll; padding-left: 0px; padding-right: 10px", 
                                     # div(style = "position: relative; bottom: 5px; height: auto; background-color: #D7FCF9; overflow-y: scroll; top: 5px;", #height:500px; top: 300px; 
                                     fluidPage(
                                         # style = "height:600px;background-color: #fee6ff;",
                                         br(),
                                         p("Module class: ", strong(classFromRunline(thisModule$usage) ), 
                                           br(),
                                           a("view in user guide", href=module_userguide_url(classFromRunline(thisModule$usage))) ),
                                         p("Module instance alias: ", strong(moduleId),
                                           br(), 
                                           #actionLink(paste0(moduleId, "changeId"), "change")  #TODO make this work
                                           ), 
                                         h3(firstHeader),
                                         lapply(props, function(prop){
                                             propName = prop$property
                                             if (propName %in% names(genPropInfo())){
                                                 nonOverrideValue = isolate( values$generalProps[propName] )
                                             }else{
                                                 nonOverrideValue = isolate( values$moduleProps[[propName]] )
                                             }
                                             propUI <- tabsetPanel(
                                                 id=propModuleFlipPanel(propName, moduleId), type = "hidden",
                                                 tabPanelBody("nonOverrideUi", 
                                                              renderPropUi(prop$property,
                                                                           prop,
                                                                           nonOverrideValue,
                                                                           isolate(defaults),
                                                                           ownership=prop$ownership,
                                                                           moduleId = moduleId,
                                                                           trailingUiFun=ifelse(propName==lastKeyPropName, midBorderFun, standardBorderFun)
                                                              )
                                                 ),
                                                 tabPanelBody("overrideUi", 
                                                              renderPropUi(prop$override,
                                                                           prop,
                                                                           isolate(values$moduleProps[[prop$override]]),
                                                                           isolate(defaults),
                                                                           ownership="override",
                                                                           moduleId = moduleId,
                                                                           trailingUiFun=ifelse(propName==lastKeyPropName, midBorderFun, standardBorderFun)
                                                              )
                                                 )
                                             )
                                         
                                             # main observers ####
                                             regUiId = propUiName(propName, moduleId)

                                             message("property ", prop$property, " in module ", moduleId, " has ownership: ", prop$ownership)
                                             if (prop$ownership == "general"){
                                                 propCategory = unlist(strsplit(prop$property, split = ".", fixed = TRUE))[1]
                                                 observeEvent(input[[propEditBtnId(propName, moduleId)]], {
                                                     message("The button was pushed! Button ", propEditBtnId(propName, moduleId))
                                                     message("Set the general props tab to category: ", propCategory)
                                                     updateTabsetPanel(session=session, "genPropsTabSet", selected = propCategory)
                                                     # thanks to sinterworp for their post here: https://www.reddit.com/r/rstats/comments/7fmkah/moving_focus_to_next_input_in_shiny/
                                                     session$sendCustomMessage("refocus",list(propUiName(propName)))
                                                 })
                                                 observeEvent(input[[propUiName(propName)]], {
                                                     value = input[[propUiName(propName)]]
                                                     default = defaults$values[propName]
                                                     if(BioLockR::isReadableValue(value)){
                                                         u = renderUI(p(strong(propName), "=", value ))
                                                     }else if(BioLockR::isReadableValue(default)){
                                                         u = renderUI(p(strong(propName), "=", em(default, style="color: gray;") ))
                                                     }else{
                                                         u = renderUI(p(strong(propName), em("has no value", style="color: gray;") ))
                                                     }
                                                     output[[propShowId(prop$property, moduleId)]] = u
                                                 })
                                             }else if (prop$ownership == "shared"){
                                                 observeEvent(input[[regUiId]], {
                                                     values$moduleProps[[propName]] = input[[regUiId]]
                                                     value = values$moduleProps[[propName]]
                                                     # message("Saw change in [", regUiId, "] need to update shared property")
                                                     sharedWith = setdiff(pipelineModsPerProp()[[propName]], moduleId)
                                                     for (otherModule in sharedWith){
                                                         if (input[[propUiName(propName, otherModule)]] != value){
                                                             # message("Updating so ui element [", propUiName(propName, otherModule), "] now has value: ", value)
                                                             updateSharedPropUi(session, prop, otherModule, value)
                                                         }
                                                     }
                                                 }
                                                 )
                                             }else if (prop$ownership == "single"){
                                                 observeEvent(input[[regUiId]], {
                                                     values$moduleProps[[propName]] = input[[regUiId]]
                                                 })
                                             }
                                             
                                             overrideUiId = propUiName(prop$override, moduleId)
                                             observeEvent(input[[overrideUiId]],{
                                                 if(BioLockR::isReadableValue(input[[overrideUiId]])){
                                                     values$moduleProps[[prop$override]] <- input[[overrideUiId]]
                                                 }
                                             })
                                             
                                             # shinyFiles backend ####
                                             if( prop$type == "file path" ){
                                                 buildFilePathPropObservers(session, input, output, prop$override, volumes, values, moduleId)
                                                 if (prop$ownership != "general"){
                                                     buildFilePathPropObservers(session, input, output, prop$property, volumes, values, moduleId) #myVolumes()
                                                 }
                                             }else if( prop$type == "list of file paths" ){
                                                 buildFileListPropObservers(session, input, output, prop$override, volumes, values, moduleId)
                                                 if (prop$ownership != "general"){
                                                     buildFileListPropObservers(session, input, output, prop$property, volumes, values, moduleId) #myVolumes()
                                                 }
                                             }
                                             
                                             
                                             # feedback ####
                                             
                                             applyShinyFeedback(inputId=regUiId, 
                                                                prop=prop, 
                                                                projPath=projectDirPath(),
                                                                moduleClass=isolate( classFromRunline(thisModule$usage) ), 
                                                                input=input)
                                             
                                             applyShinyFeedback(inputId=overrideUiId, 
                                                                prop=prop, 
                                                                projPath=projectDirPath(),
                                                                moduleClass=isolate( classFromRunline(thisModule$usage) ), 
                                                                input=input)
                                             
                                             # override flipper ####
                                             if (BioLockR::isReadableValue(isolate(values$moduleProps[[prop$override]]))){
                                                 updateTabsetPanel(session=session, propModuleFlipPanel(propName, moduleId), selected = "overrideUi")
                                             }
                                             
                                             observeEvent(input[[propOverrideBtnId(propName, moduleId)]], {
                                                 message("The button was pushed! Button ", propOverrideBtnId(propName, moduleId))
                                                 updateTabsetPanel(session=session, propModuleFlipPanel(propName, moduleId), selected = "overrideUi")
                                             })
                                             observeEvent(input[[propRmOverrideBtnId(propName, moduleId)]], {
                                                 message("The button was pushed! Button ", propRmOverrideBtnId(propName, moduleId))
                                                 updateTabsetPanel(session=session, propModuleFlipPanel(propName, moduleId), selected = "nonOverrideUi")
                                                 values$moduleProps[[prop$override]] <- NULL
                                             })
                                             # /end override flipper
                                             propUI
                                         }),
                                         p("end")
                                     )
                                 )
                        )
                    })
                    incProgress(1 / totalSteps)
                    # refreshFileChoosers()
                    argsList$id = "modPropsTabSet"
                    incProgress(1 / totalSteps)
                    result = do.call(tabsetPanel, argsList)
                }, message = "Rendering module properties...")
            }else{
                result=h4("No modules.")
            }
            result
        })
        
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
        shinyFiles::shinyFileChoose(input, "LocalExistingConfig", roots = volumes, session = session)
        
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
            if (input$serverType == "local") projectDirPath( dirname(localFilePath()) )
            else projectDirPath( "" )
            populateModules()
            populateProps()
            populateDefaultProps()
            populateProjectName( basename( localFilePath() ) )
            updateTabsetPanel(session, "HomeTabs", selected="Save to file")
        })
        
        observeEvent(input$populateUploadedConfig, {
            message("The button got pushed: populateUploadedConfig")
            req( input$uploadExistingConfig )
            existingLines( readLines( input$uploadExistingConfig$datapath ) )
            projectDirPath( "" )
            populateModules()
            populateProps()
            populateDefaultProps()
            populateProjectName(input$uploadExistingConfig$name)
            updateTabsetPanel(session, "HomeTabs", selected="Save to file")
        })
        
        observeEvent(input$populateExampleConfig, {
            message("The button got pushed, button: populateExampleConfig")
            existingLines( readLines( input$selectExample ) )
            projectDirPath( dirname(input$selectExample) )
            populateModules()
            populateProps()
            populateDefaultProps()
            populateProjectName( basename( input$selectExample ) )
            updateTabsetPanel(session, "HomeTabs", selected="Save to file")
        })
        
        # Defaults ####
        # a dataframe, such as that returned by inputFiles
        newDefaultPropsFiles <- reactiveVal()
        readNewDefaultProps = reactive({
            req( newDefaultPropsFiles() )
            for (i in 1:nrow( newDefaultPropsFiles() )){
                lines = readLines( newDefaultPropsFiles()$datapath[i] )
                allProps = BioLockR::extract_defautlProps(BioLockR::read_properties( lines ))
                if ( length(allProps$defaultProps)==0 || input$ignoreChain){
                    defaults$defaultPropsChain[newDefaultPropsFiles()$name[i]] = NA
                }else{
                    defaults$defaultPropsChain[[newDefaultPropsFiles()$name[i]]] = allProps$defaultProps
                }
                defaults$defaultPropsList[[newDefaultPropsFiles()$name[i]]] = allProps$properties
            }
            wasSelected = input$selectDefaultProps
            defaults$uploadedFiles <- rbind(defaults$uploadedFiles, newDefaultPropsFiles())
            if (input$serverType == "local") choices = defaults$uploadedFiles$datapath
            else choices = defaults$uploadedFiles$name
            names(choices) = basename(choices)
            updateSelectInput(session, "selectDefaultProps", choices = choices, selected=wasSelected)
            shinyjs::enable("selectDefaultProps")
            updateCheckboxInput(session, "ignoreChain", value=FALSE)
        })
        
        observeEvent(input$defaultPropsFiles,{
            message("Event: input$defaultPropsFiles")
            req(input$defaultPropsFiles)
            newDefaultPropsFiles( input$defaultPropsFiles )
            readNewDefaultProps()
        })
        
        shinyFiles::shinyFileChoose(input, "findLocalDefaultProps", roots = volumes, session = session)
        observeEvent( input$findLocalDefaultProps, {
            message("Event: input$findLocalDefaultProps")
            req( ! is.integer(input$findLocalDefaultProps) )
            newDefaultPropsFiles( shinyFiles::parseFilePaths(volumes, input$findLocalDefaultProps) )
            readNewDefaultProps()
        })
        
        observe({
            if (length(input$selectDefaultProps)==0){ 
                shinyFeedback::hideFeedback("selectDefaultProps") 
            }
        })
        
        observeEvent(input$loadDefaultProps, {
            message("Pushed button: loadDefaultProps")
            selectDefaultProps_value(input$selectDefaultProps)
            loadDefaultProps_action()
        })
        selectDefaultProps_value = reactiveVal( character(0) )
        loadDefaultProps_action <- reactive({
            message("Do the loadDefaultProps action...")
            chainInfo = orderDefaultPropFiles(start=selectDefaultProps_value(), chain=defaults$defaultPropsChain)
            if (length(chainInfo$dangling) > 0){
                shinyjs::disable("loadDefaultProps")
                shinyFeedback::showFeedbackDanger("selectDefaultProps", c("Missing file: ", BioLockR::printListProp(chainInfo$missing)))
            }else{
                # save state
                prevCheckBox = input$include_standard_defaults
                updateCheckboxInput(session, "include_standard_defaults", value=FALSE)
                defaults$activeFiles = chainInfo$chained
                save_modify_restore()
                # restore state
                values$defaultProps = selectDefaultProps_value()
                updateCheckboxInput(session, "include_standard_defaults", value=prevCheckBox)
                shinyFeedback::hideFeedback("selectDefaultProps")
            }
        })
        
       # Module ####
        observeEvent(input$AddModuleButton, {
            runLine = makeRunLine(input$AddBioModule, moduleRunLines(), input$newAlias)
            msg = utils::capture.output({
                goodAlias = isValidAlias(aliasFromRunline(runLine), allActiveAliases())
            }, type="message")
            shinyFeedback::feedbackDanger("newAlias", !goodAlias, msg)
            req(goodAlias)
            # names(runLine) = aliasFromRunline(runLine) #<- this messes up the drag-sorter ui
            values$moduleList <- c(isolate(input$orderModules), runLine)
            updateTextInput(session, "newAlias", value = "")
            #
            trueCustomProps = setdiff(names(values$customProps), names(values$moduleProps))
            claimProps = setdiff(names(values$customProps), trueCustomProps)
            if (length(claimProps) > 0){
                msg = paste0("Module ", aliasFromRunline(runLine), " claimed the following custom properties: ", paste0(claimProps, collapse=", "))
                showNotification(msg, type = "message")
                for (propName in claimProps){
                    values$moduleProps[[propName]] = values$customProps[[propName]]
                    values$customProps[[propName]] <- NULL
                }
            }
        })
        
        observeEvent(input$emptyModuleTrash, {
            ## why do this? Let moduleProps remember.
            ## It will mostly have no cost, and will sometimes be convenient
            # for (runLine in values$removedModule){
            #     alias = aliasFromRunline(runLine)
            #     module = isolate(pipelineModuleInfo()[[alias]])
            #     for (prop in module$properties){
            #         values$moduleProps[[prop$override]] <- NULL
            #         if (prop$ownersip=="single"){
            #             values$moduleProps[[prop$property]] <- NULL
            #         }
            #     }
            # }
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
            if ( length(values$moduleList) == 0 ) validate("You need to include at least one module")
            if ( BioLockR::isReadableValue(projectDirPath()) && input$serverType =="local" ){
                message("Running locally with a configured project root dir; using project root dir")
                tempFileLoc = tempfile(pattern = input$projectName, tmpdir = projectDirPath(), fileext = ".config")
            }else{
                tempFileLoc = file.path(getwd(),"temp", configFileName()) #TODO: use tempfile() here too
            }
            
            message("writing to file: ", tempFileLoc)
            writeLines(configLines(), tempFileLoc)
            
            tmpDir = tempdir(TRUE)
            message("tmpDir for BLJ_PROJ: ", tmpDir)
            command = gsub(pattern=configFileName(), replacement = tempFileLoc, precheckCommand())
            message("Running command:")
            message( command )
            args = unlist(strsplit( substring(command, 6), split = " "))
            precheckRestultText(system2("java", args = args, 
                                        stdout = TRUE, stderr = TRUE,
                                        env = paste0("BLJ_PROJ=", tmpDir)
                                        )
                                )
            #
            file.remove(tempFileLoc)
        })
        
        # Settings ####
        output$coreSettings <- renderUI({
            if ( bljVer()=="" ){
                jarBtnClass="btn-lg btn-success"
            }else{
                jarBtnClass=NULL
            }
            tagList(
                h4("BioLockJ jar file"),
                em("required"),
                textOutput("showCurrentJar"),
                shinyFiles::shinyFilesButton("biolockjJarFile", "Find BioLockJ Jar", "Choose jar file", 
                                             multiple=FALSE, class=jarBtnClass),
                shinyjs::disabled(actionButton("updateJar", "update", class=jarBtnClass)),
                verbatimTextOutput("showNewJarFile"),
                
                br(),
                h4("BioLockJ external modules"),
                em("optional"),
                p("This directory should contain one or more jar files with additional BioLockJ modules. If applicable in the precheck process, this value can be used for argument --external_modules ."),
                textOutput("showCurrentMods"),
                shinyFiles::shinyDirButton("extMods", "Find External Modules", "Choose directory", multiple=FALSE),
                shinyjs::disabled(actionButton("updateMods", "update")),
                verbatimTextOutput("showNewExtMods"),
                
                br(),
                h4("BioLockJ projects ouput directory"),
                em("optional"),
                p("This directory is where new pipleine instances will be created. If not set, precheck pipeline instances will be created in a temp folder.  In the precheck process, this value can be used for argument --blj_proj ."),
                textOutput("showCurrentProj"),
                shinyFiles::shinyDirButton("bljProjChooser", "Find Pipelines Directory", "Choose directory", multiple=FALSE),
                shinyjs::disabled(actionButton("updateProj", "update")),
                verbatimTextOutput("showNewBljProj"),
                
                br()
                
                )
        })
        
        output$showCurrentJar <- renderText(paste("Current location:", jarFilePath() ))
        output$showCurrentMods <- renderText(paste("Current location:", extModsDir() ))
        output$showCurrentProj <- renderText(paste("Current location:", bljProjDir() ))
        
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
            else (showNotification("There was a problem setting the jar path.", type="error"))
        })
        
        observeEvent(input$rememberSetNewJar, {
            removeModal()
            message("Switching to newJar path, for this and future sessions.")
            good = BioLockR::set_BLJ_JAR( newJar(), remember = TRUE, doublecheck = FALSE )
            if (good) respondToUpdateJar()
            else (showNotification("There was a problem setting the jar path.", type="error"))
        })
        
        respondToUpdateBljJar <- reactive({
            msg = appendUpdatingText("Updating BioLockJ jar file...", "")
            showModal(modal_please_wait)
            
            msg = appendUpdatingText(msg, "Updating jar file location...")
            jarFilePath( tryCatch({
                BioLockR::get_BLJ_JAR()
            }, error=function(...){""}) )
            msg = appendUpdatingText(msg, "done.")
            
            msg = appendUpdatingText(msg, "Updating biolockj version...")
            bljVer( tryCatch({
                BioLockR::biolockjVersion()
            }, error=function(...){""}) )
            msg = appendUpdatingText(msg, "done.")
            
            respondToUpdateJar()
        })
            
        
        respondToUpdateJar <- reactive({
            msg = isolate( updatingText() )
            msg = appendUpdatingText(msg, "")
            showModal(modal_please_wait)
            
            msg = appendUpdatingText(msg, "Updating available modules...")
            allModuleInfo( tryCatch({
                BioLockR::moduleInfo( externalModules=extModsDir() )
            }, error=function(...){""}) )
            
            updateSelectInput(session, "AddBioModule", choices=names(allModuleInfo()) )
            
            msg = appendUpdatingText(msg, "done.")
            
            msg = appendUpdatingText(msg, "Updating available properties...")
            genPropInfo( tryCatch({
                propInfoSansSpecials()
            }, error=function(...){""}) )
            msg = appendUpdatingText(msg, "done.")
            
            msg = appendUpdatingText(msg, "Updating property default values...")
            defaults$defaultPropsList[["standard"]] = namedDefaultVals( genPropInfo() )
            
            #TODO: update modules properties
            
            save_modify_restore()
            msg = appendUpdatingText(msg, "done.")
            
            msg = appendUpdatingText(msg, "Rendering properties...")
            message("Go to the Properties tab...")
            updateTabsetPanel(session, "topTabs", selected="Properties") 
            showModal(modal_please_wait)
            shinyjs::delay(500, {
                msg = appendUpdatingText(msg, "done.")
                msg = appendUpdatingText(msg, "")
                msg = appendUpdatingText(msg, "Done with update.")
                updateTabsetPanel(session, "topTabs", selected="Settings")
                showModal(modal_please_wait)
                shinyjs::delay(500, {
                    removeModal()
                    updatingText("Updating...")
                }) 
            })
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
            respondToUpdateMods()
        })
        
        observeEvent(input$rememberSetNewMods, {
            removeModal()
            message("Switching to new mods dir, for this and future sessions.---JK I have no way of remembering!")
            respondToUpdateMods()
        })
        
        respondToUpdateMods <- reactive({
            msg = appendUpdatingText("Updating external modules directory...", "")
            showModal(modal_please_wait)
            
            msg = appendUpdatingText(msg, "Updating external module dir setting...")
            # TODO: set up way to remember this preference
            extModsDir( newModsDir() )

            respondToUpdateJar()
        })
        
        observe({
            if ( BioLockR::isReadableValue( extModsDir() ) && file.exists(extModsDir() )) {
                shinyjs::enable("useExtMods")
            }else{
                updateCheckboxInput(session=session, "useExtMods", value=FALSE)
                shinyjs::disable("useExtMods")
            }
        })
        
        
        # Settings - BLJ_PROJ ####
        
        newProjDir = reactiveVal("")
        
        shinyFiles::shinyDirChoose(input, "bljProjChooser", roots = volumes)
        output$showNewBljProj <- renderPrint( cat(newProjDir()) )
        
        observeEvent(input$bljProjChooser, {
            req(input$bljProjChooser)
            
            if (!is.integer(input$bljProjChooser)){
                newProjDir( shinyFiles::parseDirPath(volumes, input$bljProjChooser) )
                message("you just chose folder: ", newProjDir() )
                shinyjs::enable("updateProj")
            }else{
                newProjDir( "" )
                message("you just chose no folder." )
                shinyjs::disable("updateProj")
            }
        })
        
        observeEvent(input$updateProj, {
            message("The button was pushed, the button updateProj")
            showModal(modal_confirm_proj)
        })
        
        modal_confirm_proj <- modalDialog(
            "This action does not affect the layout of the app. Saving this value so the same location is used in future sessions is recommended.",
            title = "Update pipeline output location ?",
            footer = tagList(
                actionButton("cancelProj", "Cancel"),
                actionButton("setNewProj", "Use new path (this time)"),
                actionButton("rememberSetNewProj", "Always use this path", class = "btn btn-success")
            )
        )
        
        observeEvent(input$cancelProj, {
            message("JK, I don't want to change the proj dir.")
            removeModal()
        })
        
        observeEvent(input$setNewProj, {
            message("Output will go to new BLJ_PROJ dir, for this session.")
            good = BioLockR::set_BLJ_PROJ( newProjDir(), remember = FALSE )
            removeModal()
            respondToUpdateProj()
        })
        
        observeEvent(input$rememberSetNewProj, {
            message("Output will go to new BLJ_PROJ dir, for this AND future sessions.")
            good = BioLockR::set_BLJ_PROJ( newProjDir(), remember = TRUE, doublecheck = FALSE )
            removeModal()
            respondToUpdateProj()
        })
        
        respondToUpdateProj <- reactive({
            bljProjDir( tryCatch({
                BioLockR::get_BLJ_PROJ()
            }, error=function(...){""}) )
            shinyjs::enable("useBljProj")
            updateCheckboxInput(session=session, "useBljProj", value=TRUE)
        })
        
        observe({
            if ( file.exists(bljProjDir() )) {
                shinyjs::enable("useBljProj")
            }else{
                updateCheckboxInput(session=session, "useBljProj", value=FALSE)
                shinyjs::disable("useBljProj")
            }
        })
        
        
        #############################           Actions            #########################################
        # reactive expressions that are intuitively like functions
        
        observeEvent(list(input$updateJar,
                          bljVer() ), {
                              id="badJarNoteId"
                              if ( bljVer()=="" ){
                                  showNotification(id=id, 
                                                   ui=tagList(strong("Cannot access BioLockJ!"), 
                                                               p("Correct the setting for BLJ_JAR.")),
                                                   type = "error", 
                                                   duration=0, 
                                                   closeButton = FALSE)
                                  updateTabsetPanel(session=session, inputId="topTabs", selected = "Settings")
                              }else{
                                  removeNotification(id=id) 
                              }
                          })
        
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
            message("Populating properties...")
            ppCounter=0
            newProps = BioLockR::extract_defautlProps(BioLockR::read_properties( existingLines() ))
            #values$defaultProps = newProps$defaultProps
            vals = newProps$properties
            message("...working with up to ", length(vals), " properties...")
            if (length(vals) > 0 ){
                for( propName in names(vals) ){
                    message("updating property value: ", propName, " = ", vals[propName])
                    if ( !BioLockR::hasReadableValue(vals[propName])){
                        message("Ignoring empty property.")
                    }else if ( propName %in% names(genPropInfo()) ){
                        ppCounter = ppCounter + 1
                        # If an input object is established, set through that
                        # otherwise, set value in generalProps object Directly
                        if (!is.null(input[[propUiName(propName)]])){
                            # most props, if rendered, go this way
                            updateTextInput(session, propName, value = paste(vals[propName]))
                        }else{ 
                            # file path props got this way, rendered or not
                            # all props, if NOT rendered, go this way
                            values$generalProps[propName] <- vals[propName]
                        }
                    }else if( propName %in% pipelineModuleProps() ){
                        values$moduleProps[[propName]] <- vals[propName]
                    }else{
                        values$customProps[[propName]] <- vals[propName]
                    }
                }
            }
            message("...done populating ", ppCounter, " properties.")
        })
        
        populateDefaultProps <- reactive({
            message("Consider default properties...")
            newProps = BioLockR::extract_defautlProps(BioLockR::read_properties( existingLines() ))
            if ( !BioLockR::hasReadableValue( values$defaultProps ) 
                 && !BioLockR::hasReadableValue( newProps$defaultProps )){
                message("No need to change the underlying defaults.")
            }else if ( BioLockR::hasReadableValue( values$defaultProps ) 
                       && BioLockR::hasReadableValue( newProps$defaultProps )
                       && values$defaultProps == newProps$defaultProps ){
                message("No need to change the underlying defaults.")
            }else{
                showModal(modal_confirm_defaults)
            }
        })
        
        modal_confirm_defaults <- modalDialog(
            uiOutput("defaultsModal"),
            title = "Update default props",
            footer = tagList(
                actionButton("clearAllDP", "Clear all defaults files", width = '100%'),br(),
                actionButton("keepOldDP", "Keep current defaults", width = '100%'),br(),
                actionButton("setNewDP", "Go to Defaults tab", width = '100%'),p()
            )
        )
        output$defaultsModal <- renderUI({
            currentSetting = values$defaultProps
            current = ifelse(BioLockR::hasReadableValue( currentSetting ), currentSetting, "none")
            newSetting = BioLockR::extract_defautlProps(BioLockR::read_properties( existingLines() ))$defaultProps
            new = ifelse(BioLockR::hasReadableValue( newSetting ), newSetting, "none")
            tagList(
                "The defaults used in the config file you have loaded are different the default file(s) currently set.",
                h4("current defaults come from:"),
                current,
                h4("config file uses defaults from:"),
                new
            )
        })
        observeEvent(input$clearAllDP, {
            message("The only defaults are the 'standard' defaults.")
            updateSelectInput(session=session, "selectDefaultProps", selected = character(0))
            selectDefaultProps_value( character(0) )
            loadDefaultProps_action()
            removeModal()
        })
        observeEvent(input$keepOldDP, {
            message("The config file was loaded onto the existing default properties files.")
            removeModal()
        })
        observeEvent(input$setNewDP, {
            updateTabsetPanel(session=session, "topTabs", selected="Defaults")
            files = BioLockR::extract_defautlProps(BioLockR::read_properties( existingLines() ))$defaultProps
            if (BioLockR::hasReadableValue(files)){
                fullPath = writeFilePathList(files, projectDirPath(), useRelPath=FALSE)
                modal_path_view <- modalDialog( fullPath, footer=actionButton("closeViewPath", "done"))
                observeEvent(input$closeViewPath, {
                    removeModal()
                })
                observeEvent(input$viewFullPath, {
                    showModal(modal_path_view)
                })
                # using input$projectName assumes that this notification is send as part of the process of loading a config file, 
                # which includes setting the project name to match the config file name.
                showNotification(tagList(
                    strong(paste("File", input$projectName, "had pipeline.defaultProps=")),
                    br(), 
                    fullPath), 
                    action=actionButton("viewFullPath", "view full path"),
                    duration = 120, closeButton = TRUE, session = session, type="message")
            }
            removeModal()
        })
        
        # Actions - configLines ####
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
            if ( BioLockR::hasReadableValue(values$generalProps) ){#TODO something better
                for(p in names(values$generalProps)){
                    value = values$generalProps[p]
                    if ( doIncludeProp(p, value, default=defaults$values[p], input=input) ){
                        line = writeConfigProp(p, value, genPropInfo()[[p]]$type, projectDirPath(), input$checkRelPaths)
                        lines = c(lines, line)
                    }
                }
            }
            if ( input$gather_shared_props && length(sharedModuleProps() > 0) && BioLockR::hasReadableValue(unlist(values$moduleProps[sharedModuleProps()])) ){
                lines = c(lines, "", "# Shared module properties")
                for( p in sharedModuleProps() ){
                    value = values$moduleProps[[p]]
                    if ( doIncludeProp(p, value, default=defaults$values[p], input=input) ){
                        propUsedBy = pipelineModsPerProp()[p]
                        message("propUsedBy: ", propUsedBy)
                        # exampleModuleClass = unlist(propUsedBy)[1]
                        # message("exampleModuleClass: ", exampleModuleClass)
                        # exampleModule = allModuleInfo()[[exampleModuleClass]]
                        exampleModuleAlias = unlist(propUsedBy)[1]
                        # message("exampleModuleAlias: ", exampleModuleAlias)
                        exampleModule = pipelineModuleInfo()[[exampleModuleAlias]]
                        #
                        # message("exampleModule: ", exampleModule)
                        exampleOfProp = exampleModule$properties[[p]]
                        # message("exampleOfProp: ", exampleOfProp)
                        type = exampleOfProp$type
                        # message("type: ", type)
                        line = writeConfigProp(p, value, type, projectDirPath(), input$checkRelPaths)
                        lines = c(lines, line)
                    }
                }
            }
            # Module properties
            for (runline in values$moduleList){
                al = aliasFromRunline(runline)
                alsProps = pipelineModuleInfo()[[al]]$properties
                if ( length(alsProps) > 0 ){
                    lines = c(lines, "", paste("#", al))
                    for (prop in alsProps){
                        if (BioLockR::hasReadableValue(values$moduleProps[[prop$override]])){
                            p = prop$override
                            value = values$moduleProps[[p]]
                            if ( doIncludeProp(p, value, default=defaults$values[p], input=input) ){
                                line = writeConfigProp(p, value, prop$type, projectDirPath(), input$checkRelPaths)
                                lines = c(lines, line)
                            }
                        }else if(prop$ownership == "general"){ 
                            # message("already wrote property: ", prop$property)
                        }else if(prop$ownership == "shared" && input$gather_shared_props){
                            # message("already wrote shared: ", prop$property)
                        }else{
                            p = prop$property
                            value = values$moduleProps[[p]]
                            if ( doIncludeProp(p, value, default=defaults$values[p], input=input) ){
                                line = writeConfigProp(p, value, prop$type, projectDirPath(), input$checkRelPaths)
                                lines = c(lines, line)
                            }
                        }
                    }
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
        
        save_modify_restore <- reactive({
            # save state
            tempFile = tempfile()
            message("Saving to temp file: ", tempFile) #TODO: remove
            writeLines(configLines(), tempFile)
            # modify underlying state
            defaults$values = vector("character")
            values$generalProps = vector("character")
            defaults$values = defaults$defaultPropsList[["standard"]]
            for (file in defaults$activeFiles){
                newProps = defaults$defaultPropsList[[file]]
                defaults$values[names(newProps)] = newProps
            }
            genPropsWithDefaults = intersect(names(defaults$values), names(genPropInfo()))
            for (propName in genPropsWithDefaults){
                values$generalProps[propName] = defaults$values[propName]
            }
            # restore state
            existingLines( readLines( tempFile ) )
            populateProps()
        })
        
        # Precheck
        precheckCommand <- reactive({
            command = ifelse(input$callJavaDirectly, paste("java -jar", jarFilePath()), "biolockj")
            if (input$checkPrecheck) command = paste(command, "--precheck-only")
            if (input$checkUnused) command = paste(command, "--unused-props")
            if (input$checkDocker) command = paste(command, "--docker")
            # if (input$checkAws) command = paste(command, "--aws")
            if (input$checkForground) command = paste(command, "--foreground")
            if (input$checkVerbose) command = paste(command, "--verbose")
            if (input$checkMapBlj) command = paste(command, "--blj")
            # if (input$extModsDir) command = paste(command, "--external-modules", input$extModsDir$name) #TODO this should use a path/to/dir
            if (bljProjDir() != "" && input$useBljProj) command = paste(command, "--blj_proj", bljProjDir())
            command = paste0(command, " ", input$projectName, ".config")
            command
        })
        
        
        
        
        #############################           Synchrony          #########################################
        # Reactive espressions and observers that serve to keep things smooth and synchronized.
        # These make the app nice, but they are not fundamental to the understanding of the layout and workings.
        
        observe({
            updateTabsetPanel(session=session, "localExistingConfig", selected = input$serverType)
            updateTabsetPanel(session=session, "localDefaultPropsFinder", selected = input$serverType)
        })
        
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
                shinyjs::enable("populateUploadedConfig")
            }else{
                shinyjs::disable("populateUploadedConfig")
            }
        })
        
        observeEvent(input$selectExample, {
            if (file.exists(input$selectExample)){
                shinyjs::enable("populateExampleConfig")
            }else{
                shinyjs::disable("populateExampleConfig")
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
            msg = utils::capture.output({
                goodAlias = isValidAlias(aliasFromRunline(runLine), allActiveAliases())
            }, type="message")
            shinyFeedback::feedbackSuccess("newAlias", goodAlias)
        })
        
        # Properties ####
        # The essential observers that keep the generalProps synchronized are defined within 
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