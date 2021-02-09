#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
source('propertiesDynamicUI.R')


####################################################################################################
#############################      initial JAVA calls      #########################################
####################################################################################################
# Run these calls before starting so these bulk values are available.
# IFF the user chooses to change the BioLockJ jar file, (which will a rare thing)
# then the reactive values that store these are updated.

initbljVer = biolockjVersion()
initpropInfo <- propInfoSansSpecials()
initmoduleInfo <- moduleInfo()
initGenDefaults <- lapply(initpropInfo, function(prop){ prop$default })

####################################################################################################
#############################              UI              #########################################
####################################################################################################

ui <-  fluidPage( 
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    navbarPage(
        position = "fixed-top",
        theme = shinythemes::shinytheme("cerulean"),
        "BioLockJ",
        tabPanel("Home",
                 tags$style(".shiny-input-container {margin-bottom: 0px} #existingConfig_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 tags$style(".shiny-input-container {margin-bottom: 0px} #defaultPropsFiles_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 tags$style(".shiny-input-container {margin-bottom: 0px} #projectRootDir_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 p("navbar"),p("spacer"),
                 h1("BioLockJ Pipeline Builder"),
                 tabsetPanel(
                     tabPanel("Save to file",
                              br(),
                              textInput("projectName", "Project name", value="myPipeline", placeholder = "new project name"),
                              checkboxInput("include_standard_defaults", "include values that match defaults"),
                              checkboxInput("include_biolockj_version", "include BioLockJ version"),
                              shinyjs::disabled(checkboxInput("checkRelPaths", "write relative file paths")),
                              downloadButton("downloadData", "Save config file"),
                              uiOutput("configText")),
                     tabPanel("Load from file",
                              br(),
                              em("(optional)"),
                              p(em("When you pull values from an existing file, the values from the file will replace anything configured here.")),
                              fileInput("existingConfig", label="Upload an existing config file", accept = c(".properties", ".config")),
                              actionButton("populateExistingConfig", "pull values"))
                 )
        ),
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
        tabPanel("Defaults",
                 p("navbar"),p("spacer"),
                 fluidPage(
                     h2("Chain default properties"),
                     em("(optional)"),
                     p(em("The property 'pipeline.defaultProps=[file]' allows you to use property values from another file. That file may also include a reference to another file creating a chain.  When you run the pipeline, BioLockJ puts all the properties together. Properties defined in multiple files are set according the most recent value in the chain.")),
                     h4("Locate defaults"),
                     em("This does not affect your configuration."),
                     br(),br(),
                     fileInput("defaultPropsFiles", label="upload default properties files", accept = c(".properties")),
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
        tabPanel("Settings",
                 p("navbar"),p("spacer"),
                 fluidPage(
                     h2("Settings"),
                     p("Control aspects of this user interface."),
                     h4("Project Root Directory"),
                     em("(recommended)"),
                     p(em("File paths can be shown relative to the project root directory.")),
                     # checkboxInput("checkRelPaths", "write relative file paths"),
                     fileInput("projectRootDir", label="Project Root", width = "100%"),#TODO accept directory
                     h4("File access"),
                     radioButtons("radioServerType", "How should this interface access files?", 
                                  choiceNames = list("remote server", "local virtual server", "local machine"), 
                                  choiceValues = c("remote", "virtual", "local"), inline=TRUE, 
                                  selected = ifelse(isInDocker(), "remote", "local")),
                     br(),
                     h4("Core"),
                     strong("BioLockJ version: "),
                     # p("Currently referencing BioLockJ version:"),
                     textOutput("bljVersion"),
                     br(),
                     fluidRow(
                         column(6, fileInput("biolockjJarFile", "BioLockJ Jar File Location", accept=c(".jar"))),
                         column(6, actionButton("updateJar", "update jar location", class="btn-danger", style = "margin-top: 25px;"))),
                     textOutput("textWarningOnUpdateJar")
                 )),
        tabPanel("Help",
                 p("navbar"),p("spacer"),
                 includeMarkdown("HelpPage.md"))
    )
) # end of UI


####################################################################################################
#############################            SERVER            #########################################
####################################################################################################

server <- function(input, output, session) {
    
    ####################################################################################################
    #############################         Core Objects         #########################################
    ####################################################################################################
    ## Use reactive objects as the single source of truth.
    
    values <- reactiveValues(defaultProps=NA,
                             moduleList=list(), 
                             customProps=list(), 
                             removedModules=list(),
                             ### IMPORTANT !
                             # GET the property values through this object; the pipelineProperties reactiveValues object
                             # SET the property values through the input$[propName] object
                             # an observeEvent ensures the flow of info from the input$ to the reactiveValues
                             pipelineProperties=initGenDefaults, # TODO: maybe change name to backboneProperties
                             moduleProperties=modulePerProp(initmoduleInfo))
    
    defaults <- reactiveValues(
        defaultPropsChain = list(),
        defaultPropsList = list(standard=initGenDefaults),
        values = initGenDefaults,
        uploadedFiles = data.frame(name=c(), size=c(), type=c(), datapath=c()),
        activeFiles = c()
    )

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
    
    ### These are usually only run one time; 
    # Start with the pre-run init* values.
    # (TODO: add that dependency)

    jarFilePath <- reactiveVal("BioLockJ/dist/BioLockJ.jar")
    
    bljVer <- reactiveVal(initbljVer)
    
    allModuleInfo <- reactiveVal(initmoduleInfo)
    
    # map module short name to run line syntax
    moduleRunLines <- reactiveVal(getModuleRunLines(initmoduleInfo))
    
    genPropInfo <- reactiveVal(initpropInfo)
    
    # break out properties into categories
    groupedProps <- reactiveVal( groupPropsByCategory(initpropInfo) )
    
    ####################################################################################################
    #############################         Dynamic UI           #########################################
    ####################################################################################################
    # Defining the UI.  This would be in the ui function... but its dynamic.
    
    # Home
    output$configText <- renderUI({
        do.call(pre, as.list(configLines()))
    })
    
    # Defaults
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
    
    # Modules
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

    
    # Properties
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
    
    output$modulePropsHeader <- renderText("The properties for a given module include the properties that are specific to that module, as well as any general properties that the module is known to reference.")
    
    # Precheck
    

    
    ####################################################################################################
    #############################       Button Actions         #########################################
    ####################################################################################################
    # define event handlers for buttons
    
    
    # Home
    observeEvent(input$populateExistingConfig, {
        input$AddModuleButton
        message("The button got pushed: populateExistingConfig")
        req( input$existingConfig )
        populateModules()
        populateProps()
        populateProjectName()
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(input$projectName, ".config")
        },
        content = function(file) {
            writeLines(configLines(), file)
        }
    )
    
    # Defaults
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
    
    # Module
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
    
    # Properties
    observeEvent(input$addCostomPropBtn, {
        message("The button was pushed! button: addCostomPropBtn")
        req(input$customPropName)
        req(trimws(input$customPropName) != "")
        req(input$customPropVal)
        values$customProps[[input$customPropName]] <- input$customPropVal
        updateTextInput(session, "customPropName", value = "")
        updateTextInput(session, "customPropVal", value = "")
    })
    
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
    
    observeEvent(input$runPrecheckBtn, {
        message("Running command:")
        message(precheckCommand())

        if ( length(values$moduleList) == 0 ) validate("You need to include at least one module")
        configFileName = paste0(input$projectName, ".config")
        tempFileLoc = file.path(getwd(),"temp", configFileName)
        message("writing to file: ", tempFileLoc)
        writeLines(configLines(), tempFileLoc)
        
        # short term cheats - TODO
        command = gsub(pattern=configFileName, replacement = tempFileLoc, precheckCommand())
        defaultBljProj=paste("--precheck-only", "--blj_proj", file.path(getwd(), "BioLockJ", "pipelines"))
        command = gsub("--precheck-only", defaultBljProj, command)
        message("...actually running command:")
        message(command)
        #
        precheckRestultText(system2("exec", args = command, stdout = TRUE, stderr = TRUE))
    })
    
    ####################################################################################################
    #############################           Actions            #########################################
    ####################################################################################################
    # reactive expressions that are intuitively like functions
    
    existingLines <- reactiveVal()
    
    observeEvent(input$existingConfig, {
        existingLines( readLines( input$existingConfig$datapath ) )
    })
    observeEvent(input$populateExistingConfig, {
        existingLines( readLines( input$existingConfig$datapath ) )
    })
    
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
    
    populateProjectName <- reactive({
        newName = tools::file_path_sans_ext(input$existingConfig$name)
        updateTextInput(session, "projectName", value=newName)
    })
    
    configLines <- reactive({
        lines = c()
        if (input$include_biolockj_version){
            lines = c(lines, paste("# Updated using BioLockJ version:", bljVer()))
        }
        #
        lines = c(lines, "")
        lines = c(lines, values$moduleList)
        lines = c(lines, "")
        #
        if ( !is.na(values$defaultProps) && length(values$defaultProps) > 0 ){
            lines = c(lines, writeConfigProp("pipeline.defaultProps", values$defaultProps, "list"))
            lines = c(lines, "")
        }
        if ( length(values$customProps) > 0 ){
            lines = c(lines, "# Custom Properties")
            for(cp in names(values$customProps)){
                line = paste(cp, "=", values$customProps[[cp]])
                lines = c(lines, line)
            }
            lines = c(lines, "")
        }
        lines = c(lines, "# General Properties")
        for(p in names(genPropInfo())){
            value = values$pipelineProperties[[p]]
            line = writeConfigProp(p, value, genPropInfo()[[p]]$type)
            if ( !is.null(value) && !is.na(value) && length(value) > 0 && nchar(value) > 0 ){
                notTheDefault = !is.null(defaults$values[[p]]) && value != defaults$values[[p]]
                # message("value of property ", p, "=", value, " is ", ifelse(notTheDefault, "NOT", ""), " the same as the default value: ", defaults$values[[p]])
                if ( is.null(defaults$values[[p]]) || notTheDefault || input$include_standard_defaults ){
                    lines = c(lines, line)
                }
            }
        }
        lines
    })
    
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

    
    
    ####################################################################################################
    #############################           Synchrony          #########################################
    ####################################################################################################
    # Reactive espressions and observers that serve to keep things smooth and synchronized.
    # These make the app nice, but they are not fundamental to the understanding of the layout and workings.
    
    # Home
    observeEvent(input$projectRootDir, {
        if(isTruthy(input$projectRootDir)) {
            shinyjs::enable("checkRelPaths")
            removeTooltip(session, "checkRelPaths")
        }else{
            updateCheckboxInput("checkRelPaths", value = FALSE)
            shinyjs::disable("checkRelPaths")
            addTooltip(session, "checkRelPaths", title="requires Project Root Directory")
        }
    })
    
    # Modules
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
    
    # Properties
    # The essential observers that keep the pipelineProperties synchronized are defined within 
    # the renderUI function that creates the inputs.
    
    # Precheck
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
    
}



# Run the application 
shinyApp(ui = ui, server = server)
