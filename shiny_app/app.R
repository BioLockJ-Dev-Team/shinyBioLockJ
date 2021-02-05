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
# install.packages("sortable")
library(sortable)
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
        theme = shinytheme("cerulean"),
        "BioLockJ",
        tabPanel("Home",
                 tags$style(".shiny-input-container {margin-bottom: 0px} #existingConfig_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 tags$style(".shiny-input-container {margin-bottom: 0px} #defaultPropsFiles_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 tags$style(".shiny-input-container {margin-bottom: 0px} #projectRootDir_progress { margin-bottom: 0px } .checkbox { margin-top: 0px}"),
                 p("navbar"),p("spacer"),
                 h1("BioLockJ Pipeline Builder"),
                 sidebarLayout(
                     sidebarPanel(
                         width = 5,
                         h4("Project Root Directory"),
                         em("(recommended)"),
                         p(em("File paths can be shown relative to the project root directory.")),
                         checkboxInput("checkRelPaths", "write relative file paths"),
                         fileInput("projectRootDir", label="Project Root", width = "100%"),#TODO accept directory
                         h4("Chain default properties"),
                         em("(optional)"),
                         p(em("The property 'pipeline.defaultProps=[file]' allows you to use property values from another file. That file may also include a reference to another file creating a chain.  When you run the pipeline, BioLockJ puts all the properties together. Properties defined in multiple files are set according the most recent value in the chain.")),
                         fileInput("defaultPropsFiles", label="default properties files", accept = c(".properties"), width = "100%"),
                         fluidRow(
                             column(5, actionButton("chainDefaults", "chain defaults", width = '100%')),
                             column(5, actionButton("loadDefaultProps", "set as defaults", width = '100%'))
                         ),
                         h4("Load from file"),
                         em("(optional)"),
                         p(em("When you pull values from an existing file, the values from the file will replace anything configured here.")),
                         fileInput("existingConfig", label="Upload an existing config file", accept = c(".properties", ".config"), width = "100%"),
                         actionButton("populateExistingConfig", "pull values")
                     ),
                     mainPanel(
                         width = 7,
                         h4("Save to file"),
                         textInput("projectName", "Project name", value="myPipeline", placeholder = "new project name"),
                         checkboxInput("include_standard_defaults", "include defaults"),
                         checkboxInput("include_biolockj_version", "include BioLockJ version"),
                         downloadButton("downloadData", "Save config file"),
                         uiOutput("configText")))
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
                               p("General properties are not specific to any one module."),
                               uiOutput("genProps")),
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
                         checkboxInput("callJavaDirectly", "direct call to java", value=FALSE),
                         h4("flags"),
                         checkboxInput("checkPrecheck", "--precheck", value=TRUE),
                         checkboxInput("checkUnused", "--unused-props", value=FALSE),
                         checkboxInput("checkDocker", "--docker", value=FALSE),
                         # checkboxInput("checkAws", "--aws", value=FALSE),
                         checkboxInput("checkForground", "--foreground", value=FALSE),
                         checkboxInput("checkVerbose", "--verbose", value=FALSE),
                         h4("arguments"),
                         fileInput("extModsDir", "External Modules", placeholder = "optional"),
                         # render text "contains X additional jar files
                         fileInput("bljProjDir", "Projects (Output) Directory", placeholder = "$BLJ_PROJ"),
                         h4("core"),
                         strong("BioLockJ version"),
                         p("Currently referencing BioLockJ version:"),
                         verbatimTextOutput("bljVersion"),
                         fileInput("biolockjJarFile", "BioLockJ Jar File Location", accept=c(".jar")),
                         actionButton("updateJar", "update jar location", class="btn-danger"),
                         textOutput("textWarningOnUpdateJar")
                     ),
                     mainPanel(
                         # width=7,
                         h3("Test current configuration"),
                         p("Pass this config file to BioLockJ to build the pipeline and check dependencies. This does not actually execute the pipeline (because we include the --precheck flag), but it rus the initial phases to look for potential problems."),
                         h4("biolockj command:"),
                         verbatimTextOutput("precheckCommand"), 
                         actionButton("runPrecheckBtn", "Run Precheck", class = "btn-success"),
                         h4("command output:"),
                         verbatimTextOutput("precheckOutput")
                     )
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
    
    values <- reactiveValues(moduleList=list(), 
                             customProps=list(), 
                             removedModules=list(),
                             ### IMPORTANT !
                             # GET the property values through this object; the pipelineProperties reactiveValues object
                             # SET the property values through the input$[propName] object
                             # an observeEvent ensures the flow of info from the input$ to the reactiveValues
                             pipelineProperties=initGenDefaults )
    
    ### These are usually only run one time; 
    # Start with the pre-run init* values.
    # (TODO: add that dependency)

    jarFilePath <- reactiveVal("jar/BioLockJ.jar")
    
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
        argsList =  lapply(as.list(names(groupedProps())), function(groupName){
            group = groupedProps()[[groupName]]
            tabPanel(groupName,
                     p(paste("See the user guide for more info about", groupName, "properties.", collaps=" ")),
                     lapply(group, function(propName){
                         prop = genPropInfo()[[propName]]
                         propUI <- renderPropUi(propName, prop, default=isolate(values$pipelineProperties[[propName]]))
                         observeEvent(input[[propName]],{
                             values$pipelineProperties[[propName]] <- input[[propName]]
                         })
                         propUI
                     }))
        })
        argsList[[length(argsList)+1]] <- tabPanel("ADD MORE",
                                                   p("Add custom properties here. All property names must be unique."),
                                                   p("Any property can reference the exact value of any other property, for example:"),
                                                   p("prop2 = build on ${prop1}"),
                                                   fluidRow(
                                                       column(4, textInput("customPropName","property name")),
                                                       column(5, textInput("customPropVal", "= value")),
                                                       column(3, actionButton("addCostomPropBtn", "add", style = "margin-top: 25px;"))
                                                   ),
                                                   lapply(names(values$customProps), function(cp){
                                                       message("creating option to remove property: ", cp)
                                                       buttonId=paste0("rm-", cp)
                                                       customPropUi <- fluidRow(
                                                           column(9, renderText(paste(cp, "=", values$customProps[[cp]]))),
                                                           column(3, actionButton(buttonId, "remove")))
                                                       observeEvent(input[[buttonId]],{
                                                           values$customProps[[cp]] <- NULL
                                                           updateTabsetPanel(session, "genPropsTabSet", selected = "ADD MORE")
                                                       })
                                                       customPropUi
                                                   })
        )
        argsList$selected = "input"
        argsList$id = "genPropsTabSet"
        do.call(tabsetPanel, argsList)
    })
    
    output$modulePropsHeader <- renderText("The properties for a given module include the properties that are specific to that module, as well as any general properties that the module is known to reference.")
    
    # Precheck
    

    
    ####################################################################################################
    #############################       Button Actions         #########################################
    ####################################################################################################
    # define event handlers for buttons
    
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
    
    
    observeEvent(input$addCostomPropBtn, {
        message("The button was pushed! button: addCostomPropBtn")
        values$customProps[[input$customPropName]] <- input$customPropVal
        updateTabsetPanel(session, "genPropsTabSet", selected = "ADD MORE")
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
        # TODO update defaults
        # restore button to disabled
        shinyjs::disable("updateJar")
        output$textWarningOnUpdateJar <- renderText("")
    })
    
    observeEvent(input$runPrecheckBtn, {
        message("Running command:")
        message(precheckCommand())
        precheckRestultText( system( precheckCommand(), intern=TRUE) )
    })
    
    ####################################################################################################
    #############################           Actions            #########################################
    ####################################################################################################
    # reactive expressions that are intuitively like functions
    
    existingLines <- reactive({
        readLines( input$existingConfig$datapath )
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
        existingLines <- existingLines()
        exProps = existingLines[ grep("^#", existingLines, invert = TRUE) ]
        if ( any(grepl("=", exProps)) ){
            splits = strsplit(exProps, split="=", fixed=TRUE)
            splits = splits[which(sapply(splits, function(s){length(s) >= 2}))]
            vals = sapply(splits, function(pair){trimws(paste0(pair[2:length(pair)], collapse=""))})
            names(vals) = sapply(splits, function(pair){trimws(pair[1])})
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
        if ( length(values$customProps) > 0 ){
            lines = c(lines, "# Custom Properties")
            for(cp in names(values$customProps)){
                line = paste(cp, "=", values$customProps[[cp]])
                lines = c(lines, line)
            }
            lines = c(lines, "")
        }
        #
        lines = c(lines, "# General Properties")
        for(p in names(genPropInfo())){
            value = values$pipelineProperties[[p]]
            line = writeConfigProp(p, value, genPropInfo()[[p]]$type)
            if ( !is.null(value) && !is.na(value) && length(value) > 0 && nchar(value) > 0 ){
                notTheDefault = !is.null(genPropInfo()[[p]]$default) && value != genPropInfo()[[p]]$default
                if ( is.null(genPropInfo()[[p]]$default) || notTheDefault || input$include_standard_defaults ){
                    lines = c(lines, line)
                }
            }
        }
        lines
    })
    
    # Precheck
    precheckCommand <- reactive({
        command = ifelse(input$callJavaDirectly, paste("java -jar", jarFilePath()), "biolockj")#"biolockj"
        if (input$checkPrecheck) command = paste(command, "--precheck")
        if (input$checkUnused) command = paste(command, "--unused-props")
        if (input$checkDocker) command = paste(command, "--docker")
        # if (input$checkAws) command = paste(command, "--aws")
        if (input$checkForground) command = paste(command, "--foreground")
        if (input$checkVerbose) command = paste(command, "--verbose")
        # if (input$extModsDir) command = paste(command, "--external-modules", input$extModsDir$name) #TODO this should use a path/to/dir
        # if (input$bljProjDir) command = paste(command, "--blj_proj", input$bljProjDir$name) #TODO this should use a path/to/dir
        command = paste0(command, " ", input$projectName, ".config")
        command
    })

    
    
    ####################################################################################################
    #############################           Synchrony          #########################################
    ####################################################################################################
    # Reactive espressions and observers that serve to keep things smooth and synchronized.
    # These make the app nice, but they are not fundamental to the understanding of the layout and workings.
    
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
            updateCheckboxInput("checkDocker", session, value = FALSE)
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
    
    precheckRestultBottomLine <- reactiveVal("bottom line...")
    precheckRestultText <- reactiveVal("results...")
    output$precheckOutput <- renderText(precheckRestultText())
    
}



# Run the application 
shinyApp(ui = ui, server = server)
