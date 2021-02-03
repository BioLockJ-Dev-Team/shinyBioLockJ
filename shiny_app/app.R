#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# install.packages("shinythemes")
library(shinythemes)
# install.packages("sortable")
library(sortable)
library(shinyFeedback)
source('biolockj.R')
source('biolockj_gui_bridge.R')
source('propertiesDynamicUI.R')


### get initial BioLockJ info
bljVer = biolockjVersion()
propInfo <- propInfo()
moduleInfo <- moduleInfo()

# remove special properties that are set separately or 
propInfo[["biolockj.version"]] <- NULL # only meant to be set by running biolockj
propInfo[["pipeline.defaultProps"]] <- NULL # gui needs to actually upload files
propInfo[["project.defaultProps"]] <- NULL # gui needs to actually upload files

# break out properties into categories
splits = strsplit(names(propInfo), split = ".", fixed = TRUE)
category = sapply(splits, function(s){s[1]})
groupedProps = split(names(propInfo), f=category)

# map module short name to run line syntax
moduleRunLines <- sapply(moduleInfo, function(mi){mi$usage})
names(moduleRunLines) <- names(moduleInfo)


####################################################################################################
#############################              UI              #########################################
####################################################################################################

ui <-  fluidPage( 
    shinyFeedback::useShinyFeedback(),
    navbarPage(
        position = "fixed-top",
        theme = shinytheme("cerulean"),
        "BioLockJ",
        tabPanel("Home",
                 p("navbar"),p("spacer"),
                 h1("BioLockJ Pipeline Builder"),
                 em("(optional)"),
                 p(em("When you pull values from an existing file, the values from the file will replace anything configured here.")),
                 fluidRow(
                     column(6, fileInput("existingConfig", label="Upload an existing config file", accept = c(".properties", ".config"), width = "100%")),
                     column(6, actionButton("populateExistingConfig", "pull values", style = "margin-top: 25px;"))
                 ),
                 textInput("projectName", "Project name", value="myPipeline", placeholder = "new project name"),
                 checkboxInput("include_standard_defaults", "include defaults"),
                 checkboxInput("include_biolockj_version", "include BioLockJ version"),
                 downloadButton("downloadData", "Save config file"),
                 uiOutput("configText")),
        tabPanel("Modules",
                 fluidPage(    
                     h2("BioModule Run Order"),
                     fluidRow(
                         column(5,selectInput("AddBioModule", 
                                              "Select new BioModule", 
                                              names(moduleInfo), 
                                              selected = "GenMod",
                                              width = '100%')),
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
                 )
        ),
        tabPanel("Data Flow",
                 p("navbar"),p("spacer"),
                 p("This panel is a placeholder tab.")),
        tabPanel("Help",
                 p("navbar"),p("spacer"),
                 includeMarkdown("HelpPage.md"))
    )
)


####################################################################################################
#############################            SERVER            #########################################
####################################################################################################

server <- function(input, output, session) {
    
    ####################################################################################################
    #############################         Core Objects         #########################################
    ####################################################################################################
    ## Use reactive objects as the single source of truth.
    # customProps <- reactiveValues()
    values <- reactiveValues(moduleList=list(), customProps=list(), removedModules=list())
    
    ### IMPORTANT !
    # GET the property values through this object; the pipelineProperties reactiveValues object
    # SET the property values through the input$[propName] object
    # an observeEvent ensures the flow of info from the input$ to the reactiveValues
    pipelineProperties <- do.call(reactiveValues, lapply(propInfo, function(prop){ prop$default }))


    ####################################################################################################
    #############################         Dynamic UI           #########################################
    ####################################################################################################
    # Defining the UI.  This would be in the ui function... but its dynamic.
    
    # Home
    output$configText <- renderUI({
        do.call(pre, as.list(configLines()))
    })
    
    # Modules
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
        argsList =  lapply(as.list(names(groupedProps)), function(groupName){
            group = groupedProps[[groupName]]
            tabPanel(groupName, 
                     p(paste("See the user guide for more info about", groupName, "properties.", collaps=" ")),
                     lapply(group, function(propName){
                         prop = propInfo[[propName]]
                         propUI <- renderPropUi(propName, prop, default=isolate(pipelineProperties[[propName]]))
                         observeEvent(input[[propName]],{
                             pipelineProperties[[propName]] <- input[[propName]]
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
    
    ####################################################################################################
    #############################       Button Actions         #########################################
    ####################################################################################################
    # define event handlers for buttons
    
    observeEvent(input$populateExistingConfig, {
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
            goodAlias = isValidAlias(aliasFromRunline(runLine), aliases())
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
        }
    })
    
    populateProps <- reactive({
        existingLines <- existingLines()
        exProps = existingLines[ grep("^#", existingLines, invert = TRUE) ]
        exProps = exProps[ grep("=", exProps) ]
        splits = strsplit(exProps, split="=", fixed=TRUE)
        splits = splits[which(sapply(splits, function(s){length(s) >= 2}))]
        vals = sapply(splits, function(pair){trimws(paste0(pair[2:length(pair)], collapse=""))})
        names(vals) = sapply(splits, function(pair){trimws(pair[1])})
        if (length(vals) > 0 ){
            for( propName in names(vals) ){
                message("updating property value: ", propName, " = ", vals[propName])
                # If propName is a genProp, set genProp
                if ( propName %in% names(propInfo) ){
                    # if input object is established, set through that
                    # otherwise, set value in pipelineProperties object Directly
                    if (!is.null(input[[propName]])){
                        updateTextInput(session, propName, value = paste(vals[propName]))
                    }else{
                        pipelineProperties[[propName]] <- vals[propName]
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
            lines = c(lines, paste("# This config file was last updated while referencing BioLockJ version:", bljVer))
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
        for(p in names(propInfo)){
            value = pipelineProperties[[p]]
            line = writeConfigProp(p, value, propInfo[[p]]$type)
            if ( !is.null(value) && !is.na(value) && length(value) > 0 && nchar(value) > 0 ){
                notTheDefault = !is.null(propInfo[[p]]$default) && value != propInfo[[p]]$default
                if ( is.null(propInfo[[p]]$default) || notTheDefault || input$include_standard_defaults ){
                    lines = c(lines, line)
                }
            }
        }
        lines
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
    
    aliases <- reactive({
        sapply(values$moduleList, aliasFromRunline)
    })
    
    # green for valid alias
    observeEvent(input$newAlias, {
        runLine = makeRunLine(input$AddBioModule, input$newAlias)
        msg = capture.output({
            goodAlias = isValidAlias(aliasFromRunline(runLine), aliases())
        }, type="message")
        shinyFeedback::feedbackSuccess("newAlias", goodAlias)
    })
    
    # Properties
    # The essential observers that keep the pipelineProperties synchronized are defined within 
    # the renderUI function that creates the inputs.
    
}

# Run the application 
shinyApp(ui = ui, server = server)
