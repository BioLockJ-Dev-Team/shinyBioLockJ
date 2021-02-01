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
source('biolockj.R')




# get initial BioLockJ info
nameProperties <- function(propList){
    names = sapply(propList, function(prop){prop$property})
    names(propList) <- names
    return(propList)
}

propInfo <- nameProperties(propInfo())

splits = strsplit(names(propInfo), split = ".", fixed = TRUE)
category = sapply(splits, function(s){s[1]})
groupedProps = split(names(propInfo), f=category)

moduleInfo <- moduleInfo()
names(moduleInfo) <- sapply(moduleInfo, function(mi){mi$title})
moduleInfo <- lapply(moduleInfo, function(mi){
    mi$properties = nameProperties(mi$properties)
    return(mi)
})
moduleRunLines <- sapply(moduleInfo, function(mi){mi$usage})
names(moduleRunLines) <- names(moduleInfo)

###
makeRunLine <- function(moduleName, alias=""){
    className = moduleRunLines[moduleName]
    if ( nchar(alias) > 0 ) {
        runLine = paste(className, "AS", alias)
    }else{
        runLine = paste(className)
    }
    return(runLine)
}

###

ui <-  navbarPage(
    position = "fixed-top",
    theme = shinytheme("cerulean"),
    "BioLockJ",
    tabPanel("Home", 
             p("navbar"),p("spacer"),
             h1("BioLockJ Pipeline Builder"),
             em("(optional)"),
             fileInput("existingConfig", label="Upload an existing config file"),
             textInput("projectName", "Project name", value="myPipeline", placeholder = "new project name"),
             checkboxInput("include_standard_defaults", "include defaults"),
             downloadButton("downloadData", "Save config file"),
             uiOutput("configText")),
    tabPanel("Modules",
             fluidPage(
                 h2("BioModule Run Order"),
                 fluidRow(
                     column(6,selectInput("AddBioModule", 
                                          "Select new BioModule", 
                                          names(moduleInfo), 
                                          selected = "GenMod",
                                          width = '100%')),
                     column(1, "AS", style = "margin-top: 30px;"),
                     column(3, textInput("newAlias", "", 
                                         placeholder = "alternative alias",
                                         width = '100%')),
                     column(2, actionButton("AddModuleButton", style = "margin-top: 20px;", "add to pipeline", class = "btn-success"))),
                 uiOutput("orderModules"))),
    tabPanel("Properties",
             splitLayout(
                 fluidPage(p("navbar"),p("spacer"),
                           h2("General Properties"),
                           p("General properties are not specific to any one module."),
                           uiOutput("genProps")),
                 fluidPage(p("navbar"),p("spacer"),
                           h2("Module Properties"),
                           # uiOutput("modProps"),
                           p("The properties for a given module include the properties that are specific to that module, as well as any general properties that the module is known to reference."))
             )
    ),
    tabPanel("Data Flow", 
             p("navbar"),p("spacer"),
             p("This panel is a placeholder tab.")),
    tabPanel("Help", 
             p("navbar"),p("spacer"),
             uiOutput("biolockjGitHub"),
             uiOutput("biolockjUserGuide"))
)


server <- function(input, output, session) {
    values <- reactiveValues()
    pipelineProperties <- do.call(reactiveValues, lapply(propInfo, function(prop){ prop$default }))
    customProps <- reactiveValues()
    hasCustomProps <- reactiveVal(FALSE)

    # Home
    output$biolockjGitHub <- renderUI({
        ghUrl <- a("BioLockJ GitHub", href="https://github.com/BioLockJ-Dev-Team/BioLockJ")
        tagList("The central BioLockJ resource is the GitHub repository:", ghUrl)
    })
    output$biolockjUserGuide <- renderUI({
        ugUrl <- a("BioLockJ userguide", href="https://biolockj.readthedocs.io/en/latest/")
        tagList("See the BioLockJ user guide:", ugUrl)
    })
    
    # Modules
    output$orderModules <- renderUI({
        rank_list(
            text = "Drag and drop to re-order",
            labels = values$moduleList,
            input_id = "orderModules",
            options = sortable_options(multiDrag = TRUE))
    })
    output$moduleOrder <- renderPrint(values$moduleList)
    observeEvent(input$AddModuleButton, {
        message("I know the button got pushed")
        runLine = makeRunLine(input$AddBioModule, input$newAlias)
        values$moduleList <- c(isolate(input$orderModules), runLine)
        updateTextInput(session, "newAlias", value = "")
    })
    
    # Properties
    output$genProps <- renderUI({
        argsList =  lapply(as.list(names(groupedProps)), function(groupName){
                    group = groupedProps[[groupName]]
                    tabPanel(groupName, 
                             p(paste("See the user guide for more info about", groupName, "properties.", collaps=" ")),
                             lapply(group, function(propName){
                                 prop = propInfo[[propName]]
                                 if ( is.null(pipelineProperties[[propName]]) ) {
                                     message("property was null: ", propName)
                                     pipelineProperties[[propName]] <- prop$default
                                     message("now property has value: ", pipelineProperties[[propName]])
                                 }
                                 tagList(em(prop$type),
                                         renderText(prop$description),
                                         textInput(inputId = paste(prop$property),
                                                   label = prop$property,
                                                   value = pipelineProperties[[propName]],
                                                   placeholder = prop$default))
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
                                                 lapply(names(customProps), function(cp){
                                                     message("creating option to remove property: ", cp)
                                                            fluidRow(column(9, renderText(paste(cp, "=", customProps[[cp]]))),
                                                                     column(3, actionButton(paste0("rm-", cp), "remove")))
                                                     })
        )
        argsList$selected = "input"
        do.call(tabsetPanel, argsList)
    })

    output$moduleListText <- renderUI({
        moduleLines = input$BioModules #sapply(input$BioModules, function(module){ module$usage })
        pre(paste(moduleLines, collapse ="<br>"))
    })
    
    observeEvent(input$addCostomPropBtn, {
        message("The button was pushed! button: addCostomPropBtn")
        for(p in names(propInfo)){
            pipelineProperties[[p]] <- input[[p]]
        }
        hasCustomProps(TRUE)
        customProps[[input$customPropName]] <- input$customPropVal
    })
    
    
    # Save config file
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(input$projectName, ".config")
        },
        content = function(file) {
            writeLines(configLines(), file)
        }
    )
    
    configLines <- reactive({
        lines = c("")
        lines = c(lines, input$orderModules)
        lines = c(lines, "")
        message("I'm looking at customProps...")
        if ( hasCustomProps() ){
            lines = c(lines, "# Custom Properties")
            for(cp in names(customProps)){
                line = paste(cp, "=", customProps[[cp]])
                lines = c(lines, line)
            }
            lines = c(lines, "")
        }
        
        message("I'm looking at propInputBoxes...")
        lines = c(lines, "# General Properties")
        for(p in names(propInfo)){
            message("I'm looking at...", p)
            value = input[[p]]
            message("its value is:", value)
            line = paste(p, "=", value)
            # message("the default is: ", propInfo[[p]]$default)
            message("new config line: ", line)
            if ( length(value) > 0 && nchar(value) > 0 ){
                notTheDefault = !is.null(propInfo[[p]]$default) && value != propInfo[[p]]$default
                if ( is.null(propInfo[[p]]$default) || notTheDefault || input$include_standard_defaults ){
                    lines = c(lines, line)
                }
            }
        }
        lines
    })
    
    output$configText <- renderUI({
        do.call(pre, as.list(configLines()))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
