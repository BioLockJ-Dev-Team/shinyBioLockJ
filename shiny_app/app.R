#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# install.packages("sortable")
library(sortable)
source('~/git/shiny_BioLockJ/shiny_app/biolockj.R')




# get initial BioLockJ info
nameProperties <- function(propList){
    names = sapply(propList, function(prop){prop$property})
    names(propList) <- names
    return(propList)
}

propInfo <- nameProperties(propInfo())

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
             uiOutput("biolockjGitHub"),
             uiOutput("biolockjUserGuide"),
             p(""),
             fileInput("existingConfig", label="Edit an existing config file")),
    tabPanel("Pipeline",
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
                 uiOutput("orderModules"),
                 textOutput("moduleOrder"))),
    tabPanel("Settings",
             p("navbar"),p("spacer"),
             h2("General Properties"),
             p("General properties are not specific to any one module."),
             uiOutput("genProps"),
             h2("Module Properties")),
    tabPanel("Data Flow", 
             p("navbar"),p("spacer"),
             p("This panel is a placeholder tab.")),
    tabPanel("Save config file", 
             p("navbar"),p("spacer"),
             "This how this pipeline configuration will be saved as a config file.",
             actionButton("saveFile", "Save to file"),
             uiOutput("configText")),
    tabPanel("Help", 
             p("navbar"),p("spacer"),
             p("This panel is a placeholder tab."))
)


server <- function(input, output, session) {
    values <- reactiveValues()
    
    # Home
    output$biolockjGitHub <- renderUI({
        ghUrl <- a("BioLockJ GitHub", href="https://github.com/BioLockJ-Dev-Team/BioLockJ")
        tagList("The central BioLockJ resource is the GitHub repository:", ghUrl)
    })
    output$biolockjUserGuide <- renderUI({
        ugUrl <- a("BioLockJ userguide", href="https://biolockj.readthedocs.io/en/latest/")
        tagList("See the BioLockJ user guide:", ugUrl)
    })
    
    # Pipeline
    output$orderModules <- renderUI({
        rank_list(
            text = "Drag and drop to re-order",
            labels = values$moduleList,
            input_id = "orderModules",
            options = sortable_options(multiDrag = TRUE))
    })
    getModuleOrder <- reactive({
        text = input$orderModules
        if(is.null(text) || length(text)==0 ) text = "(add modules to create a pipeline)"
        text
    })
    output$moduleOrder <- renderPrint(getModuleOrder())
    observeEvent(input$AddModuleButton, {
        message("I know the button got pushed")
        runLine = makeRunLine(input$AddBioModule, input$newAlias)
        values$moduleList <- c(isolate(input$orderModules), runLine)
        updateTextInput(session, "newAlias", value = "")
    })
    
    # Settings
    output$genProps <- renderUI({
        lapply(propInfo, function(prop){
            tagList(
                em(prop$type,),
                renderText(prop$description),
                textInput(inputId = paste(prop$property),
                          #value = propValue(prop$property), # commend this out to run MUCH faster!!
                          label = prop$property
                )
            )
        })
    })
    output$moduleListText <- renderUI({
        moduleLines = input$BioModules #sapply(input$BioModules, function(module){ module$usage })
        pre(paste(moduleLines, collapse ="<br>"))
    })
    
    # Config file
    output$configText <- renderUI({
        lapply(1:5, function(module){
            pre(paste0("#BioModule path.to.module.class", module))
        })
    })
    
    # Testing
    
}

# Run the application 
shinyApp(ui = ui, server = server)
