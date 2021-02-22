#' BioLockJ ui.R
#'
#' @return the shiny ui object
#' 
biolockj_ui <- function(){
    
    envType = detect_deployment()
    helpMdFile = system.file("md", "HelpPage.md", package = "shinyBioLockJ")
    
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
                                                        selected = envType ) ),
                         br(), 
                         hr(),
                         h4("Core"),
                         strong("BioLockJ version: "),
                         textOutput("bljVersion"),
                         br(),
                         uiOutput("coreSettings")
                         # shinyFiles::shinyFilesButton("biolockjJarFile", "BioLockJ Jar File Location", "Choose jar file", multiple=FALSE, accept=c(".jar"), width='100%'),
                         # actionButton("updateJar", "update jar location"),
                         # verbatimTextOutput("showNewJarFile")
                     )),
            # Help ####
            tabPanel("Help",
                     p("navbar"),p("spacer"),
                     includeMarkdown(helpMdFile))
        )
    ) # end of UI ####
    return(ui)
}