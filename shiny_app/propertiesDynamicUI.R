# PropertiesUI

# As of this writing, the Properties class in BioLockJ has these KNOWN_TYPES:
# KNOWN_TYPES = {STRING_TYPE, BOOLEAN_TYPE, FILE_PATH, EXE_PATH, LIST_TYPE, FILE_PATH_LIST, INTEGER_TYPE, NUMERTIC_TYPE};
# KNOWN_TYPES = {"string", "boolean", "file path", "executable", "list", "list of file paths", "integer", "numeric"};

propertyValues <- reactiveValues()
fileBins <- reactiveValues()

# render general prop ui ####
renderPropUi <- function(propName, prop, default, value, defaults){
    # message("Treating property ", propName, " as a ", prop$type, " property.")
    uiName = propUiName(propName)
    if(prop$type == "boolean"){
        selected = ""
        if ( !is.null(value) ){
            if ( value=="Y" || value=="TRUE" || value==TRUE ) {
                selected = "Y"
            }else if(value=="N" || value=="FALSE" || value==FALSE ){
                selected = "N"
            }
        }
        inputObj <- radioButtons(inputId = uiName,
                                 label = propName,
                                 choices = c(Y="Y", N="N", omit=""),
                                 selected = selected,
                                 inline = TRUE)
    }else if(prop$type == "numeric"){
        inputObj <- numericInput(inputId = uiName,
                              label = propName,
                              value = value,
                              width = '60%')
    }else if(prop$type == "integer"){
        inputObj <- numericInput(inputId = uiName,
                                 label = propName,
                                 value = value,
                                 step = 1,
                                 width = '40%')
    }else if(prop$type == "file path"){
        inputObj <- buildFilePathPropUI(propName)
    }else if(prop$type == "list of file paths"){
        inputObj <- buildFileListPropUI(propName)
    }else {
        inputObj <- textInput(inputId = uiName,
                              label = propName,
                              value = value,
                              placeholder = default,
                              width = '100%')
    }
    
    if (is.null(default)){
        propUI <- tagList(
            em(prop$type),
            renderText(prop$description),
            inputObj,
            hr())
    }else{
        content = "<b>source of default value</b>"
        content = c(content, paste("standard default:", defaults$defaultPropsList$standard[[propName]]))
        for (name in defaults$activeFiles){
            val = " "
            if (propName %in% names(defaults$defaultPropsList[[name]])){
                val = defaults$defaultPropsList[[name]][propName]
            }
            content = c(content, paste0(name, ": ", val))
        }

        propUI <- tagList(
            shinyBS::popify(
                actionLink(propInfoId(propName), "", icon = icon("angle-double-left")),
                title=paste0("<b>", propName, " = ",  defaults$values[[propName]], "</b>"),
                paste0(content, collapse = "<br>"),#content=paste("default:", defaults$values[[propName]]),
                trigger = c('hover','click'), placement='right'),
            em(prop$type),
            renderText(prop$description),
            inputObj,
            hr())
    }

    return(propUI)
}

# file path and file path list ####

buildFilePathPropUI <- function(propName){
    # propName: a string, the name of the property
    tagList(
        p(strong(propName)),
        verbatimTextOutput(propShowId(propName), placeholder = TRUE),
        shinyFiles::shinyFilesButton(id=propFileChooserId(propName), "set file", title = "select a file", multiple = FALSE),
        shinyFiles::shinyDirButton(id=propDirChooserId(propName), "set directory", title = "select a directory"),
        actionButton(propClearBtn(propName), label = "clear")
    )
}

buildFileListPropUI <- function(propName){
    # propName: a string, the name of the property
    tagList(
        selectInput(propSelectFromId(propName), label=propName, multiple = TRUE, choices = c(), width = '100%'),
        shinyFiles::shinyFilesButton(id=propFileChooserId(propName), "add file", title = "select a file", multiple = TRUE, style = "margin-top: 1px;"),
        shinyFiles::shinyDirButton(id=propDirChooserId(propName), "add directory", title = "select a directory"),
    )
}

buildFilePathPropObservers <- function(session, input, output, propName, myVolumesNow, values){
    shinyFileChoose(input, propFileChooserId(propName), roots = myVolumesNow, session = session)
    shinyDirChoose(input, propDirChooserId(propName), roots = myVolumesNow, session = session, restrictions = system.file(package = "base"))
    #
    observeEvent(input[[propClearBtn(propName)]], {
        values$pipelineProperties[[propName]] = ""
    })
    observeEvent( input[[propFileChooserId(propName)]], {
        if (! is.integer(input[[propFileChooserId(propName)]])){
            values$pipelineProperties[[propName]] = parseFilePaths(myVolumesNow, input[[propFileChooserId(propName)]])$datapath
        }
    })
    observeEvent( input[[propDirChooserId(propName)]], {
        if (! is.integer(input[[propDirChooserId(propName)]])){
            values$pipelineProperties[[propName]] = parseDirPath(myVolumesNow, input[[propDirChooserId(propName)]])
        }
    })
    output[[propShowId(propName)]] <- renderText( values$pipelineProperties[[propName]] )
}

buildFileListPropObservers <- function(session, input, output, propName, myVolumesNow, values, fileBins){
    shinyFileChoose(input, propFileChooserId(propName), roots = myVolumesNow, session = session)
    shinyDirChoose(input, propDirChooserId(propName), roots = myVolumesNow, session = session, restrictions = system.file(package = "base"))
    #
    observeEvent( input[[propFileChooserId(propName)]], {
        if (! is.integer(input[[propFileChooserId(propName)]])){
            oldSet = fileBins[[propName]]
            newPath = parseFilePaths(myVolumesNow, input[[propFileChooserId(propName)]])$datapath
            fileBins[[propName]] = c(oldSet, newPath)
        }
    })
    observeEvent( input[[propDirChooserId(propName)]], {
        if (! is.integer(input[[propDirChooserId(propName)]])){
            oldSet = fileBins[[propName]]
            newPath = parseDirPath(myVolumesNow, input[[propDirChooserId(propName)]])
            fileBins[[propName]] = c(oldSet, newPath)
        }
    })
    choices = reactive({
        message("making choices...")
        if ( isWritableValue( fileBins[[propName]] ) ){
            opts = fileBins[[propName]]
            names(opts) <- basename(opts)
            opts
        }else{
            c()
        }
    })
    observeEvent(fileBins[[propName]], {
        message("updating choice/selection/presentation")
        newPath = fileBins[[propName]][length(fileBins[[propName]])]
        updateSelectInput(session, propSelectFromId(propName), choices = choices(), 
                          selected = c(values$pipelineProperties[[propName]], newPath))
                          # label = paste0("(can be multiple) list of: ", length(values$pipelineProperties[[propName]])+1 ) )
    })
    observeEvent(input[[propSelectFromId(propName)]], {
        values$pipelineProperties[[propName]] = input[[propSelectFromId(propName)]]
    })
}


# Create UI element id's based on the property name ####

propUiName <- function(propName){
    gsub(".", "", propName, fixed=TRUE)
}

propInfoId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "Info")
}

propFileChooserId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "FileChooser")
}

propDirChooserId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "DirChooser")
}

propShowId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "Show")
}

propSelectFromId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "SelectFromList")
}

propClearBtn <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "ClearBtn")
}
