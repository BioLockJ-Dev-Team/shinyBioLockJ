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
        inputObj <- buildFilePathPropUI(propName, value)
        # inputObj <- textInput(inputId = uiName,
        #                       label = propName,
        #                       value = value,
        #                       placeholder = default,
        #                       width = '100%')
    }else if(prop$type == "list of file paths"){
        inputObj <- textInput(inputId = uiName,
                              label = propName,
                              value = value,
                              placeholder = default,
                              width = '100%')
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
            br())
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
            br())
    }

    return(propUI)
}

# TODO: use the value!
buildFilePathPropUI <- function(propName, value){
    # propName - character vector of length 1, a string, the name of the property
    # value - the initial value to set
    uiObj <- renderUI({
        tagList(
            p(strong(propName)),
            shinyFiles::shinyFilesButton(id=propFileChooserId(propName), "set file", title = "select a file", multiple = FALSE),
            shinyFiles::shinyDirButton(id=propDirChooserId(propName), "set directory", title = "select a directory"),
            actionButton(propClearBtn(propName), label = "clear"),
            verbatimTextOutput(propShowId(propName), placeholder = TRUE)
        )
    })
    uiObj
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

propSelectFromList <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "SelectFromList")
}

propClearBtn <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "ClearBtn")
}
