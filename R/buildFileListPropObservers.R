#'
#' @describeIn buildFilePathPropUI Build/refresh the backend of the UI for a BioLockJ file list property
#'
#' @param session usually session=session
#' @param input usually input=input
#' @param output usually output=output
#' @param myVolumesNow the volumes object to pass to shinyFiles operations
#' @param values the list of lists reactive values object that represents the current configuration
#' @param fileListUpdates a reactive values object that keeps the file list options per property
#'
#' @return
#'
buildFileListPropObservers <- function(session, input, output, propName, myVolumesNow, values, fileListUpdates){
    shinyFiles::shinyFileChoose(input, propFileChooserId(propName), roots = myVolumesNow, session = session)
    shinyFiles::shinyDirChoose(input, propDirChooserId(propName), roots = myVolumesNow, session = session, restrictions = system.file(package = "base"))
    #
    observeEvent( input[[propFileChooserId(propName)]], {
        if (! is.integer(input[[propFileChooserId(propName)]])){
            newPath = shinyFiles::parseFilePaths(myVolumesNow, input[[propFileChooserId(propName)]])$datapath
            choices = filePathChoices( values$pipelineProperties[propName], newPath)
            updateSelectInput(session, propSelectFromId(propName), choices = choices, selected = choices)
        }
    })
    observeEvent( input[[propDirChooserId(propName)]], {
        if (! is.integer(input[[propDirChooserId(propName)]])){
            newPath = shinyFiles::parseDirPath(myVolumesNow, input[[propDirChooserId(propName)]])
            choices = filePathChoices( values$pipelineProperties[propName], newPath)
            updateSelectInput(session, propSelectFromId(propName), choices = choices, selected = choices)
        }
    })
    
    observeEvent(input[[propSelectFromId(propName)]], {
        values$pipelineProperties[propName] = BioLockR::printListProp( input[[propSelectFromId(propName)]] )
    })
    observeEvent(fileListUpdates[[propName]], {
        choices = filePathChoices(fileListUpdates[[propName]] )
        updateSelectInput(session, propSelectFromId(propName), choices = choices, selected = choices)
    })
}