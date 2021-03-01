#' 
#' @describeIn buildFilePathPropUI Build/refresh the backend of the UI for a BioLockJ file property
#' 
#' @param session usually session=session
#' @param input usually input=input
#' @param output usually output=output
#' @param myVolumesNow the volumes object to pass to shinyFiles operations
#' @param values the list of lists reactive values object that represents the current configuration
#'
buildFilePathPropObservers <- function(session, input, output, propName, myVolumesNow, values){
    shinyFiles::shinyFileChoose(input, propFileChooserId(propName), roots = myVolumesNow, session = session)
    shinyFiles::shinyDirChoose(input, propDirChooserId(propName), roots = myVolumesNow, session = session, restrictions = system.file(package = "base"))
    #
    observeEvent( input[[propFileChooserId(propName)]], {
        if (! is.integer(input[[propFileChooserId(propName)]])){
            path = shinyFiles::parseFilePaths(myVolumesNow, input[[propFileChooserId(propName)]])$datapath
            updateTextInput(session=session, propUiName(propName), value=as.character(path) )
        }
    })

    observeEvent( input[[propDirChooserId(propName)]], {
        if (! is.integer(input[[propDirChooserId(propName)]])){
            updateTextInput(session=session, propUiName(propName), 
                            value=shinyFiles::parseDirPath(myVolumesNow, input[[propDirChooserId(propName)]]) )
        }
    })

    observeEvent(input[[propUiName(propName)]], {
        values$pipelineProperties[propName] = input[[propUiName(propName)]]
    })
}