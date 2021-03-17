#' 
#' @describeIn buildFilePathPropUI Build/refresh the backend of the UI for a BioLockJ file property
#' 
#' @param session usually session=session
#' @param input usually input=input
#' @param output usually output=output
#' @param myVolumesNow the volumes object to pass to shinyFiles operations
#' @param values the list of lists reactive values object that represents the current configuration
#' @param moduleId module id if property is associated with a module
#'
buildFilePathPropObservers <- function(session, input, output, propName, myVolumesNow, values, moduleId=NULL){
    shinyFiles::shinyFileChoose(input, propFileChooserId(propName, moduleId), roots = myVolumesNow, session = session)
    shinyFiles::shinyDirChoose(input, propDirChooserId(propName, moduleId), roots = myVolumesNow, session = session, restrictions = system.file(package = "base"))
    #
    observeEvent( input[[propFileChooserId(propName, moduleId)]], {
        if (! is.integer(input[[propFileChooserId(propName, moduleId)]])){
            path = shinyFiles::parseFilePaths(myVolumesNow, input[[propFileChooserId(propName, moduleId)]])$datapath
            updateTextInput(session=session, propUiName(propName, moduleId), value=as.character(path) )
        }
    })

    observeEvent( input[[propDirChooserId(propName, moduleId)]], {
        if (! is.integer(input[[propDirChooserId(propName, moduleId)]])){
            updateTextInput(session=session, propUiName(propName, moduleId), 
                            value=shinyFiles::parseDirPath(myVolumesNow, input[[propDirChooserId(propName, moduleId)]]) )
        }
    })

    # I *think* this is redundant
    # observeEvent(input[[propUiName(propName, moduleId)]], {
    #     values$generalProps[propName] = input[[propUiName(propName)]]
    # })
}