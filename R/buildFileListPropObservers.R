#'
#' @describeIn buildFilePathPropUI Build/refresh the backend of the UI for a BioLockJ file list property
#'
#' @return
#'
buildFileListPropObservers <- function(session, input, output, propName, myVolumesNow, values, moduleId=NULL){
    shinyFiles::shinyFileChoose(input, propFileChooserId(propName, moduleId), roots = myVolumesNow, session = session)
    shinyFiles::shinyDirChoose(input, propDirChooserId(propName, moduleId), roots = myVolumesNow, session = session, restrictions = system.file(package = "base"))
    #
   observeEvent( input[[propFileChooserId(propName, moduleId)]], {
        if (! is.integer(input[[propFileChooserId(propName, moduleId)]])){
            newPath = shinyFiles::parseFilePaths(myVolumesNow, input[[propFileChooserId(propName, moduleId)]])$datapath
            choices = filePathChoices( input[[propUiName(propName, moduleId)]], newPath)
            updateTextAreaInput(session, propUiName(propName, moduleId), value = writeFilePathList(choices, "", FALSE))
        }
    })
    
    observeEvent( input[[propDirChooserId(propName, moduleId)]], {
        if (! is.integer(input[[propDirChooserId(propName, moduleId)]])){
            newPath = shinyFiles::parseDirPath(myVolumesNow, input[[propDirChooserId(propName, moduleId)]])
            choices = filePathChoices( input[[propUiName(propName, moduleId)]], newPath)
            updateTextAreaInput(session, propUiName(propName, moduleId), value = writeFilePathList(choices, "", FALSE))
        }
    })
    
    # observeEvent(input[[propUiName(propName, moduleId)]], {
    #     values$generalProps[propName, moduleId] = input[[propUiName(propName)]]
    # })
}