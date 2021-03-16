#' 
#' @describeIn buildFilePathPropUI Build the UI for a BioLockJ file list property
#' 
#' @param propName a string, the name of the property
#' @param moduleId the module id if the property ui is associated with a module.
#' @param value in the case of a list property, the value of the property, most likely a single string, possibly comma-separated list
#' @param default value shown as a placeholder if no value is present, or value is removed
#'
buildFileListPropUI <- function(propName, value, default, moduleId=NULL){
    tagList(
        textAreaInput(propUiName(propName, moduleId), label = propName, 
                      value = ifelse(BioLockR::isReadableValue(value), value, ""), placeholder = default),
        shinyFiles::shinyFilesButton(id=propFileChooserId(propName, moduleId), 
                                     "add file", title = "select a file", multiple = TRUE, style = "margin-top: 1px;"),
        shinyFiles::shinyDirButton(id=propDirChooserId(propName, moduleId), 
                                   "add directory", title = "select a directory"),
    )
}