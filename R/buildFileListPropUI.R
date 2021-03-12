#' 
#' @describeIn buildFilePathPropUI Build the UI for a BioLockJ file list property
#' 
#' @param propName a string, the name of the property
#' @param value in the case of a list property, the value of the property, most likely a single string, possibly comma-separated list
#' @param default value shown as a placeholder if no value is present, or value is removed
#'
buildFileListPropUI <- function(propName, value, default){
    tagList(
        textAreaInput(propUiName(propName), label = propName, value = ifelse(BioLockR::isReadableValue(value), value, ""), placeholder = default),
        shinyFiles::shinyFilesButton(id=propFileChooserId(propName), "add file", title = "select a file", multiple = TRUE, style = "margin-top: 1px;"),
        shinyFiles::shinyDirButton(id=propDirChooserId(propName), "add directory", title = "select a directory"),
    )
}