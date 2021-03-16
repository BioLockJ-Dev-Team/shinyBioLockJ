#' Build UI for File Path Properties
#'
#' @describeIn buildFilePathPropUI Build the UI for a BioLockJ file property
#'
#' @return ui object
#'
buildFilePathPropUI <- function(propName, value, default, moduleId=NULL ){
    tagList(
        textInput(propUiName(propName, moduleId), propName, 
                  value=value, placeholder = default),
        shinyFiles::shinyFilesButton(id=propFileChooserId(propName, moduleId), 
                                     "set file", title = "select a file", multiple = FALSE),
        shinyFiles::shinyDirButton(id=propDirChooserId(propName, moduleId), 
                                   "set directory", title = "select a directory"),
    )
}