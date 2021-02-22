#' Build UI for File Path Properties
#'
#' @describeIn buildFilePathPropUI Build the UI for a BioLockJ file property
#'
#' @param propName a string, the name of the property
#'
#' @return ui object
#'
buildFilePathPropUI <- function(propName){
    tagList(
        p(strong(propName)),
        verbatimTextOutput(propShowId(propName), placeholder = TRUE),
        shinyFiles::shinyFilesButton(id=propFileChooserId(propName), "set file", title = "select a file", multiple = FALSE),
        shinyFiles::shinyDirButton(id=propDirChooserId(propName), "set directory", title = "select a directory"),
        actionButton(propClearBtn(propName), label = "clear")
    )
}