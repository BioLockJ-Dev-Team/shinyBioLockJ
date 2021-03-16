#' 
#' @describeIn propUiName name for a file chooser ui element
#'
propFileChooserId <- function(propName, moduleId=NULL){
    paste0(propUiName(propName, moduleId), "FileChooser")
}