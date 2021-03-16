#' 
#' @describeIn propUiName name for a directory chooser ui element
#'
propDirChooserId <- function(propName, moduleId=NULL){
    paste0(propUiName(propName, moduleId), "DirChooser")
}