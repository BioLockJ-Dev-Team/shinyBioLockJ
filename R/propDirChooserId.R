#' 
#' @describeIn propUiName name for a directory chooser ui element
#'
propDirChooserId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "DirChooser")
}