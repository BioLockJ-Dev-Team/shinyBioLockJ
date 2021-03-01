#' 
#' @describeIn propUiName name for a file chooser ui element
#'
propFileChooserId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "FileChooser")
}