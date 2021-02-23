#' 
#' @describeIn propUiName name for a file chooser ui element
#'
propFileChooserId <- function(propName){
    message("propName: ", propName)
    paste0(gsub(".", "", propName, fixed=TRUE), "FileChooser")
}