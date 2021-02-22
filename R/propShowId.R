#' 
#' @describeIn propUiName name for ui element that shows the property value
#'
propShowId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "Show")
}