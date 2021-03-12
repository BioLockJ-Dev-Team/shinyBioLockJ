#' 
#' @describeIn propUiName name for button to clear the value of a property
#' 
#' @param propName the name of the property
#'
propClearBtn <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "ClearBtn")
}