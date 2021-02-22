#' 
#' @describeIn propUiName name for button to clear the value of a property
#'
propClearBtn <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "ClearBtn")
}