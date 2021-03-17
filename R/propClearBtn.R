#' 
#' @describeIn propUiName name for button to clear the value of a property
#' 
#' @param propName the name of the property
#'
propClearBtn <- function(propName, moduleId = NULL){
    paste0(propUiName(propName, moduleId = NULL), "ClearBtn")
}