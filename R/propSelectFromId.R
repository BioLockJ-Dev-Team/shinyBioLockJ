#' 
#' @describeIn propUiName name for selector tool as part of a ui element
#'
propSelectFromId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "SelectFromList")
}