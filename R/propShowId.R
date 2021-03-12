#' 
#' @describeIn propUiName name for ui element that shows the property value
#'
propShowId <- function(propName, moduleId){
    paste0(gsub(".", "", propName, fixed=TRUE), "Show", moduleId)
}