#' Name UI elements for BioLockJ Properties
#'
#' @param propName the name of a BioLockJ property, such as "script.numThreads"
#' @param moduleId the module id
#' 
#' @return a string to use for an associated UI element
#'
propUiName <- function(propName, moduleId=NULL){
    if (is.null(moduleId)) moduleId="Backbone"
    paste0(moduleId, "HasProp", gsub(".", "", propName, fixed=TRUE))
}