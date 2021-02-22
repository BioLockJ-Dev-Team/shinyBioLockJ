#' Name UI elements for BioLockJ Properties
#'
#' @param propName the name of a BioLockJ property, such as "script.numThreads"
#' 
#' @return a string to use for an associated UI element
#'
propUiName <- function(propName){
    gsub(".", "", propName, fixed=TRUE)
}