#' Property Info For Type
#' 
#' Get a subset of the propInfo() value for only one category of property.
#' 
#' @param propInfo named list of property objects
#' @param type the type of property to keep
#'
#' @return a named list of property objects, a subset of propInfo.
#'
propsInfoForType <- function(propInfo, type){
    keepIf = sapply(propInfo, function(prop){prop$type == type})
    return(propInfo[keepIf])
}