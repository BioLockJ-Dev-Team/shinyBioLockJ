#' Named Default Values
#' 
#' Given a list of property infos, get a named vector of default values.
#' 
#' @details 
#' Only the properties that have default values are included in the output.
#'
#' @param propInfo the result of BioLockJ::propInfo() or a similar list of property atrribute lists.
#'
#' @return a named character vector where the names are the property names and the values are the property values
#'
namedDefaultVals <- function(propInfo){
    if (BioLockR::hasReadableValue(propInfo)){
        return( unlist( sapply(propInfo, function(prop){ prop$default }) ) )
    }else{
        vector("character")
    }
    
}