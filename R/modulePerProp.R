#' Module Per Prop
#' 
#' List the modules organized by property name.  Thus, get a list of all modules that use a given property.
#' 
#' @param moduleInfo the lis returned by BioLockJ::moduleInfo(), or a similar object
#'
#' @return a list of properties, each element is a vector of module names giving the modules that use that property
#'
modulePerProp <- function(moduleInfo=BioLockR::moduleInfo()){
    if ( !BioLockR::hasReadableValue(moduleInfo) ) return( list() )
    a = lapply(moduleInfo, function(mi){names(mi$properties)})
    d1=utils::stack(a)
    return(split(as.character(d1$ind), d1$values))
}