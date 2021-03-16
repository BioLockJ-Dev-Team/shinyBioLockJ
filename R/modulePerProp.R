#' Module Per Prop
#' 
#' List the modules organized by property name.  Thus, get a list of all modules that use a given property.
#' 
#' @param moduleInfo the lis returned by BioLockJ::moduleInfo(), or a similar object
#' @param moduleClasses a named character vector with values that match the names of moduleInfo elements and names to be applied to them
#'
#' @return a list of properties, each element is a vector of module names giving the modules that use that property
#'
modulePerProp <- function(moduleInfo=BioLockR::moduleInfo(), moduleClasses){
    if ( !BioLockR::hasReadableValue(moduleInfo) ) return( list() )
    if ( !BioLockR::hasReadableValue(moduleClasses) ) return( list() )
    useModuleInfo = moduleInfo[moduleClasses]
    names(useModuleInfo) = names(moduleClasses)
    a = lapply(useModuleInfo, function(mi){names(mi$properties)})
    d1=utils::stack(a)
    return(split(as.character(d1$ind), d1$values))
}