#' Module Run Lines
#' 
#' Given a lis of module objects, such as that returned by BioLockJ::moduleInfo(), 
#' create a named vector linking the simple class name to the usage line.
#' 
#' @param moduleInfo 
#'
#' @return named vector
#'
getModuleRunLines <- function(moduleInfo){
    moduleRunLines = sapply(moduleInfo, function(mi){mi$usage})
    names(moduleRunLines) <- names(moduleInfo)
    return(moduleRunLines)
}