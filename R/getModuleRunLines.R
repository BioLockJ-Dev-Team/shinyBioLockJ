#' Module Run Lines
#' 
#' Given a lis of module objects, such as that returned by BioLockJ::moduleInfo(), 
#' create a named vector linking the simple class name to the usage line.
#' 
#' @param moduleInfo the return value of BioLockR::moduleInfo() or a similarly structured list
#'
#' @return named vector
#'
getModuleRunLines <- function(moduleInfo){
    if (BioLockR::hasReadableValue(moduleInfo)){
        moduleRunLines = sapply(moduleInfo, function(mi){mi$usage})
        names(moduleRunLines) <- sapply(moduleInfo, function(mi){mi$title})
        moduleRunLines = moduleRunLines[order(names(moduleRunLines))]
        return(moduleRunLines)
    }else{
        return(c())
    }
}