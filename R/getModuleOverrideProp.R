#' Module Override Prop
#' 
#' For a given module name/alias, and a given property, what would be the module-override property.
#' 
#' @param moduleAlias the alias of a BioLockJ module
#' @param propName a BioLockJ property
#'
#' @return the new property name
#'
# @examples
# getModuleOverrideProp("Part3", "script.numThreads")
# #> [1] "Part3.numThreads"
getModuleOverrideProp <- function(moduleAlias, propName){
    parts=strsplit(propName, split=".", fixed=TRUE)[[1]]
    parts[1] = moduleAlias
    return( paste(parts, collapse=".") )
}