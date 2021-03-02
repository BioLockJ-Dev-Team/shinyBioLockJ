#' Derive the module Alias from a given runline
#'
#' @param line A line from a BioLockJ module run order
#'
#' @seealso classFromRunline
#' 
#' @return the alias
#' 
# @examples
# line = "#BioModule path.to.class.for.dostuff AS MyStuff"
# aliasFromRunline( line )
# #> "MyStuff"
aliasFromRunline <- function(line){
    alias=NA
    if ( BioLockR::isReadableValue(line) ){
        if (grepl(" AS ", line)){
            parts = strsplit(line, " AS ", fixed=TRUE)[[1]]
        }else{
            parts = strsplit(line, ".", fixed=TRUE)[[1]]
        }
        alias = trimws(parts[length(parts)])
    }
    return(alias)
}