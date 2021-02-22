#' Derive the module Alias from a given runline
#'
#' @param line A line from a BioLockJ module run order
#'
#' @return the alias
#'
#' @examples
#' line = "#BioModule path/to/class/for/dostuff AS MyStuff"
#' aliasFromRunline()
#' #> "MyStuff"
aliasFromRunline <- function(line){
    alias=NA
    if (!is.null(line) && !is.na(line) && length(line)>0){
        if (grepl(" AS ", line)){
            parts = strsplit(line, " AS ", fixed=TRUE)[[1]]
        }else{
            parts = strsplit(line, ".", fixed=TRUE)[[1]]
        }
        alias = trimws(parts[length(parts)])
    }
    return(alias)
}