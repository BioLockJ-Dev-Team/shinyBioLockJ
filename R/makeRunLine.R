#' Assemble the run line for a module
#'
#' @param moduleName the simple name of a module class
#' @param moduleRunLines named vector linking the module name to the full classpath preceeded by keyword
#' @param alias optional alias
#'
#' @return
#'
#' @examples
#' avail = c(dostuff="#BioModule path/to/class/for/dostuff",
#' thing="#BioModule path/to/class/for/thing")
#' makeRunLine("dostuff", avail, "MyStuff")
#' #> [1] "#BioModule path/to/class/for/dostuff AS MyStuff"
makeRunLine <- function(moduleName, moduleRunLines, alias=""){
    className = moduleRunLines[moduleName]
    if ( nchar(alias) > 0 ) {
        runLine = paste(className, "AS", alias)
    }else{
        runLine = paste(className)
    }
    return(runLine)
}