#' Derive the module class from a given runline
#'
#' @inheritParams aliasFromRunline
#'
#' @return the class of the module specified by the runline
#' 
#' @seealso aliasFromRunline
#'
#' @examples
#' line = "#BioModule path.to.class.for.Dostuff AS MyStuff"
#' classFromRunline( line )
#' #> "path.to.class.for.Dostuff"
#' 
classFromRunline <- function(line){
    class=NA
    if ( BioLockR::isReadableValue(line) ){
        mainline = unlist(strsplit(line, " AS ", fixed=TRUE))[1]
        classPlus = gsub("#BioModule", "", mainline)
        class = trimws(classPlus)
    }
    return(class)
}