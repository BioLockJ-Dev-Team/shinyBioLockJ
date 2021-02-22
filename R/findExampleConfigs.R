#' Find Example Configs
#' 
#' Look in the greater BioLockJ folder that contains the currently configured BioLockJ.jar file.
#' Examples are expected to be under BioLockJ/templates/exampleProject/example.config, 
#' where "BioLockJ" is the path passed in as _bljDir_.
#'
#' @param bljDir 
#'
#' @return
#'
findExampleConfigs <- function( bljDir = dirname(dirname(BioLockR::get_BLJ_JAR())) ){
    examples = tryCatch({
        templatesDir = file.path(bljDir, "templates")
        examples = sapply(dir(templatesDir, include.dirs = TRUE, full.names = TRUE), 
                          function(d){
                              set = dir(d, pattern = ".config", full.names = TRUE)
                              names(set) = basename(set)
                              set
                          })
        examples = examples[ sapply(examples, function(e){ length(e) > 0}) ]
        names(examples) = basename(names(examples))
        examples
    }, error=function(...){
        message("There was an error in getting the built-in examples.")
        ""
    })
    return(examples)
}