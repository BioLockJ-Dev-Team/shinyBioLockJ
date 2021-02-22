#' Shared Properties
#' 
#' Determines if the given property is used by more than one of the modules in the pipeline.
#' 
#' @param propMods output of modulePerProp
#' @param propName property in question
#' @param includeMods the modules in the current pipeline
#'
#' @return
#' 
isSharedProp <- function(propMods=modulePerProp(), propName, includeMods=unique(unlist(propMods))){
    propsMods = propMods[[propName]]
    countMods = intersect(propsMods, includeMods)
    if (length(countMods) > 1 ) return(TRUE)
    else( return(FALSE) )
}