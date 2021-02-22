#' Update File Path Choices
#'
#' @param existing current string value given in properties
#' @param newPath optional, new path to add to list
#'
#' @return the new set of choices, essentially c(existing, newPath)
#'
filePathChoices = function(existing, newPath=NULL){
    if ( isWritableValue( existing ) ){
        oldSet = BioLockR::parseListProp( existing )
    }else{
        oldSet = c()
    }
    if (!is.null(newPath)) opts = c(oldSet, newPath)
    else opts = oldSet
    names(opts) <- NULL
    return(opts)
}