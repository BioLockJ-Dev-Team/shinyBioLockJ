#' Name UI elements for BioLockJ Properties
#'
#' @param propName the name of a BioLockJ property, such as "script.numThreads"
#' @param module the module id
#' 
#' @return a string to use for an associated UI element
#'
module_prop_UI_name <- function(propName, module){
    paste(module, "_", propUiName(propName))
}