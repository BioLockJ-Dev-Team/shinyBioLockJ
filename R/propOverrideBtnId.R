#' 
#' @describeIn propUiName name for a button to create a module-instance-override form of the property
#' @param module the module alias
#'
propOverrideBtnId <- function(propName, module){
    paste0(module, propUiName(propName), "overrideBtn")
}
