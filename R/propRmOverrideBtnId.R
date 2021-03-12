#' 
#' @describeIn propUiName name for a button to remove a module-instance-override form of the property
#'
propRmOverrideBtnId <- function(propName, module){
    paste0(module, propUiName(propName), "RmOverrideBtn")
}