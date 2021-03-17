#' 
#' @describeIn propUiName name for a button to remove a module-instance-override form of the property
#'
propRmOverrideBtnId <- function(propName, moduleId){
    paste0(moduleId, propUiName(propName), "RmOverrideBtn")
}