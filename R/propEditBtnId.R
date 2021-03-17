#' 
#' @describeIn propUiName name for a button to switch focus to a ui where you can edit a displayed property
#'
propEditBtnId <- function(propName, moduleId){
    paste0(moduleId, propUiName(propName), "EditBtn")
}