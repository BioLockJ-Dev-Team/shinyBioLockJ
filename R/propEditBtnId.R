#' 
#' @describeIn propUiName name for a button to switch focus to a ui where you can edit a displayed property
#'
propEditBtnId <- function(propName, module){
    paste0(module, propUiName(propName), "EditBtn")
}