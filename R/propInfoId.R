#' Title
#'
#' @param propUiName often the output of propUiName or module_prop_UI_name
#'
#' @return an id
#'
propInfoId <- function(propUiName){
    paste0(propUiName, "Info")
}