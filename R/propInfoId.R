#'
#' @describeIn propUiName the ui id for the info icon about defaults
#'
#' @return an id
#'
propInfoId <- function(propName, moduleId = NULL){
    paste0(propUiName(propName, moduleId = NULL), "Info")
}