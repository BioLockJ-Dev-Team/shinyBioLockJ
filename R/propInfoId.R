#' 
#' @describeIn propUiName
#'
propInfoId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "Info")
}