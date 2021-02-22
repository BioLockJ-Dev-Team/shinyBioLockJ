#' Limit Prop Info for GUI
#' 
#' Get the properties from BioLockJ, but remove the properties that are not desired.
#'
#' @return
#'
propInfoSansSpecials <- function(){
    info = BioLockR::propInfo()
    info[["biolockj.version"]] <- NULL # only meant to be set by running biolockj
    info[["pipeline.defaultProps"]] <- NULL # gui needs to actually upload files
    return(info)
}