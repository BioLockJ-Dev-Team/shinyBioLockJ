#' 
#' @describeIn propUiName name for a tabpanel within a module section, flips between panel for active ui and panel for viewing the proprety that is set elsehwere.
#'
propModuleFlipPanel <- function(propName, module){
    return( paste0(propUiName(propName), module, "FlipperPanel") )
}