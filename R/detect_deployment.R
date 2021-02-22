#' detect deployment
#' 
#' Determine if the the app is running on a local machine, on a virtual 
#' machine (ie, from a docker container) or from a host such as shiny.io
#'
#' @return string, one of "remote", "virtual", or "local"
#'
#' 
detect_deployment <- function(){
    envType = ifelse(isInDocker(), "remote", "local") 
    return(envType)
}
