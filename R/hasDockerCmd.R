#' Has Docker Command
#' 
#' Test if the docker command is available.
#'
#' @return boolean
#'
#' @examples
#' hasDockerCmd()
hasDockerCmd <- function(){
    dockerVersion = tryCatch({
        system("docker --version", intern = TRUE)
    }, error=function(...){""})
    if (grepl("Docker version", dockerVersion)){
        message("Found docker version: ", dockerVersion)
        return(TRUE)
    }else{
        message("No docker command.")
        return(FALSE)
    }
}