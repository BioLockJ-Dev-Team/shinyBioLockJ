#' Test for docker container
#'
#'
isInDocker <- function(){
    if (file.exists("/.dockerenv")){
        message("I must be running inside a docker container; because the /.dockerenv file exists.")
        return(TRUE)
    }else{
        message("I must NOT be running inside a docker container; there is no /.dockerenv file")
        return(FALSE)
    }
}