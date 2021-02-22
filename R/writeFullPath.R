#'
#' @describeIn writeFilePath write the path from root.
#' 
writeFullPath <- function(path, projectDir){
    newPath = path
    if (file.exists(projectDir)){
        if ( startsWith(path, "./") ){
            message("There is a project dir, and the path starts with it... make the conversion...")
            newPath = file.path(projectDir, substring(path, 3))
            message("newPath: ", newPath)
        }else if (startsWith(path, "../")){
            newPath = file.path(dirname(projectDir), substring(path, 4))
        }
    }
    return(newPath)
}