#'
#' @describeIn writeFilePath write the path from the project directory (ie, ./ or ../) if possible.
#' 
writeRelPath <- function(path, projectDir){
    newPath = path
    if (file.exists(projectDir)){
        if ( startsWith(path, projectDir) ){
            newPath = gsub(pattern=projectDir, replacement=".", path, fixed=TRUE)
        }else if (startsWith(path, dirname(projectDir))){
            newPath = gsub(pattern=dirname(projectDir), replacement="..", path, fixed=TRUE)
        }
    }
    return(newPath)
}
