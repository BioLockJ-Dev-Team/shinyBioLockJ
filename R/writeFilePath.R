#' File Paths for BioLockJ
#' 
#' Write file paths for a BioLockJ config file.  
#'
#' @param path a file path (written as either from root or starting with ./ or ../)
#' @param projectDir the path that the paths should be relative to
#' @param useRelPath if TRUE, relative paths are written, if false, then full paths are determined.
#'
#' @details 
#' Paths can be passed in as EITHER relative to root or relative to project dir (ie, starting with './' or '../').  
#' _useRelPath_ dictacts how the output is written, not how the input is interpreted.
#' 
#' @return a string representing the file path
#'
# @examples
# writeFilePath("/Users/me/my/path/to/file.txt", "/Users/me/my/path", FALSE)
# #> [1] "/Users/me/my/path/to/file.txt"
# writeFilePath("/Users/me/my/path/to/file.txt", "/Users/me/my/path", TRUE)
# #> [1] "/Users/me/my/path/to/file.txt"
writeFilePath <- function(path, projectDir, useRelPath){
    message("Formatting file path: ", path)
    if (useRelPath) message("relative to: ", projectDir)
    else message("possibly extracted relative path from: ", projectDir)
    
    if (useRelPath){
        return( writeRelPath(path, projectDir) )
    }else{
        return( writeFullPath(path, projectDir) )
    }
}