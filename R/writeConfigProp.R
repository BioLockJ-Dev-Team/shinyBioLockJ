#' Format a property for BioLockJ
#' 
#' Write a property in the format suitable for a BioLockJ property.
#'
#' @param propName the name of the property
#' @param propVal the value for the property
#' @param propType the type of property
#' @param projectDir the project directory (the folder where the config file lives), only used if _useRelPath=TRUE_
#' @param useRelPath boolean, should file paths be written as relative to the projectDir
#'
#' @return
#'
writeConfigProp <- function(propName, propVal=NULL, propType="string", projectDir="", useRelPath=FALSE){
    message("Writting up the value for prop ", propName, " which is of type ", propType)
    if ( is.null(propVal) || is.na(propVal) ){
        line = ""
    }else if(trimws(paste(propVal,collapse="")) == ""){
        line = paste(propName, "=", propVal)
    }else{
        if (propType == "boolean"){
            result = "N" # any non-empty non-null value is treated as false
            if (propVal==TRUE || propVal == "Y" || propVal == "TRUE") result = "Y"
            line = paste(propName, "=", result)
        }else if(propType == "list"){
            line = paste(propName, "=", BioLockR::printListProp(propVal) )
        }else if (propType == "file path"){
            line = paste(propName, "=", writeFilePath(propVal, projectDir, useRelPath ) )
        }else if(propType == "list of file paths"){
            line = paste(propName, "=", writeFilePathList(propVal, projectDir, useRelPath) )
        }else{
            line = paste(propName, "=", propVal)
        }
    }
    line
}