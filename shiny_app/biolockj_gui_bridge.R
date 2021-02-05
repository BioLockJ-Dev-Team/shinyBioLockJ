
#
# biolockj.R represents assumptions based on the biolockj help page and the biolockj-api help page.
# It should be updated anytime there is a change to the api that warrants updating the help page.
# It should be robust to under-the-hood changes that do not warrant changes in the help page.
#
# The gui requires some logic that makes assumptions BEYOND the basics that biolockj.R relies on.
#
# This set of functions represent additional assumptions.  

makeRunLine <- function(moduleName, alias=""){
    className = moduleRunLines[moduleName]
    if ( nchar(alias) > 0 ) {
        runLine = paste(className, "AS", alias)
    }else{
        runLine = paste(className)
    }
    return(runLine)
}

aliasFromRunline <- function(line){
    alias=NA
    if (!is.null(line) && !is.na(line) && length(line)>0){
        if (grepl(" AS ", line)){
            parts = strsplit(line, " AS ", fixed=TRUE)[[1]]
        }else{
            parts = strsplit(line, ".", fixed=TRUE)[[1]]
        }
        alias = trimws(parts[length(parts)])
    }
    return(alias)
}

isValidAlias <- function(alias, existingAlia=c() ){
    firstChar = substr(alias,1,1)
    if ( ! firstChar %in% LETTERS ) {
        message("To be a valid alias, the first character must be a capital letter.")
        return(FALSE)
    }
    if ( grepl(" ", alias)){
        message("An alias cannot contain spaces.")
        return(FALSE)
    }
    if ( grepl(pattern="[^[:alnum:]_-]", alias) ){
        message("An alias can only contain alphanumeric characters, '_', and '-'.")
        return(FALSE)
    }
    if (alias %in% existingAlia){
        message("Each alias must be unique. There is already a module called ", alias)
        return(FALSE)
    }
    return(TRUE)
}

writeConfigProp <- function(propName, propVal=NULL, propType="string"){
    if ( is.null(propVal) || is.na(propVal) ){
        line = ""
    }else if(trimws(propVal) == ""){
        line = paste(propName, "=", propVal)
    }else{
        if (propType == "boolean"){
            result = "N" # any non-empty non-null value is treated as false
            if (propVal==TRUE || propVal == "Y" || propVal == "TRUE") result = "Y"
            line = paste(propName, "=", result)
        }else{
            line = paste(propName, "=", propVal)
        }
    }
    line
}

propInfoSansSpecials <- function(){
    info = propInfo()
    # remove special properties that are set separately or 
    info[["biolockj.version"]] <- NULL # only meant to be set by running biolockj
    info[["pipeline.defaultProps"]] <- NULL # gui needs to actually upload files
    return(info)
}

groupPropsByCategory <- function(propInfo) { 
    props = names(propInfo)
    splits = strsplit(props, split = ".", fixed = TRUE)
    category = sapply(splits, function(s){s[1]})
    return(split(props, f=category))
}

getModuleRunLines <- function(moduleInfo){
    moduleRunLines = sapply(moduleInfo, function(mi){mi$usage})
    names(moduleRunLines) <- names(moduleInfo)
    return(moduleRunLines)
}

hasDockerCmd <- function(){
    dockerVersion = system("docker --version", intern = TRUE)
    if (grepl("Docker version", dockerVersion)){
        message("Found docker version: ", dockerVersion)
        return(TRUE)
    }else{
        message("No docker command.")
        return(FALSE)
    }
}

isInDocker <- function(){
    if (file.exists("/.dockerenv")){
        message("I must be running inside a docker container; because the /.dockerenv file exists.")
        return(TRUE)
    }else{
        message("I must NOT be running inside a docker container; there is no /.dockerenv file")
        return(FALSE)
    }
}

