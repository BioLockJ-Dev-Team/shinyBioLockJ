
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
    if (grepl("AS", line)){
        parts = strsplit(line, "AS", fixed=TRUE)[[1]]
    }else{
        parts = strsplit(line, ".", fixed=TRUE)[[1]]
    }
    alias = trimws(parts[length(parts)])
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
    if (alias %in% existingAlia){
        message("Each alias must be unique. There is already a module called ", alias)
        return(FALSE)
    }
    return(TRUE)
}