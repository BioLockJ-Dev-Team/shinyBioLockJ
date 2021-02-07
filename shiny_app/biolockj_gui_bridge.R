
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

isInDocker <- function(){
    if (file.exists("/.dockerenv")){
        message("I must be running inside a docker container; because the /.dockerenv file exists.")
        return(TRUE)
    }else{
        message("I must NOT be running inside a docker container; there is no /.dockerenv file")
        return(FALSE)
    }
}

isBadPrecheckResponse <- function(response){
    badKey = "There is a problem with this pipeline configuration."
    any( grepl(badKey, response) )
}

isGoodPrecheckResponse <- function(response){
    goodKey = "Precheck is complete. No problems were found in this pipeline configuration."
    any( grepl(goodKey, response) )
}

getModuleOverrideProp <- function(moduleAlias, propName){
    parts=strsplit(propName, split=".", fixed=TRUE)[[1]]
    parts[1] = moduleAlias
    return( paste(parts, collapse=".") )
}

modulePerProp <- function(moduleInfo=moduleInfo()){
    # Returns a list of properties, each element is a vector of module names 
    # giving the modules that use that property
    a = sapply(initmoduleInfo, function(mi){names(mi$properties)})
    d1=stack(a)
    return(split(as.character(d1$ind), d1$values))
}

isSharedProp <- function(propMods=modulePerProp(), propName, includeMods=unique(unlist(propMods))){
    # Determines if the given property is used by more than one of the modules in the pipeline.
    # propMods - output of modulePerProp
    # propName - property in question
    # includeMods - the modules in the current pipeline
    propsMods = propMods[[propName]]
    countMods = intersect(propsMods, includeMods)
    if (length(countMods) > 1 ) return(TRUE)
    else( return(FALSE) )
}

readBljProps <- function(existingLines){
    exProps = existingLines[ grep("^#", existingLines, invert = TRUE) ]
    if ( any(grepl("=", exProps)) ){
        splits = strsplit(exProps, split="=", fixed=TRUE)
        splits = splits[which(sapply(splits, function(s){length(s) >= 2}))]
        vals = sapply(splits, function(pair){trimws(paste0(pair[2:length(pair)], collapse=""))})
        names(vals) = sapply(splits, function(pair){trimws(pair[1])})
        return(vals)
    }else{
        return(c())
    }
}

orderDefaultPropFiles <- function(start, chain){
    # chain - a named list whose names are file names (basename) 
    # and whose elements are each the vecter of file names (full file path) that the named file lists as default props.
    # start - a file name (basename) that correstponds to one of the names of the chain list
    #
    # returns a list of 3:
    # missing: full file paths whose basename is not in names(chain)
    # dangling: The name of the file that lead to the file path(s) that are missing
    # chained: a vector of basenames, ending with 'start'.  This is the order in which to load the available default props files.
    result = list(missing=c(), dangling=c(), chained=c())
    
    message("start: ", start, "; chain: ", chain)
    
    # if (start %in% names(chain)){
    missingStarts = setdiff(start, names(chain))
    if (length(missingStarts) == 0){
        result$chained=c(start)
    }else{
        result$missing = missingStarts
        return(result)
    }
    parent = c(start)
    parentIndex = 1
    children = chain[[parent[parentIndex]]]
    children = children[!is.na(children)]
    if (length(children) > 0) missing = setdiff(basename(children), names(chain))
    while(length(children) > 0 && length(missing) == 0){
        message("in while loop with children: ", children)
        
        result$chained = c(basename(children), result$chained)
        parent = c(parent, basename(children))
        parentIndex = parentIndex + 1
        children = chain[[parent[parentIndex]]]
        children = children[!is.na(children)]
        while( length(children) == 0 && length(parent) > parentIndex){
            parentIndex = parentIndex + 1
        }
        
        message("who's missing? current children: ", children)
        if (length(children) > 0) missing = setdiff(basename(children), names(chain))
    }
    if (length(children) > 0){
        result$dangling = parent[parentIndex]
        result$missing = missing
    }
    return(result)
}

parseListProp <- function(value){
    trimws( unlist( strsplit( value, ",", fixed=TRUE) ) )
}
