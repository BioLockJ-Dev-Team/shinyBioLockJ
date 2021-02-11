
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
            line = paste(propName, "=", printListProp(propVal) )
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
    # Returns a list of 2:
    #   defaultProps: the vector of values given by the pipeline.defaultProps list
    #   properties: all properties other than pipeline.defaultProps
    exProps = existingLines[ grep("^#", existingLines, invert = TRUE) ]
    ret = list(defaultProps=c(), properties=c())
    if ( any(grepl("=", exProps)) ){
        splits = strsplit(exProps, split="=", fixed=TRUE)
        splits = splits[which(sapply(splits, function(s){length(s) >= 2}))]
        vals = sapply(splits, function(pair){trimws(paste0(pair[2:length(pair)], collapse=""))})
        names(vals) = sapply(splits, function(pair){trimws(pair[1])})
        #
        ret$properties=vals[names(vals) != "pipeline.defaultProps"]
        chainsTo = vals["pipeline.defaultProps"]
        if ( !is.null(chainsTo) && !is.na(chainsTo) && !chainsTo==""){
            ret$defaultProps = parseListProp(chainsTo)
        }
    }
    return(ret)
}

orderDefaultPropFiles <- function(start, chain){
    # chain - a named list whose names are file names (basename) and whose elements are 
    #         each the vecter of file names (full file path) that the named file lists as default props.
    # start - a file name (basename) that correstponds to one of the names of the chain list
    #
    # returns a list of:
    # missing: full file paths whose basename is not in names(chain)
    # dangling: The name of the file that lead to the file path(s) that are missing
    # chained: a vector of basenames, ending with 'start'.  This is the order in which to load the available default props files.
    result = list(missing=c(), dangling=c(), chained=c(start))
    if ( length(chain) == 0 || length(start) == 0 ) return(result)
    
    message("start: ", printListProp(start), "; chain: ", chain)
    
    missingStarts = setdiff(start, names(chain))
    if (length(missingStarts) > 0){
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
        result$missing = missing[!is.na(missing)]
    }
    return(result)
}

parseListProp <- function(value){
    # value - single string that is the property value
    # returns a vector
    trimws( unlist( strsplit( value, ",", fixed=TRUE) ) )
}

printListProp <- function(values){
    paste(values, collapse=", ")
}

findExampleConfigs <- function(bljDir="BioLockJ"){
    examples = tryCatch({
        templatesDir = file.path(bljDir, "templates")
        examples = sapply(dir(templatesDir, include.dirs = TRUE, full.names = TRUE), 
                          function(d){
                              set = dir(d, pattern = ".config", full.names = TRUE)
                              names(set) = basename(set)
                              set
                          })
        examples = examples[ sapply(examples, function(e){ length(e) > 0}) ]
        names(examples) = basename(names(examples))
        examples
    }, error=function(...){""})
    return(examples)
}

isWritableValue <- function(value){
    return( !is.null(value) && !is.na(value) && length(value) > 0 && nchar(value) > 0 )
}

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

writeFilePathList <- function(pathList, projectDir, useRelPath){
    # pathList - The value of a list property, probably represented as a single string
    # projectDir - the path that the paths should be relative to
    # useRelPath - if TRUE, relative paths are written, if false, then full paths are determined.
    paths = parseListProp(pathList)
    newPaths = sapply(paths, writeFilePath, projectDir=projectDir, useRelPath=useRelPath)
    # if (useRelPath){
    #     newPaths = writeRelPath(paths, projectDir)
    # }else{
    #     newPaths = writeFullPath(paths, projectDir)
    # }
    return(printListProp(newPaths))
}
