#' biolockj and biolockj-api
#'
#' Makes a call to the biolockj main program 
#'
#' @param options String
#' @param mainArg String
#'
#' @return writes a text file named as OneMimatchCluster.txt
#'
# biolockj <- function(config=NULL, pipelineDir=NULL, version=FALSE,
#                      precheck_only=FALSE, unused_props=FALSE, verbose=FALSE,
#                      docker=FALSE, aws=FALSE, external_modules=NULL, blj=FALSE, env_var=NULL,
#                      blj_proj=NULL)

### packages should be called via "namespace" ?
# library(rjson)

.bljJar <- function(){
    return("jar/BioLockJ.jar")
}

.bljProj <- function(){
    return("pipelines")
}

.getClassPath <- function(external.modules=NULL){
    cp = .bljJar
    
    # Add JAR file to class path
    if ( !is.null(external.modules)){
        extras = list.files(path = external.modules, pattern=".jar")
        for (jar in extras ){
            cp = paste0(cp, ":", "")
        }
    }
    
    return(cp)
}

.callBioLockJ <- function(args, external.modules=NULL){
    JAR = .bljJar()
    CLASS = "biolockj/BioLockJ"
    cp = .getClassPath( external.modules )
    CMD = capture.output( cat("java -cp", JAR, CLASS, args) )
}



##### API

# Options:
#     
# --external-modules <dir>
#     path to a directory containing additional modules
# --module <module_path>
#     class path for a specific module
# --property <property>
#     a specific property
# --value <value>
#     a vlue to use for a specific property
# --config <file>
#     file path for a configuration file giving one or more property values
# --verbose true
# flag indicating that all messages should go to standard err, including some that are typically disabled.


.callBioLockJApi <- function(args, external.modules=NULL){
    JAR = .bljJar()
    CLASS = "biolockj/api/BioLockJ_API"
    cp = .getClassPath( external.modules )
    CMD = capture.output( cat("java -cp", JAR, CLASS, args) )
    message(CMD)
    return(system(CMD, intern = TRUE))
}

last_pipeline <-function(){
    # Returns the path to the most recent pipeline.
    args = "last-pipeline"
    if ( length( Sys.getenv("BLJ_PROJ") ) == 0 ){
        message("This method depends on the BLJ_PROJ variable.  Please set your pipline directory:")
        message("> Sys.setenv(BLJ_PROJ='/path/to/pipelines/dir')")
    } 
    .callBioLockJApi(args)
}


listModules <- function(external.modules=NULL){
    # Returns a list of classpaths to the classes that extend BioModule.
    args = c("listModules")
    .callBioLockJApi(args, external.modules=external.modules)
}

listApiModules <- function(external.modules=NULL){
    # Like listModules but limit list to modules that implement the ApiModule interface.
    args = c("listApiModules")
    .callBioLockJApi(args, external.modules=external.modules)
}

listProps <- function(module=NULL){
    # Returns a list of properties.
    # If no args, it returns the list of properties used by the BioLockJ backbone.
    # If a modules is given, then it returns a list of all properties used by
    # that module.
    args = c("listProps")
    if ( !is.null(module) ){
        args = c(args, "--module", module)
    }
    .callBioLockJApi(args)
}

listAllProps <- function(external.modules=NULL){
    # Returns a list of all properties, include all backbone properties and all module properties.
    # Optionally supply the path to a directory containing additional modules to include their properties.
    args = c("listAllProps")
    .callBioLockJApi(args, external.modules=external.modules)
}

propType <- function(property, module=NULL, external.modules=NULL ){
    # Returns the type expected for the property: String, list, integer, positive number, etc.
    # If a module is supplied, then the modules propType method is used.
    args = c("propType", "--property", property)
    if ( !is.null(module) ){
        args = c(args, "--module", module)
    }
    .callBioLockJApi(args, external.modules=external.modules)
}

describeProp <- function(property, module=NULL, external.modules=NULL ){
    # Returns a description of the property.
    # If a module is supplied, then the modules getDescription method is used.
    args = c("describeProp", "--property", property)
    if ( !is.null(module) ){
        args = c(args, "--module", module)
    }
    .callBioLockJApi(args, external.modules=external.modules)
}

propValue <- function(property, config=NULL, module=NULL, external.modules=NULL ){
    # Returns the value for that property given that config file (optional) or 
    # no config file (ie the default value)
    args = c("propValue", "--property", property)
    if ( !is.null(config) ){
        args = c(args, "--config", config)
    }
    if ( !is.null(module) ){
        args = c(args, "--module", module)
    }
    returnVal = .callBioLockJApi(args, external.modules=external.modules)
    if (returnVal=="null") returnVal = ""
    return(returnVal)
}

isValidProp <- function(property, value, module=NULL, external.modules=NULL ){
    # T/F/NA. Returns true if the value (val) for the property (prop) is valid;
    # false if prop is a property but val is not a valid value,
    # and NA if prop is not a recognized property.
    # IF a module is supplied, then additionally call the validateProp(key, value)
    # for that module, or for EACH module if a comma-separated list is given.
    args = c("isValidProp", "--property", property, "--value", value)
    if ( !is.null(module) ){
        args = c(args, "--module", module)
    }
    val = .callBioLockJApi(args, external.modules=external.modules)
    returnVal = NA
    if (val=="true") returnVal = TRUE
    if (val=="false") returnVal = FALSE
    return(returnVal)
}

propInfo <- function(){
    # Returns a json formatted list of the general properties (listProps)
    # with the type, descrption and default for each property
    args=c("propInfo")
    json_lines = .callBioLockJApi(args)
    json_str = paste(json_lines, collapse="")
    obj = rjson::fromJSON(json_str)
    return(obj)
}

moduleInfo <- function(external.modules=NULL){
    # Returns a json formatted list of all modules and for each module that 
    # implements the ApiModule interface, it lists the props used by the module,
    # and for each prop the type, descrption and default.
    args=c("moduleInfo")
    json_lines = .callBioLockJApi(args, external.modules=external.modules)
    json_str = paste(json_lines, collapse="")
    obj = rjson::fromJSON(json_str)
    return(obj)
}

listMounts <- function(){
    # Returns a list of directories that would need to be mounted in order for 
    # the files listed in the config file to be available to a pipeline running in docker.
    args=c("listMounts")
    .callBioLockJApi(args)
}

listUploads <- function(){
    # Returns a list of file and directories that would need to be uploaded in order for 
    # the files listed in the config file to be available to a pipeline running in the cloud.
    args=c("listUploads")
    .callBioLockJApi(args)
}
