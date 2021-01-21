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
biolockj_api <- function(args)
{
    # initialize Java Virtual Machine (JVM)
    rJava::.jinit()
    
    inputArray <- rJava::.jarray(args)
    jarDir <- "/Users/ieclabau/git/BioLockJ/dist"
    jarName <- 'BioLockJ.jar'
    
    # Add JAR file to class path
    rJava::.jaddClassPath(file.path(jarDir, jarName))
    
    # Display class path
    print(paste0('Class Path: ', rJava::.jclassPath()))
    
    # call biolockj
    rJava::.jcall("biolockj/api/BioLockJ_API", returnSig = "V", "main", inputArray)
    
}

listModules <- function(prefix=NULL, external.modules=NULL)
{
    querry = "listModules"
    
    # initialize Java Virtual Machine (JVM)
    rJava::.jinit()
    
    args = c(querry)
    if (!is.null(prefix)){
        args = c( args, prefix)
    }
    inputArray <- rJava::.jarray(c("listModules", args))
    jarDir <- "/Users/ieclabau/git/BioLockJ/dist"
    jarName <- 'BioLockJ.jar'
    
    # Add JAR file to class path
    rJava::.jaddClassPath(file.path(jarDir, jarName))
    if ( !is.null(external.modules)){
        rJava::.jaddClassPath(list.files(path = external.modules, pattern=".jar"))
    }
    
    # Display class path
    print(paste0('Class Path: ', rJava::.jclassPath()))
    
    # call biolockj
    rJava::.jcall("biolockj/api/BioLockJ_API", returnSig = "List<String>", "listModules", inputArray)
}

.bljJar <- function(){
    return("/Users/ieclabau/git/BioLockJ/dist/BioLockJ.jar")
}

.bljProj <- function(){
    return("/Users/ieclabau/git/shiny_BioLockJ/pipelines")
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
    CLASS = "biolockj/BioLockJ"
    cp = .getClassPath( external.modules )
    CMD = capture.output( cat("java -cp", JAR, CLASS, args) )
}

.callBioLockJApi <- function(args, external.modules=NULL){
    CLASS = "biolockj/api/BioLockJ_API"
    cp = .getClassPath( external.modules )
    CMD = capture.output( cat("java -cp", JAR, CLASS, args) )
    message(CMD)
    return(system(CMD, intern = TRUE))
}

last_pipeline <-function(){
    args = "last-pipeline"
    if ( length( Sys.getenv("BLJ_PROJ") ) == 0 ){
        message("This method depends on the BLJ_PROJ variable.  Please set your pipline directory:")
        message("> Sys.setenv(BLJ_PROJ='/path/to/pipelines/dir')")
    } 
    .callBioLockJApi(args)
}

listModules <- function(external.modules=NULL){
    args = c("listModules")
    .callBioLockJApi(args, external.modules=external.modules)
}

propType <- function(property, module=NULL, external.modules=NULL ){
    
}

# 
# propType <- function(property, module=NULL, external.modules=NULL )
# {
#     .initBioLockJ(external.modules)
#         
#     args = c("propType", "--property", property)
#     if (!is.null(module)){
#         args = c( args, "--module", module)
#     }
#     
#     # call biolockj
#     val = capture.output( rJava::.jcall("biolockj/api/BioLockJ_API", returnSig = "V", method = "main", rJava::.jarray(args) ), 
#                           file=NULL )
#    return(val)
# }
# 
# .initBioLockJ <- function(external.modules=NULL){
#     # initialize Java Virtual Machine (JVM)
#     rJava::.jinit()
#     jarDir <- "/Users/ieclabau/git/BioLockJ/dist"
#     jarName <- 'BioLockJ.jar'
#     
#     # Add JAR file to class path
#     rJava::.jaddClassPath(file.path(jarDir, jarName))
#     if ( !is.null(external.modules)){
#         rJava::.jaddClassPath(list.files(path = external.modules, pattern=".jar"))
#     }
#     
#     # Display class path
#     #print(paste0('Class Path: ', rJava::.jclassPath()))
# }
