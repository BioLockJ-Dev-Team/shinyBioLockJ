#' Title
#'
#' @param modules list of module info, elements should be named, generally similar what is returned by BioLockR::moduleInfo(), elements must be named using their alias within the pipeline.
#' @param sharedModuleProps a vector of properties that are used by more than one module (should not include general properties)
#' @param genPropInfo list of property info object, similar to what is returned by BioLockR::propInfo()
#'
#' @return 
#' a list similar to _modules_, but where the properties objects have added elements "override", "isKey", and "ownership";
#' override is the module-specific override property for this this module and property and 
#' ownership is one of "general", "shared", or "single"
#'
#' @examples
applyPropOwnership <- function( modules, sharedModuleProps, genPropInfo=BioLockR::propInfo()){
    new = lapply(names(modules), function(alias){
        mod = modules[[alias]]
        #
        # add features to properties
        mod$properties = lapply(mod$properties, function(propObj){
            propName = propObj$property
            propObj$override = module_override_prop(propName, alias)
            if (propName %in% names(genPropInfo)){
                propObj$ownership = "general"
            }else if (propName %in% sharedModuleProps){
                propObj$ownership = "shared"
            }else{
                propObj$ownership = "single"
            }
            propObj$isKey = is_key_module_prop(propObj, genPropInfo)
            propObj
        })
        #
        # Order the properties
        orderedPropNames = names(mod$properties)
        #names in alphabetical order
        orderedPropNames = orderedPropNames[order(orderedPropNames)] 
        # order so that key props are first
        orderedPropNames = orderedPropNames[order(sapply(orderedPropNames, function(name){mod$properties[[name]]$isKey}), decreasing = TRUE)]
        # apply order
        mod$properties = mod$properties[orderedPropNames]
        #
        mod
    })
    names(new) = names(modules)
    
    return( new )
}
