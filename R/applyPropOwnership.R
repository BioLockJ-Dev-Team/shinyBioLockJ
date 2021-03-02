#' Title
#'
#' @param modules list of module info, elements should be named, generally similar what is returned by BioLockR::moduleInfo()
#' @param genProps vector of general property names
#'
#' @return 
#' a list similar to _modules_, but where the properties objects have added elements "override" and "ownership";
#' override is the module-specific override property for this this module and property and 
#' ownership is one of "general", "shared", or "single"
#'
#' @examples
applyPropOwnership <- function( modules, genProps){
    
    modsPerProp = modulePerProp(modules)
    numMods = sapply(modsPerProp, length)
    multiModule = modsPerProp[which(numMods >1)]

    new = lapply(names(modules), function(alias){
        mod = modules[[alias]]
        mod$properties = lapply(mod$properties, function(propObj){
            propName = propObj$property
            propObj$override = module_override_prop(propName, alias)
            if (propName %in% genProps){
                propObj$ownership = "general"
            }else if (propName %in% multiModule){
                propObj$ownership = "shared"
            }else{
                propObj$ownership = "single"
            }
            propObj
        })
        mod
    })
    names(new) = names(modules)
    
    return( new )
}
