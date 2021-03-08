#' Is this a key property
#' 
#' The logic for this mimics the logic used to separate properties in the user guide.
#' A property is a "key" property for a given module if the property is not also a general property OR
#' if the description is different from the general property description.
#'
#' @param prop 
#' @param genPropInfo 
#'
#' @return boolean
#'
is_key_module_prop <- function(prop, genPropInfo=BioLockR::propInfo()){
    propName = prop$property
    if ( propName %in% names(genPropInfo)){
        propDesc = prop$description
        genPropDesc = genPropInfo[[propName]]$description
        if (propDesc == genPropDesc){
            isKey = FALSE
        }else{
            isKey = TRUE
        }
    }else{
        isKey = TRUE
    }
    return(isKey)
}