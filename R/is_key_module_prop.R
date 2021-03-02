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