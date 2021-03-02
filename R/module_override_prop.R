module_override_prop <- function(propName, alias){
    if (BioLockR::hasReadableValue(propName)){
        parts = unlist(strsplit(propName, split = ".", fixed = TRUE))
        parts[1] = alias
        return(paste0(parts, collapse="."))
    }else{
        return(c())
    }
}