#' Group Properties By Category
#'
#' The portion of a property name that comes before the first '.' is (generally) its category.
#'
#' @param propInfo the return value fo BioLockR::propInfo(), or a similarly structured list
#'
#' @return category portion fo the property
#' 
groupPropsByCategory <- function(propInfo) { 
    if (isWritableValue(propInfo)){
        props = names(propInfo)
        splits = strsplit(props, split = ".", fixed = TRUE)
        category = sapply(splits, function(s){s[1]})
        return(split(props, f=category))
    }else{
        return(c())
    }
}