#' Group Properties By Category
#'
#' The portion of a property name that comes before the first '.' is (generally) its category.
#'
#' @param propInfo 
#'
#' @return
#'
groupPropsByCategory <- function(propInfo) { 
    props = names(propInfo)
    splits = strsplit(props, split = ".", fixed = TRUE)
    category = sapply(splits, function(s){s[1]})
    return(split(props, f=category))
}