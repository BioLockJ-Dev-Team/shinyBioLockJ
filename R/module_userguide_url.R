#' Create a user guide link for a module.
#'
#' @param class class path for a BioLockJ module
#' @param baseUrl the base url for the user guide; see details
#'
#' @return url as a string
#'
#' @examples
#' module_userguide_url("biolockj.module.diy.GenMod")
module_userguide_url <-function(class, baseUrl="https://biolockj-dev-team.github.io/BioLockJ/GENERATED/"){
    parts = unlist(strsplit(trimws(class), ".", fixed=TRUE))
    last = length(parts)
    pathend = last - 1
    name = parts[last]
    path = paste0(parts[1:pathend], collapse = ".")
    url = paste0(baseUrl, path, "/", name, "/")
    return(url)
}