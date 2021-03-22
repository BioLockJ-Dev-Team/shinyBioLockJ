#' markdownToHTML
#'
#' A modified form of the markdown::markdownToHTML function.
#'
#' @param text passed to the text argument in markdown::markdownToHTML
#'
#' @return html
#' 
markdownTextToHTML <- function(text){
    if ( !BioLockR::hasReadableValue(text)) text = ""
    html <- markdown::markdownToHTML(text=text, fragment.only = TRUE)
    Encoding(html) <- "UTF-8"
    return(HTML(html))
}