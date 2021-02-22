#' Test if a string is a valid module alias
#' 
#' Test if an alias is usable for a BioLockJ pipeline module.
#'
#' @param alias the candidate alias
#' @param existingAlia character vector of the aliases currently in use
#'
#' @return boolean
#' 
#' @details 
#' If false, a message is printed to stderr with explaination.
#'
# @examples
# isValidAlias("foo")
# #> FALSE
# isValidAlias("Foo")
# #> TRUE
# isValidAlias("Foo", c("Foo", "Bar", "Baz"))
# #' #> FALSE
isValidAlias <- function(alias, existingAlia=c() ){
    firstChar = substr(alias,1,1)
    if ( ! firstChar %in% LETTERS ) {
        message("To be a valid alias, the first character must be a capital letter.")
        return(FALSE)
    }
    if ( grepl(" ", alias)){
        message("An alias cannot contain spaces.")
        return(FALSE)
    }
    if ( grepl(pattern="[^[:alnum:]_-]", alias) ){
        message("An alias can only contain alphanumeric characters, '_', and '-'.")
        return(FALSE)
    }
    if (alias %in% existingAlia){
        message("Each alias must be unique. There is already a module called ", alias)
        return(FALSE)
    }
    return(TRUE)
}