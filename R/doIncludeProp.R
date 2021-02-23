#' Include Property
#' 
#' Determine if a given property should be include in the config file.
#'
#' @param property property name
#' @param value property value
#' @param default the default property for the value (if any)
#' @param input in general, input=input
#'
#' @return
#' @export
#'
#' @examples
doIncludeProp <- function(property, value, default=NULL, input){
    if (isWritableValue(value)){
        notTheDefault = !is.null(default) && value != default
        # message("value of property ", property, "=", value, " is ", ifelse(notTheDefault, "NOT", ""), " the same as the default value: ", defaults$values[p])
        return(is.null(default) || notTheDefault || input$include_standard_defaults)
    }else{
        return(FALSE)
    }
}