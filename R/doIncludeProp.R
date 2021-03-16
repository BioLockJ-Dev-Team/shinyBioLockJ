#' Include Property
#' 
#' Determine if a given property should be include in the config file.
#'
#' @param property property name
#' @param value property value
#' @param default the default property for the value (if any)
#' @param input in general, input=input
#'
#' @return TRUE if the property should be included in the config file
#' 
doIncludeProp <- function(property, value, default=NULL, input){
    if (BioLockR::isReadableValue(value)){
        if ( BioLockR::isReadableValue(default) ){
            notTheDefault = value != default
        }else{
            notTheDefault = TRUE
        }
        message("value of property ", property, "=", value, " is ", ifelse(notTheDefault, "NOT", ""), " the same as the default value: ", default)
        return( !BioLockR::isReadableValue(default) || notTheDefault || input$include_standard_defaults)
    }else{
        return(FALSE)
    }
}