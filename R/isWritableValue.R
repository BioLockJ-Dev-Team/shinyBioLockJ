#' Is Writable Value
#' 
#' Test that a value is something a method can work with.
#' 
#' @param value The value to test
#'
#' @return boolean
#'
# @examples
# isWritableValue(NULL)
# #> [1] FALSE
# isWritableValue(NA)
# #> [1] FALSE
# isWritableValue("apple")
# #> [1] TRUE
# isWritableValue( -5 )
# #> [1] TRUE
# isWritableValue( c(1,2,3))
# #> [1] TRUE
isWritableValue <- function(value){
    BioLockR::isReadableValue(value)
}