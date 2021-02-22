#' @describeIn isBadPrecheckResponse check if the response is GOOD
#'
#' @return isGoodPrecheckResponse: boolean, TRUE if the response is GOOD
#' 
isGoodPrecheckResponse <- function(response){
    goodKey = "Precheck is complete. No problems were found in this pipeline configuration."
    any( grepl(goodKey, response) )
}