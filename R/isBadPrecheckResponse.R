#' Precheck Response
#' 
#' Determine if a response should overall be viewed as good (isGoodPrecheckResponse) or bad (isBadPrecheckResponse)
#'
#' @describeIn isBadPrecheckResponse check if the response is BAD
#' 
#' @param response the output of the precheck command to biolockj
#'
#' @return isBadPrecheckResponse: boolean, TRUE if the response is BAD
#'
isBadPrecheckResponse <- function(response){
    badKey = "There is a problem with this pipeline configuration."
    any( grepl(badKey, response) )
}