#' Order Default Props Files
#' 
#' Model default props inter-dependences
#'
#' @param start a file name (full path or basename) that correstponds to one of the names of the chain list
#' @param chain a named list whose names are file names (basename) and whose elements are 
#' each the vecter of file names (full file path) that the named file lists as default props.
#'
#' @details 
#' On a remote server, the value of _start_ is already the basename; locally, its a full file path. If a file path is passed in, the basename of the path is taken to match to the names of _chain_.
#' 
#' @return 
#' a list of:
#' missing: full file paths whose basename is not in names(chain)
#' dangling: The name of the file that lead to the file path(s) that are missing
#' chained: a vector of basenames, ending with 'start'.  This is the order in which to load the available default props files.
#' 
orderDefaultPropFiles <- function(start, chain){
    if( BioLockR::isReadableValue(start) ) start = basename(start)
    
    result = list(missing=c(), dangling=c(), chained=c(start))
    if ( (!BioLockR::isReadableValue(chain)) || (!BioLockR::isReadableValue(start)) ) return(result)
    
    message("start: ", BioLockR::printListProp(start), "; chain: ", chain)
    
    missingStarts = setdiff(start, names(chain))
    if (length(missingStarts) > 0){
        result$missing = missingStarts
        return(result)
    }
    parent = c(start)
    parentIndex = 1
    children = chain[[parent[parentIndex]]]
    children = children[!is.na(children)]
    if (length(children) > 0) missing = setdiff(basename(children), names(chain))
    while(length(children) > 0 && length(missing) == 0){
        message("in while loop with children: ", children)
        
        result$chained = c(basename(children), result$chained)
        parent = c(parent, basename(children))
        parentIndex = parentIndex + 1
        children = chain[[parent[parentIndex]]]
        children = children[!is.na(children)]
        while( length(children) == 0 && length(parent) > parentIndex){
            parentIndex = parentIndex + 1
        }
        
        message("who's missing? current children: ", children)
        if (length(children) > 0) missing = setdiff(basename(children), names(chain))
    }
    if (length(children) > 0){
        result$dangling = parent[parentIndex]
        result$missing = missing[!is.na(missing)]
    }
    return(result)
}