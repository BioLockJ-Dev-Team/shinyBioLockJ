#' Update the values in the ui controls for shared properties
#'
#' @param session session=session
#' @param prop the property object, must have a $type
#' @param moduleId the module whose ui must be updated
#' @param value the new value to apply
#' 
#' @seealso renderPropUi, buildFilePathPropUI, buildFileListPropUI
#'
updateSharedPropUi <- function(session, prop, moduleId, value){
    propName = prop$property
    uiName = propUiName(propName, moduleId)
    if(prop$type == "boolean"){
        updateRadioButtons(session, uiName, selected = value)
    }else if(prop$type == "numeric"){
        updateNumericInput(session, uiName, value=value)
    }else if(prop$type == "integer"){
        updateNumericInput(session, uiName, value=value)
    }else if(prop$type == "file path"){
        updateTextInput(session, uiName, value=value)
    }else if(prop$type == "list of file paths"){
        updateTextAreaInput(session, uiName, value=value)
    }else {
        updateTextInput(session, uiName, value=value)
    }
    message("Updated shared property, so ui element [", uiName, "] now has value: ", value)
}