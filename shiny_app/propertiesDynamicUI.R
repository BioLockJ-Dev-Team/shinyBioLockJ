# PropertiesUI

# As of this writing, the Properties class in BioLockJ has these KNOWN_TYPES:
# KNOWN_TYPES = {STRING_TYPE, BOOLEAN_TYPE, FILE_PATH, EXE_PATH, LIST_TYPE, FILE_PATH_LIST, INTEGER_TYPE, NUMERTIC_TYPE};
# KNOWN_TYPES = {"string", "boolean", "file path", "executable", "list", "list of file paths", "integer", "numeric"};


# render prop ui
renderPropUi <- function(propName, prop, default, value){
    # message("Treating property ", propName, " as a ", prop$type, " property.")
    uiName = propUiName(propName)
    if(prop$type == "boolean"){
        selected = ""
        if ( !is.null(value) ){
            if ( value=="Y" || value=="TRUE" || value==TRUE ) {
                selected = "Y"
            }else if(value=="N" || value=="FALSE" || value==FALSE ){
                selected = "N"
            }
        }
        inputObj <- radioButtons(inputId = uiName,
                                 label = propName,
                                 choices = c(Y="Y", N="N", omit=""),
                                 selected = selected,
                                 inline = TRUE)
    }else if(prop$type == "numeric"){
        inputObj <- numericInput(inputId = uiName,
                              label = propName,
                              value = value,
                              width = '60%')
    }else if(prop$type == "integer"){
        inputObj <- numericInput(inputId = uiName,
                                 label = propName,
                                 value = value,
                                 step = 1,
                                 width = '40%')
    }else if(prop$type == "file path"){
        inputObj <- textInput(inputId = uiName,
                              label = propName,
                              value = value,
                              placeholder = default,
                              width = '100%')
    }else if(prop$type == "list of file paths"){
        inputObj <- textInput(inputId = uiName,
                              label = propName,
                              value = value,
                              placeholder = default,
                              width = '100%')
    }else {
        inputObj <- textInput(inputId = uiName,
                              label = propName,
                              value = value,
                              placeholder = default,
                              width = '100%')
    }
    propUI <- tagList(em(prop$type),
                      renderText(prop$description, ),
                      inputObj,
                      br())
    return(propUI)
}

propUiName <- function(propName){
    gsub(".", "", propName, fixed=TRUE)
}
