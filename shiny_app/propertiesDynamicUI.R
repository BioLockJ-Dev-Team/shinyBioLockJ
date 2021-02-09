# PropertiesUI

# As of this writing, the Properties class in BioLockJ has these KNOWN_TYPES:
# KNOWN_TYPES = {STRING_TYPE, BOOLEAN_TYPE, FILE_PATH, EXE_PATH, LIST_TYPE, FILE_PATH_LIST, INTEGER_TYPE, NUMERTIC_TYPE};
# KNOWN_TYPES = {"string", "boolean", "file path", "executable", "list", "list of file paths", "integer", "numeric"};


# render prop ui
renderPropUi <- function(propName, prop, default, value, defaults){
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
    
    if (is.null(default)){
        propUI <- tagList(
            em(prop$type),
            renderText(prop$description),
            inputObj,
            br())
    }else{
        content = paste0("<b>", propName, "</b>")
        content = c(content, paste("standard default:", defaults$defaultPropsList$standard[[propName]]))
        for (name in defaults$activeFiles){
            val = " "
            if (propName %in% names(defaults$defaultPropsList[[name]])){
                val = defaults$defaultPropsList[[name]][propName]
            }
            content = c(content, paste0(name, ": ", val))
        }
        content = c(content, paste0("= <b>", defaults$values[[propName]], "</b>"))

        propUI <- tagList(
            shinyBS::popify(
                actionLink(propInfoId(propName), "", icon = icon("angle-double-left")),
                title="source of default value",
                paste0(content, collapse = "<br>"),#content=paste("default:", defaults$values[[propName]]),
                trigger = c('hover','click'), placement='right'),
            em(prop$type),
            renderText(prop$description),
            inputObj,
            br())
    }

    return(propUI)
}

propUiName <- function(propName){
    gsub(".", "", propName, fixed=TRUE)
}
propInfoId <- function(propName){
    paste0(gsub(".", "", propName, fixed=TRUE), "Info")
}
