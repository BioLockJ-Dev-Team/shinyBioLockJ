#' apply shiny feedback
#'
#' @param inputId the input object to pull a value from
#' @param feedbackId the input object to apply the feedback onto (by default, feedbackId=inputId)
#' @param moduleClass the class name of an associated module, used for the module arg to BioLockR::isValidProp
#' @param input generally use: input=input
#'
applyShinyFeedback <- function(inputId, prop, feedbackId=inputId, moduleClass=NULL, projPath=projectDirPath(), input){
    observeEvent(input[[inputId]],{
        req(input$checkLiveFeedback)
        req(prop$type != "boolean")
        shinyFeedback::hideFeedback( feedbackId )
        req(input[[inputId]] != "")
        if (prop$type=="file path" || prop$type=="list of file paths"){
            absPaths = writeFilePathList(input[[inputId]], projectDir = projPath, useRelPath = FALSE)
            isGood = isolate(BioLockR::isValidProp(prop$property, absPaths, module = moduleClass))
        }else{
            isGood = isolate(BioLockR::isValidProp(prop$property, input[[inputId]], module = moduleClass))
        }
        message("isGood: ", isGood)
        if (is.na(isGood)){
            shinyFeedback::hideFeedback( feedbackId )
        }else if(isGood){
            shinyFeedback::showFeedbackSuccess( feedbackId )
        }else{
            shinyFeedback::showFeedbackWarning( feedbackId, "not good" )
        }
    })
    
    observeEvent(input$checkLiveFeedback,{
        req(!input$checkLiveFeedback)
        shinyFeedback::hideFeedback( feedbackId )
    })
    
}