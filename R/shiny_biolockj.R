#' shiny biolockj
#' 
#' This is the essential launch command for the shiny app.
#'
#' @param ... parameters passed to shinyApp(ui, server, ...)
#' 
#' @import shiny
#'
#' @return no return value
#' 
#' @export
#' 
#' @examples
#' # shiny_biolockj()
#' 
shiny_biolockj <- function(...) {
    # Plus other stuff that was previously in app.R
    # Hopefully refactored in a few function calls
    
    #############################          Libraries          #########################################
    # library(shiny)
    # library(shinyjs)
    # # install.packages("shinythemes")
    # library(shinythemes)
    # # install.packages("shinyBS")
    # library(shinyBS)
    # # install.packages("sortable")
    # library(sortable)
    # # install.packages('shinyFiles')
    # library(shinyFiles)
    # library(shinyFeedback)
    # 
    # # install_github("IvoryC/BioLockR")
    # library("BioLockR")
    # if (numeric_version( packageVersion("BioLockR") ) < numeric_version("0.0.0.9001") ){
    #     stop("Requires BioLockR pakcage version 0.0.0.9001 or later.")
    # }

    #############################              UI              #########################################
    ui = biolockj_ui()
    
    server <- biolockj_server(input, output, session)
    
    #############################            Notes            #########################################
    
    # Collapse all sections: cmd + alt + O
    # Expand all sections: shift + cmd + alt + O
    
    
    # Run the application ####
    # Supress Shiny's auto-load behaviour
    old <- options(shiny.autoload.r = FALSE)
    on.exit(options(old), add = TRUE)  
    
    shinyApp(ui, server, ...)
}
