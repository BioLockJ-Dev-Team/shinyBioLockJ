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
shiny_biolockj <- function(...) {

    ui = biolockj_ui()
    
    server <- biolockj_server(input, output, session)
    
    # Run the application ####
    # Supress Shiny's auto-load behaviour
    old <- options(shiny.autoload.r = FALSE)
    on.exit(options(old), add = TRUE)  
    
    shinyApp(ui, server, ...)
}
