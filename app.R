
## This is an alternative to the primary launch mechanism:
# devtools::install_github(IvoryC/shinyBio)
# library("shinyBioLockJ")
# shiny_biolockj()

# This was a haphazardly put together by loosely following instructions to make 
# this shiny app package compatible with remote hosting options like shiny.io

# pkgload::load_all(".")
for (file in dir("R)")) source(file)

# shiny_biolockj()
ui = biolockj_ui()
server <- biolockj_server(input, output, session)
shinyApp(ui, server)
