
## This is an alternative to the primary launch mechanism:
# devtools::install_github(BioLockJ-Dev-Team/shinyBioLockJ)
# shinyBioLockJ::shiny_biolockj()

# This was a haphazardly put together by loosely following instructions to make 
# this shiny app package compatible with remote hosting options like shiny.io

pkgload::load_all(".")
shiny_biolockj()
