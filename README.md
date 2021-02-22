# shinyBioLockJ
A GUI for BioLockJ implemented in R shiny

## Getting Started

#### BioLockJ
Make sure you have BioLockJ, see [BioLockJ - Getting Started](https://biolockj-dev-team.github.io/BioLockJ/Getting-Started/).

In a **terminal or command line** session, take sure you have values for the following variables:
```bash
echo $BLJ
echo $BLJ_PROJ
```

#### BioLockR
Make sure you have the R pakcage _BioLockR_.  See [BioLockR](https://biolockj-dev-team.github.io/BioLockR) for details.  

In short, in an **R session**:
```R
# install.packages("devtools")
devtools::install_github("BioLockJ-Dev-Team/BioLockR")
BioLockR::setup_BioLockR() # launches interactive setup
```
In the interactive set up, enter the path where you saved the BioLockJ folder and the path to your BLJ_PROJ folder.

#### shiny BioLockJ

In an **R session**:
```R
devtools::install_github("IvoryC/shinyBioLockJ")
shinyBioLockJ::shiny_biolockj()
```

This will open the app in your web browser.

***Alternatively***:
 1. download this repository,
 1. open the project in RStudio (double-click the shinyBioLockJ.Rproj file), 
 2. open the file called "app.R", and 
 3. click the button "Run App".

This will open the app in an RStudio window.
