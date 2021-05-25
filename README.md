# shinyBioLockJ

<img src="https://user-images.githubusercontent.com/8933011/108736478-a80f7580-74ff-11eb-81ee-fd649cfb6637.png" width="500">

A GUI for BioLockJ implemented in R shiny

## Getting Started

#### BioLockJ
Make sure you have BioLockJ, **version v1.3.17** or later. 

In a **terminal** or **command line** session:

```bash
echo $BLJ
echo $BLJ_PROJ
biolockj --version
# requires version v1.3.17 or later
```

To install or update BioLockJ, see [BioLockJ - Getting Started](https://biolockj-dev-team.github.io/BioLockJ/Getting-Started/).

#### Install shinyBioLockJ and BioLockR

shinyBioLockJ requires the R package _BioLockR_.  See [BioLockR](https://github.com/BioLockJ-Dev-Team/BioLockR) for details.  

Start an R session, and make sure you have have devtools package.
```
R
install.packages("devtools")
```

In the R session, use devtools to install BioLockR and shinyBioLockJ from github.
```
devtools::install_github("BioLockJ-Dev-Team/BioLockR")
devtools::install_github("BioLockJ-Dev-Team/shinyBioLockJ")
```

Note that the R session is still active, which is best for the next step. 

#### Link BioLockR to BioLockJ

In the interactive set up, enter the path where you saved the BioLockJ folder and the path to your BLJ_PROJ folder. **Rstudio is NOT recommended for this step.** Once these are set up, BioLockR will remember them even when used from Rstudio.

In an **R session**:
```
BioLockR::setup_BioLockR()
```
Note that the R session is still active, which is best for the next step.

#### Launch shinyBioLockJ

In an **R session**:
```
shinyBioLockJ::shiny_biolockj()
```

This will open the app in your web browser.

***Alternatively***:
 1. download this repository,
 1. open the project in RStudio (double-click the shinyBioLockJ.Rproj file), 
 2. open the file called "app.R", and 
 3. click the button "Run App".

This will open the app in an RStudio window.

The app may appear differently when hosted on a remote server, but either method of locally launching it should produce the same result.

The command to exit an R session (without saving it) is: `quit("no")`
