## INSTALLING PACKAGES THAT WE WILL USE TODAY

doInstall <- TRUE  # Change to FALSE if you don't want packages installed.

toInstall <- c(
    "devtools", ## library to install packages from github
    "ggplot2", "scales", "gridExtra", ## gplot2 and extensions
    "gdata", ## library to open excel files
    "igraph", ## library to create network objects
    "cshapes", 
    "sp", "maps", "maptools", ## map utilities
    "RColorBrewer", ## additional color pallets
    "XML", ## scraping tool
    "reshape", ## data manipulation library
    "jpeg" ## library to open jpeg images from R
    )

if(doInstall){

    install.packages(toInstall, repos = "http://cran.r-project.org")

    library(devtools)
    install_github("Rfacebook", "pablobarbera")
}






