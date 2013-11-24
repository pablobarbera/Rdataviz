## INSTALLING PACKAGES THAT WE WILL USE TODAY

doInstall <- TRUE  # Change to FALSE if you don't want packages installed.

toInstall <- c(
    "ggplot2", "scales", "gridExtra", "hexbin", ## gplot2 and extensions
    "gdata", ## package to open excel files
    "igraph", ## package to create network objects
    "cshapes", 
    "sp", "maps", "maptools", ## map utilities
    "RColorBrewer", ## additional color pallets
    "XML", ## scraping tool
    "reshape", ## data manipulation package
    "jpeg", ## package to open jpeg images from R
    "Rfacebook" ## my package to capture Facebook data
    )

if(doInstall){
    install.packages(toInstall, repos = "http://cran.r-project.org")
}






