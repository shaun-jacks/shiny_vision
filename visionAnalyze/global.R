#### REFERENCES: The code was inspired by these references ####
# https://github.com/stoltzmaniac/ML-Image-Processing-R/blob/master/Google%20Vision%20API/Google%20Vision%20API%20in%20R.md
# https://cran.r-project.org/web/packages/googleAuthR/vignettes/google-authentication-types.html


# Normal Libraries
library(tidyverse)

# devtools::install_github("flovv/RoogleVision")
library(RoogleVision)
library(jsonlite) # to import credentials

# For image processing
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")
library(EBImage)
library(magick)

# file.edit("~/.Renviron")

options("googleAuthR.client_id" = Sys.getenv("googleAuthR.client_id"))
options("googleAuthR.client_secret" = Sys.getenv("googleAuthR.client_secret"))

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth('.httr-oauth')

  