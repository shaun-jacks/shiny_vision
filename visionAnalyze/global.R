# Set Google Authentication Parameters
options("googleAuthR.client_id" = Sys.getenv("googleAuthR.client_id"))
options("googleAuthR.client_secret" = Sys.getenv("googleAuthR.client_secret"))
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
# Authenticate
googleAuthR::gar_auth('.httr-oauth')


library(RoogleVision)
# For image processing
library(magick)
library(shinycssloaders)
