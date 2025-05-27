## ----setup, include = FALSE---------------------------------------------------
library(knitr)

## ----install, eval = TRUE, echo = TRUE, results = 'hide', message = FALSE, warning = FALSE----

# install and load rExpertQuery
if (!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}

remotes::install_github("USEPA/rExpertQuery", ref = "training", dependencies = TRUE, force = TRUE)

library(rExpertQuery)

## ----add.pkgs, results = FALSE, message = FALSE-------------------------------
# list of additional required packages
# demo.pkgs <- c("datasets", "data.table", "dplyr", "DT", "ggplot2", "httr", #"leaflet", "maps", "plotly", "sf", "usdata", "usmap", "stringi")

# install additional packages not yet installed
# installed_packages <- demo.pkgs %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(demo.pkgs[!installed_packages])
# }

# load additional packages for this vignette
library(datasets)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(httr)
library(leaflet)
library(maps)
library(plotly)
library(sf)
library(usdata)
library(usmap)
library(stringi)

## ----sci.not------------------------------------------------------------------
# turn off scientific notation (for catchment ids)
options(scipen = 999)

## ----testkey, include=TRUE----------------------------------------------------
# load test api  key
testkey <- "53BVce47MQ3KXKibjx35g4ojaDQGh8qWfbdO8cE0"

