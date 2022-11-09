suppressMessages({
  # general purpose ----
  library(here)
  library(tidyverse)
  library(cli)
  library(fs)
  library(readxl)
  library(gt)
  
  # web stuff ---
  library(curl)
  library(rvest)
  library(pdftools)
  
  # alarm project ----
  library(geomander)
  library(censable)
  #library(redist)
  #library(redistmetrics)
  library(ggredist)
  library(PL94171)
  library(cvap)
  
  # spatial ----
  library(sf)
  
  # plotting ----
  library(wacolors)
  library(patchwork)
  library(scales)
  library(geomtextpath)
  library(ggdist)
  if (Sys.info()[['sysname']] == 'Windows') {
    library(extrafont)
    loadfonts(device = 'win')
  }
})

lapply(Sys.glob(here('R/utils/*.R')), source)

cli_alert_success('Packages loaded and utilities prepared.')