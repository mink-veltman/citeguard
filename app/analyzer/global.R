options(shiny.maxRequestSize = 1000 * 1024^2)

library(shiny)
library(shinyjs)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)
library(DT)
library(tibble)
library(httr2)
library(metacheck)
library(digest)
library(htmltools)
library(later)

r_dir <- if (dir.exists("R")) "R" else "../R"
for (f in list.files(r_dir, pattern = "\\.R$", full.names = TRUE)) source(f)

sync_db_from_github()
