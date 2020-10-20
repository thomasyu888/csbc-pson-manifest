library(shiny)
library(shinyjs)
library(shinydashboard)
library(reticulate)
library(readxl)
library(tibble)
library(DT)
library(waiter)

#' Global
use_condaenv("synapse", required = TRUE)
reticulate::import("sys")

