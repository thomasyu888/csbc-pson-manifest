library(shiny)
library(shinyjs)
library(shinydashboard)
library(reticulate)
library(tibble)
library(waiter)
library(dccvalidator)

#' Global
use_condaenv("synapse", required = TRUE)
reticulate::import("sys")

