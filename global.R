library(shiny)
library(shinyjs)
library(shinydashboard)
library(reticulate)
library(waiter)

#' Global
use_condaenv("synapse", required = TRUE)
reticulate::import("sys")

