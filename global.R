library(shiny)
library(shinyjs)
library(quantmod)
library(RColorBrewer)

source("func.R")
source("time.utilities.R")

iter <- 0

col_vec <- brewer.pal(9,"Set1")