library(shiny)
library(lpSolveAPI)
library(scales)
library(ggplot2)
library(DT)
library(plyr)
library(dplyr)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(RColorBrewer)
library(grid)
library(plotly)

path <- getwd()  # Set the working directory as the current path

p <- 5 # Number of years
y <- 21 # Must be updated each POM cycle to reflect first year of FYDP
solution_list <- list()

showplot <- c(paste0('plot',seq(1:3)),paste0('descriptive',seq(1:4))
              ,'fullSolution','table2','table3','costValuePlot')

showcontrols <- c('choice','export','exportAll','remove','removeAll')
