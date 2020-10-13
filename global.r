# Loading libraries
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(digest)
library(shinyjs)
library(DT)
library(glue)
library(arules)
#
# Loading predefined functions from SegmentsUtil
library(SegmentsUtil)

source("auth_module.R")
source("pwc.R")

user_base <- tibble::tibble(
    user = c("admin", "user"),
    password = c("admin_pass", "user_pass"), 
    permissions = c("admin", "admin")
)

# Static functions  
k_means_rep <- repeatable(kmeans)

check_and_handle_NA <- function(data, method='drop'){
  
  if(dim(data)[1] > dim(data[complete.cases(data),])[1]){
    
    if(method == 'drop'){
      data_wo_NA <- data[complete.cases(data), ]  
    }
    
  } else {
    data_wo_NA <- data
  }
  
  return(data_wo_NA)
}

###############################################################################
##
##  Change the flag to avoid having to authenticate during development
##
###############################################################################

DEVELOPMENT <- T

PERMISSION <- NULL
USER_AUTH <- F
MAX_SELECTED_COLS <- 5
MIN_GROUPS <- 1
MAX_GROUPS <- 5
MIN_SUPPORT <- 0.1
MAX_SUPPORT <- 1.0
MIN_PRECLUSTERS <- 2
MAX_PRECLUSTERS <- 5

style_brks <- seq(MIN_GROUPS, MAX_GROUPS, 1)
style_clrs <- round(seq(255, 80, length.out = length(style_brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",0)")}

# rgb(255, 100 ,0) rgb(255, 170 ,0)

if(DEVELOPMENT){
  PERMISSION <- "admin"
  USER_AUTH <-  T
}
