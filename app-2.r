# Setup ----
library(shiny)
library(tidyverse)

## Read relevant data
df_channel <- readRDS("data/df_channel.rds")
df_contact <- readRDS("data/df_contact.rds")
df_seg <- readRDS("data/df_seg.rds")

source('page-1.R')
source('page-2.R')
source('page-3.R')

## Pre-calculations
df_channel_agg <- df_channel %>%
                  group_by(year, name) %>%
                  summarise(value=sum(value))


# UI ----
ui <- fluidPage(
  navbarPage("Analysis",
             p1u("p1"),
             p2u("p2"),
             p3u("p3"))
)


# Server ----
server <- function(input, output) {
  callModule(p1s, "p1")
  callModule(p2s, "p2")
  callModule(p3s, "p3")
}

# Run the app ----
shinyApp(ui = ui, server = server)
