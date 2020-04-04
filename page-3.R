library(BTYD)
library(RODBC)
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(Gmisc)
library(gridExtra)
library(grid)
library(dplyr)
library(ggrepel)
#library(chorddiag)

# Global Variables ----
colors = brewer.pal(n = 8, name = 'Spectral')

## UI Function
p3u <- function(id, label = "Transitions") {
  ns <- NS(id)
  
  tabPanel("Transitions",
           sidebarLayout(
             
             sidebarPanel(
               helpText("Here you can see the transition of each segment"),
               selectInput(ns("var_indicator"), 
                           label = "For the starting year:",
                           choices = c('Assume each segment has the same number of clients',
                                       'Use real data'),
                           selected = 'Assume each segment has the same number of clients'),
               selectInput(ns("var_year"), 
                           label = "Choose a starting year:",
                           choices = seq(2014,2016),
                           selected = 2017),
               # selectInput(ns("var_viz"), 
               #             label = "Select a Viz:",
               #             choices = c('Pie', 'Arrow'),
               #             selected = 'Arrow'),
               helpText("DO: one-shot donation / PA:automatic deduction / MA: by mail / WW: by the internet")
             ),
             
             mainPanel(
               plotOutput(ns("chart_tr")),
               plotOutput(ns("chart_pro"), width = "60%"))
             
           ))
  
}

## Server Function
p3s <- function(input, output, session) {
  
  #data 1.1
  data <- reactive({
    df_old_segment <- subset(df_seg, year == as.numeric(input$var_year))
    df_old_segment <- subset(df_old_segment, select = c(segment, id))
    df_new_segment <- subset(df_seg, year == as.numeric(input$var_year) + 1)
    df_new_segment <- subset(df_new_segment, select = c(segment, id))
    
    df_final <- merge(df_old_segment, df_new_segment, by = 'id', all = TRUE)
    df_final$count <- 1
    df_final <- df_final[, -1]
    df_final[is.na(df_final)] <- 'NO DONATION'
    df_final <- aggregate(df_final$count, by=list(segment = df_final$segment.x, segment.1 = df_final$segment.y), FUN=sum)
    df_final <- df_final %>% rename(count = 'x')
    df_final <- df_final[]
    
    sums  <- aggregate(df_final$count, by=list(segment=df_final$segment), FUN=sum)
    df_final <- merge(df_final, sums, by = 'segment', all.x = TRUE)
    
    data <- df_final
    
    data$prob = round(data$count / data$x * 100, 2)
    
    return(data)
  })
  
  #data 1.2
  tm <- reactive({
    tm <- acast(data(), segment~segment.1, value.var="prob")
    tm[is.na(tm)] <- 0
    return(tm)
  })
  
  #chart1
  
  output$chart_tr <- renderPlot({
    if (input$var_indicator == 'Assume each segment has the same number of clients'){
      transitionPlot(tm(), txt_start_clr = "black", txt_end_clr = "black",
                     fill_start_box = colors,
                     fill_end_box = colors,
                     overlap_add_width = 1.3, type_of_arrow = "simple",
                     main = "The Transition of Each Segment",
                     box_label = c(as.numeric(input$var_year), as.numeric(input$var_year)+1),
                     min_lwd = unit(1, "mm"), max_lwd = unit(8, "mm"))
    } else if (input$var_indicator == 'Use real data'){


      s1 <- data() %>%
        group_by(segment) %>%
        summarise(sum=sum(count))

      transitionPlot(tm() * s1$sum, txt_start_clr = "black", txt_end_clr = "black",
                     fill_start_box = colors,
                     fill_end_box = colors,
                     overlap_add_width = 1.3, type_of_arrow = "simple",
                     main = "The Transition of Each Segment",
                     box_label = c(as.numeric(input$var_year), as.numeric(input$var_year)+1),
                     min_lwd = unit(1, "mm"), max_lwd = unit(8, "mm"))
    }
  })
  
  
  #chart2
  output$chart_pro <- renderPlot({
  # profitability of each segment
  pro_old <- df_seg %>%
    filter(year==as.numeric(input$var_year)) %>%
    group_by(segment) %>%
    summarise(avg_amount_year=sum(amount_DO_MA+amount_DO_WW+amount_PA)/n())
  
  pro_new <- df_seg %>%
    filter(year==as.numeric(input$var_year)+1) %>%
    group_by(segment) %>%
    summarise(avg_amount_year=sum(amount_DO_MA+amount_DO_WW+amount_PA)/n())
  
  pro <- tibble(segment=pro_old$segment, avg_old=pro_old$avg_amount_year, avg_new=pro_new$avg_amount_year)
  pro <- pivot_longer(pro, cols=c('avg_old','avg_new'))
  pro$name <- factor(pro$name, levels=c('avg_old', 'avg_new'))
  # plot the comparison between the profitability of each segment
  ggplot(data=pro) +
    geom_line(aes(x=name, y=value, group=segment, color=segment), size=2) +
    geom_point(aes(x=name, y=value, group=segment, color=segment), size=3) +
    labs(title='Average Donation Amount per Capita', x ='Year', y ='Annual Sum') + 
    scale_x_discrete(labels=c(as.numeric(input$var_year), as.numeric(input$var_year)+1)) +
    geom_text_repel(
      data = subset(pro, name=='avg_new'),
      aes(x=name, y=value, label = segment, color = segment),
      size = 5,
      hjust = 0,
      direction = "y",
      nudge_x = 2,
      segment.color = NA,
      show.legend = FALSE
    ) + 
    theme(legend.position = "none")
  })
  
}