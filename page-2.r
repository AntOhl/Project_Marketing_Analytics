library(tidyverse)
library(DT)
library(data.table)
library(devtools)
library(highcharter)
library(scales)

# Global Variables ----
datatable_channel <- data.table(df_channel)
df_chart_3 <- datatable_channel[, c("indicator", "type", "method") := tstrsplit(name, "_", fixed=TRUE)] %>%
  .[, c('id', 'year', 'value', 'indicator', 'type', 'method', 'name')] %>%
  data.table()

co_cols <- setNames(c('#5899DA', '#ED4A7B', '#71c989', 
                      '#5899DA', '#ED4A7B', '#71c989'), levels(df_channel$name))

#Get id that belongs to each segment ----
df_calc <- df_chart_3 %>%
  .[ , .(sum(value)), by= .(id, name)] %>%
  .[V1 > 0] %>%
  filter(name %in% c('count_DO_WW', 'count_DO_MA',  'count_PA')) %>%
  data.table()
df_calc_2 <- dcast(df_calc, id ~ name, fun.aggregate = sum, value.var='V1') %>%
  data.table()

l_id_unique_DO_WW_MA <- unique(df_calc_2[count_PA == 0 & count_DO_WW != 0 & count_DO_MA != 0]$id)
l_id_unique_DO_WW_PA <- unique(df_calc_2[count_PA != 0 & count_DO_WW != 0 & count_DO_MA == 0]$id)
l_id_unique_DO_MA_PA <- unique(df_calc_2[count_PA != 0 & count_DO_WW == 0 & count_DO_MA != 0]$id)
l_id_unique_PA <- unique(df_calc_2[count_PA != 0 & count_DO_WW == 0 & count_DO_MA == 0]$id)
l_id_unique_DO_MA <- unique(df_calc_2[count_PA == 0 & count_DO_WW == 0 & count_DO_MA != 0]$id)
l_id_unique_DO_WW <- unique(df_calc_2[count_PA == 0 & count_DO_WW != 0 & count_DO_MA == 0]$id)
l_id_all <- unique(df_calc_2[count_PA != 0 & count_DO_WW != 0 & count_DO_MA != 0]$id)

list_segments <- list(l_id_unique_DO_WW_MA, l_id_unique_DO_WW_PA, l_id_unique_DO_MA_PA,
                      l_id_unique_PA, l_id_unique_DO_MA, l_id_unique_DO_WW, l_id_all)
list_segments_names <- list('DO_WW & DO_MA', 'DO_WW & PA', 'DO_MA & PA',
                            'Only PA', 'Only DO_MA', 'Only DO_WW', 'All channels')
df_id <- data.table(ids = list_segments_names, group = list_segments)

## UI Function
p2u <- function(id, label = "Segments") {
  
  ns <- NS(id)
  tabPanel("Channels",
           titlePanel("Revenue according to channels"),
           
           sidebarLayout(
             sidebarPanel(
               helpText("Here you can see how much each channel has an impact on revenue."),
               radioButtons(inputId = ns("indicator"), label="Indicator to display:", choices = unique(df_chart_3$indicator)),
               radioButtons(inputId = ns("segments"), label="Chanels:", choiceValues= list_segments_names,
                            choiceNames = list_segments_names, selected = list_segments_names[1]),
               helpText("DO: one-shot donation / PA:automatic deduction / MA: by mail / WW: by the internet")
             ),
             
             mainPanel(
               highchartOutput(ns("chart"))
             )
           ),
           sidebarLayout(
             column(width = 6,
                    plotOutput(ns("chart_2"))
                    ),
             column(width = 6,
                    plotOutput(ns("chart_3"))
                    )
           )
  )
}

p2s <- function(input, output, session) {
  #Chart3
  
  l_id <- reactive({
    unlist(df_id[ids %in% input$segments]$group)
  })
  
  df_chart_3_reactive <- reactive({
    df_chart_3 %>%
      .[indicator == input$indicator]%>%
      .[id %in% l_id()]%>%
      data.table()%>%
      .[which(year >= 1990)] %>%
      .[which(year < 2018)] %>%
      .[ , .(count = .N, total = sum(value)), by= .(year, name)] %>%
      .[order(year)] %>%
      data.table()
  })
  
  #Creation of a column that has the previsous total for the same channel
  df_chart_3_reactive_2 <- reactive({
    df_chart_3_reactive() %>%
      .[order(year), total_previous_same_channel := shift(total, 1, type="lag"), by=name] %>%
      .[order(year), total_current_all := sum(total), by=year] %>%
      .[order(year), total_previous_all := shift(total_current_all, 1, type="lag"), by=name] %>%
      .[order(year), percentage_current := (total/total_current_all)*100, by=year] %>%
      .[order(year), 
        percentage_increase_all := (total_current_all - total_previous_all)*100/total_previous_all] %>%
      .[order(year), percentage_increase_same_channel := (total - total_previous_same_channel)*100/total_previous_same_channel, by=.(year,name)] %>%
      .[order(year), gain_all := total_current_all - total_previous_all] %>%
      .[order(year), gain_channel := total - total_previous_same_channel] %>%
      .[order(year), gain_all := total_current_all - total_previous_all] %>%
      .[order(year), gain_channel := total - total_previous_same_channel]
    
  })
  
  output$chart <- renderHighchart({
    
    data <- df_chart_3_reactive()
    
    hc <- hchart(data, "line",
                 name=c('DO_MA', 'DO_WW', "PA"), 
                 lineWidth=1, 
                 hcaes(x = year, y = total, group=name, color=co_cols[name])) %>% #group=name, 
      hc_yAxis(title=list(text="Total Indicator")) %>%
      hc_xAxis(title=list(text="Year")) %>%
      hc_title(text="Evolution of the indicator by channel", style = list(color = "black", fontSize = 15))
    hc
  })
  
  output$chart_2 <- renderPlot({ 
    
    max_amount <- max(df_chart_3_reactive_2()$total_current_all) + 1000
    
    ggplot(data= df_chart_3_reactive_2(), aes(x=year)) + 
      geom_bar(aes(y = percentage_current * max_amount / 100.1, fill = name),
               stat="identity") +
      geom_point(aes(y = total_current_all)) + 
      geom_line(aes(y=total_current_all)) +
      scale_y_continuous(name = expression("Total Indicator"), labels = comma,
                         sec.axis = sec_axis(~ . * 100.1 / max_amount , name = "Percentage (%)"), 
                         limits = c(0, max_amount))+
      ggtitle("Evolution of the total indicator with channel repartition") +
      scale_fill_discrete(name = "Channel", labels = c('DO_MA', 'DO_WW', "PA")) +
      scale_fill_manual("Legend", values = co_cols) + 
      xlab('Year')+
      theme(text = element_text(size=15),
            axis.text.x = element_text(hjust=0.5)) 
  })
  
  output$chart_3 <- renderPlot({
    ggplot(data= df_chart_3_reactive_2(), aes(x=year)) + 
      geom_bar(aes(y = gain_channel, fill = name), position='dodge', stat="identity") +
      scale_y_continuous(name = expression("Annual Growth on indicator"), labels = comma) +
      ggtitle("Evolution of annual growth on the indicator by channel") +
      scale_fill_discrete(name = "Channel", labels = c('DO_MA', 'DO_WW', "PA")) +
      scale_fill_manual("Legend", values = co_cols) +
      xlab('Year')+
      theme(text = element_text(size=15),
            axis.text.x = element_text(hjust=0.5)) 
  })  
  
}