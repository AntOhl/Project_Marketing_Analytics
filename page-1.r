library(stringr)

# Global Variables ----
cat_amount <- c('amount_DO_MA', 'amount_DO_WW', 'amount_PA')
cat_count <- c('count_DO_MA', 'count_DO_WW', 'count_PA')
co_cols <- setNames(c('#5899DA', '#ED4A7B', '#71c989', 
                      '#5899DA', '#ED4A7B', '#71c989'), levels(df_channel$name))

cat_prefix <- c('AU', 'DR', 'MLLE', 'MME', 'MMME', 'MR')
cat_department <- str_pad(seq(1,95), 2, pad = "0")

# Page 1 - Revenue Breakdown by Individuals ----

## UI Function
p1u <- function(id, label = "Individuals") {
  
  ns <- NS(id)
  
  tabPanel("Individuals",
           sidebarLayout(
             
             sidebarPanel(
               helpText("Here you can see how much each individual donated each year"),
               selectInput(ns("var_prefix"), 
                           label = "Choose a prefix:",
                           choices = cat_prefix,
                           selected = 'MR'),
               selectInput(ns("var_department"), 
                           label = "Choose a department:",
                           choices = cat_department,
                           selected = '01'),
               selectInput(ns("var_id"), 
                           label = "Choose a contact ID:",
                           choices = c(319,144,5350,5439,6212)),
               sliderInput(ns("var_year"), 
                            label = "Choose a year range for the plots:",
                            min = 1990, max = 2020, value = c(1990, 2020)),
               htmlOutput(ns("stats")),
               helpText("DO: one-shot donation / PA:automatic deduction / MA: by mail / WW: by the internet")
               ),
             
             mainPanel(
               plotOutput(ns("channel_individual_amount")),
               plotOutput(ns("channel_individual_count")))
           
           ))
}

## Server Function
p1s <- function(input, output, session) {
  df_channel_amount <- df_channel[df_channel$name %in% cat_amount,]
  df_channel_count <- df_channel[df_channel$name %in% cat_count,]
  
  list_id <- reactive({
    temp <- df_contact %>%
      filter(prefix_id == input$var_prefix, department == input$var_department)
    return(temp$id)
  })
  
  #Update ID Inputer
  observe({
    list_id_current <- list_id()
    updateSelectInput(session, "var_id", choices = list_id_current, selected = list_id_current[1])
  })
  
  #Data for chart 1
  data_amount <- reactive({
    data_amount <- df_channel_amount %>%
      filter(id == input$var_id, input$var_year[1]<=year, input$var_year[2]>=year)
    return(data_amount)
  })
  
  #Data for chart 2
  data_count <- reactive({
    data_count <- df_channel_count %>%
      filter(id == input$var_id, input$var_year[1]<=year, input$var_year[2]>=year)
    return(data_count)
  })
  
  #Data for stats
  data_stats <- reactive({
    data_stats <- df_channel_amount %>%
      filter(id == input$var_id)
    return(data_stats)
  })  
  
  # Chart 1
  output$channel_individual_amount <- renderPlot({ 
    ggplot(data_amount()) + 
      geom_line(aes(x = year, y = value, color=name), size=1) + 
      geom_point(aes(x = year, y = value, color=name), size=4) +
      labs(title='Donation Amount By Year', x ='Year', y ='Annual Sum') +
      scale_color_manual("Legend", values = co_cols) +
      scale_x_continuous(breaks=seq(1900, 2020, 1), minor_breaks = NULL) +
      theme(text = element_text(size=15),
            axis.text.x = element_text(hjust=1)) 
  })
  
  # Chart 2
  output$channel_individual_count <- renderPlot({ 
    ggplot(data_count()) + 
      geom_line(aes(x = year, y = value, color=name), size=1) + 
      geom_point(aes(x = year, y = value, color=name), size=4) +
      labs(title='Donation Count By Year', x ='Year', y ='Annual Counts') +
      scale_color_manual("Legend", values = co_cols) +
      scale_y_continuous(breaks=seq(0, 1000, 1), minor_breaks = NULL) +
      scale_x_continuous(breaks=seq(1900, 2020, 1), minor_breaks = NULL) +
      theme(text = element_text(size=15),
            axis.text.x = element_text(hjust=1)) 
  })  

  # Stats
  output$stats <- renderText({
    data_stats <- data_stats()
    recency <- 2018 - max(data_stats$year)
    age <- 2018 - min(data_stats$year)
    total <- sum(data_stats$value)
    seg <- 'unknown'
    
    paste('<B>Rencency: </B>', recency, '<br>',
          '<B>Age: </B>', age, '<br>',
          '<B>Total Contribution: </B>', total, '<br>')
  })
}