library('shiny')
library('tidyverse')
library('tidymodels')
library('modeltime')


models <- readr::read_rds('data/models.rds')

models_extract <- models %>%
  extract_nested_test_forecast() %>%
  mutate(forecast_method = case_when(str_detect(.model_desc, 'TEMPORAL') ~'THIEF',
                                     str_detect(.model_desc, 'ETS') ~ 'ETS',
                                     TRUE ~ .model_desc))

errors_extract <- models %>%
  extract_nested_test_accuracy() %>%
  mutate(forecast_method = case_when(str_detect(.model_desc, 'TEMPORAL') ~'THIEF',
                                     str_detect(.model_desc, 'ETS') ~ 'ETS',
                                     TRUE ~ .model_desc))

countries <- models_extract %>%
  distinct(country)

dates <- models_extract %>%
  distinct(.index)

min_date <- models_extract %>%
  distinct(.index) %>%
  filter(.index == min(.index)) %>%
  pull()

max_date <- models_extract %>%
  distinct(.index) %>%
  filter(.index == max(.index)) %>%
  pull()

forecast_alg <- models_extract %>%
  distinct(forecast_method) %>%
  pull()

error_metrics <- errors_extract %>%
  select(mae:rmse) %>%
  names()

ui <- fluidPage(
  tabsetPanel(
    tabPanel("models",
             fluidRow(
               column(3, selectInput("country", "Country", choices = countries)),
               column(6, sliderInput("dates",
                                     "Time horizon:",
                                     value = c(min_date, max_date),
                                     min = as.Date(min_date,"%Y-%m"),
                                     max = as.Date(max_date,"%Y-%m"),
                                     timeFormat = "%Y-%m",
                                     width = 1200)),
               column(3, selectInput("forecast_method",
                                     "Forecasting method: ",
                                     choices = forecast_alg,
                                     selected = forecast_alg,
                                     multiple = TRUE)),
             ),
             br(),
             br(),
             br(),
             fluidRow(
               column(8, 
                      plotOutput("model_plot"),
                      res = 96),
               column(4, 
                      tableOutput("error_metrics"),
                      br(),
                      br(),
                      selectInput("error", "Select Error Metric", choices = error_metrics),
                      br(),
                      textOutput('best_model'),
                      br(),
                      fluidRow(
                        actionButton('select_best', 'Select best forecasting models.'),
                        actionButton('show_all', 'Show all forecasting models.'))
                      )
             )
    ),
    tabPanel("prediction", 
             sidebarPanel(),
             mainPanel(textOutput('tbc_pred'))
    ),
    tabPanel("safety_stock",
             sidebarPanel(),
             mainPanel(textOutput('tbc_ss'))
    )
  )
)

server <- function(input, output, session) {
  
  
  observeEvent(input$select_best, {
    updateSelectInput(session, "forecast_method",
                      choices = forecast_alg,
                      selected = c('ACTUAL', best_model()))
                      })
  
  observeEvent(input$show_all, {
    updateSelectInput(session, "forecast_method",
                      choices = forecast_alg,
                      selected = forecast_alg)
  })
  
  
  selected <- reactive({models_extract %>%
      filter(country == input$country & 
               .index %in% c(input$dates[1]:input$dates[2]) &
               forecast_method %in% input$forecast_method)
      })
  
  selected_error <- reactive({
    errors_extract %>%
      select(-c(.model_id, .type, .model_desc, rsq)) %>%
      relocate(forecast_method, .after = country) %>%
      filter(country == input$country)
  })
  
  best_model <- reactive({
    models %>%
      filter(country == input$country) %>%
      modeltime_nested_select_best(metric = input$error) %>%
      unnest(.modeltime_tables) %>%
      mutate(forecast_method = case_when(str_detect(.model_desc, 'TEMPORAL') ~'THIEF',
                                         str_detect(.model_desc, 'ETS') ~ 'ETS',
                                         TRUE ~ .model_desc)) %>%
      pull(forecast_method)
  })
  
  output$model_plot <- renderPlot({
    selected() %>%
      ggplot(aes(x = .index,
                 y = .value,
                 colour = forecast_method)) +
      geom_line() +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            ) +
      ggtitle(paste0('Demand forecast for', ' ', input$country))
  }, height = 600
)
  
  output$error_metrics <- renderTable({
    selected_error()
  }, 
  caption = 'Forecast models error metrics.')
  
  output$best_model <- renderText(paste0(best_model(),
                                         ' is the best forecasting method on the basis of ',
                                         input$error, ' metric.'))
  
  output$tbc_pred <- renderText('prediction tab - work in progress')
  output$tbc_ss <- renderText('safety stock tab - work in progress')

}


shinyApp(ui = ui, server = server)
