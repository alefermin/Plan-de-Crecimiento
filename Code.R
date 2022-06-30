library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(DBI)
library(tidyverse)

# Function for finding past growth %
past_biz_growth <- function(years = 5, current_size = 10000, beginning_size = 1000) {
  round(((current_size/beginning_size) ^ (1 / years) - 1) * 100, digits = 2)
}



biz_growth_year <- function(growth = 10, years = 10, start_size = 1, start_year = 0, 
                            growth_2 = NA, years_2 = 0, start_size_2 = NA, start_year_2 = 0) {
  compound <- (1 + growth / 100) ^ c(0:years)
  compound_2 <- (1 + growth_2 / 100) ^ c(0:years_2)
  
  df <- data.frame(
    Year = c(0:years + start_year, 0:years_2 + start_year_2),
    Size = c(start_size * compound, start_size_2 * compound_2),
    Simulation = c(rep("Predicted", length(compound)), rep("Desired", length(compound_2)))
  )
}


# Function for plotting graph of growth simulations
biz_growth_year_graph <- function(growth = 10, years = 10, start_size = 1, start_year = 0, 
                                  growth_2 = NA, years_2 = 0, start_size_2 = NA, start_year_2 = 0) {
  compound <- (1 + growth / 100) ^ c(0:years)
  compound_2 <- (1 + growth_2 / 100) ^ c(0:years_2)
  
  df <- data.frame(
    Year = c(0:years + start_year, 0:years_2 + start_year_2),
    Size = c(start_size * compound, start_size_2 * compound_2),
    Simulation = c(rep("Predicted", length(compound)), rep("Desired", length(compound_2)))
  )
  
  df_2 <- na.omit(df)
  
  ggplot(df_2, aes(Year, Size, group = Simulation, color = Simulation)) +
    geom_smooth(method = "loess", # understand what method does
                se = FALSE, # understand what se does
                formula = 'y ~ x', # understand formula does
                span = 0.8) + # understand what span does
    stat_smooth(se = FALSE, geom = "area", # understand what geom = "area" does
                method = 'loess', alpha = .5,
                span = 0.8, aes(fill = Simulation)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.line = element_blank(),
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    ) +
    scale_color_manual(values = c("Predicted" = "red", "Desired" = "green")) +
    scale_fill_manual(values = c("Predicted" = "red", "Desired" = "green")) +
    ggtitle("Crecimiento Exponencial")
}





biz_growth_year_df <- function(growth = 10, years = 10, start_size = 10000, start_year = 0, 
                               growth_2 = NA, years_2 = 0, start_size_2 = NA, start_year_2 = 0) {
  compound <- (1 + growth / 100) ^ c(0:years)
  compound_2 <- (1 + growth_2 / 100) ^ c(0:years_2)
  
  df <- data.frame(
    Year = c(0:years + start_year, 0:years_2 + start_year_2),
    Size = c(start_size * compound, start_size_2 * compound_2),
    Simulation = c(rep("Growth 1", length(compound)), rep("Growth 2", length(compound_2)))
  )
  
  na.omit(subset(df, select = c("Year", "Size")))
}

biz_growth_year_df()
#df_dollarsign






#SHOW THIS IN THE TITLE:
# 
#the 
# 
#As for a few examples: at 40% growth rate, as compared to 20%, we'll end up 
#4 times bigger (not twice as big) in 10 years, and 15 times as big in 15 years, 
#as compared to 4 times as big at 20% (IMPORTANT: GET THESE NUMBERS RIGHT, 
#CALCULATE THEM)


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                
                h2(strong("Plan de Crecimiento")), hr(),
                
                sidebarPanel("Estimar Crecimiento Pasado",
                             sliderInput("reverse_years", label = "Años de Crecimiento Pasado:", value = 10, min = 1, max = 50),
                             numericInput("reverse_start_size", label = "Valuación de Empresa en la Actualidad ($):", value = 10000),
                             numericInput("reverse_beginning_size", label = "Valuación de Empresa Pasada ($):", value = 2000),
                             h5(strong("Crecimiento Pasado Estimado:")),
                             textOutput("growth_rate_porcentage")),
                
                fluidRow(
                  
                  # Inputs for simulation case 1
                  column(3,
                         "Predicción de Crecimiento en Tasa Actual:",
                         textOutput("growth_result"),
                         sliderInput("growth", label = "Crecimiento %:", value = 20, min = 1, max = 100),
                         numericInput("years", label = "Años de Crecimiento Futuro:", value = 10, min = 1, max = 50),
                         numericInput("start_size", label = "Valuación Actual ($):", value = 10000, min = 1, max = 100000000),
                         numericInput("start_year", label = "En Cuántos Años Comienzo?:", value = 0, min = 1, max = 10)),
                  
                  # Inputs for simulation case 2
                  column(3,
                         "Crecimiento en Tasa Deseada:",
                         sliderInput("growth_2", label = "Crecimiento %:", value = 40, min = 1, max = 100),
                         numericInput("years_2", label = "Años de Crecimiento Futuro:", value = 10, min = 1, max = 50),
                         numericInput("start_size_2", label = "Valuación Actual ($):", value = 10000, min = 1, max = 100000000),
                         numericInput("start_year_2", label = "En Cuántos Años Comienzo?:", value = 0, min = 1, max = 10)),
                ),
                
                fluidRow(
                  
                  column(6,
                         plotOutput("hist"),
                         h5(strong("Análisis:")),
                         textOutput("analysis")),
                  
                  column(5,
                         dataTableOutput("table")))
                
)


# Define server function
server <- function(input, output, session) {
  
  past_biz_growth_result <- reactive(past_biz_growth(input$reverse_years, input$reverse_start_size, input$reverse_beginning_size))
  inputs_plot_df <- reactive(biz_growth_year(input$growth, input$years, input$start_size, input$start_year,
                                             input$growth_2, input$years_2, input$start_size_2, input$start_year_2))
  
  output$growth_rate_porcentage <- renderText({
    paste0(
      "La Tasa de Crecimiento ha sido, en promedio, ",
      past_biz_growth_result(),
      "% anual por los pasados ",
      input$reverse_years,
      " años"
    )
  })
  
  #  output$growth_result <- renderText({
  #    paste0(past_biz_growth_result())
  #  })
  
  output$hist <- renderPlot({
    biz_growth_year_graph(input$growth, input$years, input$start_size, input$start_year,
                          input$growth_2, input$years_2, input$start_size_2, input$start_year_2)
  }, res = 100)
  
  output$analysis <- renderText({
    paste0(
      "Esto es para mostrar el poder del crecimiento compuesto y el crecimiento exponencial, para mostrar la importancia 
      de hacer lo mejor y humanamente posible  para incrementar cada punto de porcentaje (%) de crecimiento,
      siendo así que esto nos ayudará a crecer nuestro negocio no de manera lineal, sino exponencialmente. 
      En este análisis, nos damos cuenta que ",
      input$growth_2,
      "% de crecimiento termina muy distinto años después comparado a nuestra predicción de simulación con el crecimiento actual"
    )
  })
  
  #  table$Size <- format(table$Size, "$", justify = "l")
  
  output$table <- renderDataTable({
    biz_growth_year_df(input$growth, input$years, input$start_size, input$start_year,
                       input$growth_2, input$years_2, input$start_size_2, input$start_year_2)
  })
  
  observe({
    updateSliderInput(session, "growth", value = past_biz_growth_result())
    updateNumericInput(session, "start_size", value = input$reverse_start_size)
    updateNumericInput(session, "start_size_2", value = input$reverse_start_size)
  })
  
}

shinyApp(ui, server)
