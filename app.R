pacman::p_load(shiny,haven, tidyverse, psych, mice, VIM, ggplot2,plyr, dplyr,psych, knitr,DT)

# prepare the data
source("functions/scatter_plot.R")


#### test ####
# # all the inputs that we can select
annuam_mri_name <- annual_mri_df %>%
  select(contains("adj")) %>%
  colnames(.)
base_mri_name <- base_MRI_AP_df %>%
  select(contains("adj")) %>%
  colnames(.)

AP_name <- c("PM25", "PM10", "NOX")

ui <- fluidPage(
  tags$img(
    height = 100,
    width = 80,
    src = "https://ki.se/sites/default/files/styles/article_full_width/public/qbank/ki_logo-custom20201217114903.jpg?itok=G69mNLPL"
  ),
  titlePanel("Scatter plot of air pollution and brain marker"),
  tabsetPanel(
    tabPanel(title = "Longitudinal plot",
             h3("MRI measures are averaged across years"),
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        # Input: drop down for variables
        radioButtons(inputId = "AP_long", label = "Air pollution Variable", choices = AP_name),
        selectInput(
          inputId = "MRI_long", label = "Brain marker Variable", choices = annuam_mri_name,
          selected = annuam_mri_name[1]
        )
      )
    ,
    mainPanel(
      plotOutput(outputId = "plot1", width = "600px", height = 600)
    )
    )
  ),
  tabPanel(title = "Cross-sectional plot",
           h3("MRI measures are at baseline"),
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        # Input: drop down for variables
        radioButtons(inputId = "AP_cross", label = "Air pollution Variable", choices = AP_name),
        selectInput(
          inputId = "MRI_cross", label = "Brain marker Variable", choices = base_mri_name,
          selected = base_mri_name[1]
        )
      ),
    mainPanel(plotOutput(outputId = "plot2", width = "600px", height = 600))
    ))
))



server <- function(input, output) {
  output$plot1 <- renderPlot({
    scatter_4yr_AP_MRI(joined_df = annual_MRI_AP_df, MRI_name = input$MRI_long, AP_name = input$AP_long)
  })
  output$plot2 <- renderPlot({
    scatter_4yr_AP_MRI(joined_df = base_MRI_AP_df, MRI_name = input$MRI_cross, AP_name = input$AP_cross)
  })
}

shinyApp(ui = ui, server = server)