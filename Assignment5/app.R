#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
library(tidyverse)
library(shiny)




DIG_1 <- DIG_1 %>%
  mutate(
    TRTMT    = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment")),
    SEX      = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    HYPERTEN = factor(HYPERTEN, levels = c(0, 1), labels = c("No history", "History of hypertension")),
    DIG      = factor(DIG, levels = c(0, 1), labels = c("No toxicity", "Digoxin toxicity")),
    HOSP     = factor(HOSP, levels = c(0, 1), labels = c("No hospitalisation", "Hospitalisation")),
    DEATH    = factor(DEATH, levels = c(0, 1), labels = c("Alive", "Death")),
    CVD      = factor(CVD, levels = c(0, 1), labels = c("No CVD", "Cardiovascular disease")),
    WHF      = factor(WHF, levels = c(0, 1), labels = c("No worsening HF", "Worsening Heart Failure")),
    RACE     = factor(RACE, levels = c(1, 2), labels = c("White", "Nonwhite")
  )
  )



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DIG Trial Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "RACE", label = "Select Patient Race:",
                      choices = c("White", "Nonwhite"), multiple = FALSE),
            radioButtons(inputId = "SEX", label = "Select Patient Gender:",
                        choices = c("Male", "Female")),
            radioButtons(inputId = "TRTMT", label = "Select Treatment Group:",
                         choices = c("Placebo", "Treatment")),
            sliderInput("AGE", "Select Patient Age Range:",
                    min = 21, max = 90, value = c(30,60))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  patients_subset <- reactive({
    DIG_1 %>%
      filter(
        SEX == input$SEX,
        RACE == input$RACE,
        AGE >= input$AGE[1],
        AGE <= input$AGE[2],
        TRTMT == input$TRTMT
      )
  })


  output$plot1 <- renderPlot({
    ggplot(patients_subset(), aes(x = DIABP, y = SYSBP)) +
      geom_point() +
      labs(
        title = "SYSBP vs DIABP (Filtered Patients)",
        x = "Diastolic Blood Pressure",
        y = "Systolic Blood Pressure"
      ) +
      theme_minimal()
  })
  

}
observe({
  print(nrow(patients_subset()))
})

