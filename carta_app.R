library(tidyverse)
library(readxl)
library(shiny)

ui <- fluidPage(
  titlePanel("Risiko Penyakit Tidak Menular (CARTA WHO SEAR B"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "gender",
        label = "Pilih jenis kelamin",
        choices = NULL,
        selected = NULL,
        options = list(create = TRUE)
      ),
      selectizeInput(
        inputId = "smoking",
        label = "Riwayat merokok",
        choices = NULL,
        selected = NULL,
        options = list(create = TRUE)
      ),
      selectizeInput(
        inputId = "age",
        label = "Kategori usia",
        choices = NULL,
        selected = NULL,
        options = list(create = TRUE)
      ),
      selectizeInput(
        inputId = "sbp",
        label = "Tekanan darah sistolik",
        choices = NULL,
        selected = NULL,
        options = list(create = TRUE)
      ),
      selectizeInput(
        inputId = "bmi",
        label = "Body Mass Index (BMI)",
        choices = NULL,
        selected = NULL,
        options = list(create = TRUE)
      )
    ),
  mainPanel(
    textOutput("score"),
    textOutput("criteria")
  )
)
)

server <- function(input, output, session) {
  df <- read_xlsx("WHO CARTA SEAR B dataset.xlsx", sheet = "Tnp Px Lab") |> 
  mutate(criteria =
    if_else(score < 5, "<5%", 
      if_else(between(score, 5, 9), "5-<10%", 
        if_else(between(score, 10, 19), "10-<20%",
          if_else(between(score, 20, 29), "20-<30%", 
            if_else(score >= 30, ">=30%", NA))))))
  
  observe({
    updateSelectizeInput(
      session,
      "gender",
      choices = df$gender,
      server = TRUE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "smoking",
      choices = df$smoking_hist,
      server = TRUE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "age",
      choices = df$age_category,
      server = TRUE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "sbp",
      choices = df$sbp,
      server = TRUE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "bmi",
      choices = df$bmi,
      server = TRUE
    )
  })

  output$selected_gender <- renderText({
    req(input$gender)
    paste("Dipilih", input$gender)
  })
}

shinyApp(ui, server)