library(shiny)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  titlePanel("CARTA: Cardiovascular Risk Prediction Tool for Africa"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age (years)", value = 40, min = 20, max = 80),
      radioButtons("sex", "Sex", choices = c("Male", "Female"), selected = "Male"),
      radioButtons("smoker", "Current Smoker?", choices = c("Yes", "No"), selected = "No"),
      radioButtons("diabetic", "Diabetes?", choices = c("Yes", "No"), selected = "No"),
      numericInput("sbp", "Systolic Blood Pressure (mmHg)", value = 120, min = 90, max = 200),
      numericInput("chol", "Total Cholesterol (mg/dL)", value = 180, min = 100, max = 300),
      numericInput("bmi", "BMI (kg/m²)", value = 25, min = 15, max = 50),
      actionButton("calculate", "Calculate Risk", class = "btn-primary")
    ),
    mainPanel(
      h3("Your 10-Year Cardiovascular Disease Risk"),
      progressBar(id = "riskBar", value = 0, display_pct = TRUE, status = "danger"),
      br(),
      uiOutput("riskCategory"),
      br(),
      h4("Interpretation:"),
      p("Low Risk: < 10%"),
      p("Moderate Risk: 10% - 20%"),
      p("High Risk: > 20%"),
      br(),
      h4("Disclaimer:"),
      p("This is a simplified simulation for educational purposes. Consult a doctor for accurate risk assessment.")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$calculate, {
    # Simulate CARTA risk calculation (example formula - not the actual CARTA model)
    risk <- 0
    
    # Age effect (linear increase)
    risk <- risk + (input$age - 30) * 0.02
    
    # Sex effect (higher for males)
    if (input$sex == "Male") risk <- risk + 0.5
    
    # Smoking effect
    if (input$smoker == "Yes") risk <- risk + 0.8
    
    # Diabetes effect
    if (input$diabetic == "Yes") risk <- risk + 1.2
    
    # Blood pressure effect
    if (input$sbp >= 140) risk <- risk + 0.7
    
    # Cholesterol effect
    if (input$chol >= 200) risk <- risk + 0.5
    
    # BMI effect (if overweight/obese)
    if (input$bmi >= 25) risk <- risk + 0.3
    
    # Convert to probability (logistic function for 0-100% range)
    risk_prob <- 100 / (1 + exp(-risk))
    
    # Ensure risk is between 0-100%
    risk_prob <- max(0, min(100, risk_prob))
    
    # Update progress bar
    updateProgressBar(session, "riskBar", value = risk_prob, status = ifelse(risk_prob > 20, "danger", ifelse(risk_prob > 10, "warning", "success")))
    
    # Risk category text
    risk_text <- if (risk_prob < 10) {
      "Low Risk"
    } else if (risk_prob <= 20) {
      "Moderate Risk"
    } else {
      "High Risk"
    }
    
    # Display risk category
    output$riskCategory <- renderUI({
      tags$div(
        style = "font-size: 20px; font-weight: bold;",
        paste("Risk Category:", risk_text)
      )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)