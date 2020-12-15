library(shiny)
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("DT")) install.packages("DT"); library("DT")
source("helpers.R")

# Define UI ----
ui <- fluidPage(

  # App title ----
  titlePanel("Menger Curvature Simulation"),
  helpText(div("The purpose of this app is to demonstrate how the shape of a curve
               affects Menger-Curvature measurements along the curve. Specify the 
               minimum and maximum x-values and the number of x-values to fit to 
               the curve. The response, y, is calculated using the sigmoidal function:",
               withMathJax("$$\\frac{1}{1+exp(-x)}$$"), "Customize the curve by 
               setting", code("Numerator"), ",", code("Vertical Adjustment"), ",",
               code("Horizontal Adjustment"), ", and", code("Data Multiplier"),
               "which will simulate responses using the formula:", 
               withMathJax("$$\\frac{\\color{red}{\\text{Numerator}}}{\\color{red}
                           {\\text{Vertical Adjustment}} + exp(-(\\color{red}
                           {\\text{Data Multiplier}} \\times x + \\color{red}
                           {\\text{Horizontal Adjustment}}))}$$"),
               "The", code("Add Noise"), "option will randomly jitter the responses.
               In the result, the point with the highest Menger Curvature 
               will be labeled as the Point of Departure (POD)")),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Dose Range ----
      sliderInput(inputId = "x",
                  label = strong("Range of x-axis values"),
                  min = -100,
                  max = 100,
                  value = c(0,10)),

      # Input: Length of vector for dose interpolation ----
      numericInput(inputId = "interp_length",
                   label = strong("How many x-values to use?"),
                   value = 50),

      # Input: Function to derive responses ----
      numericInput(inputId = "numerator",
                   label = "Numerator",
                   value = 10),
      
      numericInput(inputId = "add_val",
                   label = "Vertical Adjustment",
                   value = 1),
      
      numericInput(inputId = "shift_val",
                   label = "Horizontal Adjustment",
                   value = -5),
      
      numericInput(inputId = "multiplier",
                   label = "Data Multiplier",
                   value = 1),
      
      numericInput(inputId = "noise",
                   label = "Add Noise",
                   value = 0,
                   min = 0,
                   step = 0.1)
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "plot"),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  doseInput <- reactive({
    calculate_interpolated_doses(min_dose = input$x[1],
                                 max_dose = input$x[2],
                                 size = input$interp_length)
  })

  responseInput <- reactive({
    create_responses(doseInput(), numerator = input$numerator, add_val = input$add_val, 
                     shift_val = input$shift_val, multiplier = input$multiplier, noise_value = input$noise)
  })

  curvatureInput <- reactive({
    MC_simulation(doseInput(), responseInput())
  })

  output$plot <- renderPlot({
    plot_doseresponse(doseInput(), responseInput(), curvatureInput())
  })

  output$table <- DT::renderDataTable({
    make_dose_response_table(doseInput(), responseInput(), curvatureInput())
  })
}

shinyApp(ui = ui, server = server)


