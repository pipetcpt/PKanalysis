library(shiny)
library(mrgsolve)
library(NonCompart)
library(lattice)
options(mrgsolve_mread_quiet=TRUE)


code <- '
$PARAM

k1 = 5
F = 0.5 
VC = 10
VP = 40
Q = 1
CL = 3

$SET delta= 0.1

$CMT GUT CENT PERI


$OMEGA 0 0 0
$SIGMA 0
$MAIN

F_GUT = F * 1000;
double k21 = Q/VP;
double k12 = Q/VC;
double k10 = CL/VC;

$ODE

dxdt_GUT= -k1*GUT;
dxdt_CENT = k1 * GUT - k12 * CENT + k21 * PERI - k10 * CENT;
dxdt_PERI = k12 * CENT - k21 * PERI;

$TABLE
double CP = CENT/VC;

$CAPTURE CP;'

library(mrgsolve)
library(plotly)
mod <- mcode_cache("DDIa", code)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Initial parameter estimation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxInput('fit', "Observation data", FALSE),
      conditionalPanel(
        condition = "input.fit == 1", fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))),
      sliderInput("amt",
                  "Dose (mg)",
                  min = 1,
                  max = 1000,
                  value = 30),
      sliderInput("k1", "Absorption rate constant (ka, 1/h)", 0.1, 10, 5),
      sliderInput("CL", "Clearance (CL, L/h)", 0.1, 100, 5),
      sliderInput("VC", "Central volume (VC, L)", 0.1, 1500, 5),
      sliderInput("VP", "Peripheral volume (VP, L)", 0.1, 2000, 5),
      sliderInput("Q", "Intercompartmental clearance (Q, L/h)", 0.1, 150, 5),
      sliderInput("F", "Bioavailability (F)", 0, 1, 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("simPlot"),
      tableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  uploaded_data <- reactive({
    validate(
      need(input$file1, 'Please upload a file')
    )
    read.csv(input$file1$datapath, header = T, stringsAsFactors = F)
  })
  
  e <- reactive({
    ev(amt=input$amt)
  })
  
  # Reactive parameter value
  par <- reactive({
    param(k1 = input$k1, CL = input$CL, VC = input$VC, VP = input$VP, Q = input$Q, F = input$F)
  })
  
  uploaded_data2 <- reactive({
    uploaded_data() %>%
      group_by(time) %>%
      summarise(mean = mean(conc)) %>%
      mutate(Subject = 1) %>%
      as.data.frame()
  })
  output$simPlot <- renderPlotly({
    a<- mod %>% 
      param(par())%>%
      ev(e()) %>% 
      mrgsim(end = 48) %>%
      as.data.frame()
    
    p <- ggplot(a, aes(x = time, y= CP)) +
      geom_line(color = "#6666FF", alpha = 0.7) + 
      scale_y_continuous(trans = 'log10', limits = c(0.1, 10)) +
      theme_bw()
    
    if (input$fit) {
      p <- p +
        geom_point(data = uploaded_data2(), aes(x = time, y = mean), alpha = 0.5, colour = "orange")+
        geom_line(data = uploaded_data2(), aes(x = time, y = mean), alpha = 0.5, colour = "orange") +
        scale_y_continuous(trans = 'log10') +
        labs(y = "concentration (ng/mL)", x= "time (h)")
      ggplotly(p)
    }
    
    ggplotly(p)    

  })

  output$table <- renderTable({
    a <- tblNCA(uploaded_data2(), key = "Subject", colTime = "time", colConc = "mean", dose = input$amt, adm = "Extravascular", doseUnit = "mg", timeUnit = "h", concUnit = "ng/mL", down = "Linear", R2ADJ = 0.1)
    a <- a[ ,c("CMAX", "TMAX", "TLAG", "LAMZHL", "LAMZ", "AUCLST", "AUCIFO", "VZFO", "CLFO")]
    a$VC = (input$amt * input$F * 1000) / a$CMAX
    a$CL = a$CLFO * input$F
    a$VZ = a$VZFO * input$F
    a
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

