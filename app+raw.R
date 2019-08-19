#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mrgsolve)
library(mrgsolve)
library(lattice)
options(mrgsolve_mread_quiet=TRUE)


code <- '
$PARAM

k1 = 5
k0 = 2
F = 0.5 

$SET delta= 0.01

$CMT GUT


$OMEGA 0 0 0
$SIGMA 0
$MAIN

F_GUT = F;

$ODE

dxdt_GUT= -k1*GUT - k0;

$CAPTURE k1 k0 GUT'

library(mrgsolve)
library(plotly)
mod <- mcode_cache("DDIa", code)
mod %>% 
  ev(amt= 100) %>% 
  mrgsim


mod <- mod %>% update(end=4,delta=0.001) %>% Req(GUT)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Hwanin"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
         sliderInput("amt",
                     "Dose",
                     min = 1,
                     max = 500,
                     value = 30),
         sliderInput("k1", "First order elimination rate constant", 0.1, 100, 5),
         sliderInput("k0", "Zero order elimination rate", 0.1, 100, 5),
         sliderInput("F", "Fraction", 0, 1, 0.5)
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
    read_csv(input$file1$datapath)
  })
  
  e <- reactive({
    ev(amt=input$amt)
  })
  
  # Reactive parameter value
  par <- reactive({
    param(k1 = input$k1, k0 = input$k0, F = input$F)
  })
   output$simPlot <- renderPlotly({
     a<- mod %>% 
       param(par())%>%
       ev(e()) %>% 
       mrgsim %>%
       as.data.frame()
     
     a<- ggplot(a, aes(x = time, y= GUT)) +
       geom_line(color = "#6666FF", alpha = 0.7)+
       geom_point(data = uploaded_data(), aes(x = time, y = DV))+
       theme_bw()
     
     ggplotly(a)
   })
  output$table <- renderTable({
    a <- mod %>%
      param(par()) %>%
      ev(e()) %>%
      mrgsim %>%
      filter(GUT >= 0) %>%
      tail(n = 1)
    a
  })
   
   }

# Run the application 
shinyApp(ui = ui, server = server)

