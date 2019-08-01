# Load the shiny package
library(shiny)

# Define UI for the application
ui <- fluidPage(
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h4("Plot parameters"),
      textInput("title", "Plot title", "Car speed vs distance to stop"),
      numericInput("num", "Number of cars to show", 30, 1, nrow(cars)),
      sliderInput("size", "Point size", 1, 5, 2, 0.5)
    ),
    # Add a main panel around the plot and table
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(cars[1:input$num, ], main = input$title, cex = input$size)
  })
  output$table <- renderTable({
    cars[1:input$num, ]
  })
}

# Run the application
shinyApp(ui = ui, server = server)



library(shiny)

# Define UI for the application
ui <- fluidPage(
  # Create a numeric input with ID "age" and label of
  # "How old are you?"
  numericInput("age", "How old are you?", value = 20),
  
  # Create a text input with ID "name" and label of 
  # "What is your name?"
  textInput("name", "What is your name?")
)

# Define the server logic
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)

# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Create a text input with an ID of "name"
      textInput("name", "What is your name?", "Dean"),
      numericInput("num", "Number of flowers to show data for",
                   10, 1, nrow(iris))
    ),
    mainPanel(
      # Add a placeholder for a text output with ID "greeting"
      textOutput(outputId = "greeting"),
      # Add a placeholder for a plot with ID "cars_plot"
      plotOutput("cars_plot"),
      # Add a placeholder for a table with ID "iris_table"
      tableOutput("iris_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)


# Load the shiny package
library(shiny)

# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("name", "What is your name?", "Dean"),
      numericInput("num", "Number of flowers to show data for",
                   10, 1, nrow(iris))
    ),
    mainPanel(
      textOutput("greeting"),
      plotOutput("cars_plot"),
      tableOutput("iris_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Create a plot of the "cars" dataset 
  output$cars_plot <- renderPlot({
    plot(cars)
  })
  
  # Render a text greeting as "Hello <name>"
  output$greeting <- renderText({
    paste("Hello", input$name)
  })
  
  # Show a table of the first n rows of the "iris" data
  output$iris_table <- renderTable({
    data <- iris[1:input$num, ]
    data
  })
}

# Run the application
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  numericInput("num1", "Number 1", 5),
  numericInput("num2", "Number 2", 10),
  textOutput("result")
)

server <- function(input, output) {
  # Calculate the sum of the inputs
  my_sum <- reactive({
    input$num1 + input$num2
  })
  
  # Calculate the average of the inputs
  my_average <- reactive({
    my_sum() / 2
  })
  
  output$result <- renderText({
    paste(
      # Print the calculated sum
      "The sum is", my_sum(),
      # Print the calculated average
      "and the average is", my_average()
    )
  })
}

shinyApp(ui, server)


# Load the gapminder package
library(gapminder)

# Define UI for the application
ui <- fluidPage(
  "The population of France in 1972 was",
  textOutput("answer")
)

# Define the server function
server <- function(input, output) {
  output$answer <- renderText({
    # Determine the population of France in year 1972
    subset(gapminder, country == "France" & year == 1972)$pop
  })
}

# Run the application
shinyApp(ui = ui, server = server)



# Load the ggplot2 package for plotting
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Add a title text input
      textInput("title", "Title", value = "GDP vs life exp")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10() +
      # Use the input value as the plot's title
      ggtitle(input$title)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      # Add a size numeric input
      numericInput("size", "Point size", value = 1, min = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      # Use the size input as the plot point size
      geom_point(size = input$size) +
      scale_x_log10() +
      ggtitle(input$title)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      # Add a checkbox for line of best fit
      checkboxInput("fit", "Add line of best fit", value = FALSE)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size) +
      scale_x_log10() +
      ggtitle(input$title)
    
    # When the "fit" checkbox is checked, add a line
    # of best fit
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)



# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      # Add radio buttons for colour
      radioButtons("color", "Point color", choices = c("blue", "red", "green", "black"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      # Use the value of the color input as the point colour
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("color", "Point color",
                   choices = c("blue", "red", "green", "black")),
      # Add a continent dropdown selector
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    # Subset the gapminder dataset by the chosen continents
    data <- subset(gapminder,
                   continent %in% input$continents)
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("color", "Point color",
                   choices = c("blue", "red", "green", "black")),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      # Add a slider selector for years to filter
      sliderInput("years", "Years", min = min(gapminder$year), max = max(gapminder$year), value = c(1977, 2002))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    # Subset the gapminder data by the chosen years
    data <- subset(gapminder,
                   continent %in% input$continents &
                     year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)



# Load the colourpicker package
library(colourpicker)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      
      # Replace the radio buttons with a color input
      colourInput("color", "Point color",
                  value = "blue"),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      sliderInput("years", "Years",
                  min(gapminder$year), max(gapminder$year),
                  value = c(1977, 2002))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    data <- subset(gapminder,
                   continent %in% input$continents &
                     year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      colourInput("color", "Point color", value = "blue"),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      sliderInput("years", "Years",
                  min(gapminder$year), max(gapminder$year),
                  value = c(1977, 2002))
    ),
    mainPanel(
      # Make the plot 600 pixels wide and 600 pixels tall
      plotOutput("plot", width = 600, height= 600)
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    data <- subset(gapminder,
                   continent %in% input$continents &
                     year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)


# Load the plotly package
library(plotly)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      colourInput("color", "Point color", value = "blue"),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      sliderInput("years", "Years",
                  min(gapminder$year), max(gapminder$year),
                  value = c(1977, 2002))
    ),
    mainPanel(
      # Replace the `plotOutput()` with the plotly version
      plotlyOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Replace the `renderPlot()` with the plotly version
  output$plot <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({   data <- subset(gapminder,
                                continent %in% input$continents &
                                  year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
    })
  })
}

shinyApp(ui = ui, server = server)


ui <- fluidPage(
  h1("Gapminder"),
  # Add a placeholder for a table output
  tableOutput("table")
)

server <- function(input, output) {
  # Call the appropriate render function 
  output$table <- renderTable({
    # Show the gapminder object in the table
    gapminder
  })
}

shinyApp(ui, server)



ui <- fluidPage(
  h1("Gapminder"),
  # Add a slider for life expectancy filter 
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      # Use the life expectancy input to filter the data
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    data
  })
}

shinyApp(ui, server)


ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  # Add a continent selector dropdown
  selectInput("continent", "Continent", choices = levels(gapminder$continent)),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    data <- subset(
      data,
      # Filter the data according to the continent input value
      continent == input$continent
    )
    data
  })
}

shinyApp(ui, server)


ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  # Add an "All" value to the continent list
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    # Don't subset the data if "All" continent are chosen
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
}

shinyApp(ui, server)



ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  # Add a plot output
  plotOutput("plot"),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  # Create the plot render function  
  output$plot <- renderPlot({
    # Use the same filtered data that the table uses
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)



ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  # Add a download button
  downloadButton(outputId = "download_data", label = "Download"),
  plotOutput("plot"),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  # Create a download handler
  output$download_data <- downloadHandler(
    # The downloaded file is named "gapminder_data.csv"
    filename = "gapminder_data.csv",
    content = function(file) {
      # The code for filtering the data is copied from the
      # renderTable() function
      data <- gapminder
      data <- subset(
        data,
        lifeExp >= input$life[1] & lifeExp <= input$life[2]
      )
      if (input$continent != "All") {
        data <- subset(
          data,
          continent == input$continent
        )
      }
      
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)


ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  downloadButton(outputId = "download_data", label = "Download"),
  plotOutput("plot"),
  tableOutput("table")
)

server <- function(input, output) {
  # Create a reactive variable named "filtered_data"
  filtered_data <- reactive({
    # Filter the data (copied from previous exercise)
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  output$table <- renderTable({
    # Use the filtered_data variable to render the table output
    data <- filtered_data()
    data
  })
  
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      # Use the filtered_data variable to create the data for
      # the downloaded file
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    # Use the filtered_data variable to create the data for
    # the plot
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)

ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  downloadButton("download_data"),
  plotOutput("plot"),
  # Replace the tableOutput() with DT's version
  DT::dataTableOutput("table")
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  # Replace the renderTable() with DT's version
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)



ui <- fluidPage(
  h1("Gapminder"),
  # Create a container for tab panels
  tabsetPanel(
    # Create an "Inputs" tab
    tabPanel(
      title = "Inputs",
      sliderInput(inputId = "life", label = "Life expectancy",
                  min = 0, max = 120,
                  value = c(30, 50)),
      selectInput("continent", "Continent",
                  choices = c("All", levels(gapminder$continent))),
      downloadButton("download_data")
    ),
    # Create a "Plot" tab
    tabPanel(
      title = "Plot",
      plotOutput("plot")
    ),
    # Create "Table" tab
    tabPanel(
      title = "Table",
      DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)


my_css <- "
#download_data {
/* Change the background color of the download button
to orange. */
background: orange;

/* Change the text size to 20 pixels. */
font-size: 20px;
}

#table {
/* Change the text color of the table to red. */
color: red;
}
"

ui <- fluidPage(
  h1("Gapminder"),
  # Add the CSS that we wrote to the Shiny app
  tags$style(my_css),
  tabsetPanel(
    tabPanel(
      title = "Inputs",
      sliderInput(inputId = "life", label = "Life expectancy",
                  min = 0, max = 120,
                  value = c(30, 50)),
      selectInput("continent", "Continent",
                  choices = c("All", levels(gapminder$continent))),
      downloadButton("download_data")
    ),
    tabPanel(
      title = "Plot",
      plotOutput("plot")
    ),
    tabPanel(
      title = "Table",
      DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)



head(artofwar)
str(artofwar)
library(wordcloud2)
# Define UI for the application
ui <- fluidPage(
  h1("Word Cloud"),
  # Add the word cloud output placeholder to the UI
  wordcloud2Output(outputId = "cloud")
)

# Define the server logic
server <- function(input, output) {
  # Render the word cloud and assign it to the output list
  output$cloud <- renderWordcloud2({
    # Create a word cloud object
    create_wordcloud(artofwar)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  h1("Word Cloud"),
  # Add a numeric input for the number of words
  numericInput(inputId = "num", label = "Maximum number of words",
               value = 100, min = 5),
  # Add a color input for the background color
  colourInput("col", "Background color", "white"),
  wordcloud2Output("cloud")
)

server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    # Use the values from the two inputs as
    # parameters to the word cloud
    create_wordcloud(artofwar,
                     num_words = input$num , background = input$col)
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  h1("Word Cloud"),
  # Add a sidebar layout to the UI
  sidebarLayout(
    # Define a sidebar panel around the inputs
    sidebarPanel(
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    # Define a main panel around the output
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    create_wordcloud(artofwar,
                     num_words = input$num, background = input$col)
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      # Add a textarea input
      textAreaInput("text", "Enter text", row = 7),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    # Use the textarea's value as the word cloud data source
    create_wordcloud(data = input$text, num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Enter text", rows = 7),
      # Add a file input
      fileInput("file", "Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    create_wordcloud(input$text, num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)


ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Enter text", rows = 7),
      fileInput("file", "Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  # Define a reactive variable named `input_file`
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    # Read the text in the uploaded file
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Use the reactive variable as the word cloud data source
    create_wordcloud(data = input_file(), num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)


ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      # Add radio buttons input
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          # First choice is "book", with "Art of War" displaying
          "Art of War" = "book",
          # Second choice is "own", with "Use your own words" displaying
          "Use your own words" = "own",
          # Third choice is "file", with "Upload a file" displaying
          "Upload a file" = "file"
        )
      ),
      textAreaInput("text", "Enter text", rows = 7),
      fileInput("file", "Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    create_wordcloud(input_file(), num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          "Art of War" = "book",
          "Use your own words" = "own",
          "Upload a file" = "file"
        )
      ),
      textAreaInput("text", "Enter text", rows = 7),
      fileInput("file", "Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  # Create a "data_source" reactive variable
  data_source <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$source == "book") {
      data <- artofwar
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Use the data_source reactive variable as the data
    # in the word cloud function
    create_wordcloud(data = data_source(), num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          "Art of War" = "book",
          "Use your own words" = "own",
          "Upload a file" = "file"
        )
      ),
      conditionalPanel(
        condition = "input.source == 'own'",
        textAreaInput("text", "Enter text", rows = 7)
      ),
      # Wrap the file input in a conditional panel
      conditionalPanel(
        # The condition should be that the user selects
        # "file" from the radio buttons
        condition = "input.source == 'file'",
        fileInput("file", "Select a file")
      ),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- artofwar
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(), num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          "Art of War" = "book",
          "Use your own words" = "own",
          "Upload a file" = "file"
        )
      ),
      conditionalPanel(
        condition = "input.source == 'own'",
        textAreaInput("text", "Enter text", rows = 7)
      ),
      conditionalPanel(
        condition = "input.source == 'file'",
        fileInput("file", "Select a file")
      ),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- artofwar
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Isolate the code to render the word cloud so that it will
    # not automatically re-render on every parameter change
    isolate({
      # Render the word cloud using inputs and reactives
      create_wordcloud(data = data_source(), num_words = input$num,
                       background = input$col)
    })
  })
}

shinyApp(ui = ui, server = server)



ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          "Art of War" = "book",
          "Use your own words" = "own",
          "Upload a file" = "file"
        )
      ),
      conditionalPanel(
        condition = "input.source == 'own'",
        textAreaInput("text", "Enter text", rows = 7)
      ),
      conditionalPanel(
        condition = "input.source == 'file'",
        fileInput("file", "Select a file")
      ),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white"),
      # Add a "draw" button to the app
      actionButton(inputId = "draw", label = "Draw!")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- artofwar
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Add the draw button as a dependency to
    # cause the word cloud to re-render on click
    input$draw
    isolate({
      create_wordcloud(data_source(), num_words = input$num,
                       background = input$col)
    })
  })
}

shinyApp(ui = ui, server = server)



