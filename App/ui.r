library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Multidim"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("step", label = "Step")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("TState"),
      textOutput("OText")
    )
  )

))
