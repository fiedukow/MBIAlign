library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Multidim"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("step", label = "Step"),
      actionButton("step10", label = "10 Steps")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("TState"),
      uiOutput("OText"),
      br(),
      uiOutput("LOut"),
      br(),
      uiOutput("SOut")
    )
  )

))
