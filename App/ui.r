library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Szukanie globalnego podobieństwa 3 ciągów metodą programowania dynamicznego."),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("step", label = "Step"),
      actionButton("step10", label = "10 Steps"),
      br(),br(),
      numericInput("N", value=1000, label="N wyswietlanych komórek:"),
      br(),br(),
      uiOutput("MInput"),
      numericInput("d", value=-1, label="d"),
      textInput("s1", value="ACCAGT", label="s1"),
      textInput("s2", value="AAATT",  label="s2"),
      textInput("s3", value="CAAGT",  label="s3")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("TState"),
      br(),
      uiOutput("DescOut"),
      br(),
      uiOutput("OText"),
      br(),
      uiOutput("LOut"),
      br(),
      uiOutput("SOut")
    )
  )

))
