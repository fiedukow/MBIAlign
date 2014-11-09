library(shiny)
library(scatterplot3d)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  startTime <- Sys.time()
  i = 0
  STATE = DynAlignInit(c("a","b","c"), matrix(c(2,-1,-2,-1,3,-2,-2,-2,4), nrow=3, byrow=3), -1,
                       list(c("a","b","c","a"),c("a","c","a"),c("b","b","c")))

  output$TState <- renderPlot({
    input$step
    if (i > 1)
    STATE <<- DynAlignStep(STATE)
    points = expand.grid(1:dim(STATE$T)[1], 1:dim(STATE$T)[2], 1:dim(STATE$T)[3])
    not_infinity = which(STATE$T[as.matrix(points)] != -Inf);
    scatterplot3d(unlist(points[1])[not_infinity],
                  unlist(points[2])[not_infinity],
                  unlist(points[3])[not_infinity],
                  lwd=4,
                  color=color.scale(
                    STATE$T[as.matrix(points)][not_infinity],
                    cs1=c(1,0),
                    cs2=c(0,1),
                    cs3=c(0,0)),
                  xlim = c(1,dim(STATE$T)[1]),
                  ylim = c(1,dim(STATE$T)[2]),
                  zlim = c(1,dim(STATE$T)[3]))
  })

  output$OText <- renderText({
    input$step
    i <<- i + 1
    return(paste(i))
  })
})
