library(shiny)
library(scatterplot3d)
library(combinat)
library(plotrix)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  startTime <- Sys.time()
  STATE = DynAlignInit(c("a","b","c"), matrix(c(2,-1,-2,-1,3,-2,-2,-2,4), nrow=3, byrow=3), -1,
                       list(c("a","b","c","a"),c("a","c","a"),c("b","b","c")))
  current = 0

  output$OText <- renderText({
    return(paste("<strong>Step:</strong>", input$step + input$step10 * 10))
  })


  output$TState <- renderPlot({
    diff = (input$step + input$step10 * 10) - current
    while(diff > 0) {
      STATE <<- DynAlignStep(STATE)
      diff = diff - 1
    }
    current <<- (input$step + input$step10 * 10)
    points = expand.grid(1:dim(STATE$T)[1], 1:dim(STATE$T)[2], 1:dim(STATE$T)[3])
    not_infinity = which(STATE$T[as.matrix(points)] != -Inf);
    scatterplot3d(unlist(points[1])[not_infinity],
                  unlist(points[2])[not_infinity],
                  unlist(points[3])[not_infinity],
                  type="h",
                  lty.hplot=2,
                  lwd=1,
                  pch=19,
                  color=color.scale(
                    STATE$T[as.matrix(points)][not_infinity],
                    cs1=c(1,0),
                    cs2=c(0,1),
                    cs3=c(0,0)),
                  xlim = c(1,dim(STATE$T)[1]),
                  ylim = c(1,dim(STATE$T)[2]),
                  zlim = c(1,dim(STATE$T)[3]))
  })

  output$LOut <- renderText({
    input$step
    input$step10
    return(paste(c("<strong>L:</strong>", paste(STATE$L, collapse=", "))))
  })

  output$SOut <- renderText({
    input$step
    input$step10
    return(paste(
        "<strong>Output:</strong><pre>",
        "<strong>S1:</strong>", paste(unlist(STATE$s_out[1]), collapse=", "), "\r\n",
        "<strong>S2:</strong>", paste(unlist(STATE$s_out[2]), collapse=", "), "\r\n",
        "<strong>S3:</strong>", paste(unlist(STATE$s_out[3]), collapse=", "), "\r\n</pre>",
        collapse="\n"
    ))
  })
})
