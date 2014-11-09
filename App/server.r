library(shiny)
library(scatterplot3d)
library(combinat)
library(plotrix)

h_InM = function(id, value) {
  return(paste0("<input id=\"",id,"\" type=\"number\" value=\"",value,"\" style=\"width: 40px; text-align: center; padding-left: 0px;\"/>"))
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  startTime <- Sys.time()
  STATE = DynAlignInit(c("a","b","c"), matrix(c(2,-1,-2,-1,3,-2,-2,-2,4), nrow=3, byrow=3), -1,
                       list(c("a","b","c","a"),c("a","c","a"),c("b","b","c")))
  currentIndex = c(1,1,1)
  currentStep = 0
  vis_L = list()
  init_iteration = 0 # :( https://github.com/rstudio/shiny/issues/167

  H_initializeIfNecessary = function() {
    if (!is.null(input$AA)) {
      cm = matrix(c(input$AA,input$AC,input$AT,input$AG,
                    input$AC,input$CC,input$CT,input$CG,
                    input$AT,input$CT,input$TT,input$TG,
                    input$AG,input$CG,input$TG,input$GG),
                  nrow=4, ncol=4, byrow=TRUE)
      cs = list(unlist(strsplit(input$s1, split="")),
                unlist(strsplit(input$s2, split="")),
                unlist(strsplit(input$s3, split="")))
      same_s = length(cs) == length(STATE$s)
      for(i in 1:3) {
        if (!same_s)
          break;
        form_value = unlist(cs[[i]])
        model_value = unlist(STATE$s[[i]])
        if (length(form_value) != length(model_value) ||
            sum(form_value != model_value) != 0) {
          same_s = FALSE
        }
      }

      if(sum(dim(cm) != dim(STATE$M_base)) != 0 || sum(cm != STATE$M_base) != 0 || input$d != STATE$d || !same_s) {
        STATE <<- DynAlignInit(c("A","C","T","G"), cm, input$d, cs)
        init_iteration <<- (input$step + input$step10 * 10)
        currentIndex <<- c(1,1,1)
        currentStep <<- 0
        vis_L <<- list()
      }
    }
  }

  output$MInput <- renderText({
    return(paste("<h3>M = </h3>",
                 "<table>",
                 "<tr><th> </th><th>         A         </th><th>         C         </th><th>         T         </th><th>         G         </th></tr>",
                 "<tr><th>A</th><td>",h_InM("AA",  1),"</td><td>                   </td><td>                   </td><td>                   </td></tr>",
                 "<tr><th>C</th><td>",h_InM("AC", -1),"</td><td>",h_InM("CC",  1),"</td><td>                   </td><td>                   </td></tr>",
                 "<tr><th>T</th><td>",h_InM("AG", -1),"</td><td>",h_InM("CG", -1),"</td><td>",h_InM("TT",  1),"</td><td>                   </td></tr>",
                 "<tr><th>G</th><td>",h_InM("AT", -1),"</td><td>",h_InM("CT", -1),"</td><td>",h_InM("TG", -1),"</td><td>",h_InM("GG",  1),"</td></tr>",
                 "</table>",
                 sep="\r\n"
                 ))
  })

  output$TState <- renderPlot({
    H_initializeIfNecessary()

    diff = (input$step + input$step10 * 10 - init_iteration) - currentStep
    while(diff > 0) {
      currentIndex <<- STATE$TIndex
      STATE <<- DynAlignStep(STATE)
      if (length(STATE$L) > length(vis_L))  ### TODO - This is hack - it should be keept in state internally.
        vis_L <<- STATE$L
      diff = diff - 1
    }
    currentStep <<- (input$step + input$step10 * 10 - init_iteration)
    points = expand.grid(1:dim(STATE$T)[1], 1:dim(STATE$T)[2], 1:dim(STATE$T)[3])
    not_infinity = which(STATE$T[as.matrix(points)] != -Inf);
    values = STATE$T[as.matrix(points)][not_infinity]
    in_order = order(values, decreasing=TRUE)[1:input$N]
    sp = scatterplot3d(unlist(points[1])[not_infinity][in_order],
                       unlist(points[2])[not_infinity][in_order],
                       unlist(points[3])[not_infinity][in_order],
                       type="h",
                       lty.hplot=2,
                       lwd=1,
                       pch=19,
                       color=color.scale(
                         values[in_order],
                         cs1=c(1,0),
                         cs2=c(0,1),
                         cs3=c(0,0)),
                       xlim = c(1,dim(STATE$T)[1]),
                       ylim = c(1,dim(STATE$T)[2]),
                       zlim = c(1,dim(STATE$T)[3]),
                       xlab = "s1",
                       ylab = "s2",
                       zlab = "s3",
                       x.ticklabs = c("X", STATE$s[[1]]),
                       y.ticklabs = c("X", STATE$s[[2]]),
                       z.ticklabs = c("X", STATE$s[[3]]),
                       lab = c(5,4,0), lab.z=4)
    if (length(vis_L) > 0) {
      mL = matrix(unlist(vis_L), ncol=3, byrow=3)
      sp$points3d(mL[,1], mL[,2], mL[,3], type="l", lwd=3)
      sp$points3d(mL[,1], mL[,2], mL[,3], col="blue", lwd=3)
    }

    sp$points3d(currentIndex[1]  , currentIndex[2]  , currentIndex[3]  , lwd=2, col="black")

    #TODO - this looks horrible:
    if (currentIndex[1] > 1 && currentIndex[2] > 1 && currentIndex[3] > 1)
      sp$points3d(currentIndex[1]-1, currentIndex[2]-1, currentIndex[3]-1, lwd=2, col="blue")

    if (currentIndex[2] > 1 && currentIndex[3] > 1)
      sp$points3d(currentIndex[1]  , currentIndex[2]-1, currentIndex[3]-1, lwd=2, col="blue")

    if (currentIndex[1] > 1 && currentIndex[3] > 1)
      sp$points3d(currentIndex[1]-1, currentIndex[2]  , currentIndex[3]-1, lwd=2, col="blue")

    if (currentIndex[1] > 1 && currentIndex[2] > 1)
      sp$points3d(currentIndex[1]-1, currentIndex[2]-1, currentIndex[3]  , lwd=2, col="blue")

    if (currentIndex[1] > 1)
      sp$points3d(currentIndex[1]-1, currentIndex[2]  , currentIndex[3]  , lwd=2, col="blue")

    if (currentIndex[2] > 1)
      sp$points3d(currentIndex[1]  , currentIndex[2]-1, currentIndex[3]  , lwd=2, col="blue")

    if (currentIndex[3] > 1)
      sp$points3d(currentIndex[1]  , currentIndex[2]  , currentIndex[3]-1, lwd=2, col="blue")


    output$LOut <- renderText({
      return(paste(c("<strong>L:</strong>", paste(STATE$L, collapse=", "))))})

    output$SOut <- renderText({
      return(paste(
        "<strong>Output:</strong><pre>",
        "<strong>S1:</strong>", paste(unlist(STATE$s_out[1]), collapse=", "), "\r\n",
        "<strong>S2:</strong>", paste(unlist(STATE$s_out[2]), collapse=", "), "\r\n",
        "<strong>S3:</strong>", paste(unlist(STATE$s_out[3]), collapse=", "), "\r\n</pre>",
        collapse="\n"
      ))
    })

    output$OText <- renderText({
      return(paste("<strong>Step:</strong>", input$step + input$step10 * 10 - init_iteration))
    })
  })


})
