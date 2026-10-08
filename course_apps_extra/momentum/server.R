## X 1. start graphs earlier (so we have pre-transition trends)
## 2. label age groups in matrix
##  X 3. program ylim in births and total pop size
## X 4. move births to upper right
## 5. add slider
## 5. gamify: figure out Qs?


library(shiny)
## runApp() to debug
momentum_simu <- function(s1, s2, s3, s4,
                          f1, f2, f3, f4, f5, T_max, this_T)
{
    ## s1 <- s2 <- s3 <- s4 <- 1
    ## f1 <- f2 <- f3 <- f4 <- f5 <- .7
    ## T = 20
    T_max = 20
    A <- matrix(0, 5, 5)
    A[1,1] <- f1
    A[1,2] <- f2
    A[1,3] <- f3
    A[1,4] <- f4
    A[1,5] <- f5
    A[2,1] <- s1
    A[3,2] <- s2
    A[4,3] <- s3
    A[5,4] <- s4
    ## starting pop
    w <- Re(eigen(A)$vec[,1])
    K0 <- w/w[1]
    K.mat <- matrix(NA, 5, T_max+1)
    K.mat[,1] <- K0

    ## stationary pop
    firstrow = A[1,]
    subdi = A[cbind(2:5, 1:4)]
    nrr = sum(firstrow * cumprod(c(1, subdi)))
    A0 = A
    A0[1,] <- A[1,]/nrr
    ## print(eigen(A0)$val[1])


    for (i in 1:T_max)
    {

        K.mat[,i+1] <- A0 %*% K.mat[,i]
    }

    ## add T_back = 5 years of pre-pop
    T_back = 2
    lambda = Re(eigen(A)$val[1])
    preK <- cbind(K0) %*% rbind(lambda^{-(T_back:1)})
    K.mat <- cbind(preK, K.mat)
    colnames(K.mat) <- -T_back:T_max

    ## four panels
    ## a) starting Leslie matrix
    ## b) Bt
    Bt <- K.mat[1,]
    ## c) Kt
    Kt <- colSums(K.mat)
    ## d) start and end pop by age


    par(mfcol = c(3,2), mar = c(5.1-.5, 4.1, 4.1-1.2, 2.1))
    ## NRR
    nrr.vec <- c(rep(nrr, T_back+1), rep(1, T_max))
    names(nrr.vec) <- colnames(K.mat)
    plot(names(nrr.vec), nrr.vec, type = 'o',
         main = "NRR", xlab = "", ## xlab = "time T",
         ylim = c(0, max(nrr.vec) + 1))
    ## Bt
    this_T.vec = -T_back:this_T
    plot(this_T.vec, Bt[paste(this_T.vec)], type = 'o', log = 'y',
         xlim = c(-T_back, T_max),
         ylim = range(Bt), ## c(.1, 2),
         main = "'Births' (youngest group)",
         xlab = "", ## "time T",
         ylab = "Births")
        abline(v = 0, lty = 2)
    ## Kt
    plot(this_T.vec, Kt[paste(this_T.vec)], type = 'o',
##         plot(-T_back:this_T, Kt[1:(this_T+10)], type = 'o',
         xlim = c(-T_back, T_max),
         ylim = c(min(Kt)*.9, max(Kt)*1.1),
         main = "Total Population Size",
         xlab = "time T",
         ylab = "Population")
    abline(v = 0, lty = 2)
    ## matrix
    plot(0:6, 0:6, type = 'n', axes = F,
         ylab = "",
         xlab = "",
         main = "Leslie Projection matrix")
    for (i in 1:5)
        for (j in 1:5)
        {
            text(j, i, A[6-i,j], font = 4, cex = 1.4)
        }
    ## startend
    startend <- K.mat[,paste(c(0, this_T))]
    this_M <- sum(K.mat[,paste(this_T)])/sum(K.mat[,paste(0)])
    this_B <- sum(K.mat[1,paste(this_T)])/sum(K.mat[1,paste(0)])
    barplot(t(startend), horiz = T, beside = TRUE,
            col = c("grey", "red"),
            main = "Size of each age group",
            xlim = c(0, max(startend)*1.1))
    plot(0,0, axes = FALSE, ylab = "", xlab = "", type = 'n')
    legend("topright", legend = c("end pop",
                                  "start pop",
                                  paste("Birth ratio:", round(this_B, 2)),
                                  paste("Pop ratio:", round(this_M, 2))),
           fill = c("red", "grey", NA, NA),
           border = c("black", "black", "white", "white"),
           cex = 1.5,
           bty = 'n')
}


## Define server logic required to draw plot
shinyServer(function(input, output) {
    ## Expression that generates a histogram. The expression is
    ## wrapped in a call to renderPlot to indicate that:
    ##
    ##  1) It is "reactive" and therefore should re-execute automatically
    ##    when inputs change
    ##  2) Its output type is a plot
    output$momentumPlot <- renderPlot({
        momentum_simu(s1 = input$s1,
                      s2 = input$s2,
                      s3 = input$s3,
                      s4 = input$s4,
                      f1 = input$f1,
                      f2 = input$f2,
                      f3 = input$f3,
                      f4 = input$f4,
                      f5 = input$f5,
                      this_T = input$this_T)
    })
})
