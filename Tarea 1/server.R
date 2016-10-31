library(shiny)

function(input, output) {
  set.seed(451291)
  
  
  funInput <- reactive({
    switch(input$distribution,
           "Uniforme" = function() {return (runif(input$nsim))},
           "Exponencial" = function() {U<- runif(input$nsim) 
           return (-log(1-U)/input$lambda)},
           "Binomial" = function() {U<-matrix(runif(input$nsim*input$n),input$nsim,input$n)
           p<-input$p
           suma<-matrix(0,input$nsim,1)
           for(i in 1:input$nsim){
             for(j in 1:input$n){
               if(U[i,j]>(input$p)){suma[i]<-suma[i]+1}} 
           }
           return(suma[,1])},
           "Poisson" = function() {U<-runif(input$nsim)
           i<-1:input$nsim
           for(i in seq(along=i)){
             p=U[i]
             x=0
             while(p>=exp(-input$lambda)) {
               x=x+1
               p=p*runif(1)} 
             U[i]=x}
           return(U)},
           "Normal" = function() {U<-runif(input$nsim)
           V<-runif(input$nsim)
           x<-1:input$nsim
           y<-1:input$nsim
           i<-1:input$nsim
           for(i in seq(along=i)){
             x[i] = (sqrt(-2*log(U[i]))*cos(2*pi*V[i]))*input$sigma + input$miu
           }
           return(x)}
    )
    
  })
  
  funR <- reactive({
    switch(input$distribution,
           "Uniforme" = function() {return (runif(input$nsim))},
           "Exponencial" = function() {return(rexp(input$nsim,input$lambda))},
           "Binomial" = function() {return(rbinom(input$nsim,input$n,input$p))},
           "Poisson" = function() {return(rpois(input$nsim,input$lambda))},
           "Normal" = function() {return(rnorm(input$nsim,input$miu,input$sigma))}
    )
  })
  
  
  output$hist <- renderPlot({
    InvFun <-funInput()
    x <- InvFun()
    hist(x, 
         prob = TRUE,
         col = "slategray2",
         border = "white",
         breaks= input$nbin,
         main = input$distribution, 
         xlab = "", 
         las = 1)
    if(input$distribution=='Exponencial' | input$distribution=='Normal'){
      lines(density(x), lwd = 2, col = "darkblue")}
    
  })
  
  output$summary <- renderPrint({
    InvFun <-funInput()
    RFun <-funR()
    x <- InvFun()
    y <- RFun()
    summary(x)
    test <-ks.test(x,y)
    test
  })
  
}