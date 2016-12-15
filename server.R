library(shiny)
library(plyr)
library(ggplot2)

function(input, output) {
  set.seed(451291)
  
  ###############################   TAREA 1 ############################################### 
  
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
  
  ###############################   TAREA 2 ############################################### 
  
  integral <- function(f,a,b,N){
    
    num_sim <- runif(N,a,b)
    fun_sim <- f(num_sim)
    aux_fun <- runif(N,min(fun_sim),max(fun_sim))
    
    inferior <- sum(fun_sim >= aux_fun & (sign(aux_fun)==1 & sign(fun_sim)==1))
    superior <- sum(fun_sim >= aux_fun & (sign(aux_fun)==-1 & sign(fun_sim)==-1))
    (inferior - superior)/N * abs( max(fun_sim)-min(fun_sim))*abs(b-a)
  }
  
  fun <- reactive({
    texto <- paste("aux <- function(x) ",input$function2)
    eval(parse(text = texto))
    aux
  })
  
  
  Result <- reactive({
    a     <- input$a
    b     <- input$b
    N     <- input$nsim2
    f     <- fun()
    c <- input$c
    int <- integral(f,a,b,N)
    int
  })
  
  confidence <- function(Numero){
    a     <- input$a
    b     <- input$b
    N     <- input$nsim2
    f     <- fun()
    c <- input$c
    uniforme <- runif(Numero, min = a, max = b)
    aplicada <- (b-a)*f(uniforme)
    media <- mean(aplicada)
    desvia <- qnorm(c/2, lower.tail = F) * sd(aplicada)/sqrt(Numero)
    minimo <- media - desvia
    maximo <- media + desvia
    lista <- list(media,minimo,maximo,Numero)
    df <- data.frame(lista)
    names(df) <- paste(c("media","minimo","maximo","Numero"))
    df
  }
  
  Intervalos <- reactive({
    a     <- input$a
    b     <- input$b
    N     <- input$nsim2
    f     <- fun()
    c <- input$c
    sequ <- seq(10,N,30)
    sapply(sequ,confidence, simplify = FALSE) %>%
      bind_rows()
  })
  
  output$result <- renderText({
    
    integral <- round(Result(), 5)
    
    as.character(integral)
  })
  
  
  output$graf_conf <- renderPlot({
    a     <- input$a
    b     <- input$b
    N     <- input$nsim2
    f     <- fun()
    c <- input$c
    ggplot(Intervalos(), aes(x = Numero, y = media)) + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), 
                  alpha =0.4, fill = 'blue') + 
      geom_line(color = 'black', size = 0.8) + 
      ylab('Resultado') + 
      xlab("Número de simulaciones")
  })
  
  
  

  
  
}
  
  