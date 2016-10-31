library(shiny)

fluidPage(
  
  titlePanel("Funcion Inversa"),
  
  sidebarLayout(
    
    #INPUTS
    sidebarPanel(
      
      selectInput("distribution", "Funciones de distribucion", 
                  choices = c("Uniforme", "Binomial", "Exponencial", "Normal", "Poisson")),
      
      sliderInput("nsim", "Numero de simulaciones:", min = 1, max = 20000, value = 1000),
      sliderInput("nbin", "Numero de intervalos en histograma:", min = 1, max = 100, value = 20),
      
      conditionalPanel(
        condition="input.distribution=='Binomial'",
        numericInput("p", "Valor de p:", value = 0.5, min=0, max=1, step = 0.05),
        numericInput("n", "Valor de n", value = 10,min = 1,max = 100)        
      ),
      
      conditionalPanel(
        condition="input.distribution=='Exponencial'",
        numericInput("lambda", "Valor de lambda:", value=1, step=0.5)     
      ),     
      
      conditionalPanel(
        condition="input.distribution=='Normal'",
        numericInput("miu", "Media:", value = 0),
        numericInput("sigma", "Desviacion Estandar:", value = 1)
      ),        
      
      conditionalPanel(
        condition="input.distribution=='Poisson'",
        numericInput("lambda", "Valor de lambda:", value=1, step=0.5)     
      )
      
    ),
    
    #OUTPUTS
    mainPanel(
      
      plotOutput("hist"),
      verbatimTextOutput("summary")
    )
  )
)