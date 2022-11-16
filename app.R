library(shiny)
constant <- read.csv("data/antconst.csv")

# Define UI ----
ui <- fluidPage(
  titlePanel("Relative Volatility Calculation using Vapor Pressure and Plotting from Antoine Equation"),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                             h4("Default Calculations at room temperature (25°C)"),
                             selectInput("var_0", 
                                         label = h3("Choose your Component 1 from the below list"),
                                         choices = constant[,1]),
                             selectInput("var_1", 
                                         label = h3("Choose your Component 2 from the below list"),
                                         choices = constant[,1]),
                             numericInput("num_4", 
                                          h3("Particular Temperature in °C "), 
                                          value = 25),
                             sliderInput("slider2", h3("Choose Your temperature range(in °C)"),
                                         min = -373, max = 373, value = c(-26, 77))),
                
                
                mainPanel(h2("Relative Volatility"),
                          p("Relative volatility is a measure comparing the vapor pressures of the components in a liquid mixture of chemicals. This quantity is widely used in designing large industrial distillation processes.In effect, it indicates the ease or difficulty of using distillation to separate the more volatile components from the 
                            less volatile components in a mixture. By convention, relative volatility is usually denoted as (α)."),
                          p("Relative volatility between two components L(light component) and H (heavy component is defined as"),
                          img(src = "imgop_1.png", height = 100, width = 400),
                          p(textOutput("val1"),textOutput("val1_1")),
                          p(textOutput("val2"),textOutput("val2_1")),
                          p(textOutput("val3"),textOutput("val3_1")),
                          (h4(textOutput("val4"),textOutput("val4_1"))),
                          strong(h2(textOutput("val5"))),
                          radioButtons("plot_type", "Choose the mode of Y-axis",
                                       c("ln(Psat)", "Psat")),
                          plotOutput("plotty"),
                          plotOutput("platty")),
  )
  
)

# Define server logic ----
server <- function(input, output) {
    
  
  output$plotty <- renderPlot({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_1 == constant[i,1] ){
        num_1_1 = constant[i,2]
        num_2_1 = constant[i,3]
        num_3_1 = constant[i,4]
      }
    }
    for(i in 1:lnt){
      if (input$var_0 == constant[i,1] ){
        num_1 = constant[i,2]
        num_2 = constant[i,3]
        num_3 = constant[i,4]
      }
    }  
    
    PP_1 <- c(input$slider2[1]:input$slider2[2])+num_3
    pp_2 <- num_1-num_2*1/PP_1
    TT_f <- c(input$slider2[1]:input$slider2[2])
    pp_f <- 10^(pp_2)
    PP_1_1 <- c(input$slider2[1]:input$slider2[2])+num_3_1
    pp_2_1 <- num_1_1-num_2_1*1/PP_1_1
    TT_f_1 <- c(input$slider2[1]:input$slider2[2])
    pp_f_1 <- 10^(pp_2_1)
    if(input$plot_type == "Psat"){
      plot(TT_f,pp_f,pch = 19,xlab ="Temperature in  °C",ylab = "Psat where Psat is in mm of Hg")
      points(TT_f_1,pp_f_1,pch = 19,col = "red")
      legend("topleft",pch = 19,col = c("black","red"),legend = c("Component 1","Component 2"))
      abline(v = input$num_4,col = "blue")
      title("Plot for Psat Vs Temperature",)
    } else{
      plot(TT_f,pp_2,pch = 19,xlab ="Temperature in  °C",ylab = "log10(Psat) where Psat is in mm of Hg")
      points(TT_f_1,pp_2_1,pch = 19,col = "red")
      legend("topleft",pch = 19,col = c("black","red"),legend = c("Component 1","Component 2"))
      abline(v = input$num_4,col = "blue")
      title("Plot for log10(Psat) Vs Temperature")
    }
  })
  output$platty <- renderPlot({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_1 == constant[i,1] ){
        num_1_1 = constant[i,2]
        num_2_1 = constant[i,3]
        num_3_1 = constant[i,4]
      }
    }
    for(i in 1:lnt){
      if (input$var_0 == constant[i,1] ){
        num_1 = constant[i,2]
        num_2 = constant[i,3]
        num_3 = constant[i,4]
      }
    }  
    PP_1 <- c(input$slider2[1]:input$slider2[2])+num_3
    pp_2 <- num_1-num_2*1/PP_1
    TT_f <- c(input$slider2[1]:input$slider2[2])
    pp_f <- 10^(pp_2)
    PP_1_1 <- c(input$slider2[1]:input$slider2[2])+num_3_1
    pp_2_1 <- num_1_1-num_2_1*1/PP_1_1
    TT_f_1 <- c(input$slider2[1]:input$slider2[2])
    pp_f_1 <- 10^(pp_2_1)
    alpha_0 <- pp_f/pp_f_1
    plot(TT_f,alpha_0,pch = 19,xlab ="Temperature in  °C",ylab = "Relative Volatility (α)")
    abline(v = input$num_4,col = "blue")
    title("Plot for Relative Volatility(α) Vs Temperature")
    
  })
  
  output$val1 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_0 == constant[i,1] ){
        num_1 = constant[i,2]
      }
    }  
    paste("Your A value is",num_1)
  })
  output$val2 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_0 == constant[i,1] ){
        num_2 = constant[i,3]
      }
    }  
    paste("Your B value is",num_2)
  })
  output$val3 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_0 == constant[i,1] ){
        num_3 = constant[i,4]
      }
    }  
    paste("Your C value is",num_3)
  })
  output$val4 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_0 == constant[i,1] ){
        num_1 = constant[i,2]
        num_2 = constant[i,3]
        num_3 = constant[i,4]
      }
    }  
    parti <- num_1-num_2/(input$num_4+num_3)
    parti_f <- signif(10^(parti),digits = 5)
    paste("Vapour Pressure for Component 1 for the given constants and temperature is",parti_f,"mm of Hg")
  })
  output$val1_1 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_1 == constant[i,1] ){
        num_1_1 = constant[i,2]
      }
    }  
    paste("Your A_1 value is",num_1_1)
  })
  output$val2_1 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_1 == constant[i,1] ){
        num_2_1 = constant[i,3]
      }
    }  
    paste("Your B_1 value is",num_2_1)
  })
  output$val3_1 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_1 == constant[i,1] ){
        num_3_1 = constant[i,4]
      }
    }  
    paste("Your C_1 value is",num_3_1)
  })
  output$val4_1 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_1 == constant[i,1] ){
        num_1_1 = constant[i,2]
        num_2_1 = constant[i,3]
        num_3_1 = constant[i,4]
      }
    }  
    parti <- num_1_1-num_2_1/(input$num_4+num_3_1)
    parti_f <- signif(10^(parti),digits = 5)
  paste("Vapour Pressure for Component 2 for the given constants and temperature is",parti_f,"mm of Hg")
  })
  output$val5 <- renderText({
    lnt = dim(constant)[1]
    for(i in 1:lnt){
      if (input$var_1 == constant[i,1] ){
        num_1_1 = constant[i,2]
        num_2_1 = constant[i,3]
        num_3_1 = constant[i,4]
      }
    }
    for(i in 1:lnt){
      if (input$var_0 == constant[i,1] ){
        num_1 = constant[i,2]
        num_2 = constant[i,3]
        num_3 = constant[i,4]
      }
    }  
    parti <- num_1-num_2/(input$num_4+num_3)
    parti_f <- 10^(parti)
    parti_1 <- num_1_1-num_2_1/(input$num_4+num_3_1)
    parti_f_1 <- 10^(parti_1)
    finval <- round(parti_f/parti_f_1,digits = 3)
    paste("Relative Volatality for the given constants and temperature is",finval)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)