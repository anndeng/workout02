library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
   
   # Application title
   titlePanel("Title"),
   
   # Sidebar with a slider input
   fluidRow(
      column(4,
         sliderInput("init",
                     "Initial amount:",
                     min = 0,
                     max = 100000,
                     value = 1000,
                     step = 500),
         sliderInput("annual",
                     "Annual contribution:",
                     min = 0,
                     max = 50000,
                     value = 2000,
                     step = 500)
      ),
      column(4, 
         sliderInput("return",
                     "Return rate:",
                     min = 0,
                     max = 20,
                     value = 5,
                     step = 0.1), 
         sliderInput("growth",
                     "Growth rate",
                     min = 1,
                     max = 20,
                     value = 2,
                     step = 0.1)
      ),
      column(4, 
             sliderInput("yrs",
                         "Years:",
                         min = 0,
                         max = 50,
                         value = 20,
                         step = 1), 
             selectInput("facet", label = h3("Facet?"), 
                         choices = list("No" = 1, "Yes" = 2), 
                         selected = 1)
      )
   ),
      
  # timeline plot
  h4("Timelines"),
  plotOutput("timeline"),
  
  # table of returns
  h4("Balances"),
  verbatimTextOutput("table")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$timeline <- renderPlot({
     
     # Defining functions:
     # 1) Future Value Function
     #' @title future_value()
     #' @description compute future value of an investment
     #' @param amount, rate, years
     #' @return PV(1+r)^t
     future_value <- function(amount, rate, years) {
       FV <- amount*(1+rate)^years
       return(FV)
     }
     
     # 2) Future Value of Annuity
     #' @title annuity()
     #' @description compute future value of annuity
     #' @param contrib = contribution, rate = annual rate of return, years = number of years
     #' @return FVA = C*((1+r)^t-1)/r
     annuity <- function(contrib, rate, years) {
       FVA <- contrib*((1+rate)^years-1)/rate
       return(FVA)
     }
     
     # 3) Future Value of Growing Annuity
     #' @title growing_annuity()
     #' @description compute future value of growing annuity
     #' @param contrib = contribution, rate = annual rate of return, growth = annual growth rate, years = number of years
     #' @return FVGA = C*((1+r)^t-(1+g)^t)/(r-g)
     growing_annuity <- function(contrib, rate, growth, years) {
       FVGA <- contrib*((1+rate)^years-(1+growth)^years)/(rate-growth)
       return(FVGA)
     }
     
     # data frame set-up definitions

     period <- seq(from = 0, to = input$yrs)
     grow <- input$growth / 100
     rate <- input$return / 100
     
     # defining data frames, for loop to input data into df
     no_contrib <- data.frame(period)
     fixed_contrib <- data.frame(period)
     growing_contrib <- data.frame(period)
     for (t in 1:length(period)) {
       no_contrib$investment[t] <- future_value(input$init, rate, period[t])
       fixed_contrib$investment[t] <- future_value(input$init, rate, period[t]) + annuity(input$annual, rate, period[t])
       growing_contrib$investment[t] <- future_value(input$init, rate, period[t]) + growing_annuity(input$annual, rate, grow, period[t])
     }
     
     no_contrib$mode <- factor("Regular savings")
     fixed_contrib$mode <- factor("Regular savings")
     growing_contrib$mode <- factor("Regular savings")
     
     no_contrib$annuity <- factor("No annuity")
     fixed_contrib$annuity <- factor("Fixed annuity")
     growing_contrib$annuity <- factor("Growing annuity")
     
     pd <- rbind(no_contrib, fixed_contrib, growing_contrib)
     
     if (input$facet == 1) {
      # facet = no
       ggplot(data = pd) + 
       geom_point(aes(x = period, y = investment, color = annuity)) +
       geom_line(aes(x = period, y = investment, color = annuity)) +
       ggtitle("Three modes of investing") +
       xlab("Years") + ylab("Dollars in investment") +
       theme_light()
     } # facet = yes
       else ggplot(data = pd) + 
       geom_point(aes(x = period, y = investment, color = annuity)) +
       geom_line(aes(x = period, y = investment, color = annuity)) +
       geom_area(aes(x = period, y = investment, fill = annuity, alpha = 0.5)) +
       ggtitle("Three modes of investing") +
       xlab("Years") + ylab("Dollars in investment") + 
       facet_grid(.~annuity) +
       theme_light()
   })
   
   output$table <- renderPrint({
     # Defining functions:
     # 1) Future Value Function
     #' @title future_value()
     #' @description compute future value of an investment
     #' @param amount, rate, years
     #' @return PV(1+r)^t
     future_value <- function(amount, rate, years) {
       FV <- amount*(1+rate)^years
       return(FV)
     }
     
     # 2) Future Value of Annuity
     #' @title annuity()
     #' @description compute future value of annuity
     #' @param contrib = contribution, rate = annual rate of return, years = number of years
     #' @return FVA = C*((1+r)^t-1)/r
     annuity <- function(contrib, rate, years) {
       FVA <- contrib*((1+rate)^years-1)/rate
       return(FVA)
     }
     
     # 3) Future Value of Growing Annuity
     #' @title growing_annuity()
     #' @description compute future value of growing annuity
     #' @param contrib = contribution, rate = annual rate of return, growth = annual growth rate, years = number of years
     #' @return FVGA = C*((1+r)^t-(1+g)^t)/(r-g)
     growing_annuity <- function(contrib, rate, growth, years) {
       FVGA <- contrib*((1+rate)^years-(1+growth)^years)/(rate-growth)
       return(FVGA)
     }
     
     # defining data frame, inputs
     
     year <- seq(from = 0, to = input$yrs)
     df <- data.frame(year)
     grow <- input$growth / 100
     rate <- input$return / 100
     
     # for loop to input values into data frame
     for (t in 1:length(year)) {
       df$no_contrib[t] <- future_value(input$init, rate, year[t])
       df$fixed_contrib[t] <- future_value(input$init, rate, year[t]) + annuity(input$annual, rate, year[t])
       df$growing_contrib[t] <- future_value(input$init, rate, year[t]) + growing_annuity(input$annual, rate, grow, year[t])
     }
     
     df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

