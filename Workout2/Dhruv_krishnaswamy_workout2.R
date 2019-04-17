# Dhruv Krishnaswamy workout 2 for Stat 133


library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(readr)


#' @title: future_value
#' @param: amount : amount to be invested
#' @param: rate: This is the annual rate of return 
#' @param: years: The number of years
#' @return: Returns the future value
future_value <- function(amount= 100, rate = .05, years= 1){
  return(amount*(1+rate)^years)
}


#' @title: Growing Annuity
#' @param: contrib: amount that is contributed
#' @param: rate: This is the annual rate of return 
#' @param: growth; This is the annual growth rate
#' @param; years: The number of years
#' @return future value of annuity

growing_annuity<- function(contrib = 200, rate = .05, growth = .03, years= 1)
{
  
  return(contrib*( ( (1+rate)^years  - (1+growth)^years)/(rate - growth) ) )
}

#' @title: Annuity
#' @param: contrib: amount that is contributed
#' @param: rate: This is the annual rate of return 
#' @param; years: The number of years
#' @return future value of annuity

annuity <- function(contrib = 200, rate = 0.05, years = 1)
{
  return(contrib*(((1+rate)^years - 1)/(rate)))
  
}

create_tab <- function(amount, contrib, rate, growth, num_years) {
  # rate and growth passed in as percents
  rate = rate/100
  years = 0:num_years
  growth = growth/100
  t1 = rep(0,num_years + 1)
  t2 = rep(0, num_years + 1)
  t3 = rep(0, num_years + 1)
  
  for (y in years) 
    {
    t1[y+1] = future_value(amount, rate, y)
    t2[y+1] = future_value(amount, rate, y) +annuity(contrib, rate, y)
    t3[y+1] = future_value(amount, rate, y) + growing_annuity(contrib, rate, growth, y)
    
  }
  mods = data.frame(year=years,no_contrib = t1,fixed_contrib = t2,
         
  growing_contrib = t3)
  return(mods)
}

# Define UI for application that draws a histogram
ui <- fluidPage( titlePanel("Saving/Investing Scenarios"),
        #Main User interface
                 fluidRow(
                   column(4, sliderInput(inputId = "init", "Initial Amount", min = 1, max = 100000, step = 500, value = 1000, pre = '$', 
                                         sep = ','),
                          sliderInput(inputId = "annual", "Annual Contribution", min = 0, max =50000, step = 500,value = 2000, pre = '$',
                                      sep = ',')),
                   
                   column(4, sliderInput(inputId = "return", "Return Rate (in %)", min = 0, max = 20, step = 0.1,
                                         value = 5),
                          
                          sliderInput(inputId = "growth", "Growth Rate (in %)", min = 0, max = 20, step = 0.1,
                                      value = 2 )), 
                   
                   column(4, sliderInput(inputId = "year", "Years", min = 0, max = 50, step = 1,
                                         value = 20),
                          
                          selectInput(inputId = "facet", "Facet?", choices = c('No', 'Yes')))
                   
                 ),
        mainPanel(
                 h4("timelines"), plotOutput("finalplot", height = 300),
                 h4("balances"), verbatimTextOutput("table") )
        
)

# Define server logic required to draw a histogram
server <- function(input, output) 
  {
  tabl1<- reactive(
    {
  maintab = create_tab(input$init, input$annual,input$return, input$growth,
                      input$year)
 finalbalances = gather(maintab, Mod, Balance,-year)
 
 #finalbalances = gather(maintab(), type, value, no_contrib:growing_contrib)
 
  maintab
  finalbalances
  })
 
 output$finalplot <- renderPlot({
 if (input$facet == "Yes") 
   {
   ggplot(data= tabl1(), aes(x=year, y=Balance, col=Mod, color= variable)) +
     geom_point() +geom_line() + ggtitle("Investing Timelines")+facet_grid(.~ Mod) +geom_area(alpha=.2)
 }
 else{
   ggplot(data=tabl1(), aes(x=year, y=Balance, col=Mod, color= variable)) +
     geom_point() +geom_line() + ggtitle("Investing Timelines")
 }
 })
 
output$table = renderPrint(create_tab(input$init, input$annual,input$return, input$growth,
                                       input$year))

  
}
# Run the application 
shinyApp(ui =ui, server= server)

