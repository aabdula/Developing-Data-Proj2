
library(shiny)
library(ggvis)
fx <- function(x,y) {
  data <- mtcars
  tmp <- data[data$cyl == x , ]
  tmp <- data[data$cyl == x &
                data$hp <= y, ]
  out <- mean(tmp$mpg)
  return (out)
  p<- ggvis(mtcars, x = ~wt, y = ~mpg)
}
shinyServer(
  function(input, output) {
    output$mpgId <- renderPrint(fx(input$cyl, input$horse))
    layer_points(p)
   }
 )