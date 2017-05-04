library(shiny)
library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
data(diamonds)



# linear model
LinMmodel <- lm(price ~ carat + cut + clarity + color + x + y + z, data = diamonds)

shinyServer(function(input, output) {
      output$detailedText <- renderText({
            paste("Based on the following attributes: </br> length: ",
                  strong(input$X),
                  "mm </br> width: ",
                  strong(input$Y),
                  "mm </br> depth: ",
                  strong(input$Z),
                  "mm </br> quality: ",
                  strong(input$RBcut),
                  " </br> color: ",
                  strong(input$Scolor),
                  " </br> clarity: ",
                  strong(input$Sclar),
                  " </br> weight: ",
                  strong(input$carat),
                  "ct.</br>the predicted price would be approximately: </br> ________________________ ")
      })
      
      
      
      
      output$prediction <- renderText({
            df <- data.frame(x = input$X,
                             y = input$Y,
                             z = input$Z,
                             cut = factor(input$RBcut, levels = levels(diamonds$cut)),
                             color = factor(input$Scolor, levels = levels(diamonds$color)),
                             clarity = factor(input$Sclar, levels = levels(diamonds$clarity)),
                             carat = input$carat)
            
            pr <- predict(LinMmodel, newdata = df, interval = "prediction")
            
            paste(h1( strong("$ "), strong(floor(pr))))
            
            
      })
      output$plot <- renderPlotly({
            library(plotly)
            set.seed(150)
            d <- diamonds[sample(nrow(diamonds), 500), ]
            plot_ly(d, x = ~carat, y = ~price, color = ~carat,
                    size = ~carat, alpha = 1/2,text =  ~paste("Carat: ", carat, 
                                                              "</br> Cut: ", cut,
                                                              "</br> Color: ", color,
                                                              "</br> Clarity: ", clarity,
                                                              "</br> Length: ", x,
                                                              "</br> Width: ", y,
                                                              "</br> Depth: ", z,
                                                              "</br> Price: ", price)) %>%
                  layout(title = "A quick look over a 500 random samples from the data set")
      })
      
})







