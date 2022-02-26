library(shiny)
library(shinyalert)

shinyApp(
    ui = fluidPage(
        actionButton("btn", "Click me")
    ),
    server = function(input, output) {
        observeEvent(input$btn, {
            # Show a simple modal
            img = "https://deanattali.com/assets/img/404-southpark.jpg"
            img = "C:/GitHub/omicron/a.png"
            shinyalert(title = "You did it!", size = "l",
                       imageUrl = img, imageWidth = 800, imageHeight = 800)
        })
    }
)


