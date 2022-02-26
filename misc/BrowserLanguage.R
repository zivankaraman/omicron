jscode <- "var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('mydata', language);
console.log(language);"
library(shiny)
library(shinyjs)

shinyApp(
    ui = fluidPage(
        useShinyjs(),
        
        "This is your browser language",
        textOutput('your_lang')
        
    ),
    server = function(input, output,session) {
        runjs(jscode)
        output$your_lang <- renderPrint(input$mydata)
    }
)