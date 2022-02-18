# Load packages ----------------------------------------------------------------
library(shiny)
library(markdown)


# Load packages ----------------------------------------------------------------
library(sf)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(ggplot2)

# Load data --------------------------------------------------------------------
france7 <- readRDS("data/france7.rds")
dat7 <- readRDS("data/sidep7.rds")
tab7 <- readRDS("data/tab7.rds")


# Prepare data --------------------------------------------------------------------

# jours <- as.character(sort(unique(dat7$date)))
# names(jours) <- jours
idx <- grep("^ppm.20", names(france7))
jours <- names(france7)[idx]
names(jours) <- sub("ppm.", "", jours)

departments <- as.character(sort(unique(dat7$dep)))
names(departments) <- departments

# define global variables

# define breaks and colors
brks <- seq(0, 8000, by = 500)
NFARBEN <- length(brks) - 1
farben <- colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d'))(NFARBEN)

# caption and colors
caption <- paste("Nombre quotidien de cas positifs pour 1 million d’habitants, en moyenne sur les 7 derniers jours.", 
                 "Source des données : https://www.data.gouv.fr/en/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/", 
                 sep = "\n")
txtcol <- "#c6dbef"
bgcol <- "#525252"

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"




# Load data --------------------------------------------------------------------




# Define UI --------------------------------------------------------------------

ui <- navbarPage("Navbar!",
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType", "Plot type",
                                           c("Scatter"="p", "Line"="l")
                              )
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          )
                 ),
                 tabPanel("Summary",
                          verbatimTextOutput("summary")
                 ),
                 navbarMenu("More",
                            tabPanel("Table",
                                     DT::dataTableOutput("table")
                            ),
                            tabPanel("About",
                                     fluidRow(
                                       column(6,
                                              includeMarkdown("about.md")
                                       ),
                                       column(3,
                                              img(class="img-polaroid",
                                                  src=paste0("http://upload.wikimedia.org/",
                                                             "wikipedia/commons/9/92/",
                                                             "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                              tags$small(
                                                "Source: Photographed at the Bay State Antique ",
                                                "Automobile Club's July 10, 2005 show at the ",
                                                "Endicott Estate in Dedham, MA by ",
                                                a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                                  "User:Sfoskett")
                                              )
                                       )
                                     )
                            )
                 )
)



server <- function(input, output, session) {
  output$plot <- renderPlot({
      
      
      
    plot(1:10, type=input$plotType)
      
      
  })

  output$summary <- renderPrint({
    summary(cars)
  })

  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
}



# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)


