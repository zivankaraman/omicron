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




# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    navbarPage("NCAA Swimming", theme = shinytheme("lumen"),
        tabPanel("Program Finder", fluid = TRUE, icon = icon("globe-americas"),
            tags$style(button_color_css),
                        
            # Sidebar layout with a input and output definitions
            sidebarLayout(
                
                # Inputs: Select variables to plot
                sidebarPanel(
                    
                    # Select the date
                    selectInput(inputId = "date", 
                                label = "Date:",
                                choices = jours,
                                selected = jours[1])
                ),
                
                # Output: Show scatterplot
                mainPanel(
                    plotOutput(outputId = "map", width = "100%")
                )
            )
        ),
        tabPanel("Program Comparisons", fluid = TRUE, icon = icon("swimmer"),
                 titlePanel("Program Comparisons"),
                 # Sidebar layout with a input and output definitions
                 sidebarLayout(
                     
                     # Inputs: Select variables to plot
                     sidebarPanel(
                         
                         # Select the date
                         selectInput(inputId = "date", 
                                     label = "Date:",
                                     choices = jours,
                                     selected = jours[1])
                     ),
                     
                     # Output: Show scatterplot
                     mainPanel(
                         plotOutput(outputId = "map")
                     )
                 )
        )
    )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
    
    output$map <- renderPlot({
        
        # tmp <- france7[ , input$date]
        # jour <- sub("ppm.", "", input$date)
        # names(tmp)[1] <- "ppm" 
        p <- ggplot() + aes(x = 1:10, y= 11:20) + geom_point()
        # p <- 
        #     ggplot() + 
        #     geom_sf(data = tmp, aes(fill = ppm), show.legend = TRUE) +
        #     scale_fill_gradientn(limits = c(0, 8000), breaks = seq(0, 8000, by = 1000), colors = farben,
        #                          guide = guide_colorbar(frame.colour = txtcol, ticks.colour = txtcol)) +
        #     labs(title = format.Date(jour, "%d %b %Y"), 
        #          subtitle = "La vague Omicron en France métropolitaine", 
        #          xlab = caption,
        #          fill = "cas positifs\npar million d'habitants") +
        #     theme(plot.title = element_text(hjust = 0.5, color = txtcol), 
        #           plot.subtitle = element_text(hjust = 0.5, color = txtcol)) +
        #     theme(legend.text.align = 0,
        #           legend.title.align = 0,
        #           legend.title = element_text(colour = txtcol),
        #           legend.text = element_text(colour = txtcol)) +
        #     theme(axis.line = element_blank(), axis.text.x = element_blank(),
        #           axis.text.y = element_blank(), axis.ticks = element_blank(),
        #           axis.title.x = element_text(hjust = 0.5, colour = txtcol, size = 8),
        #           axis.title.y = element_blank(),
        #           panel.background = element_blank(), panel.border = element_blank(), 
        #           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #           plot.background = element_rect(fill = bgcol, color = NA),
        #           legend.background = element_rect(fill = bgcol)
        #     )
        print(p)
    })
    

}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)

