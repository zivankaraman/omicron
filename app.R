# Load packages ----------------------------------------------------------------

library(sf)
library(markdown)
library(ggplot2)

library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyBS)


# Load data --------------------------------------------------------------------
france7 <- readRDS("data/france7.rds")
dat7 <- readRDS("data/sidep7.rds")
tab7 <- readRDS("data/tab7.rds")
departements <- readRDS(file = "data/departements.rds")


# Local functions ---------------------------------------------------------

FindDepartment <- function(obj) {
    if (is.null(obj$x)) {
        ii <- NA_integer_     
    } else {
        loc <- c(obj$x, obj$y)
        pnt <- sf::st_sfc(sf::st_point(loc), crs = sf::st_crs(france7))
        ii <- as.integer(sf::st_intersects(pnt, france7, sparse = TRUE))
    }
    return(ii)
}

plotxy <- function(mydata) {
    if (length(unique(mydata$dep)) == 1) {
        my.cex <- 1
    } else {
        my.cex <- 0.7
    }
    xyplot(cpm1 ~ date | dept, data = mydata,  
           groups = dow,
           ylab = "Nombre de cas positifs par million d'habitants",
           par.strip.text = list(cex = my.cex),
           panel = function(x, y, subscripts, groups, ...) {
               panel.xyplot(x, y, subscripts = subscripts, type = "h", groups = groups, 
                            col = farben7, lwd = 3, alpha = 0.35, ...)
               # Note the use `panel.points` and the grouping using groups[subscripts]
               upper <- mydata$cpm7[subscripts]
               lower <- mydata$zero[subscripts]
               # panel.lines(x, upper, type = "l", col = "black", lwd = 5)
               panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                             col = "blue", alpha = 0.35, border = TRUE, lwd = 3, ...)
           })
}



# Prepare data --------------------------------------------------------------------

# jours <- as.character(sort(unique(dat7$date)))
# names(jours) <- jours
idx <- grep("^cpm7.20", names(france7))
jours <- names(france7)[idx]
names(jours) <- sub("cpm7.", "", jours)
dani <- names(jours)

deptList <- as.character(sort(unique(dat7$dept)))
names(deptList) <- deptList

# define global variables

# define breaks and colors
brks <- seq(0, 8000, by = 500)
NFARBEN <- length(brks) - 1
farben <- colorRampPalette(c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d'))(NFARBEN)
farben7 <- c('#b2182b','#ef8a62','#fddbc7','#a7a7a7','#d1e5f0','#67a9cf','#2166ac')

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

ui <- navbarPage("Omicron wave in France", theme = shinytheme("lumen"),
                 
                 tabPanel("By date", icon = icon("map"),
                          sidebarLayout(
                            sidebarPanel(width = 3,
                              # radioButtons("plotType", "Plot type",
                              #              c("Scatter"="p", "Line"="l")
                              # ),
                              # 
                              # # Select the date
                              # selectInput(inputId = "date", 
                              #             label = "Date:",
                              #             choices = jours,
                              #             selected = jours[1]
                              #             ),
                              dateInput(
                                  inputId = "date",
                                  label = "Select date",
                                  value = min(dani),
                                  min = min(dani),
                                  max = max(dani),
                                  format = "yyyy-mm-dd",
                                  startview = "month",
                                  weekstart = 1,
                                  language = "en",
                                  width = NULL,
                                  autoclose = FALSE,
                                  datesdisabled = NULL,
                                  daysofweekdisabled = NULL
                                  ),
                              tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                              
                              verbatimTextOutput("clickcoord")
                            ),
                            
                            mainPanel(
                                withSpinner(plotOutput("map", height = 800, click = "plotclick")),
                                bsModal("modalPlot", "COVID-19 cases evolution", NULL, size = "large", plotOutput("popup"))
                            )
                          )
                 ),
                 tabPanel("By department", icon = icon('chart-line'),
                          sidebarLayout(
                              sidebarPanel(width = 3,
                                           # Select the date
                                           selectInput(inputId = "dept",
                                                       label = "Select department",
                                                       choices = deptList,
                                                       selected = deptList[1],
                                                       multiple = TRUE
                                                       ),
                              ),
                              mainPanel(
                                  withSpinner(plotOutput("chart", height = 800))
                                  # verbatimTextOutput("summary")
                                 )
                            )
                 ),
                 navbarMenu("More",
                            tabPanel("Data table (long)",
                                     DT::dataTableOutput("longtable")
                            ),
                            tabPanel("Data table (widel)",
                                     DT::dataTableOutput("widetable")
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
    
    output$map <- renderPlot({
        jour <- input$date
        tmp <- france7[ , paste0("cpm7.", jour) ]
        names(tmp)[1] <- "cpm"
        # p <- ggplot() + aes(x = 1:10, y= 11:20) + geom_point()
        p <-
            ggplot() +
            geom_sf(data = tmp, aes(fill = cpm), show.legend = TRUE) +
            scale_fill_gradientn(limits = c(0, 8000), breaks = seq(0, 8000, by = 1000), colors = farben,
                                 guide = guide_colorbar(frame.colour = txtcol, ticks.colour = txtcol)) +
            labs(title = format.Date(jour, "%d %b %Y"),
                 subtitle = "La vague Omicron en France métropolitaine",
                 xlab = caption,
                 fill = "cas positifs\npar million d'habitants") +
            theme(plot.title = element_text(hjust = 0.5, color = txtcol),
                  plot.subtitle = element_text(hjust = 0.5, color = txtcol)) +
            theme(legend.text.align = 0,
                  legend.title.align = 0,
                  legend.title = element_text(colour = txtcol),
                  legend.text = element_text(colour = txtcol)) +
            theme(axis.line = element_blank(), axis.text.x = element_blank(),
                  axis.text.y = element_blank(), axis.ticks = element_blank(),
                  axis.title.x = element_text(hjust = 0.5, colour = txtcol, size = 8),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(), panel.border = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = bgcol, color = NA),
                  legend.background = element_rect(fill = bgcol))
        p
    })
    
    
    output$clickcoord <- renderPrint({
        id <- FindDepartment(input$plotclick)
        if (!is.na(id)) {
            toggleModal(session, "modalPlot", toggle = "open")
            cat(france7$nom[id])
        }        
    })
    
    output$popup <- renderPlot({
        id <- FindDepartment(input$plotclick)
        mydata <- subset(dat7, dep == france7$code[id])
        plotxy(mydata)
        # plot(1:10, type = input$plotType, main = d)
    })
    
    output$chart <- renderPlot({
        depts <- input$dept
        mydata <- subset(dat7, dept %in% depts)
        plotxy(mydata)
    })
    
    # output$plot <- renderPlot({
    #     plot(1:10, type = input$plotType)
    # })

  output$summary <- renderPrint({
    summary(cars)
  })

  output$longtable <- DT::renderDataTable(
      {
          tmp <- dat7[, c("dept", "date", "cpm1", "cpm7")]
          names(tmp) <- c("Departement", "Date", "Cases_per_million_raw", "Cases_per_million_smoothed")
          for (j in 3:4) {
              tmp[, j] <- format(round(tmp[, j], 2), nsmall = 2, justify = "right")
          }
          DT::datatable(tmp, rownames = FALSE, 
                        options = list(pageLength = 20,
                                       columnDefs = list(list(className = 'dt-right', targets = 2:3))
                                       ))
    })
  
  
    output$widetable <- DT::renderDataTable(
    {
        tmp <- tab7
        idx <- match(tmp$dep, departements$code)
        tmp$dep <- departements$dep[idx]
        names(tmp) <- sub("cpm7.", "", names(tmp))
        names(tmp)[1] <- "Departement"
        for (j in 2:ncol(tmp)) {
            tmp[, j] <- format(round(tmp[, j], 2), nsmall = 2, justify = "right")
        }
        numCols <- 1:(ncol(tmp) - 1)
        DT::datatable(tmp, rownames = FALSE,
                      options = list(pageLength = 20,
                                     columnDefs = list(list(className = 'dt-right', targets = numCols))
                      ))
    })
  
  
 }



# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)


