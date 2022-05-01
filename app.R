# Load packages ----------------------------------------------------------------

library(sf)
library(markdown)
library(ggplot2)
library(lattice)

library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyBS)


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
           ylab = "Number of positive cases per million inhabitants",
           par.strip.text = list(cex = my.cex),
           panel = function(x, y, subscripts, groups, ...) {
               panel.xyplot(x, y, subscripts = subscripts, type = "h", groups = groups, 
                            col = farben7, lwd = 3, alpha = 0.35, ...)
               # Note the use `panel.points` and the grouping using groups[subscripts]
               upper <- mydata$cpm7[subscripts]
               lower <- mydata$zero[subscripts]
               panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                             col = "blue", alpha = 0.35, border = TRUE, lwd = 3, ...)
           })
}


# Load data --------------------------------------------------------------------

france7 <- readRDS("data/france7.rds")
dat7 <- readRDS("data/sidep7.rds")
tab7 <- readRDS("data/tab7.rds")
departements <- readRDS(file = "data/departements.rds")


# Prepare data --------------------------------------------------------------------

idx <- grep("^cpm7.20", names(france7))
jours <- names(france7)[idx]
names(jours) <- sub("cpm7.", "", jours)
dani <- names(jours)

deptList <- as.character(sort(unique(dat7$dept)))
names(deptList) <- deptList


# Global variables --------------------------------------------------------

colNames <- c("Department", "Date", "Cases_per_million_raw", "Cases_per_million_smoothed")
mapFill <- c("positive cases", "per million inhabitants")

# define breaks and colors
brks <- seq(0, 8000, by = 500)
NFARBEN <- length(brks) - 1
farben <- colorRampPalette(c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d"))(NFARBEN)
farben7 <- c("#b2182b", "#ef8a62", "#fddbc7", "#a7a7a7", "#d1e5f0", "#67a9cf", "#2166ac")

# caption and colors
txtcol <- "#c6dbef"
bgcol <- "#525252"

Sys.setlocale(category = "LC_ALL", locale = "C")


# ui --------------------------------------------------------------------
ui <- fluidPage(
    navbarPage(
        title = "The Omicron wave in metropolitan France", theme = shinytheme("lumen"),
        tabPanel(title = "By date", icon = icon("map"),
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  dateInput(
                                      inputId = "date",
                                      label = "Select date",
                                      value = min(dani),
                                      min = min(dani),
                                      max = max(dani),
                                      format = "yyyy-mm-dd",
                                      startview = "month",
                                      weekstart = 1,
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
        tabPanel("By department", icon = icon("chart-line"),
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  selectInput(inputId = "dept",
                                              label = "Select department", 
                                              choices = deptList,
                                              selected = deptList[1],
                                              multiple = TRUE
                                  ),
                     ),
                     mainPanel(
                         withSpinner(plotOutput("chart", height = 800))
                     )
                 )
        ),
        navbarMenu(title = "More...", icon = icon("info-circle"),
                   tabPanel(title = "Data table (long)",
                            DT::dataTableOutput("longtable")
                   ),
                   tabPanel(title = "Data table (wide)",
                            DT::dataTableOutput("widetable")
                   ),
                   tabPanel(title = "About",
                            fluidRow(
                                column(6,
                                       includeMarkdown("about.md")
                                ),
                            )
                   )
        ),
    )
)

# server --------------------------------------------------------------------

server <- function(input, output, session) {
    
    output$map <- renderPlot({
        jour <- input$date
        tmp <- france7[ , paste0("cpm7.", jour) ]
        names(tmp)[1] <- "cpm"
        jourf <- format.Date(jour, "%d %b %Y")
        p <-
            ggplot() +
            geom_sf(data = tmp, aes(fill = cpm), show.legend = TRUE) +
            scale_fill_gradientn(limits = c(0, 8000), breaks = seq(0, 8000, by = 1000), colors = farben,
                                 guide = guide_colorbar(frame.colour = txtcol, ticks.colour = txtcol)) +
            labs(title = jourf,
                 subtitle = "The Omicron wave in metropolitan France",
                 xlab = "Daily number of positive cases per million inhabitants, averaged over the last 7 days.",
                 fill = mapFill) +
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
    })
    
    output$chart <- renderPlot({
        depts <- input$dept
        mydata <- subset(dat7, dept %in% depts)
        plotxy(mydata)
    })
    
    output$longtable <- DT::renderDataTable(
        {
            tmp <- dat7[, c("dept", "date", "cpm1", "cpm7")]
            names(tmp) <- colNames
            for (j in 3:4) {
                tmp[, j] <- format(round(tmp[, j], 2), nsmall = 2, justify = "right")
            }
            DT::datatable(tmp, rownames = FALSE, 
                          options = list(pageLength = 20,
                                         columnDefs = list(list(className = "dt-right", targets = 2:3))
                          ))
        })
    
    output$widetable <- DT::renderDataTable(
        {
            tmp <- tab7
            idx <- match(tmp$dep, departements$code)
            tmp$dep <- departements$dep[idx]
            names(tmp) <- sub("cpm7.", "", names(tmp))
            names(tmp)[1] <- colNames[1]
            for (j in 2:ncol(tmp)) {
                tmp[, j] <- format(round(tmp[, j], 2), nsmall = 2, justify = "right")
            }
            numCols <- 1:(ncol(tmp) - 1)
            DT::datatable(tmp, rownames = FALSE,
                          options = list(pageLength = 20,
                                         columnDefs = list(list(className = "dt-right", targets = numCols))
                          ))
        })
    
    
}


# app ----------------------------------------------------

shinyApp(ui = ui, server = server)
