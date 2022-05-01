# Load packages ----------------------------------------------------------------

library(sf)
library(markdown)
library(ggplot2)
library(lattice)

library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyBS)
# library(shinyjs)
library(shiny.i18n)


# Load data --------------------------------------------------------------------
france7 <- readRDS("data/france7.rds")
dat7 <- readRDS("data/sidep7.rds")
tab7 <- readRDS("data/tab7.rds")
departements <- readRDS(file = "data/departements.rds")
dico <- readRDS("data/dico.rds")
# texte <- dico$fr

# File with translations
i18n <- Translator$new(translation_csvs_path = "translations/")
i18n$set_translation_language("en") # here you select the default translation to display

if (length(grep("Windows", osVersion)) > 0) {
    loki <- list(en = "English", fr = "French")
    gloki <- list(en = "English", fr = "French")
} else {
    loki <- list(en = "en_US.utf8", fr = "fr_FR.utf8")
    gloki <- list(en = "English", fr = "French")
}

colNames <- list(en = 'c("Department", "Date", "Cases_per_million_raw", "Cases_per_million_smoothed")', 
                 fr = 'c("Département", "Date", "Cas_par_million_bruts", "Cas_par_million_lissés")')
mapFill <- list(en = 'c("positive cases", "per million inhabitants")', 
                fr = 'c("cas positifs", "par million d\'habitants")')

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
    # print(i18n$t("english"))
    xyplot(cpm1 ~ date | dept, data = mydata,  
           groups = dow,
           ylab = i18n$t("Number of positive cases per million inhabitants"),
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

# jscode <- "var language =  window.navigator.userLanguage || window.navigator.language;
# Shiny.onInputChange('mylanguage', language);
# console.log(language);"

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
farben <- colorRampPalette(c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d"))(NFARBEN)
farben7 <- c("#b2182b", "#ef8a62", "#fddbc7", "#a7a7a7", "#d1e5f0", "#67a9cf", "#2166ac")

# caption and colors
txtcol <- "#c6dbef"
bgcol <- "#525252"


# ui --------------------------------------------------------------------
ui <- fluidPage(
    shiny.i18n::usei18n(i18n),
    navbarPage(
        title = i18n$t("The Omicron wave in metropolitan France"), theme = shinytheme("lumen"),
        tabPanel(title = i18n$t("By date"), icon = icon("map"),
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  dateInput(
                                      inputId = "date",
                                      label = i18n$t("Select date"),
                                      value = min(dani),
                                      min = min(dani),
                                      max = max(dani),
                                      format = "yyyy-mm-dd",
                                      startview = "month",
                                      weekstart = 1,
                                      # language = substring(i18n$t("english"), 1, 2),
                                      # language = i18n$get_key_translation(),
                                      # language = tolower(substring(Sys.getlocale(category = "LC_TIME"), 1, 2)),
                                      width = NULL,
                                      autoclose = FALSE,
                                      datesdisabled = NULL,
                                      daysofweekdisabled = NULL
                                  ),
                                  tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                                  
                                  verbatimTextOutput("clickcoord"),
                                  verbatimTextOutput("locales")
                     ),
                     
                     mainPanel(
                         withSpinner(plotOutput("map", height = 800, click = "plotclick")),
                         bsModal("modalPlot",  i18n$t("COVID-19 cases evolution"), NULL, size = "large", plotOutput("popup"))
                     )
                 )
        ),
        tabPanel(i18n$t("By department"), icon = icon("chart-line"),
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  # Select the date
                                  selectInput(inputId = "dept",
                                              label = i18n$t("Select department"), 
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
        navbarMenu(title = i18n$t("More..."), icon = icon("info-circle"),
                   tabPanel(title = i18n$t("Data table (long)"),
                            DT::dataTableOutput("longtable")
                   ),
                   tabPanel(title = i18n$t("Data table (wide)"),
                            DT::dataTableOutput("widetable")
                   ),
                   tabPanel(title = i18n$t("About"),
                            fluidRow(
                                column(6,
                                       # p(i18n$t("about_en.md")),
                                       # htmltools::includeMarkdown(path = i18n$t("about_en.md"))
                                       includeMarkdown("about_en.md")
                                ),
                                # useShinyjs(),
                                # column(3,
                                #        img(class="img-polaroid",
                                #            src=paste0("http://upload.wikimedia.org/",
                                #                       "wikipedia/commons/9/92/",
                                #                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                #        tags$small(
                                #            "Source: Photographed at the Bay State Antique ",
                                #            "Automobile Club's July 10, 2005 show at the ",
                                #            "Endicott Estate in Dedham, MA by ",
                                #            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                #              "User:Sfoskett")
                                #        )
                                # )
                            )
                   )
        ),
        footer = 
            div(style = "position: absolute; bottom: 45px; left:15px;",
                selectInput(inputId = 'selected_language', 
                            label = i18n$t("Select language"),
                            choices = i18n$get_languages(),
                            selected = i18n$get_key_translation())
            )
    )
)

# server --------------------------------------------------------------------

server <- function(input, output, session) {
    observeEvent(input$selected_language, {
        # This print is just for demonstration
        # print(paste("Language change!", input$selected_language))
        # Change locale 
        Sys.setlocale("LC_ALL", loki[[input$selected_language]])
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
    })
    
    output$map <- renderPlot({
        jour <- input$date
        tmp <- france7[ , paste0("cpm7.", jour) ]
        names(tmp)[1] <- "cpm"
        jourf <- format.Date(jour, "%d %b %Y")
        # p <- ggplot() + aes(x = 1:10, y= 11:20) + geom_point()
        p <-
            ggplot() +
            geom_sf(data = tmp, aes(fill = cpm), show.legend = TRUE) +
            scale_fill_gradientn(limits = c(0, 8000), breaks = seq(0, 8000, by = 1000), colors = farben,
                                 guide = guide_colorbar(frame.colour = txtcol, ticks.colour = txtcol)) +
            labs(title = jourf,
                 subtitle = i18n$t("The Omicron wave in metropolitan France"),
                 xlab = i18n$t("Daily number of positive cases per million inhabitants, averaged over the last 7 days."),
                 fill = eval(parse(text = mapFill[[input$selected_language]]))) +
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
    
    output$locales <- renderPrint({
        locales <- system("locale -a", intern = TRUE)
        cat(locales)
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
    
  output$longtable <- DT::renderDataTable(
      {
          tmp <- dat7[, c("dept", "date", "cpm1", "cpm7")]
          names(tmp) <- eval(parse(text = colNames[[input$selected_language]]))
          for (j in 3:4) {
              tmp[, j] <- format(round(tmp[, j], 2), nsmall = 2, justify = "right")
          }
          urla <- sprintf("//cdn.datatables.net/plug-ins/1.10.11/i18n/%s.json", gloki[[input$selected_language]])
          DT::datatable(tmp, rownames = FALSE, 
                        options = list(pageLength = 20,
                                       language = list(url = urla),
                                       columnDefs = list(list(className = "dt-right", targets = 2:3))
                                       ))
    })
  
  
    output$widetable <- DT::renderDataTable(
    {
        tmp <- tab7
        idx <- match(tmp$dep, departements$code)
        tmp$dep <- departements$dep[idx]
        names(tmp) <- sub("cpm7.", "", names(tmp))
        names(tmp)[1] <- eval(parse(text = colNames[[input$selected_language]]))[1]
        for (j in 2:ncol(tmp)) {
            tmp[, j] <- format(round(tmp[, j], 2), nsmall = 2, justify = "right")
        }
        numCols <- 1:(ncol(tmp) - 1)
        urla <- sprintf("//cdn.datatables.net/plug-ins/1.10.11/i18n/%s.json", gloki[[input$selected_language]])
        DT::datatable(tmp, rownames = FALSE,
                      options = list(pageLength = 20,
                                     language = list(url = urla),
                                     columnDefs = list(list(className = "dt-right", targets = numCols))
                      ))
    })
  
  
 }



# app ----------------------------------------------------

shinyApp(ui = ui, server = server)


