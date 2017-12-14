#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

# Set working directiory
setwd("/Users/kerihartman/Documents/GitHub/DataMan-FinalProject")

# Load Data
load("CrimeDataReduced.Rda")


lbls <- c("Total crime", "Violent crime", "Street crime", "Robbery and theft", "Murder and manslaughter", 
            "Rape", "Migration crimes", "Drug crimes", "White collar crimes", "Human trafficking", "Cybercrime", "Male Share", "Married Share")

vars <- c("crtotal", "violent", "street", "robbery", "murder", 
               "rape", "crmig", "crdrug", "whcollar", "htraffic", "cyber", "maleshare", "marriedshare")


crimes <- data.frame(cbind(lbls, vars), stringsAsFactors = FALSE)


# Define UI for application that draws a histogram


ui <- dashboardPage(
    dashboardHeader(title = "German Crime Data by Nationality"),
    dashboardSidebar(
        sidebarMenu(
          menuItem("Development over Time", tabName = "linegr", icon = icon("line-chart")),
          menuItem("Bivariate Scatter Plots", icon = icon("refresh"), tabName = "scatter"),
          menuItem("World Map", icon = icon("globe"), tabName = "map")
        )
      ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "linegr",
                fluidRow(
                  box(width = 9,
                      title = "Development over Time",
                      plotlyOutput("linePlot")),
                  box(width = 3,
                      selectInput("nat", "Nationality Group:", 
                                  choices=unique(dat2$nat)),
                      hr(),
                      selectInput("var", "Variable:",
                                  choices = crimes$lbls)
                      )
                  )
                ),
        tabItem(tabName = "scatter",
                fluidRow(
                  box(width = 9,
                      title = "Bivariate Scatter Plots",
                      plotlyOutput("scatPlot")),
                  box(width = 3,
                      selectInput("var1", "Variable 1:", 
                                  choices=crimes$lbls),
                      hr(),
                      selectInput("var2", "Variable 2:",
                                  choices = crimes$lbls)
                      )
                  )
                ),
        tabItem(tabName = "map",
                fluidRow(
                  box(width = 3,
                      selectInput("var1", "Variable:", 
                                  choices=crimes$lbls)
                      ),
                  fluidRow(
                    box(width = 12,
                        title = "Chloropleth Map",
                        plotlyOutput("mapPlot")))
                  )
                )
        )
    )
)
    
   

# Define server logic 
server <- function(input, output) {
   
   output$linePlot <- renderPlotly({
     
     plotdat <- dat2[dat2$nat == input$nat, c("year", crimes$vars[crimes$lbls == input$var])]
     names(plotdat)[2] <- "value"
     
     p <- plot_ly(plotdat, x = ~year, hoverinfo = 'text',
                  text = ~paste(round(value)
                  )) %>% 
       add_lines(y = ~value, name = "linear", line = list(shape = "linear")) %>% 
       layout(yaxis = list(title = input$var), xaxis = list(title = "Year"))
   
   })
   
   output$scatPlot <- renderPlotly({
     
    plotdat <- dat2[,c("region", "nat", "year", crimes$vars[crimes$lbls == input$var1], crimes$vars[crimes$lbls == input$var2])]
    names(plotdat)[4] <- "var1"
    names(plotdat)[5] <- "var2"
     
     p <- plot_ly(plotdat[!is.na(plotdat$region),], x = ~var2, y = ~var1, type = 'scatter', mode = 'markers',
                  hoverinfo = 'text', color = ~region, colors = "Paired",
                  text = ~paste('Nationality: ', nat, 
                                '<br> Region: ', region, 
                                '<br> Year: ', year,
                                '<br>', as.character(input$var2), ":", round(var2, digits = 2),
                                '<br>', as.character(input$var1), ":", round(var1, digits = 2)
                                )) %>% 
       layout(xaxis = list(title = input$var2), yaxis = list(title = input$var1))
   })
   
   # Map is not dynamically updating for some reason although the code follows the same pattern as above
   output$mapPlot <- renderPlotly({
     
     plotdat <- dat2[, c("nat", crimes$vars[crimes$lbls == input$var1])]     
     names(plotdat)[2] <- "value"

     plotdat <- plotdat %>% 
       dplyr::group_by(nat) %>% 
       dplyr::summarize(value = mean(value, na.rm = TRUE))
     
     # Create map
     l <- list(color = toRGB("grey"), width = 0.5)
     
     g <- list(
       showframe = FALSE,
       showcoastlines = FALSE,
       projection = list(type = 'Mercator')
     )
     
     p <- plot_geo(plotdat) %>%
       add_trace(
         z = ~value, color = ~value, colors = 'Blues',
         locations = ~nat, locationmode = "country names", marker = list(line = l)
       ) %>%
       colorbar(title = "Rate") %>%
       layout(geo = g)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

