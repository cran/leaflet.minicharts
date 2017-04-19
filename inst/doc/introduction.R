## ------------------------------------------------------------------------
library(leaflet.minicharts)
data("eco2mix")
head(eco2mix)

## ------------------------------------------------------------------------
data("regions")
class(regions)

## ----message=FALSE-------------------------------------------------------
library(dplyr)

prod2016 <- eco2mix %>%
  mutate(
    renewable = bioenergy + solar + wind + hydraulic,
    non_renewable = total - bioenergy - solar - wind - hydraulic
  ) %>%
  filter(grepl("2016", month) & area != "France") %>%
  select(-month) %>%
  group_by(area, lat, lng) %>%
  summarise_all(sum) %>%
  ungroup()

head(prod2016)

## ----message=FALSE-------------------------------------------------------
library(leaflet)

tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

basemap <- leaflet(width = "100%", height = "400px") %>%
  addTiles(tilesURL) %>%
  addPolygons(data = regions, color = "brown", weight = 1, fillOpacity = 0)

basemap

## ------------------------------------------------------------------------
colors <- c("#4fc13c", "#cccccc")

basemap %>%
  addMinicharts(
    prod2016$lng, prod2016$lat,
    type = "pie",
    data = prod2016[, c("renewable", "non_renewable")], 
    colorPalette = colors, 
    width = 60 * sqrt(prod2016$total) / sqrt(max(prod2016$total))
  ) %>% 
  addLegend(
    "topright",
    colors = colors, opacity = 1,
    labels = c("Renewable", "Non renewable")
  )

## ------------------------------------------------------------------------
renewable2016 <- prod2016 %>% select(hydraulic, solar, wind)
colors <- c("#3093e5", "#fcba50", "#a0d9e8")
basemap %>%
  addMinicharts(
    prod2016$lng, prod2016$lat,
    data = renewable2016,
    colorPalette = colors,
    width = 45, height = 45
  ) %>% 
  addLegend(
    "topright",
    colors = colors, opacity = 1,
    labels = c("Hydraulic", "Solar", "Wind")
  )

## ------------------------------------------------------------------------
basemap %>%
  addMinicharts(
    prod2016$lng, prod2016$lat,
    data = prod2016$load,
    showLabels = TRUE,
    width = 45
  )

## ------------------------------------------------------------------------
appdata <- eco2mix %>% filter(area != "France")
maxValue <- max(appdata[, c("hydraulic", "solar", "wind")])
appdata <- split(appdata, appdata$month)

## ----eval=FALSE----------------------------------------------------------
#  ui <- fluidPage(
#    sliderInput("monthId", "", 1, length(appdata), value = 1, step = 1, animate = TRUE),
#    tags$h4(textOutput("month")),
#    leafletOutput("map")
#  )
#  
#  server <- function(input, output, session) {
#    # Display month
#    output$month <- renderText(names(appdata)[input$monthId])
#  
#    # Initialize the map
#    output$map <- renderLeaflet({
#      data <- appdata[[1]]
#  
#      basemap %>%
#        addMinicharts(
#          data$lng, data$lat,
#          data = data[, c("hydraulic", "solar", "wind")],
#          maxValues = maxValue,
#          colorPalette = colors,
#          width = 45, height = 45,
#          layerId = data$area
#        ) %>%
#        addLegend(
#          "topright",
#          colors = colors, opacity = 1,
#          labels = c("Hydraulic", "Solar", "Wind")
#        )
#    })
#  
#    # Update minicharts when the slider value changes
#    observeEvent(input$monthId ,{
#      data <- appdata[[input$monthId]]
#  
#      leafletProxy("map", session) %>%
#        updateMinicharts(
#          layerId = data$area,
#          data = data[, c("hydraulic", "solar", "wind")]
#        )
#    })
#  
#  }
#  
#  shinyApp(ui, server)

