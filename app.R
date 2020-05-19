# Load in necessary packages
# push again

#General Shiny 
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(here)
library(janitor)
library(snakecase)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(stringr)
library(png)
library(shinyWidgets)
library(rgdal)
library(spatstat)
library(sp)
library(stargazer)


#Mapping 
library(sf)
library(tmap)
library(leaflet)
library(htmltools)
library(raster)
library(tiler)
library(lwgeom)

#Deploying
library(rsconnect)
library(curl)
library(devtools)

source("about_page.R")

########################################## DATA WRANGLING

#cv_all <- read_sf(dsn = here::here("data"),
                  #layer = "cv") %>% 
  #st_transform(crs = 4326) %>% 
  #clean_names()

sj_valley_basins <- read_sf(dsn = here::here("data"),
                           layer = "sj_valley_basins") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() 

sj_basin_pop_area <- read_csv(here("data", "sj_basin_pop_area.csv"))

sj_basins <- sj_valley_basins %>% 
  separate(basin_su_1, c("basin", "sub_basin"), sep = " - ") %>% 
  mutate(sub_basin_final = ifelse(is.na(sub_basin), basin, sub_basin)) %>% 
  mutate(sub_basin_final = to_upper_camel_case(sub_basin_final, sep_out = " ")) %>% 
  arrange(sub_basin_final) %>% 
  full_join(sj_basin_pop_area) %>% 
  dplyr::select(-sub_basin)

##############

wgs84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs" # Just have this ready to copy/paste

max_score_raster <- raster::raster(here::here("data", "sj_suit_score.tif"))
max_score_reproj = projectRaster(max_score_raster, crs = wgs84, method = "bilinear")

################

drywells <- read_sf(here("data",
                         "sj_dry_wells.shp")) %>%
  st_transform(crs = 4326)

################

geotracker <- read_sf(here("data",
                           "sj_geotracker.shp")) %>%
  st_transform(crs = 4326)

#################

nhd <- read_sf(here("data",
                    "sj_nhd.shp")) %>% 
  st_transform(crs = 4326) %>% 
  dplyr::select(FType, FCode)%>% 
  st_zm(drop = T, what = "ZM")

#################

gde <- read_sf(here("data",
                    "sj_gdes.shp")) %>% 
  st_transform(crs = 4326)

gde_fix <- st_make_valid(gde) %>% 
  st_cast("MULTIPOLYGON")

################################################# APP DEESIGN

# User interface

ui <- navbarPage(
  
  header = tagList(
  useShinydashboard()
),

"San Joaquin Valley Decision Support Tool",
                 #themeSelector(),
                 theme = shinytheme("flatly"),

####### Tab 1
                 tabPanel("Project Information",
                          icon = icon("home"),
                          header,
                          project,
                          creators),

####### Tab 2
                 tabPanel("Groundwater Basins", 
                          icon = icon("tint"),
                          sidebarLayout(
                            sidebarPanel(h4("Select a groundwater basin to see its location within the Central Valley. Learn more about its size, population, and priority status as assigned by the Department of Water Resources. After making a basin selection, you can further explore recharge suitability for projects that acheive multiple benefits on the next page."),
                                         shiny::HTML("<br><br><br>"),
                                         selectInput("gw_basin",
                                                     label = ("Central Valley Groundwater Basins:"),
                                                     choices = c(unique(sj_basins$sub_basin_final)),
                                                     selected = NULL),
                                         shiny::HTML("<br><br><br>"),
                                         shiny::HTML("<br><br><br>")
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Basin Information",
                                                   tmapOutput("ca_map"),
                                                   tableOutput("basin_table")),
                                          tabPanel("Recharge Suitability Viewer",
                                                   leafletOutput("max_map"))
                              )
                            )
                          )
                 ),
             

########## Tab 4

                 tabPanel("Learn More",
                          icon = icon("envelope"),
                          h1("Bren School Masters Group Project"),
                          shiny::HTML("<p> The analysis contained within this web app was completed as a component of a Masters' Thesis Group Project in partial satisfaction of the requirements for the degree of Master of Environmental Science and Management at the Bren School of Environmental Science & Management. This project was completed in partnership with the Environmental Defense Fund, with support from Dr. Scott Jasechko. <br><br>
                          The decision support tool was developed in ArcMap model builder. The reproducible workflow is free and available for use. If you would like to use the tool to explore potential groundwater recharge project locations in a Central Valley Basin, please contact the Bren student team through the contact page of our website below. <br><br>
                            
                            The analyses displayed in this platform were created by: Jenny Balmagia, Bridget Gibbons, Claire Madden, and Anna Perez Welter. <br><br>
                                      
                                      The data visualizations provided in this platform were created by: Lydia Bleifuss, Bridget Gibbons, and Claire Madden. "),
                          tags$div(class = "submit",
                                   tags$a(href = "https://waterresilience.wixsite.com/waterresilienceca", 
                                          "Learn More About Our Project", 
                                          target="_blank")),
                          tags$div(class = "submit",
                                   tags$a(href = "http://bren.ucsb.edu/", 
                                          "Learn More About the Bren School", 
                                          target="_blank")),
                          tags$div(class = "submit",
                                   tags$a(href = "https://www.edf.org/ecosystems/rebalancing-water-use-american-west", 
                                          "Learn More About the Environmental Defense Fund's Western Water Initiative", 
                                          target="_blank")),
                          tags$hr(),
                          fluidRow(tags$img(src = "bren.jpg", height = "10%", width = "10%"),
                                   tags$img(src = "edf.jpg", height = "15%", width = "15%")
                          )),

############ Tab 5

                 tabPanel("Data Sources",
                          icon = icon("server"),
                          shiny::HTML("<h3> References: </h3>
                                      <p> [1] Soil Agricultural Groundwater Banking Index. https://casoilresource.lawr.ucdavis.edu/sagbi/ <br><br>
                                      [2] Depth to Groundwater. https://gis.water.ca.gov/app/gicima/ <br><br>
                                      [3] Corcoran Clay Depth: https://water.usgs.gov/GIS/metadata/usgswrd/XML/pp1766_corcoran_clay_depth_feet.xml <br><br>
                                      [4] Corcoran Clay Thickness: https://water.usgs.gov/GIS/metadata/usgswrd/XML/pp1766_corcoran_clay_thickness_feet.xml <br><br>
                                      [5] National Hydrography Dataset: https://www.usgs.gov/core-science-systems/ngp/national-hydrography/nhdplus-high-resolution <br><br>
                                      [6] Natural Communities Commonly Associated With Groundwater: https://gis.water.ca.gov/app/NCDatasetViewer/ <br><br>
                                      [7] GeoTracker: https://geotracker.waterboards.ca.gov/map/?CMD=runreport&myaddress=Sacramento <br><br>
                                      [8] California Household Water Shortage Data: https://mydrywatersupply.water.ca.gov/report/publicpage <br><br>
                                      [9] CalEnviroScreen: https://oehha.maps.arcgis.com/apps/webappviewer/index.html?id=4560cfbce7c745c299b2d0cbb07044f5 <br><br>
                                      [10] California Zip Codes: https://earthworks.stanford.edu/catalog/stanford-dc841dq9031"))
)


#########################################################

# Server

server <- function(input, output){
  
  #######################
  # First map!
  
  # Filtering for basins based on dropdown menu
  
  basin_filter <- reactive({
    
    sj_basins %>% 
      filter(sub_basin_final == input$gw_basin) 
    
  })
  
  
  # Making the reactive map with basin selection
  
  
  basin_map <- reactive({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = sj_basins,
                  label = ~sub_basin_final,
                  labelOptions = labelOptions(direction = 'bottom',
                                              offset=c(0,15)),
                  color = "black",
                  weight = 0.5,
                  fillOpacity = 0.1
                          ) %>% 
      addPolygons(data = basin_filter(),
                  color = "blue",
                  weight = 0.5,
                  fillOpacity = 0.8,
                  label = ~sub_basin_final,
                  labelOptions = labelOptions(direction = 'bottom',
                                              offset=c(0,15))
                  ) 
    
 })
  
  
  output$ca_map = renderLeaflet({
    basin_map()
  })
  

  
  #################################
  
  # Table with basin stats!
  
  
  output$basin_table <- renderTable({
    
    table_df <- data.frame(basin_name = c(input$gw_basin), basin_area = c(basin_filter()$area_sq_mi), population = c(basin_filter()$population), DWR_priority = c(basin_filter()$priority))
    
    `colnames<-`(table_df, c("Basin Name", "Area (sq. mi.)", "Population", "DWR Priority"))
    
  })
  
  
  ####################################################
  #Second Map!
  
  basin_select <- reactive({ 
   
    sj_basins %>% 
      dplyr::filter(sub_basin_final == input$gw_basin)
    
    })
  
  ######
  
   max_score_filter <- reactive({
    
    raster_mask <- raster::mask(max_score_reproj, basin_select())
    
    })
  
  ######
  
   wells_filter <- reactive({
     wells_crop <- st_intersection(drywells, basin_select())
   })
  
  ######
   
   geo_filter <- reactive({
     geo_crop <- st_intersection(geotracker, basin_select())
   })
  
  ######
   
   nhd_filter <- reactive({
     nhd_crop <- st_intersection(nhd, basin_select())
   })
  
  ######
   
   gde_filter <- reactive({
     gde_crop <- st_intersection(gde_fix, basin_select())
   })
  
  #####
   
   pal <- colorNumeric("RdYlGn", reverse = TRUE, values(max_score_reproj), na.color = "transparent")
   
   max_score_map <- reactive({
     leaflet() %>%
       #Base layers
       addProviderTiles(providers$CartoDB.Positron, group = "basemap") %>%
       addPolygons(data = sj_basins, color = "black", weight = 0.5, fillOpacity = 0) %>% 
       addRasterImage(max_score_filter(), colors = pal) %>%
       addLegend(pal = pal, values = values(max_score_filter()), title = "Recharge Suitability") %>% 
       #Overlay groups
       addCircleMarkers(data = wells_filter(), group = "Domestic Wells that Have Run Dry", color = "blue", radius = 3, weight = 1) %>%
       addCircleMarkers(data = geo_filter(), color = "purple", weight = 1, radius = 3, group = "GeoTracker Clean-Up Sites") %>%
       addPolylines(data = nhd_filter(), group = "Conveyance Infrastructure", color = "black", weight = 5) %>% 
       addPolygons(data = gde_filter(), group = "Groundwater Dependent Ecosystems", color = "green") %>% 
       addLayersControl(
         overlayGroups = c("Domestic Wells that Have Run Dry", "Groundwater Dependent Ecosystems", "GeoTracker Clean-Up Sites", "Conveyance Infrastructure"),
         options = layersControlOptions(collapsed = FALSE)
       )
       
     
   })
  
  output$max_map <- renderLeaflet({
    max_score_map()
  })
  
}

################################################

# Put them together to make our app!

shinyApp(ui = ui, server = server)

