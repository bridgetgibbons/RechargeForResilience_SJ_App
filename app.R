# Load in necessary packages
# push again

#General Shiny 
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
#library(ggmap)
library(here)
library(janitor)
library(snakecase)
#library(RColorBrewer)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(stringr)
library(png)
library(shinyWidgets)
#library(paletteer)


#Mapping 
library(sf)
library(tmap)
#library(mapview)
#library(tmaptools)
library(leaflet)
library(htmltools)
library(raster)
library(tiler)
library(lwgeom)
#library(rmapshaper)

#Deploying
library(rsconnect)
library(curl)
library(devtools)

#1 Map of central valley basins, when you select your basin, the fill color changes (cv.shp and sgma_basins.shp)

# Read in our data
cv_all <- read_sf(dsn = here::here("data"),
                  layer = "cv") %>% 
  st_transform(crs = 4326) %>% 
  clean_names()

sgma_basins_all <- read_sf(dsn = here::here("data"),
                           layer = "sgma_basins") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() 

basin_pop_area <- read_csv(here("data", "basin_pop_area.csv"))

sgma_basins <- sgma_basins_all %>% 
  separate(basin_su_1, c("basin", "sub_basin"), sep = " - ") %>% 
  mutate(sub_basin_final = ifelse(is.na(sub_basin), basin, sub_basin)) %>% 
  mutate(sub_basin_final = to_upper_camel_case(sub_basin_final, sep_out = " ")) %>% 
  arrange(sub_basin_final) %>% 
  full_join(basin_pop_area) %>% 
  dplyr::select(-sub_basin)

wgs84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs" # Just have this ready to copy/paste

max_score_raster <- raster::raster(here::here("data", "Max_final_score_LU.tif"))

max_score_reproj = projectRaster(max_score_raster, crs = wgs84, method = "bilinear")

zipcodes <- read_sf(dsn = here::here("data"),
                           layer = "ZCTA2010") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() %>% 
  dplyr::select(zcta) 


drywells <- read_sf(here("data",
                         "drywells_cv.shp")) %>%
  st_transform(crs = 4326)


geotracker <- read_sf(here("data",
                           "geotracker_cv.shp")) %>%
  st_transform(crs = 4326)

nhd <- read_sf(here("data",
                    "NHD_select_cv.shp")) %>% 
  st_transform(crs = 4326) %>% 
  dplyr::select(FType, FCode)%>% 
  st_zm(drop = T, what = "ZM")

gde <- read_sf(here("data",
                    "GDE_cv.shp")) %>% 
  st_transform(crs = 4326)

gde_fix <- st_make_valid(gde) %>% 
  st_cast("MULTIPOLYGON")


# User interface

ui <- navbarPage(
  
  header = tagList(
  useShinydashboard()
),

"Recharge for Resilience",
                 #themeSelector(),
                 theme = shinytheme("flatly"),
                 tabPanel("Project Information",
                          icon = icon("home"),
                          h1("Explore a Decision Support Tool for",
                             style = "font-size:32px",
                             align = "center"),
                          h1("Strategic Siting of Multi-Benefit Groundwater Recharge Projects",
                             style = "font-size:40px",
                             align = "center"),
                          img(src="image.jpg", height="100%",width="100%",style = 'position: absolute; opacity: 0.2;'
                          ),
                          fluidRow(column(2), column(8,shiny::HTML("
                          <h4> <center> <br> California has an increasingly scarce and unreliable surface water supply. As the climate changes, droughts are expected to become more frequent and extreme, precipitation is expected to fall as rain rather than snow in shorter, more intense periods, and reliance on the Sierra snowpack for storage will become less tenable. Strategically planning for water storage, including the protection and augmentation of groundwater resources, can help make farms, cities, and ecosystems more resilient to less predictable future water availability. <br> <br>

                            The Sustainable Groundwater Management Act of 2014 (SGMA) adds regulatory structure to the goal of protecting and augmenting groundwater supplies by requiring a regionalized approach to groundwater management throughout the state. Many Groundwater Sustainability Agencies (GSAs) have identified managed aquifer recharge (MAR) as a tool they will use to comply with SGMA during  the 20-year implementation period, beginning in 2020. <br> <br>
                            
                            Currently, groundwater managers lack the tools and information necessary to identify ideal locations to invest in groundwater recharge projects that are able to achieve multiple benefits. The spatial visualization in this app allows users to see how physical surface and subsurface conditions along with a surficial nitrogen balance inform areas that are better and worse for implementing recharge in a groundwater basin. In addition, users are able to overlay the location of other points of interest, including: domestic wells that have run dry, potential groundwater dependent ecosystems, listed contamination cleanup sites, and water conveyance infrastructure. <br> <br>
                            
                            This tool makes information regarding multi-benefit groundwater recharge at a regional level available to groundwater management entities, allowing Groundwater Sustainability Agencies to meet compliance requirements while realizing other locally relevant benefits. The tool incorporates publicly available information from research institutions and state agencies, eliminating some costs associated with using a recharge siting tool and ensuring transferability across the Central Valley to basins with a varying degree of local data. </h4>"
                            )),
                                    column(2)
                 )),
                 tabPanel("Groundwater Basins", 
                          icon = icon("tint"),
                          sidebarLayout(
                            sidebarPanel(h4("The decision support tool is designed for use in any Central Valley groundwater basin. Enter a zipcode below to display the zipcode on the map, and then use your cursor to identify which groundwater basin it is in."),
                                         shiny::HTML("<br><br><br>"),
                                         textInput("zip_code",
                                                   label = ("Enter a zipcode:"),
                                                   value = "e.g. 93638"),
                                         shiny::HTML("<br><br><br>"),
                                         h4("Select a groundwater basin to see its location within the Central Valley. Learn more about its size, population, and priority status as assigned by the Department of Water Resources. After making a basin selection, you can further explore recharge suitability for projects that acheive multiple benefits on the next page."),
                                         shiny::HTML("<br><br><br>"),
                                         selectInput("gw_basin",
                                                     label = ("Central Valley Groundwater Basins:"),
                                                     choices = c(unique(sgma_basins$sub_basin_final)),
                                                     selected = NULL),
                                         shiny::HTML("<br><br><br>"),
                                         shiny::HTML("<br><br><br>")
                            ),
                            mainPanel(h5("Map of California's Central Valley Groundwater Basins:"),
                                      tmapOutput("ca_map"),
                                      h5("Under the Sustainable Groundwater Management Act of 2014 (SGMA), the CA Department of Water Resources assigned basins different priorities based on groundwater conditions. High priority basins have until 2040 to reach balanced inflows and outflows to the aquifer."),
                                      h5("Learn about your selected basin:"),
                                      tableOutput("basin_table")
                            )
                          )
                 ),
                 tabPanel("Benefits and Feasibility",
                          icon = icon("swatchbook"),
                          sidebarLayout(
                            sidebarPanel(h4("The color ramp displayed in your basin outline represents a relative ranking of groundwater recharge suitability from better (green) to worse (red). The groundwater recharge suitability ranking represents a combination of the following surface and subsurface considerations: The Soil Agricultural Groundwater Banking Index (SAGBI) to describe soil surface conditions, depth to groundwater to approximate aquifer storage space, percent of coarse grained soil materials to a depth of 250 feet below ground surface as a proxy for how easily water can move in the subsurface, and the depth and thickness of the Corcoran clay, which is a low permeability layer that may inhibit recharge in some locations. In addition, this map includes a calculated nitrate balance from three representatve years (1990, 2005, and 2020) to demonstrate the relative likelihood of added nitrate contamination in groundwater as a result of recharge.")
                            ),
                            mainPanel(h5("Groundwater Recharge Suitability with Additional Considerations"),
                                      leafletOutput("max_map"),
                                      h5("The map above allows for visualization of additional considerations related to groundwater recharge benefits and feasibility. Users can turn on or off layers to see the locations of domestic wells that have run dry, GeoTracker contamination clean up sites, conveyance infrastructure and groundwater dependent ecosystems. It would be preferrable to locate recharge projects closer to domestic wells that have run dry, conveyance infrastructure and groundwater dependent ecosystems but farther away from GeoTracker contamination cleanup sites. For a more detailed output of recharge suitability with weighted additional considerations, please contact our team via the link on the Learn More page.")
                            )
                          )),
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




# Server

server <- function(input, output){
  
  ################################################
  # First map!
  
  # Filtering for basins based on dropdown menu
  
  basin_filter <- reactive({
    
    sgma_basins %>% 
      filter(sub_basin_final == input$gw_basin) 
    
  })
  
  # Filtering for zip code based on user entry
  
  zipcode_filter <- reactive({
    
    zipcodes %>% 
      filter(zcta == input$zip_code)
    
  })
  
  
  # Making the reactive map with basin and zip code selections
  
  
  basin_map <- reactive({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = sgma_basins,
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
                  ) %>% 
      addPolygons(data = zipcode_filter(),
                  color = "red",
                  weight = 0.4)
    
 })
  
  
  output$ca_map = renderLeaflet({
    basin_map()
  })
  

  
  ###################################################
  # Table with basin stats!
  
  
  output$basin_table <- renderTable({
    
    table_df <- data.frame(basin_name = c(input$gw_basin), basin_area = c(basin_filter()$area_sq_mi), population = c(basin_filter()$population), DWR_priority = c(basin_filter()$priority))
    
    `colnames<-`(table_df, c("Basin Name", "Area (sq. mi.)", "Population", "DWR Priority"))
    
  })
  
  
  ####################################################
  #Second Map!
  
  basin_select <- reactive({ 
   
    sgma_basins %>% 
      dplyr::filter(sub_basin_final == input$gw_basin)
    
    })
  
   max_score_filter <- reactive({
    
    raster_mask <- raster::mask(max_score_reproj, basin_select())
    
    })
  # 'mask' is not working, need to find a new method of clipping raster to selected basin 

   wells_filter <- reactive({
     wells_crop <- st_intersection(drywells, basin_select())
   })
   
   geo_filter <- reactive({
     geo_crop <- st_intersection(geotracker, basin_select())
   })
   
   nhd_filter <- reactive({
     nhd_crop <- st_intersection(nhd, basin_select())
   })
   
   gde_filter <- reactive({
     gde_crop <- st_intersection(gde_fix, basin_select())
   })
   
   pal <- colorNumeric("RdYlGn", reverse = TRUE, values(max_score_reproj), na.color = "transparent")
   
   max_score_map <- reactive({
     leaflet() %>%
       #Base layers
       addProviderTiles(providers$CartoDB.Positron, group = "basemap") %>%
       addPolygons(data = sgma_basins, color = "black", weight = 0.5, fillOpacity = 0) %>% 
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



# Put them together to make our app!

shinyApp(ui = ui, server = server)


