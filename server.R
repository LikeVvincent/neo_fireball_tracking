######################################################################################################
# Data Loading                                                                                       #
######################################################################################################

api_key <- read.table("api_key.txt", col.names = "key", stringsAsFactors = FALSE)

neo_query <- stri_paste("https://api.nasa.gov/neo/rest/v1/feed?start_date=", Sys.Date(), "&api_key=", api_key$key)
neo_data_raw <- fromJSON(neo_query, flatten = TRUE)

tmp0 = do.call(rbind, neo_data_raw$near_earth_objects)
tmp1 = do.call(rbind, tmp0$close_approach_data)

neo_data_raw <- cbind(tmp0, tmp1)
neo_data_raw$close_approach_data <- NULL; neo_data_raw$links.self <- NULL

fireball_query <- stri_paste("https://ssd-api.jpl.nasa.gov/fireball.api&api_key=", "&api_key=", api_key$key)
fireball_data_raw <- fromJSON(fireball_query, flatten = TRUE)

fireball_version <- fireball_data_raw$signature$version
fireball_fields <- fireball_data_raw$fields

fireball_data_raw = data.frame(fireball_data_raw$data, stringsAsFactors = FALSE)
colnames(fireball_data_raw) <- fireball_fields

######################################################################################################
# Data Transformation                                                                                #
######################################################################################################

# Near Earth Object ----------------------------------------------------------------------------------
neo_data_trans <- neo_data_raw

# Set neo_reference_id as rowname --------------------------------------------------------------------
rownames(neo_data_trans) <- neo_data_trans$neo_reference_id

# Convert is_potentially_hazardous_asteroid to a No/Yes factor ---------------------------------------
# Create display as nasa_jpl_url ---------------------------------------------------------------------
# Date conversion ------------------------------------------------------------------------------------
neo_data_trans <- mutate(neo_data_trans, is_potentially_hazardous_asteroid = as.character(is_potentially_hazardous_asteroid)) %>%
    mutate(is_potentially_hazardous_asteroid = if_else(is_potentially_hazardous_asteroid == "FALSE", "No", "Yes")) %>%
    mutate(is_potentially_hazardous_asteroid = as.factor(is_potentially_hazardous_asteroid),
           display_name = stri_paste("<a href='", nasa_jpl_url, "' target='_blank'>", name, "</a>"),
           close_approach_date = ymd(close_approach_date))

# Remove not needed columns --------------------------------------------------------------------------
for(i in c("neo_reference_id", 
           "estimated_diameter.kilometers.estimated_diameter_min",
           "estimated_diameter.kilometers.estimated_diameter_max",
           "estimated_diameter.miles.estimated_diameter_min",
           "estimated_diameter.miles.estimated_diameter_max",
           "epoch_date_close_approach",
           "relative_velocity.kilometers_per_second")) {
    neo_data_trans[, i] <- NULL
}

# Convert characters into numerics ------------------------------------------------------------------- 
for(i in c("relative_velocity.kilometers_per_hour",
           "relative_velocity.miles_per_hour",
           "miss_distance.astronomical",
           "miss_distance.lunar",
           "miss_distance.kilometers",
           "miss_distance.miles")) {
    neo_data_trans[, i] <- as.numeric(neo_data_trans[, i])
}

# Rename columns for easy/pretty display -------------------------------------------------------------
colnames(neo_data_trans) <- c("Name",
                              "NASA JPL Url",
                              "Absolute Magnitude (H)",
                              "Potentially Hazardous",
                              "Estimated Minimum Diameter (Meters)",
                              "Estimated Maximum Diameter (Meters)",
                              "Estimated Minimum Diameter (Feet)",
                              "Estimated Maximum Diameter (Feet)",
                              "Close Approach Date",
                              "Orbiting Body",
                              "Relative Velocity (Kilometers/Hour)",
                              "Relative Velocity (Miles/Hour)",
                              "Miss Distance (Astronomical)",
                              "Miss Distance (Lunar)",
                              "Miss Distance (Kilometers)",
                              "Miss Distance (Miles)",
                              "Display Name")

# Create title ---------------------------------------------------------------------------------------
min_neo_date <- as.character(min(neo_data_trans$`Close Approach Date`))
max_neo_date <- as.character(max(neo_data_trans$`Close Approach Date`))
neo_title <- stri_paste("<b>Near Earth Object close approaches from ", min_neo_date, " to ", 
                        max_neo_date, "</b>")

# Set color palatte for Potentially Hazardous --------------------------------------------------------
pha_pal <- c("green3", "red2")

# Fireball -------------------------------------------------------------------------------------------
fireball_data_trans <- fireball_data_raw

# Numeric conversion ---------------------------------------------------------------------------------
for(i in c("energy",
           "impact-e",
           "lat",
           "lon",
           "alt",
           "vel")) {
    fireball_data_trans[, i] <- as.numeric(fireball_data_trans[, i])
}

# Remove lon/lat NA's --------------------------------------------------------------------------------
fireball_data_trans <- subset(fireball_data_trans, !is.na(lon) & !is.na(lat))
fireball_data_trans[is.na(fireball_data_trans)] <- "-"

# Create lon/lat that can be plotted correctly -------------------------------------------------------
# Create geo_location column -------------------------------------------------------------------------
# Date conversion ------------------------------------------------------------------------------------
# Create id column -----------------------------------------------------------------------------------
fireball_data_trans <- mutate(fireball_data_trans, lat = if_else(`lat-dir` == "S", -lat, lat),
                              lng = if_else(`lon-dir` == "W", -lon, lon)) %>%
    mutate(geo_location = stri_paste("(", lat, ", ", lng, ")"),
           date = ymd_hms(date),
           id = 1:nrow(fireball_data_trans))

# Remove not needed columns --------------------------------------------------------------------------
for(i in c("lon", "lat-dir", "lon-dir")) {
    fireball_data_trans[, i] <- NULL
}

# Rename columns for easy/pretty display -------------------------------------------------------------
colnames(fireball_data_trans) <- c("date",                           # This column will be use as rowname
                                   "Energy (joules)",
                                   "Impact Energy (kt)",
                                   "lat",                            # This column is not displayed
                                   "Altitude (km)",
                                   "Velocity (km/s)",
                                   "lng",                            # This column is not displayed
                                   "Location (latitude, longitude)",
                                   "id")                             # This column is not displayed   

# Create title ---------------------------------------------------------------------------------------
min_fireball_date <- as.character(min(fireball_data_trans$date))
max_fireball_date <- as.character(max(fireball_data_trans$date))
fireball_title <- stri_paste("<b>Near Earth Object close approaches from ", min_fireball_date, " to ", 
                             max_fireball_date, "</b>")

# Set color palatte for fireballs --------------------------------------------------------------------
fireball_pal <- colorBin(c("#FFFF00", "#FF0000"), log(fireball_data_trans$`Impact Energy (kt)`), 10)

# Select last recorded fireball ----------------------------------------------------------------------
fireball_last <- fireball_data_trans[fireball_data_trans$date == max(fireball_data_trans$date), ]

# Set date as rowname --------------------------------------------------------------------------------
rownames(fireball_data_trans) <- fireball_data_trans$date; fireball_data_trans$date <- NULL

# Set initial lng and lat ----------------------------------------------------------------------------
lng <- 0
lat <- 0

######################################################################################################
# Shiny                                                                                              #
######################################################################################################

shinyServer(function(input, output, session) {
    observe({
        # Near Earth Object (NEO) --------------------------------------------------------------------
        # Subset the data based on selected input parameters -----------------------------------------
        select_columns <- stri_paste("1,2,17,9,10,4,3", input$diam_velo_unit, input$miss_dist_unit, sep = ",")
        neo_data_trans_subset <- subset(neo_data_trans, select = c(as.numeric(stri_split(select_columns, fixed = ",")[[1]])))
        
        output$neo_plot <- renderPlotly({
            # Detect selected row in the neo_table ---------------------------------------------------
            s <- input$neo_table_rows_selected
            
            # If row not selected hide annotations otherwise display ---------------------------------
            a <- if(is.null(s)) {
                list(x = 0,
                     y = 0,
                     text = "",
                     xref = "x",
                     yref = "y",
                     showarrow = FALSE,
                     arrowhead = 0,
                     arrowsize = 0,
                     ax = 20,
                     ay = -40)
            } else {
                list(x = neo_data_trans_subset[, 9][s],
                     y = neo_data_trans_subset[, 11][s],
                     text = neo_data_trans_subset[, 1][s],
                     xref = "x",
                     yref = "y",
                     showarrow = TRUE,
                     arrowhead = 0,
                     arrowsize = 0,
                     ax = 20,
                     ay = -40)
            }
            
            # Set x/y title based on selected input parameters --------------------------------------- 
            x <- list(title = names(neo_data_trans_subset)[9])
            y <- list(title = names(neo_data_trans_subset)[11])
            
            # Create the plot ------------------------------------------------------------------------
            plot_ly(data = neo_data_trans_subset,
                    x = neo_data_trans_subset[, 9], y = neo_data_trans_subset[, 11],
                    text = neo_data_trans_subset[, 1],
                    color = neo_data_trans_subset[, 6], colors = pha_pal,
                    size = neo_data_trans_subset[, 9], sizes = c(10, 250)) %>%
                add_markers() %>%
                add_annotations(text = "Potentially<br>Hazardous",
                                xref = "paper", yref = "paper",
                                x = 1.02, xanchor = "left",
                                y = 0.8, yanchor = "bottom",
                                legendtitle = TRUE, showarrow = FALSE) %>%
                layout(title = neo_title, annotations = a, xaxis = x, yaxis = y, 
                       legend = list(y = 0.8, yanchor = "top"))
        })
        
        # Create the NEO table -----------------------------------------------------------------------
        output$neo_table <- DT::renderDataTable(neo_data_trans_subset[, -c(1:2)], server = FALSE, filter = "top",
                                                options = list(dom = "tp", autoWidth = TRUE, 
                                                               order = list(list(2, "asc"), list(4, "desc"), list(1, "asc"))),
                                                autoHideNavigation = TRUE, selection = "single", escape = FALSE)
    })
    
    observe({
        acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius = 15,
                                                             fill = FALSE, color = "yellow", 
                                                             opacity = 0.5, weight = 2, 
                                                             stroke = TRUE, layerId = "selected")
        
        output$Map <- renderLeaflet({
            leaflet() %>% setView(lng, lat, 3) %>% 
                addProviderTiles("Esri.WorldImagery", options = tileOptions(noWrap = TRUE)) %>%
                addCircleMarkers(data = fireball_data_trans, radius = ~sqrt(`Impact Energy (kt)`) + 4, 
                                 fillColor = ~fireball_pal(log(`Impact Energy (kt)`)), 
                                 color = ~fireball_pal(log(`Impact Energy (kt)`)), 
                                 fillOpacity = 0.5, opacity = 0.5, weight = 1, stroke = TRUE,
                                 layerId = ~id) %>%
                addPulseMarkers(data = fireball_last,
                                icon = makePulseIcon(color = ~fireball_pal(log(`Impact Energy (kt)`)),
                                                     iconSize = ~sqrt(`Impact Energy (kt)`) + 14, 
                                                     animate = TRUE, heartbeat = 0.5),
                                layerId = ~id) %>%
                addLegend(pal = fireball_pal, values = log(fireball_data_trans$`Impact Energy (kt)`), 
                          title = "Approximate Total<br>Impact Energy [log(kt)]")
        })
        
        observeEvent(input$Map_marker_click, {
            p <- input$Map_marker_click
            lat <- p$lat
            lng <- p$lng
            id <- p$id
            
            proxy <- leafletProxy("Map")
            if(p$id == "selected") {
                proxy %>% removeMarker(layerId = "selected")
            } else {
                # Create selected marker -------------------------------------------------------------
                proxy %>% setView(lng = lng, lat = lat, input$Map_zoom) %>% acm_defaults(lng, lat)
                
                # Create the fireball table ----------------------------------------------------------
                fireball_data_trans_select <- fireball_data_trans[fireball_data_trans$id == id, ]
                
                output$fireball_table <- DT::renderDataTable(subset(fireball_data_trans_select, selec = -c(lat, lng, id)), server = FALSE, 
                                                             options = list(dom = "t", autoWidth = TRUE),
                                                             autoHideNavigation = TRUE, selection = "single", escape = FALSE)
            }
        })
    })
    
    # Datasets ---------------------------------------------------------------------------------------
    # Create downloadhandler being able to download data as csv --------------------------------------
    output$neo_data.csv <- downloadHandler(
        filename <- function() { "neo_data.csv" },
        content <- function(file) {
            write.csv(neo_data_raw, file, row.names = FALSE)
        }
    )
    
    output$fireball_data.csv <- downloadHandler(
        filename <- function() { "fireball_data.csv" },
        content <- function(file) {
            write.csv(fireball_data_raw, file, row.names = FALSE)
        }
    )
})
