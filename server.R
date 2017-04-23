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
neo_data_trans$is_potentially_hazardous_asteroid <- as.character(neo_data_trans$is_potentially_hazardous_asteroid)
neo_data_trans$is_potentially_hazardous_asteroid[neo_data_trans$is_potentially_hazardous_asteroid == "FALSE"] <- "No"
neo_data_trans$is_potentially_hazardous_asteroid[neo_data_trans$is_potentially_hazardous_asteroid == "TRUE"] <- "Yes"
neo_data_trans$is_potentially_hazardous_asteroid <- as.factor(neo_data_trans$is_potentially_hazardous_asteroid)

# Create display as nasa_jpl_url --------------------------------------------------------------------- 
neo_data_trans$display_name <- stri_paste("<a href='", neo_data_trans$nasa_jpl_url, "' target='_blank'>",
                                          neo_data_trans$name, "</a>")

# Date conversion ------------------------------------------------------------------------------------
neo_data_trans$close_approach_date <- ymd(neo_data_trans$close_approach_date)

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
for(i in 11:16) {
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
min_neo_date <- as.character(min(neo_data_trans[, 9]))
max_neo_date <- as.character(max(neo_data_trans[, 9]))
neo_title <- stri_paste("<b>Near Earth Object close approaches from ", min_neo_date, " to ", 
                        max_neo_date, "</b>")

# Set color palatte for Potentially Hazardous --------------------------------------------------------
pha_pal <- c("green3", "red2")

# Fireball -------------------------------------------------------------------------------------------
fireball_data_trans <- fireball_data_raw

# Date and numeric conversion ------------------------------------------------------------------------
fireball_data_trans$date <- ymd_hms(fireball_data_trans$date)

for(i in c(2, 3, 4, 6, 8, 9)) {
    fireball_data_trans[, i] <- as.numeric(fireball_data_trans[, i])
}

# Remove lon/lat NA's --------------------------------------------------------------------------------
fireball_data_trans <- subset(fireball_data_trans, !is.na(lon) & !is.na(lat))

# Create lon/lat that can be plotted correctly -------------------------------------------------------
fireball_data_trans$lat[fireball_data_trans$`lat-dir`== "S"] <- -fireball_data_trans$lat[fireball_data_trans$`lat-dir`== "S"]
fireball_data_trans$lon[fireball_data_trans$`lon-dir`== "W"] <- -fireball_data_trans$lon[fireball_data_trans$`lon-dir`== "W"]

# New latitude name and id column --------------------------------------------------------------------
colnames(fireball_data_trans)[6] <- "lng"
fireball_data_trans$id <- 1:nrow(fireball_data_trans)

# Create popup ---------------------------------------------------------------------------------------
fireball_data_trans[is.na(fireball_data_trans)] <- "-"

fireball_data_trans$GeoLocation <- stri_paste("(", fireball_data_trans$lat, ", ", fireball_data_trans$lng, ")")

fireball_data_trans$popup <- stri_paste("<table id='popup'>
<tr><th>Attribute</th><th>Value</th></tr>",
                                        "<tr><td>Geo Location</td><td>", fireball_data_trans$GeoLocation, "</td></tr>",
                                        "<tr><td>Date</td><td>", as.character(fireball_data_trans$date), "</td></tr>",
                                        "<tr><td>Energy</td><td>", fireball_data_trans$energy, "</td></tr>",
                                        "<tr><td>Impact Energy</td><td>", fireball_data_trans$`impact-e`, "</td></tr>",
                                        "<tr><td>Altitude</td><td>", fireball_data_trans$alt, "</td></tr>",
                                        "<tr><td>Velocity</td><td>", fireball_data_trans$vel, "</td></tr>",
                                        "</table>")

# Create title ---------------------------------------------------------------------------------------
min_fireball_date <- as.character(min(fireball_data_trans$date))
max_fireball_date <- as.character(max(fireball_data_trans$date))
fireball_title <- stri_paste("<b>Near Earth Object close approaches from ", min_fireball_date, " to ", 
                             max_fireball_date, "</b>")

# Set color palatte for fireballs --------------------------------------------------------------------
fireball_pal <- colorBin(c("#0000FF", "#FFFF00", "#FF0000"), log(fireball_data_trans$`impact-e`), 10)

# Select last recorded fireball ----------------------------------------------------------------------
fireball_last <- fireball_data_trans[fireball_data_trans$date == max(fireball_data_trans$date), ]

# Set initial lng and lat ----------------------------------------------------------------------------
lng <<- 0
lat <<- 0

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
            if(is.null(s)) {
                a <- list(
                    x = 0,
                    y = 0,
                    text = "",
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE,
                    arrowhead = 0,
                    arrowsize = 0,
                    ax = 20,
                    ay = -40
                )
            } else {
                a <- list(
                    x = neo_data_trans_subset[, 9][s],
                    y = neo_data_trans_subset[, 11][s],
                    text = neo_data_trans_subset[, 1][s],
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    arrowhead = 0,
                    arrowsize = 0,
                    ax = 20,
                    ay = -40
                )
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
                                                             fill = FALSE, color = "black", 
                                                             opacity = 0.5, weight = 2, 
                                                             stroke = TRUE, layerId = "selected")
        
        output$Map <- renderLeaflet({
            leaflet() %>% setView(lng, lat, 3) %>% addTiles(options = tileOptions(noWrap = TRUE)) %>%
                addCircleMarkers(data = fireball_data_trans, radius = ~sqrt(`impact-e`) + 3, 
                                 fillColor = ~fireball_pal(log(`impact-e`)), color = ~fireball_pal(log(`impact-e`)), 
                                 fillOpacity = 0.5, opacity = 0.5, weight = 1, stroke = TRUE,
                                 group = "fireball", layerId = ~id, popup = ~popup) %>%
                addCircleMarkers(data = fireball_last, radius = 20,
                                 fill = FALSE, color = "red", 
                                 opacity = 0.5, weight = 2, 
                                 stroke = TRUE, layerId = "last") %>%
                addLegend(pal = fireball_pal, values = log(fireball_data_trans$`impact-e`), title = "Approximate Total<br>Impact Energy [log(kt)]")
        })
        
        observeEvent(input$Map_marker_click, {
            p <- input$Map_marker_click
            lat <<- p$lat
            lng <<- p$lng
            
            proxy <- leafletProxy("Map")
            if(p$id == "selected") {
                proxy %>% removeMarker(layerId = "selected")
            } else {
                proxy %>% setView(lng = lng, lat = lat, input$Map_zoom) %>% acm_defaults(lng, lat)
            }
        })
        
        observeEvent(input$sat_button, {
            sat_query <- stri_paste("https://api.nasa.gov/planetary/earth/imagery?lon=", lng, "&lat=", lat, "&api_key=", api_key$key)
            sat_data_raw <<- fromJSON(sat_query, flatten = TRUE)
            
            try(browseURL(sat_data_raw$url), silent = TRUE)
            if(!is.null(sat_data_raw$error)) {
                output$sat_error <- renderUI("No satellite image found")
            } else {
                output$sat_error <- renderUI(stri_paste("Satellite image from", sat_data_raw$date, "found", sep = " "))
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
