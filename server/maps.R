##########################
###   LEAFLET CODE     ###
###   STARTS HERE      ###
##########################
ym <- eventReactive(input$create_map, {
  as.character(input$year_map)
}, ignoreNULL = FALSE)

im <- eventReactive(input$create_map, {
  as.character(input$indicator_map)
}, ignoreNULL = FALSE)

rm <- eventReactive(input$create_map, {
  as.character(input$region_map)
}, ignoreNULL = FALSE)

mapping_data <- eventReactive(input$create_map, {
  req(input$indicator_map, input$year_map, input$region_map)
  ym <- ym()
  whs_map <- gpw_or %>% dplyr::select(Country, Year, ISO3, Region, im()) 
  if (ym == "Latest Available") {
    temp <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Country", "Year", "ISO3", "Region", "tempvar"))
    temp[1, ] <- c("Dummy", "2000", "DMY", "CHTI", "100")
    for (i in 1:length(unique(whs_map$ISO3))) {
      whs_temp <- whs_map %>% filter(ISO3 == unique(whs_map$ISO3)[i])
      a <- as.character(whs_temp[["Year"]])
      b <- whs_temp[[im()]]
      c <- map_latest(a, b)
      temp2 <- data.frame("Country" = whs_temp$Country[1], 
                          "ISO3" = whs_temp$ISO3[1],
                          "Region" = whs_temp$Region[1],
                          "Year" = c[1],
                          "tempvar" = c[2])
      temp <- rbind(temp, temp2)
    }
    whs_map <- temp[2:195,]
    names(whs_map)[ncol(whs_map)] <- im()
    whs_map[[im()]] <- as.numeric(whs_map[[im()]])
  } else {
    whs_map <- whs_map %>% 
      filter(Year == as.numeric(input$year_map))
  }
  whs_map$Year <- as.factor(whs_map$Year)
  whs_map
}, ignoreNULL = FALSE)

observeEvent(mapping_data(), {
  output$map_of_indicator <- renderLeaflet({
    whs_map <- mapping_data()
    df <- merge(map, 
                whs_map, 
                by.x = "ISO_3_CODE", 
                by.y = "ISO3",                    
                sort = FALSE,
                all = FALSE)
    if (sum(is.na(df[[im()]])) == nrow(df) | is.null(rm())) {
      leaflet() %>% 
        setView(30, 20, zoom = 3) %>%
        addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 3, maxZoom = 3)) %>%
        addControl("No Data Available", position = "bottomleft")
    } else {
      labels <- sprintf("<strong>%s</strong><br/>Value: %g<br/>Year: %g",
                        df$Country, df[[im()]], as.numeric(as.character(df$Year))) %>% lapply(htmltools::HTML)
      q <- as.integer(input$quantile_map)
      probs <- seq(0, 1, length.out = q + 1)
      bins <- quantile(df[[im()]], probs, na.rm = TRUE, names = FALSE)
      while (length(unique(bins)) != length(bins)) {
        q <- q - 1
        probs <- seq(0, 1, length.out = q + 1)
        bins <- quantile(df[[im()]], probs, na.rm = TRUE, names = FALSE)
      }
      pal <- colorBin(input$colors_map, bins = bins, na.color = "#F3F8F5")
      if (rm() != "All") {
        df[[im()]][df$Region != rm()] <- NA
      }
      mapDraw <- map_type(rm(), df)
      mapDraw %>%
        addPolygons(fillColor = ~pal(df[[im()]]),
                    weight = 0.2,
                    opacity = 1,
                    color = 'black',
                    dashArray = '1',
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 3,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend("bottomleft", pal = pal, values = ~df[[im()]], opacity = 1,
                  title = "Quantiles")
    }
  })
}, ignoreNULL = FALSE)

observeEvent(mapping_data(), {
  output$bar_map <- renderPlotly({
    df <- mapping_data()
    df$ISO3 <- factor(df$ISO3, levels = df$ISO3[order(df[[im()]])])
    ym <- ym()
    im <- im()
    index <- match(im, gpw_choices_full)
    indic <- as.character(gpw_colnames[index])
    g <- ggplot(df, aes_string(x = "ISO3", y = im(), fill = "Region")) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values=bar_colors(rm()), 
                        drop = FALSE) +
      labs(title = paste0(indic, " by country using ISO3 code (", ym(), ")")) +
      theme(plot.title = element_text(family = "Helvetica", size = 12),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 90, size = 5),
            panel.background = element_rect(fill = "transparent"),
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent"))
    ggplotly(g) %>% layout(legend = list(orientation = 'h', 
                                         bgcolor = "none",
                                         x = 0.3, y = -0.2))
  })
}, ignoreNULL = FALSE)

mapData <- eventReactive(input$create_map, {
  df <- mapping_data()
  rm <- rm()
  if (rm != "All") {
    df <- df %>% filter(Region == rm)
  }
  df
}, ignoreNULL = FALSE)

output$downloadMapData <- downloadHandler(
  filename = function() {
    paste("map", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(mapData(), file, row.names = FALSE)
  }
)

###########################
###   LEAFLET CODE      ###
###   ENDS HERE         ###
###########################