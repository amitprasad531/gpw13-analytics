temp_data <- reactive({
  gpw %>%
    filter(Region %in% input$region_data)
})  

observe({
  updatePickerInput(session,
                    "country_data", "Select Countries (from dropdown list)",
                    choices = c(unique(as.character(temp_data()$Country))),
                    selected = input$country_data)
})

dataTable <- reactive({
  if (input$latest == TRUE) {
    if ((is.null(input$country_data)) | (is.null(input$indicators_data))) {
      temp_data() %>%
        select(Country, Region, Year)
    } else {
    df <- temp_data() %>% 
      select(Country, Year, Region, input$indicators_data) %>%
      filter(Country %in% input$country_data)
    temp <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Country", "Region", "Indicator", "Year", "Value"))
    temp[1, ] <- c("Dummy", "DMY", "Indicator DMY", "2000", "100")
    for (i in 1:length(unique(df$Country))) {
      df_temp <- df %>% filter(Country == unique(df$Country)[i])
      a <- as.character(unique(df_temp$Year))
      for (j in 1:length(unique(input$indicators_data))) {
        b <- df_temp[[input$indicators_data[j]]]
        c <- map_latest(a, b)
        temp2 <- data.frame("Country" = df_temp$Country[1], 
                            "Region" = df_temp$Region[1],
                            "Indicator" = input$indicators_data[j],
                            "Year" = c[1],
                            "Value" = c[2])
        temp <- rbind(temp, temp2)
      }
    }
    num_rows <- nrow(temp)
    df <- temp[2:num_rows,]
    }
  } else {
    temp_data() %>%
      filter(Country %in% input$country_data) %>%
      filter(Year >= input$year_data[1] & Year <= input$year_data[2]) %>%
      select(Country, Region, Year, input$indicators_data)
  }
})  

output$database <- DT::renderDataTable({
  datatable(
    dataTable(),
    options = list(pageLength = 20, lengthMenu = c(10, 20, 25, 50, 100))
  )
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste("gpw", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(dataTable(), file, row.names = FALSE)
  }
)