temp_data <- reactive({
  req(input$region_data, input$country_data)
  gpw %>%
    filter(Region %in% input$region_data)
})  

observe({
  req(input$country_data, input$region_data)
  now <- as.character(input$country_data)
  updateSelectizeInput(session,
                       "country_data", "Select Countries",
                       choices = c("All", unique(as.character(temp_data()$Country))),
                       selected = now)
})

dataTable <- reactive({
  req(input$country_data, input$indicators_data)
  if ("All" %in% input$country_data) {
    temp_data() %>%
      filter(Year >= input$year_data[1] & Year <= input$year_data[2]) %>%
      select(Country, Region, Year, input$indicators_data)
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