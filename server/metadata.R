
output$indicatorMetadata <- renderUI({
  req(input$indicator_meta)
  index <- match(input$indicator_meta, gpw_choices_full)
  indic <- as.character(gpw_colnames[index])
  h2(indic)
})

output$indicator_name <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Indicator"), "\n\n",
             tags$p(ind_label(input$indicator_meta), style = "font-size:125%;")))
})

output$indicator_definition <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Definition"), "\n\n",
             tags$p(ind_def(input$indicator_meta), style = "font-size:125%;")))
})

output$indicator_numerator <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Numerator"), "\n\n",
             tags$p(ind_num(input$indicator_meta), style = "font-size:125%;")))
})

output$indicator_denominator <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Denominator"), "\n\n",
             tags$p(ind_den(input$indicator_meta), style = "font-size:125%;")))
})

output$indicator_methods <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Methodology"), "\n\n",
             tags$p(ind_meth(input$indicator_meta), style = "font-size:125%;")))
})

output$indicator_limitations <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Limitations"), "\n\n",
             tags$p(ind_lim(input$indicator_meta), style = "font-size:125%;")))
})

output$indicator_data_source <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Data Sources"), "\n\n",
             tags$p(ind_source(input$indicator_meta), style = "font-size:125%;")))
})

output$indicator_data_avail <- renderUI({
  req(input$indicator_meta)
  HTML(paste(h3("Data Availability"), "\n\n",
             tags$p(ind_avail(input$indicator_meta), style = "font-size:125%;")))
})
