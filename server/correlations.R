###
### Scatter Plot Start
###

temp_corr <- reactive({
  req(input$region_corr, input$country_corr)
  gpw_or %>%
    filter(Region %in% input$region_corr)
})  

observe({
  req(input$country_corr)
  updateSelectizeInput(session,
                       "country_corr", "Select Countries",
                       choices = c("All", unique(as.character(temp_corr()$Country))),
                       selected = input$country_corr)
})

cc <- eventReactive(input$create_corr, {
  as.character(input$country_corr)
}, ignoreNULL = FALSE)

rc <- eventReactive(input$create_corr, {
  as.character(input$region_corr)
}, ignoreNULL = FALSE)

corr_data <- reactive({
  req(input$xvar_corr, input$yvar_corr)
  c <- cc()
  r <- rc()
  trend_corr <- gpw_or %>% filter(Region %in% r)
  if (!"All" %in% c) {
    trend_corr <- trend_corr %>%
      filter(Country %in% c) 
  }
  xvar <- trend_corr %>% filter(Year == input$xyear_corr) %>% select(input$xvar_corr)
  yvar <- trend_corr %>% filter(Year == input$yyear_corr) %>% select(input$yvar_corr)
  trend_corr <- trend_corr %>%
    filter(Year == 2017) %>%
    select(Country, Region, ISO3)
  trend_corr <- cbind(trend_corr, xvar[[input$xvar_corr]], yvar[[input$yvar_corr]])
  names(trend_corr)[4:5] <- c("xvar", "yvar")
  trend_corr
})

observeEvent(corr_data(), {
  output$correlations <- renderPlotly({
    label_index_x <- match(input$xvar_corr, gpw_ind)
    label_index_y <- match(input$yvar_corr, gpw_ind)
    y_label <- str_wrap(ind_label(label_index_y), width = 80)
    if (all(is.na(corr_data()$xvar)) | all(is.na(corr_data()$yvar))) {
      df <- data.frame(year = c(2000:2018), y = c(1:19))
      x <- ggplot(df, aes(x = year, y = y)) +
        geom_abline(intercept = 0, slope = 1, linetype=3, col="black") +
        labs(x = ind_label(label_index_x), y = y_label,  title = "Data for at least one indicator is not available.") +
        theme(rect = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              text = element_text(family = "Arial"),
              axis.title = element_text(size = 11),
              axis.text = element_text(size = 10),
              plot.margin=unit(c(0,0,0,1.2),"cm"),
              title = element_text(size = 12))
      ggplotly(x)
    } else {
      med_x <- median(corr_data()$xvar, na.rm = TRUE)
      med_y <- median(corr_data()$yvar, na.rm = TRUE)
      min_x <- min(corr_data()$xvar, na.rm = TRUE)
      min_y <- min(corr_data()$yvar, na.rm = TRUE)
      g <- ggplot(corr_data(), aes(x = xvar, y = yvar, fill = Region, col=Region)) +
        geom_point(shape=21, alpha=0.8, stroke = 0, size = 4,
                   aes(text=sprintf("Country: %s<br>xVar: %s<br>yVar: %s", ISO3, xvar, yvar))) +
        labs(x = paste(ind_label(label_index_x), " (", input$xyear_corr, ")", sep=""), 
             y = paste(y_label, " (", input$yyear_corr, ")", sep="")) +
        geom_hline(yintercept = med_y, linetype = 3, size = 0.5) +
        geom_vline(xintercept = med_x, linetype = 3, size = 0.5) +
        annotate("text", x = med_x, y = min_y, label = "X-axis median", size=3) +
        annotate("text", x = min_x, y = med_y+0.02*med_y, label = "Y-axis median", size=3) +
        ggtitle("Correlations") +
        scale_fill_manual(values=c('AFRO' = '#fda09a', 'EMRO'='#cdbc64', 'EURO'='#36c14c', 
                                   'PAHO' = '#00bfc3', 'SEARO' = '#92baff', 'WPRO' = '#fa95eb')) +
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(color = "lightgrey"),
              text = element_text(family = "Arial"),
              axis.title = element_text(size = 11),
              plot.margin=unit(c(0,0,0,1.2),"cm"),
              axis.text = element_text(size = 10))
      ggplotly(g, tooltip="text") %>% 
        layout(showlegend = TRUE, legend = list(font = list(size = 10), bgcolor="transparent", bordercolor="transparent")) 
    }
  })
}, ignoreNULL = FALSE)

# Pearson coefficient
pearson <- reactive({
  if (sum(!is.na(corr_data()$xvar)) < 3 | sum(!is.na(corr_data()$yvar)) < 3) {
    corr <- NA_character_
  } else {
    corr <- cor.test(corr_data()$xvar, corr_data()$yvar, method="pearson")
  }
})

output$stat1_corr <- renderText({
  if (is.na(pearson()[1])) {
    "NA"
  } else {
    round(pearson()$estimate, 3)
  }
})
output$stat2_corr <- renderText({
  if (is.na(pearson()[1])) {
    "NA"
  } else {
    round(pearson()$p.value, 3)
  }
})

# Spearman coefficient
spearman <- reactive({
  if (sum(!is.na(corr_data()$xvar)) < 3 | sum(!is.na(corr_data()$yvar)) < 3) {
    corr <- NA_character_
  } else {
    corr <- cor.test(corr_data()$xvar, corr_data()$yvar, method="spearman")
  }
})

output$stat3_corr <- renderText({
  if (is.na(spearman()[1])) {
    "NA"
  } else {
    round(spearman()$estimate, 3)
  }
})
output$stat4_corr <- renderText({
  if (is.na(spearman()[1])) {
    "NA"
  } else {
    round(spearman()$p.value, 3)
  }
})


###
### Scatter Plot End
###