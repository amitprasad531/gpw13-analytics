temp_trend <- reactive({
  req(input$region_trend)
  gpw %>%
    filter(Region %in% input$region_trend)
})  

observe({
  updateSelectizeInput(session,
                       "country_trend", "Select Countries",
                       choices = c("All", unique(as.character(temp_trend()$Country))),
                       selected = input$country_trend)
})

it <- eventReactive(input$create_trends, {
  as.character(input$indicator_trend)
}, ignoreNULL = FALSE)

yt <- eventReactive(input$create_trends, {
  as.character(input$years_trend)
}, ignoreNULL = FALSE)

ct <- eventReactive(input$create_trends, {
  as.character(input$country_trend)
}, ignoreNULL = FALSE)

rt <- eventReactive(input$create_trends, {
  as.character(input$region_trend)
}, ignoreNULL = FALSE)

trend_interpol <- eventReactive(input$create_trends, {
  req(input$indicator_trend, input$country_trend, input$region_trend, input$years_trend)
  c <- ct()
  y <- yt()
  r <- rt()
  if ("All" %in% c) {
    trend_data <- gpw_or %>% filter(Region %in% r) %>%
                    filter(Year >= y[1] & Year <= y[2]) %>%
                    select(Country, Region, ISO3, Year, it()) %>%
                    arrange(Country, Year)
  } else {
    trend_data <- gpw_or %>% filter(Region %in% r) %>%
                    filter(Country %in% c) %>%
                    filter(Year >= y[1] & Year <= y[2]) %>%
                    select(Country, Region, ISO3, Year, it()) %>%
                    arrange(Country, Year)
  }
  df_new <- trend_data
  iso <- unique(df_new$ISO3)
  df_new <- df_new %>% arrange(ISO3, Year)
  df_new[, "interpol"] <- NA
  for (i in iso) {
    temp <- df_new %>% filter(ISO3 == i)
    y <- ncol(temp) - 1
    if (colSums(!is.na(temp[, y, drop = FALSE])) < 3) {
      df_new <- df_new %>%
                  filter(ISO3 != i)
    } else {
      temp$interpol <- interp1(temp[["Year"]], temp[[it()]], temp[["Year"]], "linear")
      df_new[df_new$ISO3 == i, "interpol"] <- temp$interpol
    }
  }
  df_new
}, ignoreNULL = FALSE)

observeEvent(trend_interpol(), {
  output$trends <- renderPlotly({
    label_index <- match(it(), gpw_ind)
    y_label <- str_wrap(ind_label(label_index), width = 80)
    c <- ct()
    if ((dim(trend_interpol())[1] == 0) | (is.null(c))) {
      df <- data.frame(Year = c(2000:2017), y = c(1:18))
      x <- ggplot(df, aes(x = Year, y = y)) +
        geom_abline(intercept = 0, slope = 1, linetype=3, col="black") +
        labs(x = "Year", y = y_label,  title = "No trend data available for this indicator in selected countries") +
        theme(rect = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              text = element_text(family = "Arial"),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 11),
              plot.margin=unit(c(0,0,0,1.2),"cm"),
              title = element_text(size = 12))
      ggplotly(x)
    } else {
      df <- trend_interpol()
      x.lineAxisSettings <- list(
        title = "",
        ticklen = 4,
        tickwidth = 2,
        tickformat = "d",
        zeroline = FALSE,
        showline = TRUE,
        showticklabels = TRUE,
        showgrid = FALSE,
        tickfont = txline,
        linewidth = 1,
        mirror = "ticks"
      )
      y.lineAxisSettings <- list(
        title = y_label,
        titlefont = tf,
        tickwidth = 2,
        showline = TRUE,
        showticklabels = TRUE,
        showgrid = TRUE,
        tickfont = tyline,
        linewidth = 1,
        mirror = "ticks"
      )
      plot_ly(df, x=~Year, y=~get(it()), color=~ISO3, type="scatter", mode="lines+markers", line = list(width = 5), colors = "Set3") %>%
        add_trace(y=~as.numeric(interpol), color=~ISO3, line = list(width = 3, dash = 'dot'), connectgaps=TRUE, showlegend = F) %>%
        layout(xaxis = x.lineAxisSettings,
               yaxis = y.lineAxisSettings,
               legend = legend.Settings,
               font = t,
               title = "Trends",
               margin = list(
                 r = 30, 
                 t = 40, 
                 b = 40, 
                 l = 80
               )) %>%
        layout(paper_bgcolor='rgb(249,246,244)')
    }
  })
}, ignoreNULL = FALSE)