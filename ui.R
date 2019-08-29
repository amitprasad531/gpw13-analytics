shinyUI(
  navbarPage(theme=shinytheme("flatly"),
  "GPW 13 Analytics",
  tabPanel("Correlations",
           includeCSS("www/styles.css"),
           sidebarLayout(
             sidebarPanel(
               selectizeInput("xvar_corr", "Select X-axis Indicator",
                              choices = gpw_choices_full[!gpw_choices_full %in% c("transfats")],
                              selected = "uhc",
                              options = list(placeholder = "Select Indicator")),
               sliderInput("xyear_corr", "Select Year for X-axis Indicator",
                           min = 2000, max = 2018,
                           value = 2015, sep = ""),
               selectizeInput("yvar_corr", "Select Y-axis Indicator",
                              choices = gpw_choices_full[!gpw_choices_full %in% c("transfats")],
                              selected = "ihr",
                              options = list(placeholder = "Select Indicator")),
               sliderInput("yyear_corr", "Select Year for Y-axis Indicator",
                           min = 2000, max = 2018,
                           value = 2015, sep = ""),
               checkboxGroupInput("region_corr", "Select Regions",
                                  choices = levels(gpw$Region),
                                  selected = levels(gpw$Region),
                                  inline = FALSE), 
               selectizeInput("country_corr", "Select Countries",
                              choices = c("All", unique(gpw$Country)),
                              selected = "All",
                              multiple = TRUE,
                              options = list(placeholder = "Select Countries")),
               actionButton("create_corr", "Show Correlation"), br(), br(),
               HTML(paste(span("Tip: ", style="font-weight: bold;"), span("Use Show Correlation button only if you change regions or countries"))),
               width = 3
             ),
             mainPanel(
               column(12,
               wellPanel(plotlyOutput("correlations", height = "60vh")),
               useShinydashboard(),
               column(3,
                      box(title = "Pearson Coefficient", 
                          status = "info", solidHeader = TRUE,
                          width = 10, height = "12vh", textOutput("stat1_corr"))
               ),
               column(3, 
                      box(title = "Pearson P-value", status = "info", solidHeader = TRUE,
                          width = 10, height = "12vh", textOutput("stat2_corr"))
               ),
               column(3, 
                      box(title = "Spearman Coefficient", status = "info", solidHeader = TRUE,
                          width = 10, height = "12vh", textOutput("stat3_corr"))
               ),
               column(3, 
                      box(title = "Spearman P-value", status = "info", solidHeader = TRUE,
                          width = 10, height = "12vh", textOutput("stat4_corr"))
               )
               )
             )
           )),
  tabPanel("Trends",
           sidebarLayout(
             sidebarPanel(
               selectizeInput("indicator_trend", "Select Indicator",
                              choices = gpw_choices_full[!gpw_choices_full %in% c("transfats")],
                              selected = "ihr",
                              options = list(placeholder = "Select Indicator")),
               checkboxGroupInput("region_trend", "Select Regions",
                                  choices = levels(gpw$Region),
                                  selected = levels(gpw$Region),
                                  inline = FALSE), br(),
               sliderInput("years_trend", "Enter Year Range",
                           min = 2000, max = 2018,
                           value = c(2010, 2016),
                           sep = ""), 
               selectizeInput("country_trend", "Select Countries",
                              choices = unique(gpw$Country),
                              selected = sample(unique(gpw$Country), 10),
                              multiple = TRUE, 
                              options = list(placeholder = "Select Countries",
                                             closeAfterSelect = FALSE)),
               actionButton("create_trends", "Show Trends"),
               width=3
             ),
             mainPanel(
               column(12,  
                      wellPanel(plotlyOutput("trends", height = "60vh", width = "auto")),
                      useShinydashboard(),
                      column(3,
                             box(title = "Selected countries", 
                                 status = "info", solidHeader = TRUE,
                                 width = 10, height = "12vh", textOutput("stat1_trend"))
                      ),
                      column(3, 
                             box(title = "Countries with data", status = "info", solidHeader = TRUE,
                                 width = 10, height = "12vh", textOutput("stat2_trend"))
                      ),
                      column(3, 
                             box(title = "Minimum value", status = "info", solidHeader = TRUE,
                                 width = 10, height = "12vh", textOutput("stat3_trend"))
                      ),
                      column(3, 
                             box(title = "Maximum value", status = "info", solidHeader = TRUE,
                                 width = 10, height = "12vh", textOutput("stat4_trend"))
                      )
               )
             )
           )),
  tabPanel("Maps",
    sidebarLayout(
      sidebarPanel(
        selectInput("indicator_map", "Select Indicator",
                    choices = gpw_choices_full[!gpw_choices_full %in% c("transfats")],
                    selected = "u5mr"), 
        selectInput("year_map", "Select Year",
                    choices = c("Latest Available", rev(levels(as.factor(gpw$Year)))),
                    selected = "2016"), 
        selectInput("region_map", "Select Region for Map",
                    choices = c("All", levels(gpw$Region)),
                    selected = "All"),
        actionButton("create_map", "Create Map"), br(), br(),
        selectInput("colors_map", "Color Scheme for Map",
                    choices = rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                    selected = "YlGnBu"),
        sliderInput("quantile_map", "No. of Quantiles for Map",
                    min = 2, max = 6, value = 4, step = 1),
        downloadButton("downloadMapData", "Download Data"), br(), br(),
        HTML(paste(span("Tip: ", style="font-weight: bold;"), span("Create map first to download corresponding data"))), br(), br(),
        HTML(paste0(
          span("Map disclaimer: ", style="font-weight: bold;"), 
          "The boundaries and names shown and the designations used on this map do not imply the expression of any opinion
          whatsover on the part of the World Health Organization concerning the legal status of any country, territory, city
          or area or of its authorities, or concerning the delimitation of its frontiers or boundaries."
        )),
        width = 3
      ),
      mainPanel(
        fluidRow(
          column(12,
            leafletOutput("map_of_indicator", height = "68vh")
          )
        ), br(),
        fluidRow(
          column(12,
                 plotlyOutput("bar_map", height = "20vh"))
        )
      )
    )
  ),
  tabPanel("Database",
  sidebarLayout(
    sidebarPanel(
      HTML(paste(span("Data compiled from ", style="font-weight: bold;"), 
                 a(href="https://unstats.un.org/sdgs/indicators/database", "UNSD SDG Database"), 
                 " and ", 
                 a(href="https://www.who.int/gho/en/", "WHO Global Health Observatory"), " 2019")), br(), br(),
      checkboxGroupInput("region_data", "Select Regions",
                         choices = levels(gpw$Region),
                         selected = levels(gpw$Region)),
      pickerInput("country_data", "Select Countries (from dropdown list)",
                  choices = c(levels(gpw$Country)),
                  selected = sample(levels(gpw$Country), 15),
                  multiple = TRUE,
                  options = list(title = "Select Countries",
                                 size = 15,
                                 `actions-box` = TRUE)), br(),
      pickerInput("indicators_data", "Select Indicators (from dropdown list)",
                  choices = gpw_colnames,
                  multiple = TRUE,
                  selected = sample(gpw_colnames, 10),
                  options = list(title = "Select Indicators",
                                 `actions-box` = TRUE,
                                 size = 15)
      ), br(),
      sliderInput("year_data", "Select Year Range",
                  min = 2000, max = 2018,
                  value = c(2013, 2016),
                  sep = ""),
      prettySwitch("latest", "Latest Available",
                   status = "success", fill = TRUE), 
      HTML(paste(span("Note: ", style="font-weight: bold;"), 
                 "If Latest Available is selected then year range will be invalid")), br(), br(), 
      downloadButton("downloadData", "Download Data"),
      width = 3
    ),
    mainPanel(
      DT::dataTableOutput("database")
    )
  )
  ),
  tabPanel("Metadata",
    sidebarLayout(
      sidebarPanel(
        HTML(paste(span("Source: ", style="font-weight: bold;"), a(href="http://bit.ly/gpw13-metadata", "WHO GPW13 Programmatic Indicators Metadata 2019"))), br(), br(),
        selectInput("indicator_meta", "Select Indicator",
                    choices = gpw_choices_full,
                    selected = "ihr"),
        width = 3
      ),
      mainPanel(
        htmlOutput("indicatorMetadata"), br(),
        fluidRow(
          column(6, htmlOutput("indicator_name")),
          column(6, htmlOutput("indicator_definition"))
        ), hr(),
        fluidRow(
          column(6, htmlOutput("indicator_numerator")),
          column(6, htmlOutput("indicator_denominator"))
        ), hr(),
        fluidRow(
          column(6, htmlOutput("indicator_methods")),
          column(6, htmlOutput("indicator_limitations"))
        ), hr(),
        fluidRow(
          column(6, htmlOutput("indicator_data_source")),
          column(6, htmlOutput("indicator_data_avail"))
        )
      )
    )
  )
))
