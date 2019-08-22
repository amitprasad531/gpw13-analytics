shinyUI(
  navbarPage(theme=shinytheme("sandstone"),
  "GPW 13 Analytics",
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
                      wellPanel(plotlyOutput("trends", height = "60vh", width = "auto"))
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
      selectizeInput("country_data", "Select Countries",
                     choices = c("All", levels(gpw$Country)),
                     selected = "All",
                     multiple = TRUE,
                     options = list(placeholder = "Select Countries")),
      sliderInput("year_data", "Select Year Range",
                  min = 2000, max = 2018,
                  value = c(2013, 2016),
                  sep = ""),
      selectizeInput("indicators_data", "Select Indicators",
                     choices = gpw_colnames,
                     selected = sample(gpw_colnames, 10),
                     multiple = TRUE,
                     options = list(placeholder = "Select Indicators")), 
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
