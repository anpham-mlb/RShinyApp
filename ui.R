#The necessary packages used to implement the project
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(rgdal)
library(dplyr)
library(htmltools)
library(DT)
#Install github packgage to edit color of dashboard theme
library(devtools)
#install_github("nik01010/dashboardthemes")
#gauge
library(flexdashboard)
#plotly
library(plotly)
#Create slider
library(quantmod)
#Theme for ggplot2
library(ggthemes)


#Create the shiny dashboard
dashboardPage(
  #Choose the them and design the interface of the dashboard
  skin = "blue",
  dashboardHeader(title = "FIT5147 Assignment 5",
                  dropdownMenu(type="message", 
                               messageItem(from = "Creator", message = "Tuan Anh Pham", icon = icon("poo"), time = "2-6-2019"),
                               messageItem(from = "Student ID", message = "29911508", icon = icon("barcode"), time = "22:00"),
                               messageItem(from = "Tutor", message = "Shirin Ghaffarian Maghool", icon = icon("skull"))
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(text = "Shirin is watching!", icon = icon("warning"), status = "warning")
                  ),
                  dropdownMenu(type = "tasks",
                               taskItem(value = 80, color = "aqua", "Data exploration and visualisation"),
                               taskItem(value = 45, color = "red", "Data science"))
                  ),
  
  #Create the dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm("searchText", "buttonSearch", "Search"),
      menuItem("The USA", tabName = "usa", icon = icon("map")),
      menuItem("Fact", tabName = 'status', icon = icon("eye")),
      menuItem("Explanation", tabname = "reason",
               icon = icon("lightbulb")), 
                menuSubItem("Unemployment", tabName = "unemployment"),
                menuSubItem("Race & Ethnicity", tabName = "re"),
                menuSubItem("Weapon", tabName = "weapon")
    )
  ),
  
  #Create the dashboard body
  dashboardBody(
    dashboardthemes::shinyDashboardThemes(
      theme = "grey_dark"
    ),

    tabItems(
      #Display the graph for the first tab
      tabItem(
        tabName = "usa",
        h2("About the United States"),
        h4("The U.S. is a country of more 50 states covering a vast swath of North America, 
           with Alaska in the northwest and Hawaii extending the nation’s presence 
           into the Pacific Ocean. Major Atlantic Coast cities are New York, 
           a global finance and culture center, and capital Washington, DC."),
        h4("With more than 300 million people in 2016, the population of the US 
           continues to grow today, driven by a high level of immigration. The 
           growth in the US population could be a reason why so many
           crimes happen in the US."),
        h4("California is the state having the biggest number of population 
           with nearly 4,000,000 while Wyoming has only about 600,000 people,
           the smallest number of population."),
        fluidRow(
          box(title = "USA Population Density Map",
              solidHeader = T, leafletOutput("mapusa"), width = 12, status = "primary")
        ),
        fluidRow(
          box(title = "Crime in the United States 2016",
              solidHeader = T, dataTableOutput("table"), width = 12, status = "primary")
        )
      ),
      
      #Display the graph for the second tab
      tabItem(
        tabName = 'status',
        fluidRow(
          infoBoxOutput("box1"),
          infoBoxOutput("box2"),
          infoBoxOutput("box3")
        ),
        fluidRow(
          box(title = span("Average Crime Rate", 
                           style = "color: white; font-size: 20px; font-family: arial; text-align: right"),  
              gaugeOutput("gauge"), width = 12, 
              solidHeader = T, background = "navy")
        ),
        fluidRow(
          box(title = span("Types of Crime",
                           style = "color: white; font-size: 20px; font-family: arial; text-align: right"),
              plotlyOutput("scatter_plot"), width = 12,
              solidHeader = T, background = "navy")
        ),
        fluidRow(
          box(title = span("Percentage of Crimes", 
                           style = "color: white; font-size: 20px; font-family: arial; text-align: right"),  
              plotlyOutput("line_plot"), width = 12,
              solidHeader = T, background = "navy")
        ),
        fluidRow(
          box(title = span("Number of Crimes", 
                           style = "color: white; font-size: 20px; font-family: arial; text-align: right"),  
              plotlyOutput("bar_plot"), width = 12, height = 1060,
              solidHeader = T, background = "navy")
        )
      ),

      #Display the graph for the third tab
      tabItem(
        tabName = 'unemployment',
        fluidRow((
          box(
            title = span("Unemployment & Crime Rate for 51 States in the US", 
                         style = "color: white; font-size: 20px; font-family: arial; text-align: right"),
            h4("For almost states in the US, the unemployment rate is proportional to the crime rate."),
            plotlyOutput("unemployment"), width = 12,
            solidHeader = T, background = "navy"
          )
        ))
      ),
      
      #Display the graph for the fourth tab
      tabItem(
        tabName = 're',
        tabBox(
          tabsetPanel(
            #Display the graph for the first panel of the fourth tab
            tabPanel(
              'Race',
              h1("For Races"),
              h4("Asian and Native Hawaiian or Other Pacific Islander are two groups of people which did not almost cause any crimes. While the people group of White and Black or African American have the high crime rate of almost offenses, even for very serious offenses like Sex offenses, Arson or Drug abuse violations."),
              fluidRow(
                box(plotOutput("raceplot", width = "450%", height = "600px"),
                    selectInput("var", "1. Select variable", choices = c("All Race",
                                                                         "White",
                                                                         "Black_or_African_American",
                                                                         "American_Indian_or_Alaska_Native",
                                                                         "Asian",
                                                                         "Native_Hawaiian_or_Other_Pacific_Islander")),
                    radioButtons("color", "2. Select color", choices = c("purple", 
                                                                         "red", 
                                                                         "navy")),
                    numericInput("number", "3. Select size", min = 1, max = 5, step = 1, value = 2),
                    selectInput("ltype", "4. Select type of line", choices = c("twodash",
                                                                                "longdash",
                                                                                "dotdash",
                                                                                "dotted",
                                                                                "dashed",
                                                                                "solid",
                                                                                "blank"))
                )
              )),
            #Display the graph for the second panel of the fourth tab
            tabPanel(
              'Ethnicity',
              h1("For Hispanic Groups"),
              h4("Like the White and Black or African American groups, 
              people who are not hispanic or latino also were arrested in 
              almost crimes. However, these groups commit less serious offenses 
              such as Suspicion, Drunkenness or Forgery."),
              fluidRow(
                box(plotOutput("ethplot", width = "450%", height = "600px"),
                    
                    selectInput("var2", "1. Select variable", choices = c("All Ethnic Groups",
                                                                         "Hispanic_or_Latino",
                                                                         "Not_Hispanic_or_Latino")),
                    radioButtons("color2", "2. Select color", choices = c("purple", 
                                                                         "red", 
                                                                         "navy")),
                    numericInput("number2", "3. Select size of line", min = 1, max = 5, step = 1, value = 2),
                    selectInput("ltype2", "4. Select type of line", choices = c("twodash",
                                                                               "longdash",
                                                                               "dotdash",
                                                                               "dotted",
                                                                               "dashed",
                                                                               "solid",
                                                                               "blank"))
                )
              )
            )
          )
        )
      ),
    
    #Display the graph for the fifth tab
      tabItem(
      tabName = 'weapon',
      fluidPage(
        box(title = span("People who died or killed by Guns in the US", 
                         style = "color: white; font-size: 20px; font-family: arial; text-align: right"), 
            h4("Guns only account a small proportion of total crimes 
                in the US. It seems that the restriction of guns may 
                not be the first priority of the United States’s government."),
            h6('Hint: Drag square on plot to see data'),
            width = 12, 
            solidHeader = T, background = "navy",
          plotOutput(outputId = "plot", brush = "brush_plot"),
          selectInput('choice', 'Smoother:',
                      c('lm',
                        'loess')
        )
        ),
        checkboxInput('smoother', 'Show smoother', FALSE),
        fluidRow(
          column(width = 5, tags$b(tags$i("Actual Dataset")), tableOutput("dataset")),
          column(width=5, tags$b(tags$i("Rows corresponding to datapoints under brush")),  tableOutput("brush_data"), offset = 2)
        )
      )
    )
      )
  )
)

