#Import necessary packages used to implement the project
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
install_github("nik01010/dashboardthemes")
#gauge
library(flexdashboard)
#plotly
library(plotly)
#Create slider
library(quantmod)
#Theme for ggplot2
library(ggthemes)

#Read data used for the project
data <- read.csv('full_data.csv')
data <- data[,-c(1)]
die <- read.csv("gun.csv")
die <- die[,-c(1)]
weapon_data <- data
weapon_data <- weapon_data[,-c(2,3,5,6,7,8,9,10,11,12,13,14,15,16,17)]
re <- read.csv('re.csv')
re <- re[,-c(1)]
re1 <- read.csv("re1.csv")
re1 <- re1[,-c(1)]
re2 <- read.csv("re2.csv")
re2 <- re2[,-c(1)]

function(input, output, session) {

  #First tab
  #USA map
  #Read shapefile
  states <- readOGR("BoundarylineshapefileofUS1407/BoundarylineshapefileofUS14.shp")
  #Removes states that are not in dataset
  states <- states[-c(3,13,38,43,49),]
  #Create attributes for legend
  pal <- colorBin("RdYlBu", domain = data$Population)
  #Create label for each state
  labels <- paste("<p>", data$State, "</p>",
                  "<p>", "Population: ", data$Population, "</p>",
                  sep = "")
  #Create map
  output$mapusa <- renderLeaflet({
    leaflet(data = data) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      setView(lng = -95.7129, lat=37.0902, zoom = 4) %>%
      #add shapefile
      addPolygons(data = states,
                  color = "white",
                  weight = 1,
                  smoothFactor = 0.5,
                  fillOpacity = 0.7,
                  fillColor = pal(data$Population),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "666666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  ),
                  label =  lapply(labels, HTML)) 
  })
  #Table
  extracted_data <- data[,c(1,2,3,4,15)]
  output$table <- renderDataTable(extracted_data)
  
  #Second tab
  #Gauge
  output$gauge <- renderGauge(gauge(round(mean(data$Percentage_of_crimes), 
                                          digits = 3),
                                    min = 0,
                                    max = round(max(data$Percentage_of_crimes), digits = 3),
                                    gaugeSectors(success = c(0, 0.05),
                                                 warning = c(0.051, 0.09),
                                                 danger = c(0.09, 0.2),
                                                 colors = c('yellow', 'green', 'red'))
  ))
  
  #Scatter plot for different types of crime
  output$scatter_plot <- renderPlotly(
    plot_ly(data, x = ~data$Violent_crime, y = ~data$State, name = "Violent", type = 'scatter',
            mode = "markers", marker = list(color = "orange")) %>%
      add_trace(x = ~data$Murder_and_nonnegligent_manslaughter, y = ~data$State, name = "Murder and Nonneglient Manslaughter",type = 'scatter',
                mode = "markers", marker = list(color = "blue")) %>%
      add_trace(x = ~data$Rape_type_1, y = ~data$State, name = "Rape Type 1",type = 'scatter',
                mode = "markers", marker = list(color = "pink")) %>%
      add_trace(x = ~data$Rape_type_2, y = ~data$State, name = "Rape Type 2",type = 'scatter',
                mode = "markers", marker = list(color = "yellow")) %>%
      add_trace(x = ~data$Robbery, y = ~data$State, name = "Robbery",type = 'scatter',
                mode = "markers", marker = list(color = "purple")) %>%
      add_trace(x = ~data$Aggravated_assault, y = ~data$State, name = "Aggravated Assault",type = 'scatter',
                mode = "markers", marker = list(color = "green")) %>%
      add_trace(x = ~data$Property_crime, y = ~data$State, name = "Property Crime",type = 'scatter',
                mode = "markers", marker = list(color = "black")) %>%
      add_trace(x = ~data$Burglary, y = ~data$State, name = "Burglary",type = 'scatter',
                mode = "markers", marker = list(color = "pink")) %>%
      add_trace(x = ~data$Larceny_theft, y = ~data$State, name = "Lacerny Theft",type = 'scatter',
                mode = "markers", marker = list(color = "red")) %>%
      add_trace(x = ~data$Motor_vehicle_theft, y = ~data$State, name = "Motor Vehicle Theft",type = 'scatter',
                mode = "markers", marker = list(color = "navy")) %>%
      layout(
        xaxis = list(title = "Rate"),
        yaxis = list(title = "State"),
        margin = list(l = 100),
        legend = list(x=1.2, y=1.2)
      )
  )
  #Bar plot for number of crimes according to each state
  output$bar_plot <- renderPlotly(
    plot_ly(
      x = data$State,
      y = data$Number_of_crimes,
      type = "bar",
      color = I('orange')
    ) %>%
      layout(xaxis = list(title = "State", rangeslider = list(type = 'State')),
             yaxis = list(title = "Number of Crimes"),
             plot_bgcolor='grey',
             paper_bgcolor='grey',
             font = list(color = 'white'), height = 1000)
  )
  #Line plot for rate of crimes according to each state
  color = list(
    list(
      type = "buttons", x=-0.045, y = 1,
      buttons = list(
        list( args = list("line.color", "purple"), method = "restyle", label = "Purple"),
        list( args = list("line.color", "orange"), method = "restyle", label = "Orange"),
        list(args = list("line.color", "navy"), method = "restyle", label = "Navy"))))
  
  output$line_plot <- renderPlotly(
    plot_ly(data = data, x = ~data$State, y = ~data$Percentage_of_crimes, 
            type = 'scatter', mode = 'lines', 
            line = list(color = 'rgb(205, 12, 24)')) %>%
      layout(xaxis = list(title = "State"),
             yaxis = list(title = "Rate of Crimes"),
             plot_bgcolor='white',
             paper_bgcolor='white',
             font = list(color = 'black'),
             updatemenus=color)
  )
  #Info box
  output$box1 <- renderInfoBox({
    infoBox(
      "The Most Dangerous State", paste0("District of Columbia"), icon = icon("exclamation-triangle"),
      color = "red", fill = TRUE
    )
  })
  output$box2 <- renderInfoBox({
    infoBox(
      "The Safest State", paste0("New Hampshire"), icon = icon("smile"),
      color = "blue", fill = TRUE
    )
  })
  output$box3 <- renderInfoBox({
    infoBox(
      "Crime Status", paste0("Average"), icon = icon("battery-half"), 
      color = "green", fill = TRUE
    )
  })
  
  #Third tab
  #Subplot of line plot and bar plot for the unemployment
  output$unemployment <-renderPlotly(
    subplot(
      plot_ly(x = ~data$Percentage_of_crimes, y = data$State, name = 'Rate of Crimes',
              type = 'bar', orientation = 'h',
              marker = list(color = 'rgba(50, 171, 96, 0.6)',
                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
        layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
               xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE),
               width = 1400, height = 1000),
      plot_ly(x = ~data$Unemployment_rate, y = data$State, name = 'Rate of Unemployment',
              type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(128, 0, 128)')) %>%
        layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE, 
                            linecolor = 'rgba(102, 102, 102, 0.8)', linewidth = 2),
               xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,
                            side = 'top', dtick = 25000),
               width = 1400, height = 1000) %>%
        layout(
               legend = list(font = list(size = 10)),
               margin = list(l = 100, r = 20, t = 70, b = 70),
               paper_bgcolor = 'rgb(248, 248, 255)',
               plot_bgcolor = 'rgb(248, 248, 255)')
    )
  )
  
  #Fourth tab
  #First panel
  #Line plot for Race groups
  output$raceplot <- reactivePlot(function() {
    if (input$var == "All Race") {
        return(ggplot(data = re1,
                      aes(x = re1$Offense_charged, y = re1$Rate)) +
                 geom_line(group = 1, color = input$color, linetype = input$ltype, size = input$number) +
                 geom_point() +
                 theme_wsj() + scale_colour_wsj("colors6") +
                 theme(axis.text.x = element_text(angle = 90)) +
                 ggtitle('All Races') +
                 theme(legend.position="bottom") +
                 labs(x='Offense charged', y = 'Rate') +
                 facet_wrap(~re1$Race, nrow = 3))
      }
    else {
      return(ggplot(data = re, 
                      aes(x = re$Offense_charged, y = re[,input$var])) + 
                 geom_line(group = 1, color = input$color, linetype = input$ltype, size = input$number) + 
                 geom_point() +
                 theme_wsj() + scale_colour_wsj("colors6") +
                 theme(axis.text.x = element_text(angle = 90)) +
                 ggtitle("Each Race") + 
                 theme(legend.position="bottom") +
                 labs(x='Offense charged', y = 'Rate'))
    }
  })
  #Second panel 
  #Line plot for ethnicity
  output$ethplot <- reactivePlot(function() {
    if (input$var2 == "All Ethnic Groups") {
      print(ggplot(data = re2,
                   aes(x = re2$Offense_charged, y = re2$Rate)) +
              geom_line(group = 1, color = input$color2, linetype = input$ltype2, size = input$number2) +
              geom_point() +
              theme_wsj() + scale_colour_wsj("colors6") +
              theme(axis.text.x = element_text(angle = 90)) +
              ggtitle('All Races') +
              theme(legend.position="bottom") +
              labs(x='Offense charged', y = 'Rate') +
              facet_wrap(~re2$Ethnicity, nrow = 1))
    }
    else {
      print(ggplot(data = re, 
                   aes(x = re$Offense_charged, y = re[,input$var2])) + 
              geom_line(group = 1, color = input$color2, linetype = input$ltype2, size = input$number2) + 
              geom_point() +
              theme_wsj() + scale_colour_wsj("colors6") +
              theme(axis.text.x = element_text(angle = 90)) +
              ggtitle("Each Ethnic Group") + 
              theme(legend.position="bottom") +
              labs(x='Ethnicity', y = 'Rate'))
    }
  })
  
  #Fifth tab
  #Scatter plot for people killed or injured by guns
  output$plot <- reactivePlot(function(){
    if (input$smoother){
      return(
        ggplot(data = weapon_data, aes(x = Injured_weapon, y = Killed_weapon)) +
          geom_point(color = 'firebrick', size = 4) +
          labs(x='Injured People', y='Killed People') +
          theme_economist() + scale_color_economist() +
          theme(
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15)
          ) + geom_smooth(method = input$choice, color = 'black', size = 1.5, formula = y ~ poly(x, 2), se = FALSE)
      )
    }
    else{
      ggplot(data = weapon_data, aes(x = Injured_weapon, y = Killed_weapon)) +
        geom_point(color = 'firebrick', size = 4) +
        labs(x='Injured People', y='Killed People') +
        theme_economist() + scale_color_economist() +
        theme(
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)
        )
    }
  })
  #Full dataset
  output$dataset <- renderTable({
    weapon_data
  })
  #Brush data
  output$brush_data <-  renderTable({
    n = nrow(brushedPoints(weapon_data, brush = input$brush_plot))
    if(n==0)  
      return()
    else
      brushedPoints(weapon_data, brush = input$brush_plot) 
  })
  
}