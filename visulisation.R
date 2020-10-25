library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(shinydashboard)
library(lubridate)
library(tidyr)
library(reshape2)

# Load data
df <- read.csv('fatal_analysis.csv')

df <- df %>%mutate(popup_info = paste('Accident time:', ACCIDENT_TIME, '</br>',
                                      'Accident type:', ACCIDENT_TYPE, '</br>',
                                      'Day of week:', DAY_OF_WEEK, '</br>',
                                      'Lighting:', LIGHT_CONDITION, '</br>',
                                      'Surface:', Surface.Cond.Desc, '</br>',
                                      'Atmosphere:', Atmosph.Cond.Desc, '</br>',
                                      'Speed zone:', SPEED_ZONE,'</br>',
                                      'Total person', TOTAL_PERSONS, '</br>',
                                      'Alcohol:', ALCOHOL_RELATED, '</br>',
                                      'Male driver average age:', avg_age_male,'</br>',
                                      'Female driver average age:', avg_age_female, '</br>'
))
# add day of week
df$dow  <- wday(df$ACCIDENT_DATE)


# drivers
driver <- read.csv('drivers.csv')

# traffic light
tl <- read.csv('crash_with_trafficlight.csv')

# UI
ui <- dashboardPage(
  
  # title
  dashboardHeader(title = "Victoria Crash Data Analysis"),
  
  # side bar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Environmental factors", tabName = "Environmental", icon = icon("cloud")),
      menuItem("Temporal factors", tabName = "Temporal", icon = icon("clock")),
      menuItem("Driver demographics", tabName = "Driver_demographics", icon = icon("venus-mars")),
      menuItem("Alcohol",  tabName = 'Al', icon = icon("wine-glass")),
      menuItem("Traffic light",  tabName = 'traffic_light', icon = icon("traffic-light"))
    )
  ),
  
  # dashboard body
  dashboardBody(
    
    tags$head(tags$style(HTML('
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                              }

                                '))),

    tabItems(
      
      
      # environmental factors page
      tabItem(tabName = 'Environmental',
              
              
              fluidRow(
                valueBoxOutput("total", width = 4),
                valueBoxOutput("fatal_percent", width = 4),
                valueBoxOutput("fatal_number", width = 4)
              ),
              
              column(12,sidebarLayout(
                
                position = "right",
                sidebarPanel(h4("Environmental factors"), width = 3, style = "overflow-y:scroll; max-height: 700px; position:relative;",
                             selectInput('LightCondition','Select light conditon', choices = c('All','Dark No street lights','Dark Street lights on','Dark Street lights unknown','Day','Dusk/Dawn','Unk.'), selected = 'Day'),
                             plotlyOutput('plt1'),
                             selectInput('AtmosphericCondition','Select atmospheric condition', choices = c('All', 'Clear', 'Fog', 'Raining', 'Smoke', 'Strong winds', 'Not Known' ), selected = 'Clear'),
                             plotlyOutput('plt2'),
                             selectInput('SurfaceCondition','Select surface condition', choices = c('All', 'Dry', 'Icy', 'Muddy', 'Snowy', 'Wet', 'unknown'), selected = 'Dry'),
                             plotlyOutput('plt3')
                ),
                
                mainPanel(
                  # map
                  leafletOutput("wsmap", height = '600px'), 
                  
                  # description
                  tags$h4( 
                    HTML(paste("<strong><h3>Highlights</h3> </br>
                                - The most common conditions of lighting, atmoephere and road surface are 'Day', 'Clear' and 'Dry' respectively. </br>
                                - Each of this conditions are significantly more than the other classes and there are 5-6 classes in each factor.</br>
                                - Location of accident does not have any relationship with fatal accident, since the location of fatal accident distributed randomly over Victortria.</br>
                                - The combination of good lighting, clear sky and dry road surface accounts for 47.89% of fatal accident, which is highly significant.</br>
                               </strong>"))
                  ),
                  width = 9)))
      ),
      
      # temporal factors
      tabItem(tabName = 'Temporal',
              
              
              fluidRow(
                valueBoxOutput("total2", width = 4),
                valueBoxOutput("fatal_percent2", width = 4),
                valueBoxOutput("fatal_number2", width = 4)
              ),
              
              column(12,sidebarLayout(
                
                position = "right",
                sidebarPanel(h4("Temporal factors"), width = 3, style = "overflow-y:scroll; height: 300px; position:relative;",
                             selectInput('Year','Select year', choices = c('All','2014','2015','2016','2017','2018'))
                ),
                
                mainPanel(
                  # plot
                  plotlyOutput('calendar'),
                  
                  # description
                  tags$h4( 
                    HTML(paste("<strong></br><h3>Highlights</h3></br>
                                              - It is obvious that there are significantly more fatal accidents in the afternoon than in any other 
                                                period although the traffic in the afternoon is expected to be similar to the morning traffic.</br> 
                                              - The percentage of fatal accidents ocurred in the afternoon was consistenly over 35% through out the the 5 years. </br>
                                              - The time and day that happens to have the most fatal accident is Sunday 4pm with 20 fatal accidents in the five year period.</br>
                               </strong>"))
                  ),
                  width = 9)))
      ),
      
      # temporal factors
      tabItem(tabName = 'Driver_demographics',
              
              
              fluidRow(
                valueBoxOutput("male", width = 6),
                valueBoxOutput("female", width = 6)
              ),
              
              column(12,sidebarLayout(
                
                position = "right",
                sidebarPanel(h4("Year"), width = 3, style = "overflow-y:scroll; height: 300px; position:relative;",
                             selectInput('Yr_driver','Select year', choices = c('All','2014','2015','2016','2017','2018'))
                ),
                
                mainPanel(
                  # plot
                  plotlyOutput('driver'),
                  
                  # description
                  tags$h4( 
                    HTML(paste("<strong></br><h3>Highlights</h3></br>
                                              - Between the period 2014-2018, 78.95% of the fatal accidents involved male drivers, which is almost quadurable to that of
                                              female drivers. </br>
                                              - This number was consistently over 75% over the 5 year period. </br>
                                              - Young male drivers aged 20-30 are the most dangerous group. </br>
                                              - The behaviour of female drivers are drastically different, the fatal accident numbers are fairly consistent across all age group.</br>
                               </strong>"))
                                              
                  ),
                  width = 9)))
      ),
      
      # Alcohol
      tabItem(tabName = 'Al',
              
              
              fluidRow(
                valueBoxOutput("yes", width = 6),
                valueBoxOutput("no", width = 6)
              ),
              
              column(12,sidebarLayout(
                
                position = "right",
                sidebarPanel(h4("Year"), width = 3, style = "overflow-y:scroll; height: 300px; position:relative;",
                             selectInput('Yr_alcohol','Select year', choices = c('All','2014','2015','2016','2017','2018'))
                ),
                
                mainPanel(
                  # plot
                  plotlyOutput('alcohol'),
                  
                  # description
                  tags$h4( 
                    HTML(paste("<strong></br><h3>Highlights</h3></br>
                                - The number of serious accidents that are not alcohol related was almost 4 times the number of serious accidents that involved alcohol. 
                                  This is contrary to the norms</br>
                                - For most of the year, the number of serious accidents that was alcohol related was close to 0, with the exception in 2014, in which there
                                  were 9 serious accidents.</br></br></br>
                               </strong>
                                * Serious accident is defined as the number of fatal and injury is greater than 5.
                               "))
                    
                  ),
                  width = 9)))
      ),
      
      # Traffic light
      tabItem(tabName = 'traffic_light',
              
              
              fluidRow(
                valueBoxOutput("in_", width = 4),
                valueBoxOutput("out", width = 4),
                valueBoxOutput("in_percent", width = 4)
              ),
              
              column(12,sidebarLayout(
                
                position = "right",
                sidebarPanel(h4("Year"), width = 3, style = "overflow-y:scroll; height: 300px; position:relative;",
                             selectInput('Yr_traffic','Select year', choices = c('All','2014','2015','2016','2017','2018'))
                ),
                
                mainPanel(
                  # plot
                  plotlyOutput('traffic_light'),
                  # description
                  tags$h4( 
                    HTML(paste("<strong></br></br></br></br></br></br></br></br></br></br></br><h3>Highlights</h3>
                                  - 40.81% of the traffic accidents occured under traffic light coverage. </br>
                                  - Although this number is quite high, we cannot make a conclusion in this case, because the behaviour of all road users
                                    contributes to a traffic accident, there are illegal crossings of pedestrians, drivers using phone during driving, 
                                    fatigue driving, drug driving, not using proper signals when switching lanes, all of these are causes of road accidents 
                                    but they cannot be prevented by traffic lights.</br>
                                  - More data is required to answer this question.
                              </strong>
                               "))
                    
                  ),
                  width = 9)))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # filtered data
  observe({
    # OOO
    filtered_data <- if (input$LightCondition == 'All'& input$AtmosphericCondition == 'All'& input$SurfaceCondition == 'All') 
    {df} 
    # OOX
    else if (input$LightCondition == 'All' & input$AtmosphericCondition == 'All' & input$SurfaceCondition != 'All')
    {df %>% filter (Surface.Cond.Desc == input$SurfaceCondition)}
    # OXO
    else if (input$LightCondition == 'All' & input$AtmosphericCondition != 'All' & input$SurfaceCondition == 'All')
    {df %>% filter (Atmosph.Cond.Desc == input$AtmosphericCondition)}
    # XOO
    else if (input$LightCondition != 'All' & input$AtmosphericCondition == 'All' & input$SurfaceCondition == 'All')
    {df %>% filter (LIGHT_CONDITION == input$LightCondition)}
    # OXX
    else if (input$LightCondition == 'All' & input$AtmosphericCondition != 'All' & input$SurfaceCondition != 'All')
    {df %>% filter (Atmosph.Cond.Desc==input$AtmosphericCondition & Surface.Cond.Desc == input$SurfaceCondition)}
    # XOX
    else if (input$LightCondition != 'All' & input$AtmosphericCondition == 'All' & input$SurfaceCondition != 'All')
    {df %>% filter (LIGHT_CONDITION==input$LightCondition & Surface.Cond.Desc == input$SurfaceCondition)}
    # XXO
    else if (input$LightCondition != 'All' & input$AtmosphericCondition != 'All' & input$SurfaceCondition == 'All')
    {df %>% filter (LIGHT_CONDITION==input$LightCondition & Atmosph.Cond.Desc == input$AtmosphericCondition)}
    # XXX
    else 
    {df %>% filter (LIGHT_CONDITION==input$LightCondition & Atmosph.Cond.Desc == input$AtmosphericCondition & Surface.Cond.Desc == input$SurfaceCondition)}
    
    # update filter
    # Lighting
    if (input$LightCondition == 'All'){
      updateSelectInput(session, 'LightCondition', 'select light conditon', choices = c('All',unique(filtered_data %>% select(LIGHT_CONDITION))))
    }
    # Atmospheric condition
    if (input$AtmosphericCondition == 'All'){
      updateSelectInput(session, 'AtmosphericCondition', 'select atmospheric conditon', choices = c('All',unique(filtered_data %>% select(Atmosph.Cond.Desc))))
    }
    # Surfacae condition
    if (input$SurfaceCondition == 'All'){
      updateSelectInput(session, 'SurfaceCondition', 'select surface conditon', choices = c('All',unique(filtered_data %>% select(Surface.Cond.Desc))))
    }
    
    # temporal df
    temporal <- if (input$Year == 'All') 
    {df} 
    else 
    {df %>% filter (year == input$Year)}
    
    # aggregate accident number by day of week and time
    counts  <- aggregate(ACCIDENT_NO~ACCIDENT_TIME+DAY_OF_WEEK+dow,temporal,length)
    counts <- counts[,-3]
    # transform into matrix
    counts <- dcast(counts, ACCIDENT_TIME ~ DAY_OF_WEEK)
    counts <- counts[,-1]
    # rearrange week day
    counts <- counts[,c(2,6,7,5,1,3,4)]
    # set accident time as row index
    rownames(counts)<- counts$ACCIDENT_TIME
    counts <- t(as.matrix(counts))
    counts[is.na(counts)] <- 0
    
    # map
    output$wsmap <- renderLeaflet({
      leaflet(filtered_data) %>% 
        addTiles()  %>% 
        addCircles(data = filtered_data, lat = ~Y, lng = ~X, popup = ~popup_info, color = 'red')
    })
    
    # lighting bar chart
    output$plt1 <- renderPlotly(({
      lighting <- filtered_data %>%
        group_by(LIGHT_CONDITION) %>%
        summarise(count=n())
      
      plot_ly(data =lighting, x = ~ LIGHT_CONDITION, y = ~count, type = 'bar', opacity = 0.6) %>% 
        layout(height = 300, 
               paper_bgcolor='F5F5F5',
               plot_bgcolor='F5F5F5', 
               title = 'Lighting condition', 
               xaxis = list(title = 'Lighting condition'),
               yaxis = list(title = 'Accident acount'))
    }))
    
    # atmospheric condition bar chart
    output$plt2 <- renderPlotly(({
      atmos <- filtered_data %>%
        group_by(Atmosph.Cond.Desc) %>%
        summarise(count=n())
      
      plot_ly(data = atmos, x = ~ Atmosph.Cond.Desc, y = ~count, type = 'bar', opacity = 0.6) %>% 
        layout(height = 300, 
               paper_bgcolor='F5F5F5',
               plot_bgcolor='F5F5F5', 
               title = 'Atmospheric condition', 
               xaxis = list(title = 'Atmospheric condition'),
               yaxis = list(title = 'Accident acount'))
    })) 
    
    # surface condition bar chart
    output$plt3 <- renderPlotly(({
      surface <- filtered_data %>%
        group_by(Surface.Cond.Desc) %>%
        summarise(count=n())
      
      plot_ly(data = surface, x = ~ Surface.Cond.Desc, y = ~count, type = 'bar', opacity = 0.6) %>% 
        layout(height = 300, 
               paper_bgcolor='F5F5F5',
               plot_bgcolor='F5F5F5', 
               title = 'Surface condition', 
               xaxis = list(title = 'Surface condition'),
               yaxis = list(title = 'Accident acount'))
    }))
    
    # total vbox
    output$total <- renderValueBox({
      valueBox(
        nrow(df),
        HTML(paste("The total number of fatal accident between the period 2014-2018")),
        color = 'purple',
        icon = icon("user-friends")
      )
    })
    
    # fatal number vbox
    output$fatal_number <- renderValueBox({
      valueBox(
        nrow(filtered_data),
        HTML(paste("Number of fatal accident under the selected conditions")),
        icon = icon("user-friends"),
        color = 'yellow'
      )
    })
    
    # % vbox
    output$fatal_percent <- renderValueBox({
      valueBox(
        paste(round(nrow(filtered_data)/nrow(df)*100,2),'%'),
        HTML(paste("% of fatal accident under the selected conditions")),
        icon = icon("percent"),
        color = 'red'
      )
    })
    
    # calendar heatmap
    output$calendar <- renderPlotly({
      
      # name x,y axis
      x <- list(
        title = "Time"
      )
      y <- list(
        title = "Day of week"
      )
      
      # heatmap
      plot_ly(x=colnames(counts), y=rownames(counts), z = counts, type = "heatmap",colors = c("white", "blue"), hoverinfo = 'text', text = counts) %>%
        layout(margin = list(l=120),xaxis = x,
               yaxis = y, 
               title = 'Accident number by day of week and time')
      
    })
    
    # by afternoon time and year
    afternoon <- if (input$Year == 'All')
    {df[df$ACCIDENT_TIME>12 & df$ACCIDENT_TIME<19,]}
    else{
      df[df$ACCIDENT_TIME>12 & df$ACCIDENT_TIME<19 & df$year == input$Year,]
    }
    
    # by year
    by_year <- if (input$Year == 'All')
    {df}
    else{
      df[df$year == input$Year,]
    }
    
    # total vbox
    output$total2 <- renderValueBox({
      valueBox(
        nrow(by_year),
        HTML(paste("The total number of fatal accident between the selected period")),
        color = 'purple',
        icon = icon("user-friends")
      )
    })
    
    # fatal number vbox
    output$fatal_number2 <- renderValueBox({
      valueBox(
        nrow(afternoon),
        HTML(paste("Number of fatal accident in the afterron (1pm - 6pm)")),
        icon = icon("user-friends"),
        color = 'yellow'
      )
    })
    
    # % vbox
    output$fatal_percent2 <- renderValueBox({
      valueBox(
        paste(round(nrow(afternoon)/nrow(by_year)*100,2),'%'),
        HTML(paste("% of fatal accident in the afterron (1pm - 6pm)")),
        icon = icon("percent"),
        color = 'red'
      )
    })
    # driver
    # check filter
    driver_temp <- if (input$Yr_driver == 'All'){
      driver
    } else{
      driver %>% filter(yr == input$Yr_driver)
    }
    
    driver_age_count <- aggregate(ACCIDENT_NO~ SEX + AGE, driver_temp,length)
    
    m <- driver_age_count[driver_age_count$SEX == 'M',]
    f <- driver_age_count[driver_age_count$SEX == 'F',]
    
    # create df that contains all age to maege with m and f
    age_all <- data.frame(seq(from = 15, to = 97))
    colnames(age_all) <- 'AGE'
    
    # merge then fill NA
    m <- merge(x = age_all, y = m, by='AGE', all.x = TRUE)
    # fill sex
    m <- m %>% fill(SEX) %>% fill(SEX, .direction = 'up')
    # fill count
    m[is.na(m)] <- 0

    # merge then fill NA
    f <- merge(x = age_all, y = f, by='AGE', all.x = TRUE)
    # fill sex
    f <- f %>% fill(SEX) %>% fill(SEX, .direction = 'up')
    # fill count
    f[is.na(f)] <- 0
    
    output$driver <- renderPlotly({
      fig <- plot_ly()
      fig <- fig %>% add_trace(
        name = 'male',
        x = m$AGE,
        y = m$ACCIDENT_NO,
        type = 'scatter',
        fill = 'tozeroy',
        fillcolor = 'rgba(168, 216, 234, 1)',
        hoveron = 'points',
        marker = list(
          color =  'rgba(168, 216, 234, 1)'
        ),
        line = list(
          color =  'rgba(168, 216, 234, 1)'
        )
      )
      
      fig <- fig %>% add_trace(
        name = 'female',
        x = ~f$AGE, 
        y = ~f$ACCIDENT_NO, 
        type = 'scatter',
        fill = 'tozeroy',
        fillcolor = 'rgba(220, 100, 100, 0.6)',
        hoveron = 'points',
        marker = list(
          color =  'rgba(220, 100, 100, 0.6)'
        ),
        line = list(
          color =  'rgba(220, 100, 100, 0.6)'
        )
      )
      
      fig <- fig %>% layout(hovermode = 'x unified', xaxis = list(title ='Age'), yaxis = list(title ='Accident count'),
                            title = 'Number of male and female drivers that involved in fatal accidents ')
    })
    
    
    output$male <- renderValueBox({
      valueBox(
        paste(round(sum(m$ACCIDENT_NO)/ (sum(m$ACCIDENT_NO)+sum(f$ACCIDENT_NO)) *100,2),'%'),
        HTML(paste("Proportion of drivers involved in fatal accidents that are male")),
        color = 'light-blue',
        icon = icon("male")
      )
    })
    
    output$female <- renderValueBox({
      valueBox(
        paste(round(sum(f$ACCIDENT_NO)/ (sum(m$ACCIDENT_NO)+sum(f$ACCIDENT_NO))*100,2),'%'),
        HTML(paste("Proportion of drivers involved in fatal accidents that are female")),
        color = 'red',
        icon = icon("female")
      )
    })
    
    # by year
    by_year_alcohol <- if (input$Yr_alcohol == 'All')
    {df}
    else{
      df[df$year == input$Yr_alcohol,]
    }
    # involve alcohol
    yes <-  by_year_alcohol %>% filter(ALCOHOL_RELATED == 'Yes')
    # not involve alcohol
    no <- by_year_alcohol %>% filter(ALCOHOL_RELATED == 'No')

    output$alcohol <- renderPlotly({
       
       fig <- plot_ly(x = ~yes$INJ_OR_FATAL, type = 'histogram', name = 'yes')
       
       fig <- fig %>% add_histogram(x = ~no$INJ_OR_FATAL, type = 'histogram', name = 'no') %>%
         layout(title = 'Number of injured person', 
                xaxis = list(title = 'Number of injured person'),
                yaxis = list(title = 'Count'))
       

    })
    # number of serious accident for yes and no class
    yes_gt5 <- nrow(yes %>% filter(INJ_OR_FATAL>5))
    no_gt5 <- nrow(no %>% filter(INJ_OR_FATAL>5))
    
    output$yes <- renderValueBox({
      valueBox(
        paste(yes_gt5),
        HTML(paste("Number of serious accidents that were alcohol related")),
        color = 'light-blue',
        icon = icon("check-circle")
      )
    })
    
    output$no <- renderValueBox({
      valueBox(
        paste(no_gt5),
        HTML(paste("Number of serious accidents that were not alcohol related")),
        color = 'orange',
        icon = icon("times")
      )
    })
    
    # by year
    by_year_light <- if (input$Yr_traffic == 'All')
    {tl}
    else{
      tl[tl$yr == input$Yr_traffic,]
    }
    
    output$traffic_light <- renderPlotly({
      # mapbox token
      mapboxToken = 'pk.eyJ1Ijoiam9laXAwNDExIiwiYSI6ImNrOWMyZHFnNTAwOGQzbnA0d2k5dm9yNHUifQ.C8JFMHRri0u0lI2JK3N5fw'
      Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
      
      fig <- by_year_light %>% plot_mapbox(lat = ~Y, lon = ~X, split= ~traffic,  mode = 'scattermapbox')
      fig <- fig %>% config(mapboxAccessToken = Sys.getenv('MAPBOX_TOKEN'))
      
      fig <- fig %>% layout(title = 'Traffic light analysis',
                            width = 1200,
                            height = 600,
                            font = list(color='white'),
                            plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                            mapbox = list(style = 'dark', zoom = 10, center = list(lat = ~median(Y), lon = ~median(X))),
                            legend = list(orientation = 'h',
                                          font = list(size = 8)),
                            margin = list(l = 25, r = 25,
                                          b = 25, t = 25,
                                          pad = 2)

                            ) 
      fig
      
    })
    
    
    output$in_ <- renderValueBox({
      valueBox(
        paste(nrow(by_year_light%>% filter(traffic == 'inside'))),
        HTML(paste("Number of accidents that occured under traffic light coverage")),
        color = 'light-blue',
        icon = icon("traffic-light")
      )
    })
    
    output$out <- renderValueBox({
      valueBox(
        paste(nrow(by_year_light%>% filter(traffic == 'outside'))),
        HTML(paste("Number of accidents that occured without traffic light coverage")),
        color = 'orange'
      )
    })
    
    output$in_percent <- renderValueBox({
      valueBox(
        paste(round(nrow(by_year_light%>% filter(traffic == 'inside'))/ nrow(by_year_light)*100,2),'%'),
        HTML(paste("% accidetns that occured under traffic light coverage")),
        color = 'red',
        icon = icon("percent")
      )
    })
    
  })
}

shinyApp(ui, server)
