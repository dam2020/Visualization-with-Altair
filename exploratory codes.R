install.packages("plyr")
library("lubridate")
library("altair")
library("listviewer")
library("tibble")
library("dplyr")
library("jsonlite")
library("purrr")
library("plyr")


#######importing vega datasets 
vega_data = altair::import_vega_data()
###list of all available datasets
vega_data$list_datasets()

##################################

sev = read.csv("C:\\Users\\NSOH TANIH\\OneDrive\\Documents\\visualization\\sev42.csv")
sev4 = read.csv("C:\\Users\\NSOH TANIH\\OneDrive\\Documents\\visualization\\sev4.csv")

########################
####renaming variables
names(sev)[names(sev) == "Temperature.F."] <- "Temperature"
names(sev)[names(sev) == "Wind_Chill.F."] <- "Wind_Chill"
names(sev)[names(sev) == "Humidity..."] <- "Humidity"
names(sev)[names(sev) == " Pressure.in."] <- "Pressure"
names(sev)[names(sev) == "Visibility.mi."] <- "Visibility"
names(sev)[names(sev) == "Wind_Speed.mph."] <- "Wind_Speed"
names(sev)[names(sev) == "Precipitation.in."] <- "Precipitation"
names(sev)[names(sev) == "Distance.mi."] <- "Distance"

sev$weather <- sev4$weather
sev$Humidity <- as.numeric(sev$Humidity)

######converting to time-date formats
sev$start_time2 <- dmy_hms(as.character(sev$ Start_Time))
sev$end_time2 <- dmy_hms(as.character(sev$ End_Time))

sev$duration_secs <- difftime(sev$end_time2,sev$start_time2,units = "secs")
sev$duration_mins <- difftime(sev$end_time2,sev$start_time2,units = "mins")  
names(sev)[names(sev) == "end_time2"] <- "time"
sev$month <- month(sev$time, label = TRUE)
sev$month_n <- month(sev$time)
sev$hour <- hour(sev$time)
sev$year <- year(sev$time)
#######################################################
sev <- sev[sev$year %in% c(2016,2017,2018,2019),]
sev_2016<- sev[sev$year %in% c(2016),]
sev_2017 <- sev[sev$year %in% c(2017),]
sev_2019 <-sev[sev$year %in% c(2019),]
######################################
###converting distance from inches to meters
sev_2019$Distance <- (sev_2019$Distance)*1609
sev_2019 <- sev[sev$month %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),]





####################################################################################################################
#############################HISTOGRAM FOR ALL CONTINOUS VARIABLES#######################################################
####################################################################################################################

##############Histogram for duration
b1 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("Temperature:Q",bin = TRUE), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Temperature'
  )$
  interactive()
##################
b2 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("Wind_Chill:Q",bin = TRUE), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Wind_Chill'
  )$
  interactive()
################################
b3 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("Humidity:Q",bin = TRUE), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Humidity'
  )$
  interactive()
################################
b4 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("Visibility:Q",bin = alt$Bin(maxbins = 20)), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Visibility'
  )$
  interactive()
################################
b5 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("Wind_Speed:Q",bin = alt$Bin(maxbins = 50)), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Wind_Speed'
  )$
  interactive()
################################
b6 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("Precipitation:Q",bin = alt$Bin(maxbins = 50)), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Precipitation'
  )$
  interactive()
################################
b7 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("duration_mins:Q",bin = alt$Bin(maxbins = 50)
    ), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = "Duration of Accident(mins)"
  )$
  interactive()
################################
b8 <- 
  alt$Chart(sev)$
  mark_bar(color="green")$
  encode(
    alt$X("Distance:Q",bin = alt$Bin(maxbins = 50)), 
    alt$Y("count()")
  )$
  properties(
    width = 300,
    height = 300,
    title = "Distance of Accident(miles)"
  )$
  interactive()
part3 <- b1|b2|b3
part4<- b4|b5|b6
part5<- b7|b8

all_hist <- part3 & part4 & part5
htmlwidgets::saveWidget(vegawidget(all_hist),'all_hist.html')
#####to visualize the chart specification in R
vegawidget::vw_examine(all_hist, mode = "html")

########################################################################################################
#######################################scatter plot matrix#######################################################
#######################################################################################


scatter_matrix <- 
  alt$Chart(sev)$
  mark_circle()$
  encode(
    x = alt$X(alt$`repeat`("column"), type = "quantitative"),
    y = alt$Y(alt$`repeat`("row"), type = "quantitative")
  )$
  properties(width = 150, height = 150)$
  `repeat`(
    row = list("Temperature","Wind_Chill","Humidity","Wind_Speed","Precipitation"),
    column = list("Precipitation","Wind_Speed","Humidity","Wind_Chill","Temperature")
  )$
  interactive()

htmlwidgets::saveWidget(vegawidget(scatter_matrix ),'scatter_matrix.html')


####################################################################################################################
#############################Distribution of weather charateristic across the year#######################################################
####################################################################################################################

a1 <- 
  alt$Chart(sev)$
  mark_area(color= "pink")$
  encode(
    alt$X("month(end_time2):T"),
    alt$Y("Temperature:Q",scale=alt$Scale(domain = list(0,150),clamp =TRUE)
    )
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Temperature during the Year'
  )$
  interactive()
#####
a2 <- 
  alt$Chart(sev)$
  mark_area(color= "pink")$
  encode(
    alt$X("month(end_time2):T"),
    alt$Y("Wind_Chill:Q",scale=alt$Scale(domain = list(-7,130),clamp =TRUE))
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Wind_Chill during the Year'
  )$
  interactive()
##########
a3 <- 
  alt$Chart(sev)$
  mark_area(color= "pink")$
  encode(
    alt$X("month(end_time2):T"),
    alt$Y("Humidity:Q")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Humidity during the Year'
  )$
  interactive()
################
a4 <- 
  alt$Chart(sev)$
  mark_area(color= "pink")$
  encode(
    alt$X("month(end_time2):T"),
    alt$Y("Visibility:Q")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Visibility during the Year'
  )$
  interactive()
#############################
a5 <- 
  alt$Chart(sev)$
  mark_area(color= "pink")$
  encode(
    alt$X("month(end_time2):T"),
    alt$Y("Wind_Speed:Q")
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Wind_Speed during the Year'
  )$
  interactive()
#####################################
a6 <- 
  alt$Chart(sev)$
  mark_area(color= "pink")$
  encode(
    alt$X("month(end_time2):T"),
    alt$Y("Precipitation:Q",scale=alt$Scale(domain = list(0,2),clamp =TRUE)),
  )$
  properties(
    width = 300,
    height = 300,
    title = 'Precipitation  during the Year'
  )$
  interactive()

part1 <- a1 | a3
part2 <- a4 | a5
weather_area_months <- part1 & part2

htmlwidgets::saveWidget(vegawidget(weather_area_months),'weather_area_months.html')

###################################################################################################
###################################density plots for continuos variables######################
################################################################################################
chart1 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Temperature', as_=list('Temperature', 'density')
  )$mark_area()$
  encode(
    x="Temperature:Q",
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Temperature (in Fahrenheit)'
  )$
  interactive()
######################################################
chart2 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Wind_Chill', as_=list('Wind_chill', 'density')
  )$mark_area()$
  encode(
    x="Wind_chill:Q",
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Wind_chill(in Fahrenheit)'
  )$
  interactive()

#######################################################
chart3 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Humidity', as_=list('Humidity', 'density')
  )$mark_area()$
  encode(
    x="Humidity:Q",
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Humidity(Percentage)'
  )$
  interactive()
#########################################################
chart4 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Pressure.in.', as_=list('Pressure', 'density')
  )$mark_area()$
  encode(
    x="Pressure.in.:Q",
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Pressure (Inches)'
  )$
  interactive()
str(sev)
############################################
chart5 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Visibility', as_=list('Visibility', 'density')
  )$mark_area()$
  encode(
    alt$X("Visibility:Q",scale=alt$Scale(domain = list(0,20),clamp =TRUE)),
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Visibility (Miles)'
  )$
  interactive()
#################################################################
chart6 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Wind_Speed', as_=list('Wind_Speed', 'density')
  )$mark_area()$
  encode(
    x="Wind_Speed:Q",
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Wind_Speed (Miles per Hour)'
  )$
  interactive()
###############################################################
chart7 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Precipitation', as_=list('Precipitation', 'density')
  )$mark_area()$
  encode(
    alt$X("Precipitation:Q",scale=alt$Scale(domain = list(0,0.3),clamp =TRUE)),
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Amount of Precipitation(Inches)'
  )$
  interactive()
###################################
chart8 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'duration_mins', as_=list('Duration_mins', 'density')
  )$mark_area()$
  encode(
    alt$X("Duration_mins:Q",scale=alt$Scale(domain = list(0,1200),clamp =TRUE)),
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Duration of Accident in Minutes'
  )$
  interactive()
##############################
chart9 <- 
  alt$Chart(sev)$
  transform_density(
    density= 'Distance', as_=list('Distance', 'density')
  )$mark_area()$
  encode(
    alt$X("Distance:Q",scale=alt$Scale(domain = list(0,35),clamp =TRUE)),
    y='density:Q'
  )$
  properties(
    width = 200,
    height = 100,
    title = 'Distance of Accident in Miles'
  )$
  interactive()

#####################
chart_combined1 <- chart1 |chart2|chart3
chart_combined2 <- chart5 |chart6
chart_combined3 <- chart7 |chart8|chart9
chart_combined <- chart_combined1 & chart_combined3 & chart_combined2
htmlwidgets::saveWidget(vegawidget(chart_combined),'chart_combined.html')


##########################################################################################
##############################simple ticks############################################################
##########################################################################################
t1<- alt$Chart(sev)$
  mark_tick()$
  encode(
    x='Temperature'
  )
t2<- alt$Chart(sev)$
  mark_tick()$
  encode(
    x='Wind_Chill'
  )
t3<- alt$Chart(sev)$
  mark_tick()$
  encode(
    x='Humidity'
  )
t4<- alt$Chart(sev)$
  mark_tick()$
  encode(
    x='Precipitation'
  )
t5<- alt$Chart(sev)$
  mark_tick()$
  encode(
    x='Visibility'
  )
t6<- alt$Chart(sev)$
  mark_tick()$
  encode(
    x='Wind_Speed'
  )
t0 <- t1&t2&t3&t3&t4&t5&t6
htmlwidgets::saveWidget(vegawidget(t0),'simple_ticks.html')

#######################################################################################################
########################bar charts for road sign structures variables#################################
#################################################################################################

b1<-alt$Chart(sev)$
  mark_bar()$
  encode(
    alt$Y('Amenity:O'),
    x='count()',
    alt$Color('Amenity:O')
  )
b2<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Bump:O',
    x='count()',
    alt$Color('Bump:O')
  )
b3<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Crossing:O',
    x='count()',
    alt$Color('Crossing:O')
  )
b4<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Give_Way:O',
    x='count()',
    alt$Color('Give_Way:O')
  )
b5<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Junction:O',
    x='count()',
    alt$Color('Junction:O')
  )
b6<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='No_Exit:O',
    x='count()',
    alt$Color('No_Exit:O')
  )
b7<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Roundabout:O',
    x='count()',
    alt$Color('Roundabout:O')
  )
b8<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Stop:O',
    x='count()',
    alt$Color('Stop:O')
  )
b9<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Traffic_Calming:O',
    x='count()',
    alt$Color('Traffic_Calming:O')
  )
b10<-alt$Chart(sev)$
  mark_bar()$
  encode(
    y='Turning_Loop:O',
    x='count()',
    alt$Color('Turning_Loop:O')
  )

barplot_count<-(b1|b3)&(b8|b4)&(b5|b6)&(b7|b9)&(b10|b2)
htmlwidgets::saveWidget(vegawidget(barplot_count),'barplot_count.html')

####################################################################################
##############bar chat for weather########################################################
bar_weather <- 
  alt$Chart(sev4)$
  mark_bar()$
  encode(
    y = "weather:N",
    x = "count()",
    color = alt$Color("weather")
  )
htmlwidgets::saveWidget(vegawidget(bar_weather),'bar_weather.html')
#####################################################################
######################density plots for tEMPERATURE EVERY MONTH################################################
step <- 20
overlap <- 1

chart <-alt$Chart(sev)$
  transform_timeunit(Month = "month(start_time2)")$
  transform_joinaggregate(
    mean_temp = "mean(Temperature)", 
    groupby = list("Month")
  )$
  transform_bin(list("bin_max", "bin_min"), "Temperature")$
  transform_aggregate(
    value = "count()", 
    groupby = list("Month", "mean_temp", "bin_min", "bin_max")
  )$
  transform_impute(
    impute = "value", 
    groupby = list("Month", "mean_temp"), 
    key = "bin_min", 
    value = 0
  )$
  mark_area(
    interpolate = "monotone",
    fillOpacity = 0.8,
    stroke = "lightgray",
    strokeWidth = 0.5
  )$encode(
    alt$X("bin_min:Q", bin = "binned", title = "Monthly Temperature (C)"),
    alt$Y(
      "value:Q",
      scale = alt$Scale(range = list(step, -step * overlap)),
      axis = NULL
    ),
    alt$Fill(
      "mean_temp:Q",
      legend = NULL,
      scale = alt$Scale(domain = list(30, 5), scheme = "redyellowblue")
    ),
    alt$Row(
      "Month:T",
      title = NULL,
      header = alt$Header(labelAngle = 0, labelAlign = "right", format = "%B")
    )
  )$
  properties(bounds ="flush", title = "Distribution of Temperature", height = step)$
  configure_facet(spacing = 0)$
  configure_view(stroke = NULL)$
  configure_title(anchor = "end")

htmlwidgets::saveWidget(vegawidget(chart),'chart.html')
