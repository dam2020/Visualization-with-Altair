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


###############################
###############distance vs road affected per weather
stat1<- alt$Chart(sev_2019)$
  mark_point()$
  encode(
    alt$Y('Distance:Q', title='Length of Road affected (in Meters)'),
    alt$X('duration_mins:Q', title='Duration of Traffic Accident in Mins',scale=alt$Scale(domain =list(0,1000),clamp = TRUE)),
    color= alt$Color('weather:N'),
    tooltip=c('Distance','duration_mins','Precipitation')
  )$
  properties(title='Length of Road Affected vs Accident Duration per Weather',
             width=600,
             height=300)
###############################
###############distance vs road affected per day
#################################
stat2 <-alt$Chart(sev_2019)$
  mark_point()$
  encode(
    alt$Y('Distance:Q', title='Length of Road affected (in Meters)'),
    alt$X('duration_mins:Q', title='Duration of Traffic Accident in Mins',scale=alt$Scale(domain =list(0,1000),clamp = TRUE)),
    color= alt$Color('day(time):N',title = 'Day'),
    tooltip=c('Distance','duration_mins','Precipitation')
  )$
  properties(title='Length of Road Affected vs Accident Duration per Day',
             width=600,
             height=300)

stat1 & stat2
#############################################################################################################
#######################using circles to compare traffic distance per month and day###################################################################
#########################################################################################
stat3 <- alt$Chart(sev_2019)$mark_circle()$encode(
  alt$X('day(time):O',title='Day', axis=alt$Axis(labelAngle=0)),
  alt$Y('month(time):N',title= 'Month'),
  alt$Size('average(Distance)',
           scale=alt$Scale(range=list(0, 400)),
           legend=alt$Legend(title='Average Length of Road')
  ),
  tooltip=c('average(Distance)','average(duration_mins)'),
  alt$Color('month(time):N',title = 'Month')
)$
  properties(title='Average Length of Road affected per Day of the Month',
    width=600,
    height=400)
######using circles to compare traffic duration per hour and day
stat4 <- alt$Chart(sev_2019)$mark_circle()$encode(
  alt$Y('day(time):O',title='Day', axis=alt$Axis(labelAngle=0)),
  alt$X('hours(time):N',title= 'Hour'),
  alt$Size('average(duration_mins)',
           scale=alt$Scale(range=list(0, 400)),
           legend=alt$Legend(title='Average Length of Road')
  ),
  tooltip=c('average(Distance)','average(duration_mins)'),
  alt$Color('day(time):N',title = 'Day')
)$
  properties(title='Average Length of Road affected per Hour of Every Day',
    width=600,
    height=400)

##########################################################################
###count of traffic per day

alt$Chart(sev_2019)$
  mark_bar()$
  encode(
    x = alt$X("day(time):O",title = 'Day'),
    y = alt$Y("count()",title = 'Count of Traffic Accident'),
    tooltip=c("count()"),
    facet =alt$Facet('month(time):N', columns = 3,title = 'Months'),
    color = alt$Color('Sunrise_Sunset:N', title = 'Sunrise or Sunset')
  )$properties(title='Multi View of Count of Traffic Accident based on sunrise and sunset Split by Months',
    width=300,
    height=200)
####################count of traffic per hour
alt$Chart(sev_2019)$
  mark_area()$
  encode(
    x = alt$X("hours(time):O",title = 'Hour'),
    y = alt$Y("count()",title = 'Count of Traffic Accident'),
    tooltip=c("count()"),
    facet =alt$Facet('day(time):N', columns = 3,title = 'Months')
  )$properties(title='Multi View of Count of Traffic Accident based on sunrise and sunset Split by Day',
               width=300,
               height=200)
##############################################################
#######################using dual scale###################################################################
#########################################################################################
base = alt$Chart(sev_2019)$
  encode(
    alt$X('month(time):O',title='Month'))$
  properties(title="Average Duration and Average Length of Road Affected vs Month",
             width=600,
             height=400)
line_A = base$mark_line(interpolate = "bundle",color='#5276A7')$encode(
  alt$Y('average(duration_mins):Q', axis=alt$Axis(titleColor='#5276A7'),title = 'Average Traffic Duration(mins)')
)
line_B = base$mark_line(interpolate = "bundle",color='#F18727')$encode(
  alt$Y('average(Distance):Q', axis=alt$Axis(titleColor='#F18727'),title='Average length Of Road(Meters)')
)
alt$layer(line_A, line_B)$resolve_scale(y='independent') 
  ################
base = alt$Chart(sev_2019)$
  encode(
    alt$X('hours(time):O',title='Hours'))$
  properties(title="Average Duration and Average Length of traffic per Hour",
             width=600,
             height=400)
line_A = base$mark_line(interpolate = "bundle",color='#5276A7')$encode(
  alt$Y('average(duration_mins):Q', axis=alt$Axis(titleColor='#5276A7'),title = 'Average Traffic Duration(mins)')
)
line_B = base$mark_line(interpolate = "bundle",color='#F18727')$encode(
  alt$Y('average(Distance):Q', axis=alt$Axis(titleColor='#F18727'),title='Average length Of Road(Meters)')
)
 alt$layer(line_A, line_B)$resolve_scale(y='independent')

##########################################################################
 ###############using average distance and average length of road affected per month and day#######################################################################
 #################################################################################
 alt$Chart(sev_2019)$mark_point(
 )$encode(
   alt$X('average(Distance):Q',title='Average length Of Road(Meters)', axis=alt$Axis(labelAngle=0),
         scale=alt$Scale(domain =list(2,6))
   ),
   alt$Y('average(duration_mins):Q',title= 'Average Duration(mins)',scale=alt$Scale(domain =list(90,180))),
   tooltip=c('average(Distance)','average(duration_mins)','month(time)'),
   alt$Size('count()'),
   alt$Color('month(time):N',title = "Months")
 )$
   properties(title= "Average Traffic Distance Vs Average  Length of Road Affected per Month",
              width=600,
              height=400)
#########per day
 alt$Chart(sev_2019)$mark_point()$
   encode(
   alt$X('average(Distance):Q',title='Average length Of Road(Meters)', axis=alt$Axis(labelAngle=0),
         scale=alt$Scale(domain =list(2,6))
        ),
   alt$Y('average(duration_mins):Q',title= 'Average Duration(mins)',scale=alt$Scale(domain =list(90,180))),
   tooltip=c('average(Distance)','average(duration_mins)'),
   alt$Color('day(time):N',title = "Day"),
   alt$Size('count()',title = 'Count of Traffic'),
   tooltip=c('average(Distance)','average(duration_mins)','day(time)')
   )$
   properties(title= "Average Traffic Distance Vs Average  Length of Road Affected per Day",
              width=600,
              height=400)

###########################################################################################
 base = alt$Chart(sev_2019)$
   encode(
     alt$X('month(time):O',title='Month'))$
   properties(title="Average Duration and Average Length of Road Affected vs Month",
              width=600,
              height=400)
 line_A = base$mark_line(interpolate = "bundle",color='#5276A7')$encode(
   alt$Y('average(duration_mins):Q', axis=alt$Axis(titleColor='#5276A7'),title = 'Average Traffic Duration(mins)')
 )
 line_B = base$mark_line(interpolate = "bundle",color='#F18727')$encode(
   alt$Y('average(Distance):Q', axis=alt$Axis(titleColor='#F18727'),title='Average length Of Road(Meters)')
 )
 counter<- base$ mark_bar()$ encode( alt$Y('count()'), tooltip= c("weather","count()"))
lines2<- alt$layer(line_A, line_B,counter)$resolve_scale(y='independent',x='shared')
 
#####################################
###########################sunset and sunrise
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Traffic duration(Mins) per Period of Day")
counter<- base$ mark_bar()$
  encode(
    alt$Y('average(duration_mins)',title='Average Traffic Duration(mins)'),
    alt$X('Sunrise_Sunset:N', title = 'Sunrise or Sunset'),
    alt$Color('Sunrise_Sunset:N'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y='average(duration_mins)',
    size=alt$value(2)
  )
sun_dur<- alt$layer(counter, rule)$resolve_scale(y='shared')
###########################month
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Traffic duration(Mins) per Month")
counter<- base$ mark_bar()$
  encode(
    alt$Y('average(duration_mins)',title='Average Traffic Duration(mins)'),
    alt$X('month(time):N', title = 'Month'),
    alt$Color('month(time):N',title = 'Month'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y=alt$Y('average(duration_mins)',title = 'Average Duration in dataset'),
    size=alt$value(2)
  )
month_dur<- alt$layer(counter, rule)$resolve_scale(y='shared')

###########################day
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Traffic duration(Mins) per Day")
counter<- base$ mark_bar()$
  encode(
    alt$Y('average(duration_mins)',title='Average Traffic Duration(mins)'),
    alt$X('day(time):N', title = 'Day'),
    alt$Color('day(time):N',title = 'Day'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y=alt$Y('average(duration_mins)',title = 'Average Duration in dataset'),
    size=alt$value(2)
  )
day_dur<- alt$layer(counter, rule)$resolve_scale(y='shared')
###########################Hour
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Traffic duration(Mins) per Hour")
counter<- base$ mark_bar()$
  encode(
    alt$Y('average(duration_mins)',title='Average Traffic Duration(mins)'),
    alt$X('hours(time):O', title = 'Hour'),
    alt$Color('hours(time):O',scale = alt$Scale(scheme = "plasma"),title = 'Hour'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y=alt$Y('average(duration_mins)',title = 'Average Duration in dataset'),
    size=alt$value(2)
  )
hour_day<-alt$layer(counter, rule)$resolve_scale(y='shared')
#####################################
###########################DISTANCE###############################################
#####################################
###########################sunset and sunrise
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Length of Road Affected(m) per Period of Day")
counter<- base$ mark_bar()$
  encode(
    alt$Y('Distance',title='Average Length of Road Affected(m)'),
    alt$X('Sunrise_Sunset:N', title = 'Sunrise or Sunset'),
    alt$Color('Sunrise_Sunset:N'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y='average(Distance)',
    size=alt$value(2)
  )
sun_dis<- alt$layer(counter, rule)$resolve_scale(y='shared')
###########################Month
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Length of Road Affected(m) per Month")
counter<- base$ mark_bar()$
  encode(
    alt$Y('Distance',title='Average Length of Road Affected(m)'),
    alt$X('month(time):N', title = 'Month'),
    alt$Color('month(time):N',title = 'Month'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y='average(Distance)',
    size=alt$value(2)
  )
month_dis<-alt$layer(counter, rule)$resolve_scale(y='shared')
###########################DAY
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Length of Road Affected(m) per Day")
counter<- base$ mark_bar()$
  encode(
    alt$Y('average(Distance)',title='Average Length of Road Affected(m)'),
    alt$X('day(time):N', title = 'Day'),
    alt$Color('day(time):N',title = 'Day'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y='average(Distance)',
    size=alt$value(2)
  )
day_dis<-alt$layer(counter, rule)$resolve_scale(y='shared')
###########################Hour
base = alt$Chart(sev_2019)$properties(
  width=400, height=300, title="Comparing Average Length of Road Affected(m) per Hour")
counter<- base$ mark_bar()$
  encode(
    alt$Y('average(Distance)',title='Average Length of Road Affected(m)'),
    alt$X('hours(time):N', title = 'Hour'),
    alt$Color('hours(time):O',scale = alt$Scale(scheme = "plasma"),title = 'Hour'),
    tooltip= c("count()")
  )
rule = base$mark_rule()$
  encode(
    y='average(Distance)',
    size=alt$value(2)
  )
hour_dis<-alt$layer(counter, rule)$resolve_scale(y='shared')
######
sun_dis|sun_dur
month_dis|month_dur
day_dis|day_dur
hour_day|hour_dis
######################################################################################
#########################################################################################
################months and days and hours and daylight with highest counts of accidents
count_sun <- 
  alt$Chart(sev_2019)$properties(
    width=400, height=300, title="Counts of Traffic Accidents per Period of Day")$
  mark_bar()$
  encode(
    alt$Y('count()',title='Counts of Traffic Accidents'),
    alt$X('Sunrise_Sunset:N', title = 'Sunrise or Sunset'),
    alt$Color('Sunrise_Sunset:N'),
    tooltip= c("count()")
  )
############# 
count_month <- 
  alt$Chart(sev_2019)$properties(
    width=400, height=300, title="Counts of Traffic Accidents per Month")$
  mark_bar()$
  encode(
    alt$Y('count()',title='Counts of Traffic Accidents'),
    alt$X('month(time):N', title = 'Month'),
    alt$Color('month(time):N'),
    tooltip= c("count()")
  )

############# 
count_day <- 
  alt$Chart(sev_2019)$properties(
    width=400, height=300, title="Counts of Traffic Accidents per Day")$
  mark_bar()$
  encode(
    alt$Y('count()',title='Counts of Traffic Accidents'),
    alt$X('day(time):N', title = 'Day'),
    alt$Color('day(time):N'),
    tooltip= c("count()")
  )
############# 
count_hour <- 
  alt$Chart(sev_2019)$properties(
    width=400, height=300, title="Counts of Traffic Accidents per Hour")$
  mark_bar()$
  encode(
    alt$Y('count()',title='Counts of Traffic Accidents'),
    alt$X('hours(time):N', title = 'Hour'),
    alt$Color('hours(time):N'),
    tooltip= c("count()")
  )
############
(count_sun|count_month) & (count_day|count_hour)
#############
count_hour <- 
  alt$Chart(sev_2019)$properties(
    width=400, height=300, title="Counts of Traffic Accidents per State")$
  mark_bar()$
  encode(
    alt$X('count()',title='Counts of Traffic Accidents'),
    alt$Y('State:N', title = 'State'),
    alt$Color('State:N'),
    tooltip= c("count()")
  )
##########################
#############################################################
#################################facet by temperature
split_weather <- 
  alt$Chart(sev_2019)$properties(
    width=300, height=300, title="Average Traffic Duration vs Hour of the day Split By Weather Condition")$
  mark_area(color='purple')$
  encode(
    alt$X('hours(time):O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(duration_mins):Q',title= 'Average Duration(mins)'),
    tooltip=c('average(Distance)','average(duration_mins)'),
    facet = alt$Facet('weather:N',columns = 3)
  )
##########################
split_weather <- 
  alt$Chart(sev_2019)$properties(
    width=300, height=300, title='Average Length Of Road Affected vs Hour of the day Split By Weather Condition')$
  mark_area(color='lightblue')$
  encode(
    alt$X('hours(time):O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(Distance):Q',title= 'Average Distance(m)'),
    tooltip=c('average(Distance)','average(duration_mins)'),
    facet = alt$Facet('weather:N',columns = 3)
  )

##################################################################################################
##################################################################################################
#######################joining several based on month###################################################
month <- list('Apr','Aug','Dec','Jul','Jun','May','Nov','Oct','Sep')
######################
selection = alt$selection_multi(fields=list('month'))
#########
first = alt$Chart(sev_2019)$
  mark_bar()$
  encode(
    x=alt$X('month:N',title='Month'),
    y=alt$Y('count()'),
    color=alt$condition(selection, alt$value('steelblue'), alt$value('lightgray'))
  )$properties(title='Count of Traffic Accidents per Month',
    width=300, height=200
  )$add_selection(
    selection
  )
#####
left1<-
  alt$Chart(sev_2019)$mark_line(
    size = 2,
    point = TRUE
  )$encode(
    alt$X('hour:O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(duration_mins)',title= 'Average Duration'),
    tooltip=c('average(duration_mins)','month(time)','max(duration_mins)'),
    alt$Color('month:N',title = "Months")
  )$
  properties(title='Average Traffic Duration per Hour',
    width=300,
    height=200)$
  transform_filter(
    selection)
#########
right1<-
  alt$Chart(sev_2019)$mark_bar(
    color="green"
  )$encode(
    alt$X('hour:O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('count()',title= 'Count of Accidents'),
    tooltip=c('count()'),
    alt$Color('month:N',title = "Months")
  )$
  properties(title='Count of Traffic Accidents per Hour',
    width=300,
    height=200)$
  transform_filter(
    selection)

#######
right2<-
  alt$Chart(sev_2019)$mark_line(
    color="pink"
  )$encode(
    alt$X('hour:O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(Distance)',title= 'Average Distance'),
    tooltip=c('average(Distance)','hours(time)'),
    alt$Color('month:N',title = "Months")
  )$
  properties(title='Average length of Road affected by Traffic per Hour',
    width=300,
    height=200)$
  transform_filter(
    selection)

#########
left2<-
  alt$Chart(sev_2019)$mark_bar( color='red'
  )$encode(
    alt$X('weather:N',title='Weather', axis=alt$Axis(labelAngle=0)),
    alt$Y('count()',title= 'Count of Accidents'),
    tooltip=c('count()'),
    alt$Color('month:N',title = "Months")
  )$
  properties(title='Count of Traffic Accidents per Weather condition',
    width=300,
    height=200)$
  transform_filter(
    selection)
##############
right3 <- 
  alt$Chart(sev_2019)$mark_bar(
  color="green"
)$encode(
  alt$X('day(time):N',title='Day', axis=alt$Axis(labelAngle=0)),
  alt$Y('count()',title= 'Count of Accidents'),
  tooltip=c('count()'),
  alt$Color('month:N',title = "Months")
)$
  properties(title='Count of Traffic Accidents per Day',
    width=300,
    height=200)$
  transform_filter(
    selection)
#########
chart<-(first&left1)|(right1&right2)|(left2&right3);chart
####
htmlwidgets::saveWidget(vegawidget(chart),'x2.html')
####################################################################################
#############################################################################################################
#######################USING THe map and linking several plots###################################################################
#############################################################################################################
##########################################################################################
selection = alt$selection_multi(fields=list('State'))
#####
vega_data <- import_vega_data()
us <- vega_data$us_10m$url
airports <- vega_data$airports()
states <- alt$topo_feature(us, feature = "states")
##########
# US states background
background <-
  alt$Chart(states)$
  mark_geoshape(
    fill = "lightpink",
    stroke = "white"
  )$
  properties(width = 400, height = 300)$
  project("albersUsa")
# airport positions on background
points <- 
  alt$Chart(sev_2019)$
  transform_aggregate(
    latitude = "mean(Start_Lat)",
    longitude = "mean(Start_Lng)",
    count = "count()",
    groupby = list("State")
  )$
  mark_circle()$
  encode(
    longitude = "longitude:Q",
    latitude = "latitude:Q",
    size = alt$Size('count:Q', title = "Count of Traffic Accidents"),
    color=alt$condition(selection, alt$value('steelblue'), alt$value('black')),
    tooltip = list("State:N","count:Q")
  )$add_selection(selection)$
  properties(width=300, height=200,title = 'Counts Of Traffic Accidents Per State')

chart <- (background + points)
################################################
left<-
  alt$Chart(sev_2019)$mark_area(
    color="green"
  )$encode(
    alt$X('hour:O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('count()',title= 'Count of Accidents'),
    tooltip=c('average(Distance)','average(duration_mins)')
  )$
  properties(title='Count of Accidents per Hour',
             width=300,
             height=200)$
  transform_filter(
    selection)
##################dual axis
base = alt$Chart(sev_2019)$
  encode(
    alt$X('hours(time):O',title='Hour'))$
  properties(title="Average duration and Average Length of Road Affected per Hour",
             width=300,
             height=200)
line_A = base$mark_line(interpolate = "cardinal-open",color='#5276A7')$encode(
  alt$Y('average(duration_mins):Q',title = 'Average traffic duration', axis=alt$Axis(titleColor='#5276A7'))
)
line_B = base$mark_line(interpolate = "cardinal-open",color='#F18727')$encode(
  alt$Y('average(Distance):Q',title = 'Average Length of Road Affected', axis=alt$Axis(titleColor='#F18727'))
)
dual<- alt$layer(line_A, line_B)$resolve_scale(y='independent')$
  transform_filter(
    selection)
######################weather
weata<- alt$Chart(sev_2019)$mark_bar( 
)$encode(
  alt$X('weather:N',title='Weather', axis=alt$Axis(labelAngle=0)),
  alt$Y('count()',title= 'Count of Accidents'),
  tooltip=c('average(Distance)','average(duration_mins)')
)$
  properties(title='Count of Accidents per Weather Condition',
             width=300,
             height=200)$
  transform_filter(
    selection)
################################av vs av by month
month_av<- alt$Chart(sev_2019)$mark_point()$
  encode(
    alt$X('average(Distance):Q',title='Average Length of Road Affected', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(duration_mins):Q',title= 'Average Duration(mins)'),
    tooltip=c('average(Distance)','average(duration_mins)','month(time)'),
    alt$Color('month(time):N',title = "Months")
  )$
  properties(title= "Average Traffic Distance Vs Average Length of Road Affected) per Month",
             width=300,
             height=200)$
  transform_filter(
    selection)
#############################################
Heat <-
  alt$Chart(sev_2019)$
  mark_rect()$
  encode(
    alt$X('hours(time):O', title='Hour'),
    alt$Y('month(time):O', title='month'),
    color = alt$Color('count()', scale = alt$Scale(scheme = "plasma")),
    tooltip=c("hours(time)","month(time)","count()")
  )$properties(
    title="Heat Map Showing Counts of Accidents Per Hour Every Month",
    width=300,
    height=200)$
  transform_filter(
    selection)
##############################
count_day<-
  alt$Chart(sev_2019)$mark_area(
    color="green"
  )$encode(
    alt$X('day(time):N',title='Day', axis=alt$Axis(labelAngle=0)),
    alt$Y('count()',title= 'Count of Accidents'),
    tooltip=c('count()')
  )$
  properties(title='Count of Accidents per Day',
             width=300,
             height=200)$
  transform_filter(
    selection)
###################
count_month<-
  alt$Chart(sev_2019)$mark_area(
    color="green"
  )$encode(
    alt$X('month(time):N',title='Month', axis=alt$Axis(labelAngle=0)),
    alt$Y('count()',title= 'Count of Accidents'),
    tooltip=c('count()')
  )$
  properties(title='Count of Accidents per Month',
             width=300,
             height=200)$
  transform_filter(
    selection)
########################
x3<-alt$hconcat(alt$vconcat(
  chart, left,weata,
  data=sev_2019),alt$vconcat(month_av,count_day,Heat,data=sev_2019),
  alt$vconcat(count_month,dual,data=sev_2019))
###########
htmlwidgets::saveWidget(vegawidget(x3),'x3.html')
