###################################Heat map for hours versuz month##############################################################
Heat1 <-
  alt$Chart(sev_2016)$
  mark_rect()$
  encode(
    alt$X('hours(time):O', title='Hour'),
    alt$Y('month(time):O', title='month'),
    color = alt$Color('count()', scale = alt$Scale(scheme = "plasma")),
    tooltip=c("hours(time)","month(time)","count()")
  )$properties(
    title="2016 Heat Map Showing Counts of Accidents Per Hour Every Month",
    width= 300, height= 200
  )
###
Heat2 <-
  alt$Chart(sev_2017)$
  mark_rect()$
  encode(
    alt$X('hours(time):O', title='Hour'),
    alt$Y('month(time):O', title='month'),
    color = alt$Color('count()', scale = alt$Scale(scheme = "plasma")),
    tooltip=c("hours(time)","month(time)","count()")
  )$properties(
    title="2017 Heat Map Showing Counts of Accidents Per Hour Every Month",
    width= 300, height= 200
  )
###
Heat3 <-
  alt$Chart(sev_2019)$
  mark_rect()$
  encode(
    alt$X('hours(time):O', title='Hour'),
    alt$Y('month(time):O', title='month'),
    color = alt$Color('count()', scale = alt$Scale(scheme = "plasma")),
    tooltip=c("hours(time)","month(time)","count()")
  )$properties(
    title="2019 Heat Map Showing Counts of Accidents Per Hour Every Month",
    width= 400, height= 200
  )
##
(Heat1 | Heat2) & Heat3
#############################################################################################################
#######################using circles###################################################################
#########################################################################################
alt$Chart(sev_2019)$mark_circle(
  size = 60
)$encode(
  alt$X('hours(time):O',title='Hour', axis=alt$Axis(labelAngle=0)),
  alt$Y('month(time):N',title= 'Month'),
  alt$Size('average(Distance)',
           scale=alt$Scale(range=list(0, 400)),
           legend=alt$Legend(title='Average traffic distance')
  ),
  tooltip=c('average(Distance)','average(duration_mins)'),
  alt$Color('month(time):N')
)$
  properties(
    width=600,
    height=400)
#############################################################################################################
#######################using dual scale###################################################################
#########################################################################################
base = alt$Chart(sev_2019)$
  encode(
    alt$X('month(time):O',title='Month'))$
  properties(title="average duration and distance of traffic per Month",
             width=500,
             height=300)
line_A = base$mark_line(interpolate = "cardinal-open",color='#5276A7')$encode(
  alt$Y('average(duration_mins):Q', axis=alt$Axis(titleColor='#5276A7'))
)
line_B = base$mark_line(interpolate = "cardinal-open",color='#F18727')$encode(
  alt$Y('average(Distance):Q', axis=alt$Axis(titleColor='#F18727'))
)
part1<- alt$layer(line_A, line_B)$resolve_scale(y='independent')
#####################################################################
base = alt$Chart(sev_2019)$
  encode(
    alt$X('hours(time):O',title='Hours'))$
  properties(title="average duration and distance of traffic per Hour",
             width=500,
             height=300)
line_A = base$mark_line(interpolate = "cardinal-open",color='#5276A7')$encode(
  alt$Y('average(duration_mins):Q', axis=alt$Axis(titleColor='#5276A7'))
)
line_B = base$mark_line(interpolate = "cardinal-open",color='#F18727')$encode(
  alt$Y('average(Distance):Q', axis=alt$Axis(titleColor='#F18727'))
)
part2<- alt$layer(line_A, line_B)$resolve_scale(y='independent')
#####
part1 & part2
#####################################################################
#######################durattion facetting by months#################################################
brush <- alt$selection(type="interval") 
points <- 
  alt$Chart(sev)$
  mark_point(shape='diamond')$
  encode(
    alt$Y("average(duration_mins)",title= "Average Traffic Duration(mins)",scale=alt$Scale(domain = list(0,300),clamp =TRUE)),
    alt$X("hour:O",title = 'Hour'),
    facet =alt$Facet('month(time):N', columns = 4,title = 'Months'),
    color = alt$condition(brush, "month(time):N", alt$value("lightgray"),title='Month'),
    size = alt$Size('average(Distance)'),
    tooltip = c('average(duration_mins)','average(Distance)','hour')
  )$
  properties(selection = brush,width=300,height=200,title='Multiple View of Average Traffic Duration pER Month')
##############################################################################################
###############################weather conditions,average distance and hour###############################################################
##############################################################################################
brush = alt$selection(type='interval')
chart <- 
  alt$Chart(sev)$
  mark_line(interpolate="bundle")$
  encode(
    alt$X("hours(time):O",title="Hour"),
    alt$Y("average(Distance)",title = "Average Distance of Traffic"),
    color = "weather:N",
    tooltip=c("hours(time)","average(Distance)","count()")
  )$
  properties(selection= brush,
             width=500,
             height=300)
bars <- 
  alt$Chart(sev)$
  mark_bar()$
  encode(
    x='count()',
    y='weather:N',
    color=alt$Color('weather:N')
  )$
  transform_filter(brush$ref())$
  properties( width=500,height=200)

chart&bars
#################################################################################
#########################average up year down########################################################
selection = alt$selection_multi(fields=list('year'))
#######################FOR duration
base = alt$Chart(sev2)$properties(
  width=400, height=300, title="Average Traffic duration(Mins) vs Hour per Year")$
  transform_filter(
    selection)
lines = base$mark_line()$
  encode(
    x='hour:O',
    y='average(duration_mins)',
    color='year:N'
  )
rule = base$mark_rule()$
  encode(
    y='average(duration_mins)',
    size=alt$value(2)
  )
top = lines + rule
######
bottom = alt$Chart(sev2)$
  mark_bar()$
  encode(
    x='year:N',
    y='average(duration_mins):Q',
    color=alt$condition(selection, alt$value('steelblue'), alt$value('lightgray'))
  )$properties(
    width=400, height=200
  )$add_selection(
    selection)
#####################################################################################
#########################FOR DISTANCE
base1 = alt$Chart(sev2)$properties(
  width=400, height=300, title="Average Traffic Distance(Miles) vs Hour per Year")$
  transform_filter(
    selection)
lines1 = base1$mark_line()$
  encode(
    x='hour:O',
    y='average(Distance)',
    color='year:N'
  )
rule1 = base1$mark_rule()$
  encode(
    y='average(Distance)',
    size=alt$value(2)
  )
top1 = lines1 + rule1
######
bottom1 = alt$Chart(sev2)$
  mark_bar()$
  encode(
    x='year:N',
    y='average(Distance):Q',
    color=alt$condition(selection, alt$value('steelblue'), alt$value('lightgray'))
  )$properties(
    width=400, height=200
  )$add_selection(
    selection)
#################################################
#alt$vconcat(top, bottom,data=sev)
top&bottom | top1&bottom1

count(sev2$Distance)
##################################################################################################
##################################################################################################
#######################joining several based on month###################################################
selection = alt$selection_multi(fields=list('month'))
#####
top<-
  alt$Chart(sev)$mark_line(
    size = 2,
    point = TRUE
  )$encode(
    alt$X('hour:O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(duration_mins)',title= 'Average Minutes'),
    tooltip=c('average(Distance)','average(duration_mins)','month(time)'),
    alt$Color('month:N',title = "Months")
  )$
  properties(
    width=300,
    height=200)$
  transform_filter(
    selection)
##########
middle = alt$Chart(sev)$
  mark_bar()$
  encode(
    x='month:N',
    y='count()',
    color=alt$condition(selection, alt$value('steelblue'), alt$value('lightgray'))
  )$properties(
    width=300, height=200
  )$add_selection(
    selection
  )
#######
right<-
  alt$Chart(sev)$mark_line(
    color="pink"
  )$encode(
    alt$X('hour:O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(Distance)',title= 'Average Distance'),
    tooltip=c('average(Distance)','average(duration_mins)','month(time)'),
    alt$Color('month:N',title = "Months")
  )$
  properties(
    width=300,
    height=200)$
  transform_filter(
    selection)
#########
left<-
  alt$Chart(sev)$mark_bar(
    color="green"
  )$encode(
    alt$X('hour:O',title='Hour', axis=alt$Axis(labelAngle=0)),
    alt$Y('count()',title= 'Count of Accidents'),
    tooltip=c('average(Distance)','average(duration_mins)'),
    alt$Color('month:N',title = "Months")
  )$
  properties(
    width=300,
    height=200)$
  transform_filter(
    selection)
#########
bottom<-
  alt$Chart(sev)$mark_bar( color='red'
  )$encode(
    alt$X('weather:N',title='Weather', axis=alt$Axis(labelAngle=0)),
    alt$Y('count()',title= 'Count of Accidents'),
    tooltip=c('average(Distance)','average(duration_mins)'),
    alt$Color('month:N',title = "Months")
  )$
  properties(
    width=300,
    height=200)$
  transform_filter(
    selection)
#########
chart<-(middle&top&bottom)|(left&right);chart
####################################################################################
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
  properties(width = 300, height = 300)$
  project("albersUsa")
# airport positions on background
points <- 
  alt$Chart(sev)$
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
  properties(width=200, height=200,title = 'Counts Of Traffic Accidents Per State')

chart <- (background + points)

left<-
  alt$Chart(sev)$mark_area(
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
base = alt$Chart(sev)$
  encode(
    alt$X('hours(time):O',title='Hour'))$
  properties(title="average duration and distance of traffic per Hour",
             width=300,
             height=200)
line_A = base$mark_line(interpolate = "cardinal-open",color='#5276A7')$encode(
  alt$Y('average(duration_mins):Q', axis=alt$Axis(titleColor='#5276A7'))
)
line_B = base$mark_line(interpolate = "cardinal-open",color='#F18727')$encode(
  alt$Y('average(Distance):Q', axis=alt$Axis(titleColor='#F18727'))
)
dual<- alt$layer(line_A, line_B)$resolve_scale(y='independent')$
  transform_filter(
    selection)
######################weather
weata<- alt$Chart(sev)$mark_bar( 
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
month_av<- alt$Chart(sev)$mark_point()$
  encode(
    alt$X('average(Distance):Q',title='Average Distance', axis=alt$Axis(labelAngle=0)),
    alt$Y('average(duration_mins):Q',title= 'Average Duration(mins)'),
    tooltip=c('average(Distance)','average(duration_mins)','month(time)'),
    alt$Color('month(time):N',title = "Months")
  )$
  properties(title= "Average Traffic Distance Vs Average Traffic Duration(mins) per Month",
             width=300,
             height=200)$
  transform_filter(
    selection)
#############################################
Heat <-
  alt$Chart(sev)$
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

########################
alt$hconcat(alt$vconcat(
  chart, left,dual,
  data=sev),alt$vconcat(month_av,weata,data=sev))



