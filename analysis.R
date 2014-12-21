# download, unzip, and read the csv
# ```{r dl, cache=TRUE}
data_file <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
temp <- tempfile()
download.file(data_file, temp)
data <- read.csv(bzfile(temp))
unlink(temp)

# EVTYPE column is problematic, there are spelling variations, for example "THUNDERSTORM WIND" vs "THUNDERSTORM WINDS"
unique(data$EVTYPE)

# add a column indicating if this event is high wind related
data$EVTYPE <- as.character(data$EVTYPE)

data$EVTYPE[grepl("THUNDERSTORM|TSTM|WIND|WND", data$EVTYPE, ignore.case=TRUE)] <- 'wind'

# add a column indicating if this event is flooding related
data$EVTYPE[grepl("flood|fld", data$EVTYPE, ignore.case=TRUE)] <- 'flood'

data$EVTYPE <- as.factor(data$EVTYPE)


# First, explore most dangerous events, defined as combination of fatalities and injuries.

# create a column representing total population damage (fatalities and injuries)
data$popDamage <- data$FATALITIES + data$INJURIES

# sum up the population damage for each event type
totalPopDamage <- aggregate(data[, 'popDamage'], by=list(data$EVTYPE), FUN = sum)
colnames(totalPopDamage) <- c('event', 'damage')

# show top 5 most dangerous events
sorted <- totalPopDamage[with(totalPopDamage, order(-damage)), ]
top <- head(sorted, 5)
top

# visualization of scale difference between top 5 most dangerous events
barplot(
    top$damage, 
    main = 'Fatalities and injuries for top 5 most dangerous events', 
    xlab = 'Event', 
    ylab = 'Fatalies and Injuries',
    legend.text = c("Tornado", "High Wind", "Flood", "Heat", "Lightning"),
    args.legend = list(x = "topright"),
    col = c('blue', 'red', 'yellow', 'green', 'orange')
  )


# Now lets explore the economic impact

# create a column representing total economic damage (propery damage and crop damage)
data$econDamage <- data$PROPDMG + data$CROPDMG

# sum up the population damage for each event type
totalEconDamage <- aggregate(data[, 'econDamage'], by=list(data$EVTYPE), FUN = sum)
colnames(totalEconDamage) <- c('event', 'damage')

# show top 5 most dangerous events
sorted <- totalEconDamage[with(totalEconDamage, order(-damage)), ]
top <- head(sorted, 5)
top

# visualization of scale difference between top 5 most dangerous events
barplot(
  top$damage, 
  main = 'Property and Crop damage for top 5 most dangerous events', 
  xlab = 'Event', 
  ylab = 'Damage',
  legend.text = c("High Wind", "Tornado", "Flood", "Hail", "Lightning"),
  args.legend = list(x = "topright"),
  col = c('blue', 'red', 'yellow', 'green', 'orange')
)
