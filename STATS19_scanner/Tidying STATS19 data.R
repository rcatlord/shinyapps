## Loading and tidying STATS19 data ##

# STEP 1: load the necessary R packages
library(dplyr)
library(maptools)

# STEP 2: merging multiple years of casualty data and attendant data
# rbind() is used to match casualty (and attendant) data because the variables are identical between years

# read the casualty data
casualty_2005 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2005-gla-data-extract-casualty.csv", header=T)
casualty_2006 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2006-gla-data-extract-casualty.csv", header=T)
casualty_2007 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2007-gla-data-extract-casualty.csv", header=T)
casualty_2008 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2008-gla-data-extract-casualty.csv", header=T)
casualty_2009 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2009-gla-data-extract-casualty.csv", header=T)
casualty_2010 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2010-gla-data-extract-casualty.csv", header=T)
casualty_2011 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2011-gla-data-extract-casualty.csv", header=T)
casualty_2012 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2012-gla-data-extract-casualty.csv", header=T)
casualty_2013 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2013-gla-data-extract-casualty.csv", header=T)
casualty_2014 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2014-gla-data-extract-casualty.csv", header=T)
casualty_2015 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2015-gla-data-extract-casualty.csv", header=T) %>%
rename(AREFNO = Accident.Ref.)

# merge the casualty data
casualty <- do.call(rbind, list(casualty_2005, casualty_2006, casualty_2007, casualty_2008,
casualty_2009, casualty_2010, casualty_2011, casualty_2012,
casualty_2013, casualty_2014, casualty_2015))

# remove the redundant dataframes from the R session
rm(casualty_2005, casualty_2006, casualty_2007, casualty_2008, casualty_2009, casualty_2010,
casualty_2011, casualty_2012, casualty_2013, casualty_2014, casualty_2015)

# read the attendant data
attendant_2005 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2005-gla-data-extract-attendant.csv", header=T)
attendant_2006 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2006-gla-data-extract-attendant.csv", header=T)
attendant_2007 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2007-gla-data-extract-attendant.csv", header=T)
attendant_2008 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2008-gla-data-extract-attendant.csv", header=T)
attendant_2009 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2009-gla-data-extract-attendant.csv", header=T)
attendant_2010 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2010-gla-data-extract-attendant.csv", header=T)
attendant_2011 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2011-gla-data-extract-attendant.csv", header=T)
attendant_2012 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2012-gla-data-extract-attendant.csv", header=T)
attendant_2013 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2013-gla-data-extract-attendant.csv", header=T)
attendant_2014 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2014-gla-data-extract-attendant.csv", header=T)
attendant_2015 <- read.csv("https://tfl.gov.uk/cdn/static/cms/documents/2015-gla-data-extract-attendant.csv", header=T) %>%
rename(AREFNO = Accident.Ref.)

# merge the attendant data
attendant <- do.call(rbind, list(attendant_2005, attendant_2006, attendant_2007, attendant_2008,
attendant_2009, attendant_2010, attendant_2011, attendant_2012,
attendant_2013, attendant_2014, attendant_2015))

# remove the redundant dataframes from the R session
rm(attendant_2005, attendant_2006, attendant_2007, attendant_2008, attendant_2009, attendant_2010,
attendant_2011, attendant_2012, attendant_2013, attendant_2014)

# STEP 3: merge the attendant and casualty data
# merge() is used so that matching is based on a specific variable

# merge using the AREFNO variable
casualties <- merge(casualty, attendant, by="AREFNO")
rm(casualty)
rm(attendant)

# STEP 4: add in borough data
boroughs <- read.csv("boroughs.csv", header = T)
casualties <- merge(casualties, boroughs, "Borough.x")

# STEP 5: date/time variables
# convert Accident.Date to a date
casualties$Accident.Date <- as.Date(casualties$Accident.Date, "%d-%b-%y")
# extract the year
casualties$year <- format(casualties$Accident.Date, format="%Y")
# extract the month
casualties$month <- format(casualties$Accident.Date, format="%B")
casualties$month <- as.factor(casualties$month)
casualties$month <- factor(casualties$month,levels=month.name)
# extract the day of the week
casualties$day <- format(casualties$Accident.Date, format="%A")
casualties$day <- factor(casualties$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# add an hour band variable
casualties$Time <- gsub("[ [:punct:]]", "" , casualties$Time)
casualties$Time <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", casualties$Time)
casualties$hour<- as.POSIXlt(casualties$Time, format="%H:%M")$hour

# STEP 6: factor variables
# relabel the 'Casualty.Severity' categories
casualties$Casualty.Severity <- factor(casualties$Casualty.Severity,
levels= c("1 Fatal", "2 Serious", "3 Slight"),
labels= c("Fatal", "Serious", "Slight"))
# relabel the 'Mode.of.Travel' categories
casualties$Mode.of.Travel <- factor(casualties$Mode.of.Travel,
levels= c("1 Pedestrian", "2 Pedal Cycle", "3 Powered 2 Wheeler", "4 Car",
"5 Taxi", "6 Bus Or Coach", "7 Goods Vehicle", "8 Other Vehicle"),
labels= c("Pedestrian", "Pedal Cycle", "Powered 2 Wheeler", "Car",
"Taxi", "Bus or Coach", "Goods Vehicle", "Other Vehicle"))
# relabel the 'Casualty.Sex' categories
casualties$Casualty.Sex <- factor(casualties$Casualty.Sex,
levels= c("1 Male", "2 Female"),
labels= c("Male", "Female"))
# relabel the 'Light.Conditions..Banded.' categories
casualties$Light.Conditions..Banded. <- factor(casualties$Light.Conditions..Banded.,
levels= c("1 Daylight", "2 Dark"),
labels= c("Daylight", "Dark"))

# recode 'Casualty.Age' as NA when 'Casualty.Age..Banded.' is Unknown
casualties$Casualty.Age[casualties$Casualty.Age == 0 & casualties$Casualty.Age..Banded. == "Unknown"] <- NA

# create age bands
bands <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
"40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84")
casualties$ageband <- cut(casualties$Casualty.Age,
breaks=c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84),
labels = bands)
# add a text variable
casualties$Accident.Severity <- factor(casualties$Accident.Severity,
levels= c("1 Fatal", "2 Serious", "3 Slight"),
labels= c("Fatal", "Serious", "Slight"))
casualties_desc <- function(row)
with(as.list(row), paste0("At ", (Time), " on ", format.Date(Accident.Date, "%A %d %B %Y"),
" a ", tolower(Accident.Severity),
" collision occured involving ", (No..of.Vehicles.in.Acc.), " vehicle(s) and ",
(No..of.Casualties.in.Acc.), " casualtie(s)."))
strs <- apply(casualties, 1, casualties_desc)
names(strs) <- NULL
casualties$text <- strs

# STEP 7: selecting and renaming variables
casualties <- casualties %>%
select(AREFNO,
date = Accident.Date,
year, month, day, hour,
mode = Mode.of.Travel,
severity = Casualty.Severity,
light = Light.Conditions..Banded.,
sex = Casualty.Sex, ageband,
borough = NAME, GSS_CODE,
easting = Easting.x, northing = Northing.x,
text)

# STEP 8: tidying spatial data
# convert the casualties into a SpatialPointsDataFrame
coords <- SpatialPoints(casualties[,c("easting","northing")])
casualties_pts <- SpatialPointsDataFrame(coords,casualties)
proj4string(casualties_pts) <- CRS("+init=epsg:27700")
# convert to long, lat
casualties_pts <- spTransform(casualties_pts, CRS("+init=epsg:4326"))
# convert the SpatialPointsDataFrame back to a dataframe
casualties <- as.data.frame(casualties_pts)
# rename long, lat
casualties <- casualties %>%
select(everything(),
long = easting.1, lat = northing.1)

# STEP 9: export
saveRDS(casualties, file="casualties_2005-145.Rda")