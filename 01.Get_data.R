
# Get data

# These libraries need to be loaded

library(utils)
library(httr)

# Download the dataset from the ECDC website to a local temporary file

GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

# read the Dataset sheet into “R”. The dataset will be called "data".

data <- read.csv(tf)

#------------------ Data ------------------

colnames(data) <- c("Date","Day","Month","Year","Cases","Deaths","Country","GeoId","TerritoryCode","PopData2018")

# Only select countries with more than 100 cases

Aux <- data %>% group_by(Country) %>% mutate(CumSum = cumsum(Cases))
Aux <- Aux %>% filter(CumSum>100)
Countries <- unique(Aux$Country)
DfCountries <- data.frame(Countries=Countries,Id=c(1:length(Countries)))
ListCountries <- as.list(DfCountries$Id)
names(ListCountries) <- DfCountries$Countries

#------------------ Last update ------------------

data$Date <- as.Date(data$Date, format="%d/%m/%Y")
LastUpdate <- max(data$Date)

# Selected Country: Spain

whichspain <- as.numeric(ListCountries['Spain'])

#---
