install.packages("dplyr")
install.packages("lubridate")
library(dplyr)
library(lubridate)

data<-read.csv("sqf_data_sample.csv",header=T)#load data
dim(data)#display dimensions of data
nrow(data)#count the number of rows in the data
ncol(data)#count the number of columns in the data
names(data)#display column names of data

##There are various types of objects in R. Use the class function to determine which.
class(data)

##Dataframes have two dimensions: rows and columns. Just like a spreadsheet. Use bracket notation to access specific rows, columns, or data points.

data[1,]#access the first row
data[,2]#access the second column
names(data)[2]#display the second column name of the dataframe
data[,"date"]#access the second column using its name
data[1,"date"]#access the first row, second column

##Let's take a subset of these columns to make things easier to work with.

data<-data[,c("dayno","date","precinct","arrested","force","found.weapon","suspected.crime","suspect.race","suspect.sex","suspect.age")]

head(data)#view the first six rows of data

###tabulate variables
table(data$suspect.sex)#counts of each group
table(data$suspect.sex)/nrow(data)#proportions of each group

table(data$suspect.race)#counts of each group
table(data$suspect.race)/nrow(data)#proportions of each group

###cross tabulate variables
table(data$suspect.race, exclude=NULL)
table(data$arrested,data$suspect.race)

##look only in a subset of the data using bracket notation
table(data$suspect.race[data$precinct=="111"])

#compute means by group using the aggregate function
arrests_by_precincts<- aggregate(x= data$arrested,
                                # Specify group indicator
                                by = list(data$precinct),      
                                # Specify function (i.e. mean)
                                FUN = mean, na.rm=T)
head(arrests_by_precincts)
colnames(arrests_by_precincts)<-c("precinct","arrests")

#compute hit rate by precinct

data$count<-1
weapon_by_precincts<- aggregate(x= data$found.weapon,
                              # Specify group indicator
                              by = list(data$precinct),      
                              # Specify function (i.e. mean)
                              FUN = mean, na.rm=T)
head(weapon_by_precincts)
colnames(weapon_by_precincts)<-c("precinct","hit_rate")

#merge the results
precinct_data<-merge(arrests_by_precincts, 
                     weapon_by_precincts, 
                     by="precinct")


#are these variables related? let's make a scatter plot.
plot(precinct_data$hit_rate,precinct_data$arrests)

#make the plot prettier
plot(precinct_data$hit_rate,precinct_data$arrests, pch=19,
col="gray", cex=.4, main=" Precinct Arrest Rate by Precinct Hit Rate", ylab="Precinct Arrest Rate",
xlab="Precinct Hit Rate")

## add line of best fit
m<-lm(arrests~hit_rate, data=precinct_data)
abline(m, col="red")

#compute rate of force by racial group
force_by_race_means<- aggregate(x= data$force,
                                # Specify group indicator
                                by = list(data$suspect.race),      
                                # Specify function (i.e. mean)
                                FUN = mean)
colnames(force_by_race_means)<-c("group","force_rate")
force_by_race_means

####
### plot number of stops over time
####

class(data$date)
head(data$date)
data$date<-ymd(data$date)
class(data$date)

day_counts<- aggregate(x= data$dayno,
                                # Specify group indicator
                                by = list(data$dayno),      
                                # Specify function
                                FUN = sum)

colnames(day_counts)<-c("dayno","count")
head(day_counts)

##merge in dates
dates<-unique(data$date)#get unique dates
dates<-dates[order(dates)]#put them in order
head(dates)
length(dates)==nrow(day_counts)#confirm there are the same number of dates as day numbers
day_counts$date<-dates#add dates column
head(day_counts)

plot(day_counts$date, day_counts$count, pch=19,
     col="gray", cex=.2, main="Number of Stops Over Time",
     ylab="Number of Stops",
     xlab="Date")

###
### Challenge
###

# How many total weapons were found?
#Hint: use the table() or sum() functions.

# How many stops were made for the suspected crime of robbery in the 100th precinct?
#Hint: use the table() or sum() functions while looking at a subset of the data.


