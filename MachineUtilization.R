#DELIVERABLES: a list of the following components
#Character - Machine name
#Vector -  (min,median,max) Utilization of the month(excluding unknown hours)
#Logical - Has utilization ever fallen below %90? (TRUE / FALSE)
#Vector - All hours where utilization is unknown (NA's)
#Data frame - For this machine
#Plot - For all machines

#check the working directory
getwd()

#import the data
util <- read.csv("Machine-Utilization.csv")

#checkout the data
dim(util)
head(util, 10)
tail(util, 10)
str(util)
summary(util)

#Add a utilization column
util$Utilization = 1 - util$Percent.Idle

#Use Universal Dates and Times to convert your timestamp variable
util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y %H:%M")
head(util,10)
summary(util)

#Now delete the old timestamp column and rearrange the columns of the dataset
util$Timestamp <- NULL
head(util,10)
util <- util[,c(4,1,2,3)]
head(util,10)
str(util)

#Creating a list
summary(util)
RL1 <- util[util$Machine == "RL1",] #created a new dataset, containing only machine RL1 
summary(RL1)
RL1$Machine <- factor(RL1$Machine) #getting rid of the other 4 machines and leaving the variable with only  1 factor
summary(RL1)

#Start constructing the list with the following three components:
#Character - Machine name
#Vector -  (min,median,max) Utilization of the month(excluding unknown hours)
#Logical - Has utilization ever fallen below %90? (TRUE / FALSE)

#Create min,mean,max vector
util_stats_rl1 <- c(min(RL1$Utilization, na.rm = T),
                    mean(RL1$Utilization, na.rm = T),
                    max(RL1$Utilization, na.rm = T))
util_stats_rl1 

#Create the logical var util < %90
length(which(RL1$Utilization < .90)) #ignores NAs and shows which are true
#27 times utilization has fallen below %90

util_under_90_flag <- length(which(RL1$Utilization < .90)) > 0 #converting it into a logical vector, 
#could have used as.logical()
util_under_90_flag
list_rl1 <- list("RL1",util_stats_rl1,util_under_90_flag)
list_rl1

#Let's name the components of this list
names(list_rl1)
names(list_rl1) <- c("Machine", "Stats", "LowThreshold")
list_rl1

#another way the name the components of the list
rm(list_rl1)
list_rl1
list_rl1 <- list(Machine="RL1", Stats = util_stats_rl1 , LowThreshold = util_under_90_flag)
list_rl1

#Extracting components of a list
#[] - will always return a list
#[[]] - will always return the actual object
# $ - same as [[]] but prettier

list_rl1[1] #returns the first component as a list (with the name and the value of the component)
list_rl1[[1]] #returns the first component as a vector (only the value)
list_rl1$Machine #same as the previous one, returns a vector 

list_rl1[2]
typeof(list_rl1[2])
list_rl1[[2]]
typeof(list_rl1[[2]])
list_rl1$Stats
typeof(list_rl1$Stats)

#How would you access the 3rd element of the vector (max utilization) ?
list_rl1[[2]][3]
list_rl1$Stats[3]

list_rl1
list_rl1[3] 
list_rl1[[3]] 
list_rl1$LowThreshold

#Adding and deleting list components
list_rl1
list_rl1[4] <- "New Information"

#Another way to add a component
#We will add:
#Vector - All hours where utilization is unknown (NA's)
list_rl1$UnknownHours <- RL1[is.na(RL1$Utilization),"PosixTime"]
list_rl1

#Remove the component
list_rl1[4] <- NULL
list_rl1

#Notice!!! The numeration b the list has shifted unlike in a dataframe

#Add another component:
#Data frame - For this machine
list_rl1$Data <- RL1
list_rl1
summary(list_rl1)


#subsetting a list 
list_rl1
list_rl1$UnknownHours[1]
list_rl1[[4]][1]


list_rl1[1:3]
list_rl1[c(1,4)]
sublist_rl1 <- list_rl1[c("Machine", "Stats")]
sublist_rl1
sublist_rl1[[2]][2]

#Double square brackets are NOT for subsetting
list_rl1[[1:3]] #will give you an error, because double square brackets are designed to access to a single component in a list

#Building a timeseries plot
library(ggplot2)

p <- ggplot(data=util)
p + geom_line(aes(x=PosixTime, y=Utilization, colour=Machine), size=1.2)+
  facet_grid(Machine~.)+
  geom_hline(yintercept = 0.90, size=1,
             colour="Gray", linetype="dashed")
myplot <- p + geom_line(aes(x=PosixTime, y=Utilization, colour=Machine), size=1.2)+
  facet_grid(Machine~.)+
  geom_hline(yintercept = 0.90, size=1,
             colour="Gray", linetype="dashed")


#add the plot into the list
list_rl1$Plot <- myplot
list_rl1
summary(list_rl1)
str(list_rl1)
