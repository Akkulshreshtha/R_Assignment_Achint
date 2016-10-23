df <- read.csv("C:/Users/Achint/Desktop/forestfires.csv")

# Scatterplot matrix of DMC,DC,wind,rain,temp

pairs(~DMC+DC+wind+rain+temp,data=df, 
      main="Scatterplot Matrix")

# 3D Scatterplot of wind,rain,area

#install.packages("scatterplot3d")
library(scatterplot3d)
attach(df)
scatterplot3d(wind,rain,area, main="3D Scatterplot")

dev.print(pdf, '3D_Scatterplot.pdf')

# Interactive 3D Scatterplot of wind,rain,area

library(rgl)
plot3d(wind, rain, area, col="Blue", size=3)
dev.print(pdf, 'interactive_Scatterplot.pdf')

# Boxplot of X and Y

boxplot(X~Y,data=df, main="Boxplot", 
        xlab="X", ylab="Y")
dev.print(pdf, 'Box_Plot.pdf')

# Simple bar plot of temp, wind, rain [horizontal and vertical]

# Vertical Bar Plot
counts <- table(df$temp)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")
dev.print(pdf, 'Temp_Dist.pdf')

counts <- table(df$wind)
barplot(counts, main="Wind Distribution", 
        xlab="Wind")

counts <- table(df$rain)
barplot(counts, main="Rain Distribution", 
        xlab="Rain")
# Horizontal Bar Plot
counts <- table(df$temp)
barplot(counts, main="Temperature Distribution", horiz=TRUE)
dev.print(pdf, 'Hor_Temp_Dist.pdf')

counts <- table(df$wind)
barplot(counts, main="Wind Distribution", horiz=TRUE)

counts <- table(df$rain)
barplot(counts, main="Rain Distribution", horiz=TRUE)

# Grouped bar plot of X and Y

counts <- table(df$X, df$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
dev.print(pdf, 'grouped_Barplot.pdf')

# Histogram of probability distribution of X, Y, wind, temp, area along with line density

hist(df$X, 
     main="Probability Distribution of X", 
     xlab="X", 
     border="Black", 
     col="Blue", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$X))
dev.print(pdf, 'Probability_Dist.pdf')

dev.print(pdf, 'dist_X.pdf')


hist(df$Y, 
     main="Probability Distribution of Y", 
     xlab="Y", 
     border="black", 
     col="Red", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$Y))

hist(df$wind, 
     main="Probability Distribution of Wind", 
     xlab="Wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$wind))

hist(df$temp, 
     main="Probability Distribution of Temp", 
     xlab="Temp", 
     border="black", 
     col="Brown", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$temp))

hist(df$area, 
     main="Probability Distribution of Temp", 
     xlab="Area", 
     border="black", 
     col="Orange", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$area))

# Histogram of frequency distribution of X, Y, wind, temp, area

hist(df$X, 
     main="Frequency Distribution of X", 
     xlab="X", 
     border="Black", 
     col="Blue", 
     las=1, 
     breaks=5, 
     prob = TRUE)
dev.print(pdf, 'Freq_Dist.pdf')


hist(df$Y, 
     main="Frequency Distribution of Y", 
     xlab="Y", 
     border="black", 
     col="Red", 
     las=1, 
     breaks=5, 
     prob = TRUE)


hist(df$wind, 
     main="Frequency Distribution of Wind", 
     xlab="Wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)


hist(df$temp, 
     main="Frequency Distribution of Temp", 
     xlab="Temp", 
     border="black", 
     col="Brown", 
     las=1, 
     breaks=5, 
     prob = TRUE)


hist(df$area, 
     main="Frequency Distribution of Temp", 
     xlab="Area", 
     border="black", 
     col="Orange", 
     las=1, 
     breaks=5, 
     prob = TRUE)



# Pie Chart of area, wind, rain, temp by month
install.packages("dplyr")
library(dplyr)

df_pivot <- summarize(group_by(df,month),wind=sum(wind))
slices <- df_pivot[["wind"]] 
pie(slices, labels=df[["month"]], main="Pie Chart of wind")
dev.print(pdf, 'Pie_Chart.pdf')

df_pivot <- summarize(group_by(df,month),area=sum(area))
slices <- df_pivot[["area"]] 
pie(slices, labels=df[["month"]], main="Pie Chart of area")


df_pivot <- summarize(group_by(df,month),rain=sum(rain))
slices <- df_pivot[["rain"]] 
pie(slices, labels=df[["month"]], main="Pie Chart of rain")

df_pivot <- summarize(group_by(df,month),temp=sum(temp))
slices <- df_pivot[["temp"]] 
pie(slices, labels=df[["month"]], main="Pie Chart of temp")

# Pie Chart of area, wind, rain, temp by day


df_pivot <- summarize(group_by(df,day),wind=sum(wind))
slices <- df_pivot[["wind"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of wind")
dev.print(pdf, 'Pie_Chart_Day.pdf')

df_pivot <- summarize(group_by(df,day),area=sum(area))
slices <- df_pivot[["area"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of area")


df_pivot <- summarize(group_by(df,day),rain=sum(rain))
slices <- df_pivot[["rain"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of rain")

df_pivot <- summarize(group_by(df,day),temp=sum(temp))
slices <- df_pivot[["temp"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of temp")

# Map Plot of sourceAirportID

airports <- read.csv("C:/Users/Achint/Desktop/airports.dat")
head(airports)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
head(airports)

routes <- read.csv("C:/Users/Achint/Desktop/routes.dat")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(routes)

library(plyr)
departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

airportA <- merge(airports, departures, by.x = "ID", by.y = "sourceAirportID")

# install.packages("ggmap")
library(ggmap)
map <- get_map(location = 'World', zoom = 4)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportA, alpha = .5)

mapPoints
dev.print(pdf, 'Map_Plot.pdf')

# Map Plot of destinationAirportID

airportB <- merge(airports, arrivals, by.x = "ID", by.y = "destinationAirportID")

library(ggmap)
map <- get_map(location = 'World', zoom = 4)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportB, alpha = .5)

mapPoints
dev.print(pdf, 'Map_Plot1.pdf')
