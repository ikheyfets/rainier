# Igor Kheyfets
# MET CS 555
# Term Project
# ------------

# Import data
weather <- read.csv('Rainier_Weather.csv')
climbing <- read.csv('climbing_statistics.csv')

# Specifying data for investigation
climbing <- climbing[climbing$Route == 'Disappointment Cleaver',]
climbing <- data.frame(climbing$ï..Date, climbing$Attempted, climbing$Succeeded)
colnames(climbing) <- c('Date', 'Attempted', 'Succeeded')

# Aggregate data to avoid multiple variables with the same date
climbing <- aggregate(cbind(climbing$Attempted, climbing$Succeeded), by = list(Date = climbing$Date), FUN=sum)
colnames(climbing) <- c('Date', 'Attempted', 'Succeeded')

# Merge data, only keeping observations where climbing data and weather data are available 
rainier.ascends <- merge(climbing, weather, by = 'Date')

#Calculating the overall success percentage per each day
rainier.ascends["Success.Percentage"] <- rainier.ascends$Succeeded / rainier.ascends$Attempted
rainier.ascends <- rainier.ascends[rainier.ascends$Success.Percentage <= 1,]

disappointment.cleaver <- data.frame(rainier.ascends$Success.Percentage,
                                     rainier.ascends$Temperature.AVG,
                                     rainier.ascends$Relative.Humidity.AVG,
                                     rainier.ascends$Wind.Speed.Daily.AVG,
                                     rainier.ascends$Wind.Direction.AVG,
                                     rainier.ascends$Solare.Radiation.AVG)

colnames(disappointment.cleaver) <- c('Success.Percentage',
                                      'Temperature.AVG',
                                      'Relative.Humidity.AVG',
                                      'Wind.Speed.AVG',
                                      'Wind.Direction.AVG',
                                      'Solar.Radiation.AVG')

panel.cor <- function(x, y, ...) {
  par(usr = c(0, 1, 0, 1))
  txt <- as.character(format(cor(x, y), digits=2))
  text(0.5, 0.5, txt, cex = 2)
}

# Correlation Coefficinets
pairs(disappointment.cleaver[1:6], lower.panel = panel.cor)

#MLR
attach(disappointment.cleaver)
success.mlr <- lm(Success.Percentage ~ Temperature.AVG + Relative.Humidity.AVG + Wind.Speed.AVG + Wind.Direction.AVG + Solar.Radiation.AVG)
summary(success.mlr)
anova(success.mlr)

critical.F.value <- qf(0.95, df1 = 5, df2 = 171)
MS.df <- data.frame(anova(success.mlr))
MS.Reg <- sum(MS.df$Mean.Sq[1:6])
MS.Res <- MS.df$Mean.Sq[6]
F.value <- MS.Reg/MS.Res

