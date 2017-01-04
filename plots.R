#
# This script expects the input file to be in the working directory.
# Check for it and stop with an error if it is not there.
# Otherwise, read it in.
#
filename <- "_e143dff6e844c7af8da2a4e71d7c054d_payments.csv"
if (!file.exists(filename)) {
   stop("summarySCC_PM25.rds must be in working directory.\n",
        call.=FALSE)
}
data <- read.csv(filename)
#
# For first plot, select data from New York only.
#
dataNY <- subset(data, data$Provider.State=="NY")
with(dataNY, plot(Average.Total.Payments~Average.Covered.Charges))
trend <- with(dataNY, lm(Average.Total.Payments~Average.Covered.Charges))
intercept <- round(trend$coefficients[1], 2)
slope <- round(trend$coefficients[2], 2)
abline(trend, col="red", lwd=2)
text(120000, 5000, paste("intercept:", intercept), cex=.8)
text(120000, 6000, paste("slope:", slope), cex=.8)
title(main="Mean Covered Charges and Mean Total Payments in New York")
par(mfrow=c(6, 6), mar=rep(0, 4))
for (state in unique(data$Provider.State)) {
   for (condition in unique(data$DRG.Definition)) {
      dataSub <- subset(data, data$Provider.State==state & data$DRG.Definition==condition)
      with(dataSub, plot(Average.Total.Payments~Average.Covered.Charges))
      trend <- with(dataSub, lm(Average.Total.Payments~Average.Covered.Charges))
#      intercept <- round(trend$coefficients[1], 2)
#      slope <- round(trend$coefficients[2], 2)
      abline(trend, col="red", lwd=2)
#      text(120000, 5000, paste("intercept:", intercept), cex=.8)
#      text(120000, 6000, paste("slope:", slope), cex=.8)
   }
}