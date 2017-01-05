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
attach(data)
#
# For first plot, select data from New York only.
#
dataNY <- subset(data, Provider.State=="NY")
pdf(file="Plot1.pdf")
with(dataNY, plot(Average.Total.Payments~Average.Covered.Charges))
#
# Add trend line and text describing intercept and slope.
#
trend <- with(dataNY, lm(Average.Total.Payments~Average.Covered.Charges))
intercept <- round(trend$coefficients[1], 2)
slope <- round(trend$coefficients[2], 2)
abline(trend, col="red", lwd=2)
text(120000, 5000, paste("intercept:", intercept), cex=.8)
text(120000, 6000, paste("slope:", slope), cex=.8)
title(main="Mean Covered Charges and Mean Total Payments in New York")
dev.off()
#
# Second plot
#
pdf(file="Plot2.pdf")
par(mfrow=c(7, 7), mar=rep(0, 4), xaxt="n", yaxt="n", oma=c(0, 0, 2, 0))
#
# Loop through states and conditions.
# Use first row and column for labels.
#
plot(c(0, 1), c(0, 1), ann=F, bty='n', type='n')
for (condition in unique(DRG.Definition)) {
   plot(c(0, 1), c(0, 1), ann=F, bty='n', type='n')
   text(x=0.5, y=0.5, substr(condition, 1, 3))
}
for (state in unique(Provider.State)) {
   plot(c(0, 1), c(0, 1), ann=F, bty='n', type='n')
   text(x=0.5, y=0.5, state)
   for (condition in unique(DRG.Definition)) {
      dataSub <- subset(data, Provider.State==state & DRG.Definition==condition)
      with(dataSub, plot(Average.Total.Payments~Average.Covered.Charges))
      trend <- with(dataSub, lm(Average.Total.Payments~Average.Covered.Charges))
      abline(trend, col="red", lwd=2)
   }
}
title(main="Mean Covered Charges and Mean Total Payments\n by Condition and State", outer=TRUE)
dev.off()
detach("data")