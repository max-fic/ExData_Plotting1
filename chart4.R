library(tidyverse)

prepare_df =function() {
    url='https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    zipfile = 'exdata_data_household_power_consumption.zip'
    csvfile = 'household_power_consumption.txt'
    if (!file.exists(csvfile)[1] == TRUE) {
       if (!file.exists(zipfile)[1] == TRUE) {  
          cat('Downloading Data ... ')
          download.file(url, destfile=zipfile)
          cat('Done.\n')
       }
       cat('Unzipping Data ... ')
       unzip(zipfile)
       cat('Done.\n')
    }
    cat('Reading Data ... ')
    pwr = read.csv(csvfile,sep=';')
    cat('Done.\n')
    cat('Preparing Data Frame ... ')
    pwr = pwr %>% 
      filter(Date %in% c('1/2/2007', '2/2/2007')) %>%
      slice(-which((pwr == '?') | (is.na(pwr)), arr.ind= TRUE)[,1]) %>%
      mutate(dt=strptime(paste(Date,' ',Time), format='%d/%m/%Y %H:%M:%S'),
             Global_active_power = as.numeric(Global_active_power),
             Global_reactive_power = as.numeric(Global_reactive_power),
             Voltage = as.numeric(Voltage),
             Global_intensity = as.numeric(Global_intensity),
             Sub_metering_1 = as.numeric(Sub_metering_1),
             Sub_metering_2 = as.numeric(Sub_metering_2),
             Sub_metering_2 = as.numeric(Sub_metering_2)
      )
    cat('Done.\n')
    return(pwr)
}

# Plot 1
chart1 = function(pwr) {
hist(pwr$Global_active_power, 
     breaks=20, 
     col='red',
     xlim=c(0,8),
     ylim=c(0,1200),
     main='Global Active Power',
     xlab='Global Active Power (kilowatts)',
     xaxt='n',
     yaxt='n'
     )
axis(1, at = seq(0,6, by = 2),
     labels = seq(0,6, by = 2))
axis(2, at = seq(0,1200, by = 200),
     labels = seq(0,1200, by = 200),
     las=2)
}


# Plot 2
chart2 = function(pwr, ylab='Global Active Power (kilowatts)') {
plot(pwr$dt, pwr$Global_active_power,
     # xlim=c(min(pwr$dt),max(pwr$dt)),
     type='l',
     lty=1,
     xlab='',
     ylab=ylab,
     xaxt='n',
     yaxt='n'
)
axis.POSIXct(1,
     at=c(min(pwr$dt), median(pwr$dt)+60, max(pwr$dt)+60)
     , format='%a',
     # labels = c('Thur', 'Fri', 'Sat')
)
axis(2, 
     at = seq(0,6, by = 2),
     labels = seq(0,6, by = 2),
)
}

# Plot 3
chart3 = function(pwr, bty='o') {
plot(pwr$dt, pwr$Sub_metering_1,
     # xlim=c(min(pwr$dt),max(pwr$dt)),
     col='black',
     type='l',
     lty=1,
     xlab='',
     ylab='Energy sub metering',
     xaxt='n',
     yaxt='n'
)
lines(pwr$dt, pwr$Sub_metering_2,
    col='red'
)
lines(pwr$dt, pwr$Sub_metering_3,
      col='blue'
)

axis.POSIXct(1,
             at=c(min(pwr$dt), median(pwr$dt)+60, max(pwr$dt)+60)
             , format='%a',
             # labels = c('Thur', 'Fri', 'Sat')
)
axis(2, 
     at = seq(0,30, by = 10),
     
)

legend('topright', 
       legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), 
       col=c('black', 'red', 'blue'), 
       lwd=c(1,1,1),
       bty=bty,
       xjust=1
)
}


# Plot 4
chart5 = function(pwr) {
plot(pwr$dt, pwr$Voltage,
     # xlim=c(min(pwr$dt),max(pwr$dt)),
     type='l',
     lty=1,
     xlab='datetime',
     ylab='Voltage',
     xaxt='n',
     yaxt='n'
)
axis.POSIXct(1,
             at=c(min(pwr$dt), median(pwr$dt)+60, max(pwr$dt)+60)
             , format='%a',
             # labels = c('Thur', 'Fri', 'Sat')
)
axis(2, 
     at = seq(234,246, by = 4),
)
}

# Plot 5
chart6 = function(pwr) {
plot(pwr$dt, pwr$Global_reactive_power,
     # xlim=c(min(pwr$dt),max(pwr$dt)),
     type='l',
     lty=1,
     xlab='datetime',
     ylab='Global Reactive Power',
     xaxt='n',
     yaxt='n'
)
axis.POSIXct(1,
             at=c(min(pwr$dt), median(pwr$dt)+60, max(pwr$dt)+60)
             , format='%a',
             # labels = c('Thur', 'Fri', 'Sat')
)
axis(2, 
     at = seq(0,0.5, by = 0.1),
)
}

chart4 = function(pwr) {
  mfrow_og = par()$mfrow
  par(mfrow=c(2,2))
  chart2(pwr,ylab='Global Active Power')
  chart5(pwr)
  chart3(pwr, bty='n')
  chart6(pwr)
  par(mfrow = mfrow_og)
}

pwr = prepare_df()

pngfile = 'chart4.png'
interactive = dev.interactive()

if (!interactive) {
    png(pngfile, height=480, width=480)
}

chart4(pwr)
if (interactive) {
    dev.copy(png,pngfile,height=480, width=480)
}

dev.off()
