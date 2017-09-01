##############################################################################################################
## Filter data for Year range > 10 for long-term trend analysis 
## Also check for missing data issue, missing data should not be > 80%
Missing_check<-function(Datfile, 
                        sourceDir = DAILY.DATA.DIRECTORY, 
                        destDir = DAILY.OUTPUT.DIRECTORY)
{
    # sorting out file names
    dir.create(destDir, showWarnings = FALSE)
    inName <- paste0(sourceDir, "/", Datfile)
    outName <- paste0(destDir, "/", Datfile)
    
    # read in file
    dd <- read.csv(inName)
    dd$date <- as.Date(paste(dd$Year, dd$Month, dd$Day, sep="-"),
                       format = "%Y-%m-%d")
    colnames(dd) <- c("ID", "BM_number", "Year", "Month", "Day", "Rainfall",
                      "Period", "Quality", "Date")

    # drop quality = N
    dd2 <- subset(dd, Quality == "Y")
    
    # create continuous time series
    t.series <- seq.Date(from = min(dd2$Date), to = max(dd2$Date),
                         by = "day")
    
    # check for missing data % before gap filling
    target <- length(t.series)
    d2 <- dd2[complete.cases(dd2$Rainfall),]
    reality <- nrow(d2)
    miss_percent <- round((target - reality) / target * 100, 2)
    print(paste0("missing data is ", miss_percent, "% before gap filling"))
    
    # create outout df
    out <- data.frame(t.series, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("date", "Year", "Month", "Day", "Rainfall", "Period", "Quality")
    
    # assign data onto out dataframe
    out$Rainfall <- dd2$Rainfall[match(out$date, dd2$Date)]
    out$Period <- dd2$Period[match(out$date, dd2$Date)]
    out$Quality <- dd2$Quality[match(out$date, dd2$Date)]
    
    out$Year <- as.numeric(format(out$date, "%Y"))
    out$Month <- as.numeric(format(out$date, "%m"))
    out$Day <- as.numeric(format(out$date, "%d"))

    ## filling gaps when cumulative rainfall measure occurs
    # finding location of n day cumulative rainfall measures
    loc2 <- which(grepl(2, out$Period))
    loc3 <- which(grepl(3, out$Period))
    loc4 <- which(grepl(4, out$Period))
    loc5 <- which(grepl(5, out$Period))
    loc6 <- which(grepl(6, out$Period))
    loc7 <- which(grepl(7, out$Period))
    #loc8 <- which(grepl(8, out$Period))
    loc9 <- which(grepl(9, out$Period))
    #loc10 <- which(grepl(10, out$Period))
    loc11 <- which(grepl(11, out$Period))
    #loc12 <- which(grepl(12, out$Period))
    loc13 <- which(grepl(13, out$Period))
    #loc14 <- which(grepl(14, out$Period))
    #loc15 <- which(grepl(15, out$Period))
    loc16 <- which(grepl(16, out$Period))
    
    # filling gaps - 2 day
    for (i in loc2) {
        avg <- out[i, "Rainfall"] / 2
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
    }
    
    # filling gaps - 3 day
    for (i in loc3) {
        avg <- out[i, "Rainfall"] / 3
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
    }
    
    # filling gaps - 4 day
    for (i in loc4) {
        avg <- out[i, "Rainfall"] / 4
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
    }
    
    # filling gaps - 5 day
    for (i in loc5) {
        avg <- out[i, "Rainfall"] / 5
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
        out[i-4, "Rainfall"] <- avg
    }
    
    # filling gaps - 6 day
    for (i in loc6) {
        avg <- out[i, "Rainfall"] / 6
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
        out[i-4, "Rainfall"] <- avg
        out[i-5, "Rainfall"] <- avg
    }
    
    # filling gaps - 7 day
    for (i in loc7) {
        avg <- out[i, "Rainfall"] / 7
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
        out[i-4, "Rainfall"] <- avg
        out[i-5, "Rainfall"] <- avg
        out[i-6, "Rainfall"] <- avg
    }
    
    # filling gaps - 9 day
    for (i in loc9) {
        avg <- out[i, "Rainfall"] / 9
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
        out[i-4, "Rainfall"] <- avg
        out[i-5, "Rainfall"] <- avg
        out[i-6, "Rainfall"] <- avg
        out[i-7, "Rainfall"] <- avg
        out[i-8, "Rainfall"] <- avg
    }
    
    # filling gaps - 11 day
    for (i in loc11) {
        avg <- out[i, "Rainfall"] / 11
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
        out[i-4, "Rainfall"] <- avg
        out[i-5, "Rainfall"] <- avg
        out[i-6, "Rainfall"] <- avg
        out[i-7, "Rainfall"] <- avg
        out[i-8, "Rainfall"] <- avg
        out[i-9, "Rainfall"] <- avg
        out[i-10, "Rainfall"] <- avg
    }
    
    # filling gaps - 13 day
    for (i in loc13) {
        avg <- out[i, "Rainfall"] / 13
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
        out[i-4, "Rainfall"] <- avg
        out[i-5, "Rainfall"] <- avg
        out[i-6, "Rainfall"] <- avg
        out[i-7, "Rainfall"] <- avg
        out[i-8, "Rainfall"] <- avg
        out[i-9, "Rainfall"] <- avg
        out[i-10, "Rainfall"] <- avg
        out[i-11, "Rainfall"] <- avg
        out[i-12, "Rainfall"] <- avg
    }
    
    # filling gaps - 16 day
    for (i in loc16) {
        avg <- out[i, "Rainfall"] / 16
        out[i, "Rainfall"] <- avg
        out[i-1, "Rainfall"] <- avg
        out[i-2, "Rainfall"] <- avg
        out[i-3, "Rainfall"] <- avg
        out[i-4, "Rainfall"] <- avg
        out[i-5, "Rainfall"] <- avg
        out[i-6, "Rainfall"] <- avg
        out[i-7, "Rainfall"] <- avg
        out[i-8, "Rainfall"] <- avg
        out[i-9, "Rainfall"] <- avg
        out[i-10, "Rainfall"] <- avg
        out[i-11, "Rainfall"] <- avg
        out[i-12, "Rainfall"] <- avg
        out[i-13, "Rainfall"] <- avg
        out[i-14, "Rainfall"] <- avg
        out[i-15, "Rainfall"] <- avg
    }
    
    
    # check for missing data % after gap filling
    target <- length(t.series)
    d2 <- out[complete.cases(out$Rainfall),]
    reality <- nrow(d2)
    miss_percent <- round((target - reality) / target * 100, 2)
    print(paste0("missing data is ", miss_percent, "% after gap filling"))
    
    
    # exlude year 1882, 1883, 1884 because no data available
    out2 <- subset(out, Year > 1884)
    target <- length(out2$date)
    d2 <- out2[complete.cases(out2$Rainfall),]
    reality <- nrow(d2)
    miss_percent <- round((target - reality) / target * 100, 2)
    print(paste0("missing data is ", miss_percent, "% after excluding year 1982-1984"))
    
    # location of the remaining missing data
    loc <- which(is.na(out2$Rainfall))
    cons <- diff(loc)
    cont <- rle(diff(loc))
    
    # fill 1 month missing values
    subDF <- out2[4718:4748,]    
    m <- unique(subDF$Month)
    
    # compute median value for each day within this month
    t <- c(1:31) 
    #temp <- data.frame(t, NA,NA)
    #colnames(temp) <- c("day", "median", "mean")
    #for (i in t) {
    #    temp[temp$day == i, "median"] <- median(out2[out2$Month == m & out2$Day == i, "Rainfall"], na.rm=T)
    #    temp[temp$day == i, "mean"] <- mean(out2[out2$Month == m & out2$Day == i, "Rainfall"], na.rm=T)
    #}
    
    # get previous december values
    prev.mt <- subset(out2, Month == m)
    v.list <- prev.mt[!is.na(prev.mt$Rainfall), "Rainfall"]
    
    # computing frequency
    rg <- range(v.list)
    
    # insert a conditional check to make sure breaks have values
    if(rg[2]-rg[1] <= 1) {
        by.value <- 0.01
    } else if (rg[2] - rg[1] <= 100) {
        by.value <- 0.1
    } else if (rg[2] - rg[1] <= 10000) {
        by.value <- 10
    }
    
    s.value <- rg[1] - 0.0001
    e.value <- rg[2] + by.value - 0.0001
    
    breaks <- seq(s.value, e.value, by = by.value)
    fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
    precip.cut <- cut(v.list, breaks)
    precip.freq <- table(precip.cut)
    
    # computing probability
    myprob <- data.frame(precip.freq, NA)
    colnames(myprob) <- c("range", "freq", "prob")
    csum <- sum(myprob$freq)
    myprob$prob <- myprob$freq/csum
    
    # computing selection number list
    mynumbers <- fill.list[1:length(fill.list)-1]
    
    # sampling from the number list following distribution probability
    d <- sample(mynumbers, size=length(t), replace=T, prob=myprob$prob)
    
    out2[4718:4748,"Rainfall"] <- d
    
    # fill remaining missing values
    loc <- which(is.na(out2$Rainfall))
    cons <- diff(loc)
    cont <- rle(diff(loc))
    
    subDF <- out2[45260:45290,]    
    m <- unique(subDF$Month)
    l <- length(subDF$date)
    
    # get previous december values
    prev.mt <- subset(out2, Month == m)
    v.list <- prev.mt[!is.na(prev.mt$Rainfall), "Rainfall"]
    
    # computing frequency
    rg <- range(v.list)
    
    # insert a conditional check to make sure breaks have values
    if(rg[2]-rg[1] <= 1) {
        by.value <- 0.01
    } else if (rg[2] - rg[1] <= 100) {
        by.value <- 0.1
    } else if (rg[2] - rg[1] <= 10000) {
        by.value <- 10
    }
    
    s.value <- rg[1] - 0.0001
    e.value <- rg[2] + by.value - 0.0001
    
    breaks <- seq(s.value, e.value, by = by.value)
    fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
    precip.cut <- cut(v.list, breaks)
    precip.freq <- table(precip.cut)
    
    # computing probability
    myprob <- data.frame(precip.freq, NA)
    colnames(myprob) <- c("range", "freq", "prob")
    csum <- sum(myprob$freq)
    myprob$prob <- myprob$freq/csum
    
    # computing selection number list
    mynumbers <- fill.list[1:length(fill.list)-1]
    
    # sampling from the number list following distribution probability
    d <- sample(mynumbers, size=l, replace=T, prob=myprob$prob)
    
    out2[45260:45290,"Rainfall"] <- d
    
    # fill remaining missing values
    loc <- which(is.na(out2$Rainfall))
    cons <- diff(loc)
    cont <- rle(diff(loc))
    
    subDF <- out2[40179:40183,]    
    m <- unique(subDF$Month)
    l <- length(subDF$date)
    
    # get previous december values
    prev.mt <- subset(out2, Month == m)
    v.list <- prev.mt[!is.na(prev.mt$Rainfall), "Rainfall"]
    
    # computing frequency
    rg <- range(v.list)
    
    # insert a conditional check to make sure breaks have values
    if(rg[2]-rg[1] <= 1) {
        by.value <- 0.01
    } else if (rg[2] - rg[1] <= 100) {
        by.value <- 0.1
    } else if (rg[2] - rg[1] <= 10000) {
        by.value <- 10
    }
    
    s.value <- rg[1] - 0.0001
    e.value <- rg[2] + by.value - 0.0001
    
    breaks <- seq(s.value, e.value, by = by.value)
    fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
    precip.cut <- cut(v.list, breaks)
    precip.freq <- table(precip.cut)
    
    # computing probability
    myprob <- data.frame(precip.freq, NA)
    colnames(myprob) <- c("range", "freq", "prob")
    csum <- sum(myprob$freq)
    myprob$prob <- myprob$freq/csum
    
    # computing selection number list
    mynumbers <- fill.list[1:length(fill.list)-1]
    
    # sampling from the number list following distribution probability
    d <- sample(mynumbers, size=l, replace=T, prob=myprob$prob)
    
    out2[40179:40183,"Rainfall"] <- d
    
    # fill remaining missing values
    loc <- which(is.na(out2$Rainfall))
    cons <- diff(loc)
    cont <- rle(diff(loc))
    
    subDF <- out2[40186:40193,]    
    m <- unique(subDF$Month)
    l <- length(subDF$date)
    
    # get previous december values
    prev.mt <- subset(out2, Month == m)
    v.list <- prev.mt[!is.na(prev.mt$Rainfall), "Rainfall"]
    
    # computing frequency
    rg <- range(v.list)
    
    # insert a conditional check to make sure breaks have values
    if(rg[2]-rg[1] <= 1) {
        by.value <- 0.01
    } else if (rg[2] - rg[1] <= 100) {
        by.value <- 0.1
    } else if (rg[2] - rg[1] <= 10000) {
        by.value <- 10
    }
    
    s.value <- rg[1] - 0.0001
    e.value <- rg[2] + by.value - 0.0001
    
    breaks <- seq(s.value, e.value, by = by.value)
    fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
    precip.cut <- cut(v.list, breaks)
    precip.freq <- table(precip.cut)
    
    # computing probability
    myprob <- data.frame(precip.freq, NA)
    colnames(myprob) <- c("range", "freq", "prob")
    csum <- sum(myprob$freq)
    myprob$prob <- myprob$freq/csum
    
    # computing selection number list
    mynumbers <- fill.list[1:length(fill.list)-1]
    
    # sampling from the number list following distribution probability
    d <- sample(mynumbers, size=l, replace=T, prob=myprob$prob)
    
    out2[40186:40193,"Rainfall"] <- d
    
    # fill remaining missing values
    loc <- which(is.na(out2$Rainfall))
    cons <- diff(loc)
    cont <- rle(diff(loc))
    
    subDF <- out2[40198:40207,]    
    m <- unique(subDF$Month)
    l <- length(subDF$date)
    
    # get previous december values
    prev.mt <- subset(out2, Month == m)
    v.list <- prev.mt[!is.na(prev.mt$Rainfall), "Rainfall"]
    
    # computing frequency
    rg <- range(v.list)
    
    # insert a conditional check to make sure breaks have values
    if(rg[2]-rg[1] <= 1) {
        by.value <- 0.01
    } else if (rg[2] - rg[1] <= 100) {
        by.value <- 0.1
    } else if (rg[2] - rg[1] <= 10000) {
        by.value <- 10
    }
    
    s.value <- rg[1] - 0.0001
    e.value <- rg[2] + by.value - 0.0001
    
    breaks <- seq(s.value, e.value, by = by.value)
    fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
    precip.cut <- cut(v.list, breaks)
    precip.freq <- table(precip.cut)
    
    # computing probability
    myprob <- data.frame(precip.freq, NA)
    colnames(myprob) <- c("range", "freq", "prob")
    csum <- sum(myprob$freq)
    myprob$prob <- myprob$freq/csum
    
    # computing selection number list
    mynumbers <- fill.list[1:length(fill.list)-1]
    
    # sampling from the number list following distribution probability
    d <- sample(mynumbers, size=l, replace=T, prob=myprob$prob)
    
    out2[40198:40207,"Rainfall"] <- d
    
    
    # fill remaining missing values
    loc <- which(is.na(out2$Rainfall))
    
    
    for (i in 1:length(loc)) {
        fill.value <- mean(out2[loc[i]-1, "Rainfall"], out2[loc[i]-2, "Rainfall"], 
                           out2[loc[i]-3, "Rainfall"], out2[loc[i]+1, "Rainfall"], 
                           out2[loc[i]+2, "Rainfall"], out2[loc[i]+3, "Rainfall"], na.rm=T)
        out2[loc[i], "Rainfall"] <- fill.value
        
    }

    # check for missing data % after gap filling
    target <- length(out2$date)
    d2 <- out2[complete.cases(out2$Rainfall),]
    reality <- nrow(d2)
    miss_percent <- round((target - reality) / target * 100, 2)
    print(paste0("missing data is ", miss_percent, "% after gap filling"))

    # remove un-necessary columns
    out2$Period <- NULL
    out2$Quality <- NULL
    
    # save file
    write.csv(out2, outName, row.names=F)

}
