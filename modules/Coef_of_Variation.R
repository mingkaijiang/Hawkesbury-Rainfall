##############################################################################################################
##Plot annual trend of coefficient of variation and the associated focal year onto the same graph
CoefPlot<-function(Datfile, sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{

    Datfile <- "IDCJAC0009_067021_1800_Data.csv"
    dir.create(destDir, showWarnings = FALSE)
    inName <- paste0(sourceDir, "/", Datfile)
    outName <- paste0(destDir, "/", Datfile)

    ### Create a outdf to store all data in one file
    outDF <- matrix(ncol=6, nrow=1)
    outDF <- as.data.frame(outDF)
    colnames(outDF) <- c("Start_yr", "End_yr", "Yr_range",
                         "Daily_stdev", 
                         "Daily_mean", 
                         "Daily_coef_var")


        dd <- read.csv(inName)
        
        outDF[i,"Start_yr"] <- min(as.numeric(dd$Year))
        outDF[i,"End_yr"] <- max(as.numeric(dd$Year))
        
        outDF[i,"Daily_stdev"] <- sd(dd$value,na.rm=T)
        outDF[i,"Daily_mean"] <- mean(dd$value,na.rm=T)
        

    
    outDF[,"Yr_range"] <- outDF[,"End_yr"] - outDF[,"Start_yr"]
    coef_var <- outDF$Daily_stdev/outDF$Daily_mean
    outDF[,"Daily_coef_var"] <- round(coef_var,digits=2)
    
    write.csv(outDF,paste0(destDir, "/daily_coef_var.csv"))
    
        

}
