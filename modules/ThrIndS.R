##############################################################################################################
ThrIndS<-function(Datfile,
                  sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) 
{
    dir.create(destDir, showWarnings = FALSE)

    inName <- paste0(sourceDir, "/", Datfile)
    outName <- paste0(destDir, "/", Datfile)   
    
    dd <- read.csv(inName)
    colnames(dd)<-c("date","year","month","day","prcp")
    dd <- dd[,2:5]
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) {
        dddd<-dddl
    } else {
        dddd<-ddd
    }
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
        if (leapyear(year)) {
            dddd1 <- dddl 
        } else {
            dddd1 <- ddd
        }
        
        dddd1[,"year"]<-year
        
        if (year!=years) 
            dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)
    
    startpoint <- startyear-1
    
    endpoint <- endyear+1
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    prcptmp_spr<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & dd$month >= 3
                    & dd$month <= 5
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_spr<-prcptmp_spr[is.na(prcptmp_spr)==F]
    
    prcptmp_sum<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & dd$month >= 6
                    & dd$month <= 8
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_sum<-prcptmp_sum[is.na(prcptmp_sum)==F]
    
    prcptmp_aut<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & dd$month >= 9
                    & dd$month <= 11
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_aut<-prcptmp_aut[is.na(prcptmp_aut)==F]
    
    
    prcptmp_win<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & (dd$month == 12 | dd$month == 1 | dd$month == 2)
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_win<-prcptmp_win[is.na(prcptmp_win)==F]
    
    
    len_spr<-length(prcptmp_spr)
    len_sum<-length(prcptmp_sum)
    len_aut<-length(prcptmp_aut)
    len_win<-length(prcptmp_win)
    
    
    prcp05spr<-percentile(len_spr,prcptmp_spr,0.05)
    prcp01spr<-percentile(len_spr,prcptmp_spr,0.01)
    prcp05sum<-percentile(len_sum,prcptmp_sum,0.05)
    prcp01sum<-percentile(len_sum,prcptmp_sum,0.01)
    prcp05aut<-percentile(len_aut,prcptmp_aut,0.05)
    prcp01aut<-percentile(len_aut,prcptmp_aut,0.01)
    prcp05win<-percentile(len_win,prcptmp_win,0.05)
    prcp01win<-percentile(len_win,prcptmp_win,0.01)
    
    ys<-yeare-years+1
    
    dp<-matrix(0,ys,9)
    dimnames(dp)<-list(NULL,c("year","r05p_spr","r05p_sum","r05p_aut","r05p_win",
                              "r01p_spr","r01p_sum","r01p_aut","r01p_win"))
    dp[,"year"]<-years:yeare
    for(i in years:yeare)
    {
        dp[(i-years+1),"r05p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                               dd$prcp <= prcp05spr,"prcp"],na.rm=T)
        dp[(i-years+1),"r05p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                               dd$prcp <= prcp05sum,"prcp"],na.rm=T)
        dp[(i-years+1),"r05p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                               dd$prcp <= prcp05aut,"prcp"],na.rm=T)
        dp[(i-years+1),"r05p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                               dd$prcp <= prcp05win,"prcp"],na.rm=T)
        
        dp[(i-years+1),"r01p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                               dd$prcp <= prcp01spr,"prcp"],na.rm=T)
        dp[(i-years+1),"r01p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                               dd$prcp <= prcp01sum,"prcp"],na.rm=T)
        dp[(i-years+1),"r01p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                               dd$prcp <= prcp01aut,"prcp"],na.rm=T)
        dp[(i-years+1),"r01p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                               dd$prcp <= prcp01win,"prcp"],na.rm=T)
    }
    
    dp<-as.data.frame(dp)
    colnames(dp) <- c("year","r05p_aut","r05p_win","r05p_spr","r05p_sum",
                      "r01p_aut","r01p_win","r01p_spr","r01p_sum")
   
    write.csv(dp,outName,row.names=F)
}
