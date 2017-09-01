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
    
    prcptmp_spr<-dd[dd$month >= 10
                    & dd$month <= 12
                    & dd$prcp>=0,"prcp"]
    
    prcptmp_sum<-dd[dd$month >= 1
                    & dd$month <= 3
                    & dd$prcp>=0,"prcp"]
    
    prcptmp_aut<-dd[dd$month >= 4
                    & dd$month <= 6
                    & dd$prcp>=0,"prcp"]
    
    prcptmp_win<-dd[dd$month >= 7
                    & dd$month <= 9
                    & dd$prcp>=0,"prcp"]
    
    len_spr<-length(prcptmp_spr)
    len_sum<-length(prcptmp_sum)
    len_aut<-length(prcptmp_aut)
    len_win<-length(prcptmp_win)
    
    
    prcp05spr<-quantile(prcptmp_spr, 0.05)
    prcp05sum<-quantile(prcptmp_sum, 0.05)
    prcp05aut<-quantile(prcptmp_aut, 0.05)
    prcp05win<-quantile(prcptmp_win, 0.05)
    
    prcp01spr<-quantile(prcptmp_spr, 0.01)
    prcp01sum<-quantile(prcptmp_sum, 0.01)
    prcp01aut<-quantile(prcptmp_aut, 0.01)
    prcp01win<-quantile(prcptmp_win, 0.01)
    
    yeare <- max(dd$year)
    years <- min(dd$year)
    
    
    ys<-yeare-years+1
    
    dp<-matrix(0,ys,9)
    dimnames(dp)<-list(NULL,c("year","r05p_spr","r05p_sum","r05p_aut","r05p_win",
                              "r01p_spr","r01p_sum","r01p_aut","r01p_win"))
    dp <- as.data.frame(dp)
    dp[,"year"]<-years:yeare
    for(i in years:yeare)
    {
        dp[dp$year == i,"r05p_spr"]<-length(dd[dd$year == i & dd$month >= 7 & dd$month<= 9 & 
                                               dd$prcp <= prcp05spr,"prcp"])
        dp[dp$year == i,"r05p_sum"]<-length(dd[dd$year == i & dd$month >= 10 & dd$month<= 12 & 
                                               dd$prcp <= prcp05sum,"prcp"])
        dp[dp$year == i,"r05p_aut"]<-length(dd[dd$year == i & dd$month >= 1 & dd$month<= 3 & 
                                               dd$prcp <= prcp05aut,"prcp"])
        dp[dp$year == i,"r05p_win"]<-length(dd[dd$year == i & dd$month >= 4 & dd$month<= 6 & 
                                               dd$prcp <= prcp05win,"prcp"])
        
        dp[dp$year == i,"r01p_spr"]<-length(dd[dd$year == i & dd$month >= 7 & dd$month<= 9 & 
                                               dd$prcp <= prcp01spr,"prcp"])
        dp[dp$year == i,"r01p_sum"]<-length(dd[dd$year == i & dd$month >= 10 & dd$month<= 12 & 
                                               dd$prcp <= prcp01sum,"prcp"])
        dp[dp$year == i,"r01p_aut"]<-length(dd[dd$year == i & dd$month >= 1 & dd$month<= 3 & 
                                               dd$prcp <= prcp01aut,"prcp"])
        dp[dp$year == i,"r01p_win"]<-length(dd[dd$year == i & dd$month >= 4 & dd$month<= 6 &
                                               dd$prcp <= prcp01win,"prcp"])
    }
    
    dp<-as.data.frame(dp)
   
    write.csv(dp,outName,row.names=F)
}
