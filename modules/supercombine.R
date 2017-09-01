####################################################################################
##Function to calculate all technique targets
##Test 1, 10 year of data, log classification, 12 bins, all cases considered for bin boundary
##
supercombine<-function(Datfile,
                       sourceDir = Input, destDir1 = Output, destDir2 = Output2, destDir3 = Output3)
{
    dir.create(destDir1, showWarnings = FALSE)
    dir.create(destDir2, showWarnings = FALSE)
    dir.create(destDir3, showWarnings = FALSE)
    
    
    inName <- paste0(sourceDir, "/", Datfile)
    outName1 <- paste0(destDir1, "/", Datfile)    
    outName2 <- paste0(destDir2, "/", Datfile)    
    outName3 <- paste0(destDir3, "/", Datfile)    
    
    X <- read.csv(inName)
    
        
        

        X <- X[1:10,]
        
        ##Common stats        
        outDF <- matrix(nrow = 13, ncol = 7)
        outDF <- as.data.frame(outDF, stringsAsFactors = F)
        colnames(outDF) <- c("month","mean","stdev","cv","cs","tm","mc")
        outDF[,"month"] <- c(0:12)  ##first row (i.e. 0) is annual total
        
        intermDF <- matrix(nrow = 10, ncol = 7)
        intermDF <- as.data.frame(intermDF, stringsAsFactors = F)
        colnames(intermDF) <- c("year","mean","stdev","cv","cs","tm","mc")
        intermDF[,"year"] <- c(1:10)
        
        for (j in 2:13)
        {
            outDF[j,2] <- round(mean(X[,j]),2)
            outDF[j,3] <- round(sd(X[,j]),2)
            outDF[j,4] <- round(outDF[j,3]/outDF[j,2],2)
            outDF[j,5] <- round(skewness(as.numeric(X[,j])),2)
            #     outDF[j-1,6] <- round((quantile(X[,j], 90)-quantile(X[,j], 10))/quantile(X[,j],50),2)
            
        }
        
        for (a in 1:10)
        {
            intermDF[a, "mean"] <- round(mean(as.numeric(X[a,2:13])),2)
            intermDF[a, "stdev"] <- round(sd(as.numeric(X[a,2:13])),2)
            intermDF[a, "cv"] <- round(intermDF[a,3]/intermDF[a,2],2)
            intermDF[a, "cs"] <- round(skewness(as.numeric(X[a,2:13])),2)
            #        intermDF[a,"tm] <- round((quantile(X[a,2:13], 90)-quantile(X[a,2:13], 10))/quantile(X[a,2:13],50),2)
            intermDF[a, "mc"] <- round(max(X[a,2:13])-min(X[a,2:13]),2)
        }
        
        outDF[1,"mean"] <- round(mean(intermDF[,"mean"]),2)
        outDF[1,"stdev"] <- round(mean(intermDF[,"stdev"]),2)
        outDF[1,"cv"] <- round(mean(intermDF[,"cv"]),2)
        outDF[1,"cs"] <- round(mean(intermDF[,"cs"]),2)
        outDF[1,"tm"] <- round(mean(intermDF[,"tm"]),2)
        
        
        outDF[1,"mc"] <- round(mean(intermDF[,"mc"]),2)
        
        write.table(outDF,outName2,sep=",",row.names=F)    
        ##Common stats end
        
        
        ##Colwell        
        years <- min(X$Year)
        yeare <- max(X$Year)
        yearr <- yeare-years+1
        
        interval <- 12
        
        base.value <- 1.235  ##1.235^11 = 10.19 
        
        bin <- matrix(0, ncol=14, nrow=interval)
        dimnames(bin) <- list(NULL,c("bin_size","jan","feb","mar","apr","may","jun",
                                     "jul","aug","sep","oct","nov","dec","whole"))
        
        bin[,"bin_size"] <- c("0",base.value^1, base.value^2, base.value^3, 
                              base.value^4, base.value^5, base.value^6,
                              base.value^7, base.value^8, base.value^9, base.value^10,
                              base.value^11)
        
        breaks = c("0","0.00001",base.value^1, base.value^2, base.value^3, 
                   base.value^4, base.value^5, base.value^6,
                   base.value^7, base.value^8, base.value^9, base.value^10,
                   base.value^11)
        
        output2 <- matrix(nrow=1,ncol=21)
        output2 <- as.data.frame(output2, row.names = NULL, stringsAsFactors = FALSE)
        colnames(output2) <- c("year","year_count","seasons",
                               "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                               "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
        
        output <- matrix(nrow=10,ncol=23)
        output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
        colnames(output) <- c("year","year_count","seasons",
                              "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                              "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom",
                              "lambda","tau")
        
        for (j in 1:10)
        {
            myDF <- X[-j,]
            
            jan_cut = cut(myDF$Jan, breaks, include.lowest=TRUE,right=TRUE)
            feb_cut = cut(myDF$Feb, breaks, include.lowest=TRUE,right=TRUE)
            mar_cut = cut(myDF$Mar, breaks, include.lowest=TRUE,right=TRUE)
            apr_cut = cut(myDF$Apr, breaks, include.lowest=TRUE,right=TRUE)
            may_cut = cut(myDF$May, breaks, include.lowest=TRUE,right=TRUE)
            jun_cut = cut(myDF$Jun, breaks, include.lowest=TRUE,right=TRUE)
            jul_cut = cut(myDF$Jul, breaks, include.lowest=TRUE,right=TRUE)
            aug_cut = cut(myDF$Aug, breaks, include.lowest=TRUE,right=TRUE)
            sep_cut = cut(myDF$Sep, breaks, include.lowest=TRUE,right=TRUE)
            oct_cut = cut(myDF$Oct, breaks, include.lowest=TRUE,right=TRUE)
            nov_cut = cut(myDF$Nov, breaks, include.lowest=TRUE,right=TRUE)
            dec_cut = cut(myDF$Dec, breaks, include.lowest=TRUE,right=TRUE)
            
            jan_freq = table(jan_cut)
            feb_freq = table(feb_cut)
            mar_freq = table(mar_cut)
            apr_freq = table(apr_cut)
            may_freq = table(may_cut)
            jun_freq = table(jun_cut)
            jul_freq = table(jul_cut)
            aug_freq = table(aug_cut)
            sep_freq = table(sep_cut)
            oct_freq = table(oct_cut)
            nov_freq = table(nov_cut)
            dec_freq = table(dec_cut)
            
            bin[,"jan"] <- jan_freq
            bin[,"feb"] <- feb_freq
            bin[,"mar"] <- mar_freq
            bin[,"apr"] <- apr_freq
            bin[,"may"] <- may_freq
            bin[,"jun"] <- jun_freq
            bin[,"jul"] <- jul_freq
            bin[,"aug"] <- aug_freq
            bin[,"sep"] <- sep_freq
            bin[,"oct"] <- oct_freq
            bin[,"nov"] <- nov_freq
            bin[,"dec"] <- dec_freq
            
            newbin <- as.numeric(bin[,2:13])
            newbin2 <- matrix(newbin,nrow=interval,ncol=12)
            newbin3 <- matrix(nrow=interval,ncol=1)
            for (n in 1:interval)
            {
                newbin3[n,] = sum(newbin2[n,1:12])
            }
            newbin <- cbind(newbin2,newbin3)
            
            col_sum <- sum(table(myDF$Jan))
            whole_sum <- col_sum*12
            
            rowsumax <- max(newbin[,13])
            
            newbin4 <- as.data.frame(newbin[,1:12])
            colnames(newbin4) <- c("jan","feb","mar","apr","may","jun",
                                   "jul","aug","sep","oct","nov","dec")
            #    colmax <- matrix(nrow=1,ncol = 12)
            lambda <- (sum(colMax(newbin4))-rowsumax)/(whole_sum - rowsumax)
            tau <- ((1/col_sum)*col_sum*12 - (1/whole_sum)*whole_sum)/
                (whole_sum - 1/whole_sum * whole_sum)
            
            #uncertainty with respect to time H(X)
            HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
            
            #uncertainty with respect to state H(Y)
            V1 <- newbin[,13]/whole_sum
            V2 <- log10(newbin[,13]/whole_sum)
            for (k in 1:length(V2))
            {
                if(is.finite(V2[k])==F) V2[k] <- 0
                else V2[k] <- V2[k]
            }
            
            HofY <- -sum(V1*V2)
            
            #uncertainty with respect to interaction of time and state, H(XY)
            M1 <- newbin[1:interval,1:12]/whole_sum
            M2 <- log10(M1)
            for (k in 1:length(M2))
            {
                if(is.finite(M2[k])==F) M2[k] <- 0
                else M2[k] <- M2[k]
            }
            
            HofXY <- -sum(M1*M2)
            
            #Conditional uncertainty with regard to state, with time given, HXofY
            HXofY <- HofXY - HofX
            s <- interval
            t <- 12
            
            #predictability (P), constancy(C) and contingency (M)
            P <- 1-(HXofY/log10(s))
            C <- 1-(HofY/log10(s))
            M <- (HofX+HofY-HofXY)/log10(s)
            CoverP <- C/P
            MoverP <- M/P
            
            #mutual information, I(XY)
            IofXY <- HofY - HXofY
            
            #deviation from homogeneity of the columns of the matrix for constancy, GC
            GC <- 2*whole_sum*(log(s)-HofY)
            C_free <- s-1
            
            #deviation from homogeneity of the columns of the matrix for contingency, GM
            GM <- 2*whole_sum*(HofX+HofY-HofXY)
            M_free <- (s-1)*(t-1)
            
            #deviation from homogeneity of the columns of the matrix for predictability, GP
            GP <- GM + GC
            P_free <- (s-1)*t
            
            output[j,"year"] <- yeare
            output[j,"year_count"] <- col_sum
            output[j,"seasons"] <- whole_sum
            output[j,"HofX"] <- round(HofX,3)
            output[j,"HofY"] <- round(HofY,3)
            output[j,"HofXY"] <- round(HofXY,3)
            output[j,"HXofY"] <- round(HXofY,3)
            output[j,"s"] <- s
            output[j,"t"] <- t
            output[j,"P"] <- round(P,3)
            output[j,"C"] <- round(C,3)
            output[j,"M"] <- round(M,3)
            output[j,"CbyP"] <- round(CoverP,3)
            output[j,"MbyP"] <- round(MoverP,3)
            output[j,"Mutual"] <- round(IofXY,3)
            output[j,"GC"] <- round(GC,3)
            output[j,"C_freedom"] <- C_free
            output[j,"GM"] <- round(GM,3)
            output[j,"M_freedom"] <- M_free
            output[j,"GP"] <- round(GP,3)
            output[j,"P_freedom"] <- P_free
            output[j,"tau"] <- tau
            output[j,"lambda"] <- lambda
            
        }
        write.table(output,outName,sep=",",row.names=F)
        
        newDF <- matrix(ncol= 4, nrow = 120)
        newDF <- as.data.frame(newDF, stringsAsFactors = F)
        colnames(newDF) <- c("year","month","count","value")
        
        newDF[,"year"] <- rep(1:10, each = 12)
        newDF[,"month"] <- rep(c(1:12),10)
        newDF[,"count"] <- c(1:120)
        newDF <- newDF[order(newDF$month, newDF$year),]
        
        myvector <- as.vector(as.matrix((X[,2:13])))
        newDF[,"value"] <- myvector
        
        newDF <- newDF[order(newDF$year),]
        
        
        pdf(paste(outName3, "_periodicity_analyses.pdf", sep=""))
        
        ##autocorrelation analysis    
        acf(newDF[,"value"], lag.max = 120, plot = T, 
            main = paste("Case ", thisFile, " autocorrelation analysis", sep=""))
        
        ##partial autocorrelation analysis    
        pacf(newDF[,"value"], lag.max = 120, plot = T, 
             main = paste("Case ", thisFile, "partial autocorrelation analysis", sep=""))
        
        ##correlogram analysis    
        corrgram(X[,2:13], plot = T, main = "correlogram analysis")
        
        ##spectral analysis
        #    k <- kernel("daniell",1)
        #    specvalues = spec.pgram(newDF[,"value"],k,taper=0, log="no")
        
        specvalues = spec.pgram(newDF[,"value"],taper=0, log="no", main = "spectral analysis")
        out.new <- as.data.frame(cbind(specvalues$freq, specvalues$spec))
        
        colnames(out.new) <- c("freq","spec")
        out.new <- out.new[order(out.new$spec, decreasing=T),]
        
        Corner_text(text = paste("max freq = ", round(out.new[1,1],2), sep=""))
        
        ##wavelet analysis
        wave.out <- morlet(y1 = newDF[,"value"], x1 = newDF[,"count"], 
                           p2 = 9, dj = 0.1, siglvl = 0.99)
        wavelet.plot(wave.out)
        title("wavelet analysis")
        
        ##Fourier analysis (FFT)
        #    fft(newDF[,"value"], inverse = F)
        
        ##Autoregressive-moving-average model
        #    amaDF <- armaFit(formula = ~arma(2,2), data = newDF[,"value"])
        
        amaDF <- arima(newDF[,"value"]) 
        tsdiag(amaDF)
        title("Autoregressive-moving-average model")
        
        dev.off()

}

