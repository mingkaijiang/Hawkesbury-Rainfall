##############################################################################################################
## Compute wavelet analysis and auto-correlation analysis
wavelet_analysis<-function(Datfile,
                                  sourceDir = DAILY.DATA.DIRECTORY, 
                                  destDir = DAILY.OUTPUT.DIRECTORY) {

    # prepare file list
    dir.create(destDir, showWarnings = FALSE)

    inName <- paste0(sourceDir, "/", Datfile)
    outName <- paste0(destDir, "/", Datfile)
    outName <- gsub(".csv", ".pdf", outName)
    
    # read in file and prepare the df
    dd <- read.csv(inName)
    
    # wavelet analysis
    pdf(outName)
    wave.out <- morlet(y1 = dd$ann, x1 = dd$year, 
                       p2 = 9, dj = 0.1, siglvl = 0.99)
    wavelet.plot(wave.out)
    dev.off()

}
