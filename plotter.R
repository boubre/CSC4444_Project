datafiles <- list.files(pattern="*csv2$", full.names=TRUE);
  for (datafile in datafiles) {

    outfile <- gsub("./","",gsub(" ","",paste(datafile,".jpg")));
    
    a <- read.table(datafile, sep=",", header=TRUE);
    a <- ts(a);
    
    jpeg(outfile);
    plot(a);
    dev.off();
  }
