datafiles <- list.files(pattern="*csv1$", full.names=TRUE);
  for (datafile in datafiles) {

  compfiles <- gsub("1","2",datafile);
    for (compfile in compfiles) {
  
    outfile <- gsub("./","",gsub(" ","",paste(datafile,"-2.jpg")));
    
    a <- read.table(datafile, sep=",", header=TRUE);
    b <- read.table(compfile, sep=",", header=TRUE);
    a <- ts(a);
    b <- ts(b);
    c <- b/a;
    
    jpeg(outfile);
    plot(c);
    dev.off();
  }
}
