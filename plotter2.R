datafiles <- list( "diabetes.csv.ann", "diabetes.csv.bayes", "diabetes.csv.cart", "diabetes.csv.knn", "diabetes.csv.svm" );

for (datafile in datafiles) {
 
  d <- read.table(datafile, sep=",", header=TRUE);

  file <- gsub(" ", "", paste(datafile, ".jpg"));
  jpeg(file);
  plot(d);
  dev.off();

}
