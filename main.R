# Required libraries
library(e1071);
library(neuralnet);
library(kernlab);


# Read data 
d <- read.table("example.csv", sep=",", header=TRUE);
D <- ncol(d);
N <- nrow(d);


# Formula
f <- names(d)[D];
f <- paste(f, " ~ ");
f <- paste(f, names(d)[1]);
if (D > 3) {
  for (i in 3:D-1) {
    f <- paste(f, "+");
    f <- paste(f, names(d)[i]);
  }
}
f <- as.formula(f);


for (i in 2:N) {

  # Training data
  td <-  d[1:i-1,];
  tx <- td[1:i-1,-D];
  ty <- as.factor(td[1:i-1,D]);
  

  # Target
  vd <- d[i,];
  vx <- d[i,-D];
  vy <- d[i,D];
  
  
  # Classifier models
  ann   <- neuralnet(formula=f, data=td, threshold=.5);
  bayes <- naiveBayes(x=tx, y=ty);
  knn   <- knn(td, vd, cl=ty);
  #svm   <- svm(tx, ty, gamma=10, type="C-classification");
  i;

}
