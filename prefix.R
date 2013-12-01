# Required libraries
library(e1071);
library(neuralnet);
library(kernlab);
library(animation);

datafiles <- list.files(pattern="*csv", full.names=TRUE)
for (datafile in datafiles) {


# Read data 
d <- read.table(datafile, sep=",", header=TRUE);
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


# Vector of time series
T <- vector(mode="list", length=N-1);


# Need at least 4 training entries for Naive Bayes
init=4;
i=init;


# Accuracy; how many correctly classified
bayes_hits <- 0;
knn_hits   <- 0;
svm_hits   <- 0;


# Loop over data
while (i < N) {


  # Training data
  td <-  d[1:i,  ];
  tx <- td[1:i,-D];
  ty <- td[1:i, D];
  ty <- as.factor(ty);

