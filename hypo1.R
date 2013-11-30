# Required libraries
library(e1071);
library(neuralnet);
library(kernlab);
library(animation);


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

# Training data
  td <-  d[1:i,  ];
  tx <- td[1:i,-D];
  ty <- td[1:i, D];
  ty <- as.factor(ty);

  # Append last class value to training data
  tx[1,D] <- td[1,D];
  if (i > 1) {
    # Time series
    T[[i]] <- ts(td);
    for (j in 2:i) {
      tx[j,D] <- td[j-1,D];
    }
  }

  # Target
  vd <- d[i+1,  ];
  vx <- d[i+1,-D];
  vy <- d[i+1, D];

  # Append last class to validation entry 
  vx[1, D] <- d[i, D];

  # Classifier models
  ann   <- neuralnet(formula=f, data=td, threshold=.5);
  bayes <- naiveBayes(x=tx, y=ty);
  knn   <- knn(td, vd, cl=ty);
  #svm   <- svm(tx, ty, gamma=10, type="C-classification");


  # Increment _hits for each correct classification
  if (predict(bayes, vx)==vy) {
    bayes_hits <- bayes_hits + 1;
  }
  if (knn==vy) {
    knn_hits <- knn_hits + 1;
  }
  #if (predict(svm, vx)==vy) {
  #  svm_hits <- svm_hits + 1;
  #}

  i<-i+1;

}

Nval <- N-init;
print(bayes_hits/Nval);
print(knn_hits/Nval);
print(svm_hits/Nval);

