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


# Alpha and m for moving average 
a = c(.5, .25, .125);
m = 3;

# Loop over data
while (i < N) {


  # Training data
  td <-  d[1:i,  ];
  tx <- td[1:i,-D];
  ty <- td[1:i, D];
  ty <- as.factor(ty);

  # Append MA of last m values using a-weights
  for (k in 1:D) {
    tx[1,D-1+k] <- td[1,k];
  }
  if (i > 1) {
    # Time series
    T[[i]] <- ts(td);
    for (j in 2:i) {
      for (k in 1:D) {
        if (i > m) {
          for (l in 1:m) {
            tx[j,D-1+k] <- tx[j,D-1+k] + a[l]*td[j-1-l,k];
          }
          tx[j,D-1+k] <- tx[j,D-1+k] / m;
        } else {
          tx[j,D-1+k] <- td[j-1,k];
        }
      }
    }
  }

  # Target
  vd <- d[i+1,  ];
  vx <- d[i+1,-D];
  vy <- d[i+1, D];

  # Append last row to validation entry 
  for (k in 1:D) {
        if (i > m) {
          for (l in 1:m) {
            vx[1,D-1+k] <- d[i-l,D-1+k] + a[l]*d[i-l,D-1+k];
          }
            vx[1,D-1+k] <- d[i-l,D-1+k] / m
        } else {
          vx[1,D-1+k] <- d[i-1,k];
        }
  }
    
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

