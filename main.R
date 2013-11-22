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


# Loop over data
for (i in 1:N-1) {


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


  # Classifier models
  ann   <- neuralnet(formula=f, data=td, threshold=.5);
  bayes <- naiveBayes(x=tx, y=ty);
  knn   <- knn(td, vd, cl=ty);
  #svm   <- svm(tx, ty, gamma=10, type="C-classification");

}


# Animation of time series
#saveGIF({
#    ani.options(nmax = 22, loop=1, interval=.1)
#    for (i in 1:23) {
#      plot(T[[i]]);
#    }
#}, interval = 0.05, movie.name = "bm_demo.gif", ani.width = 600, ani.height = 600)

