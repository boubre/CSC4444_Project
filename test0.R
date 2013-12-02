num=0;
# Required libraries
library(rpart);
library(rpart.plot);
library(e1071);
library(neuralnet);
library(kernlab);
library(animation);

datafiles <- list.files(pattern="*csv$", full.names=TRUE)
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
init=20;
i=init;


# Accuracy; how many correctly classified
bayes_hits <- 0;
knn_hits   <- 0;
svm_hits   <- 0;
cart_hits  <- 0;
ann_hits   <- 0;

x <- data.frame(matrix(ncol=5, nrow=N-init));

# Loop over data
while (i < N) {

  system('clear');

  print(paste("Iteration: ", i));

  # Training data
  td <-  d[1:i,  ];
  tx <- td[1:i,-D];
  ty <- td[1:i, D];
  ty <- as.factor(ty);

  # Target
  vd <- d[i+1,  ];
  vx <- d[i+1,-D];
  vy <- d[i+1, D];

  # Classifier models
  print(paste(" Running ANN..."));
  ann   <- neuralnet(formula=f, data=td, threshold=.5);
  print(paste(" Running NB..."));
  bayes <- naiveBayes(x=tx, y=ty);
  print(paste(" Running KNN..."));
  knn   <- knn(td, vd, cl=ty);
  print(paste(" Running SVM..."));
  svm   <- svm(tx, ty, gamma=10, type="C-classification", scale=FALSE);
  print(paste(" Running CART..."));
  cart  <- rpart(formula=f, data=td, method="class");

  if ( abs(compute(ann, d[i+1,-D])$net.result - vy) > .5) {
    ann_hits <- ann_hits + 1;
  }
  if (predict(bayes, vx)==vy) {
    bayes_hits <- bayes_hits + 1;
  }
  if (colnames(predict(cart,vx))[which.max(predict(cart, vx))] == vy) {
    cart_hits <- cart_hits + 1;
  }
  if (knn==vy) {
    knn_hits <- knn_hits + 1;
  }
  if (predict(svm, vx)==vy) {
    svm_hits <- svm_hits + 1;
  }

  i<-i+1;

  x[i-init+1,1] <- ann_hits   / (i-init+1);
  x[i-init+1,2] <- bayes_hits / (i-init+1);
  x[i-init+1,3] <- cart_hits / (i-init+1);
  x[i-init+1,4] <- knn_hits / (i-init+1);
  x[i-init+1,5] <- svm_hits / (i-init+1);

}

Nval <- N-init;
print(bayes_hits / Nval);
print(knn_hits   / Nval);
print(svm_hits   / Nval);
print(cart_hits  / Nval);
print(ann_hits   / Nval);

write.csv(x, file=gsub("./","",gsub(" ","",paste(datafile,num))));
rm(x);

}


