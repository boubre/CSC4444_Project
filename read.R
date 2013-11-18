# Required libraries
library(e1071);
library(neuralnet);
library(kernlab);

# Training data
td <- read.table("example.csv", sep=",", header=TRUE);
N  <- ncol(td);
tx <- td[-N];
ty <- as.factor(td[[N]]);

# Validation data
vd <- td;
vx <- vd[-N];
vy <- as.factor(vd[[N]]);

#args <- commandArgs();
