library(e1071);

data       <- read.table("example.csv", sep=",", header=TRUE);
classifier <- naiveBayes(x=data[1:3], y=as.factor(data[[4]]));
table(predict(classifier, newdata=data), data[,4], dnn=list('predicted','actual'));
