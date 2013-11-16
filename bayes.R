classifier <- naiveBayes(x=data[1:3], y=as.factor(data[[4]]));
table(predict(classifier, newdata=data), data[,4], dnn=list('predicted','actual'));
