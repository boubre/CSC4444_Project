prediction(ann);
table(predict(bayes, newdata=vx), vy, dnn=list('predicted','actual'));
table(predict(knn,   newdata=vx), vy, dnn=list('predicted','actual'));
table(predict(svm,   newdata=vx), vy, dnn=list('predicted','actual'));
