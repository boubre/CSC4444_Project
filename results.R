prediction(ann);
table(predict(bayes, newdata=vx), vy, dnn=list('predicted','actual'));
table(predict(knn,   newdata=vd), vy, dnn=list('predicted','actual'));
table(predict(svm,   newdata=vd), vy, dnn=list('predicted','actual'));
