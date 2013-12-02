
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


