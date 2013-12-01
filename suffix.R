
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

  # Increment _hits for each correct classification
  if (predict(bayes, vx)==vy) {
    bayes_hits <- bayes_hits + 1;
  }
  if (knn==vy) {
    knn_hits <- knn_hits + 1;
  }
  if (predict(svm, vx)==vy) {
    svm_hits <- svm_hits + 1;
  }
  if (colnames(predict(cart,vx))[which.max(predict(cart, vx))] == vy) {
    cart_hits <- cart_hits + 1;
  }
  # Binary only
  if (abs( prediction(ann)$rep1[i-1,D] - d[i-1,D] ) < .5) {
    ann_hits <- ann_hits + 1;
  }

  i<-i+1;

}

Nval <- N-init;
print(bayes_hits / Nval);
print(knn_hits   / Nval);
print(svm_hits   / Nval);
print(cart_hits  / Nval);
print(ann_hits   / Nval);

}
