
  # Classifier models
  ann   <- neuralnet(formula=f, data=td, threshold=.5);
  bayes <- naiveBayes(x=tx, y=ty);
  knn   <- knn(td, vd, cl=ty);
  svm   <- svm(tx, ty, gamma=10, type="C-classification", scale=FALSE);


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

  i<-i+1;

}

Nval <- N-init;
print(bayes_hits/Nval);
print(knn_hits/Nval);
print(svm_hits/Nval);

}
