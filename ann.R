nn <- neuralnet(formula=as.formula(form), data=data, threshold=.5);
#compute(nn, data[1:3])$net.result;
prediction(nn);
