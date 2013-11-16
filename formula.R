N    <- ncol(data);
form <- names(data)[N];
form <- paste(form, " ~ ");
form <- paste(form, names(data)[1]);
if (N > 3) {
  for (i in 3:N-1) {
    form <- paste(form, "+");
    form <- paste(form, names(data)[i]);
  }
}
