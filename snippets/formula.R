f <- names(td)[N];
f <- paste(f, " ~ ");
f <- paste(f, names(td)[1]);
if (N > 3) {
  for (i in 3:N-1) {
    f <- paste(f, "+");
    f <- paste(f, names(td)[i]);
  }
}
f <- as.formula(f);
