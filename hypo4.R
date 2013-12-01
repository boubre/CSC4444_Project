
  # How many entries, beta value
  for (m in 2:4) {
  for (b in 2:4) {

  # Normalization constant
  norm <- 0;
  for (j in 1:m) {
    norm <- norm + 1/(b^j);
  }

  # Weights
  a <- vector();
  a <- c(1/b);
  for (j in 2:m) {
    a <- append(a, a[j-1]/b);
  }
  a <- a / norm;

  # Initialize 
  for (k in 1:D) {
    tx[1,D-1+k] <- td[1,k];
  }
  for (j in 2:m) {
    for (k in 1:D) {
        tx[j,D-1+k] <- td[j-1,k];
    }
  }

  # Append MA of last m values using a-weights
  if (i > m) {
    for (j in m+1:i) {
      for (k in 1:D) {
        tx[j,D-1+k] <- 0;
          for (l in 1:m) {
            tx[j,D-1+k] <- tx[j,D-1+k] + a[l]*td[j-l,k];
          }
          tx[j,D-1+k] <- tx[j,D-1+k] / m;
      }
    }
  }
  tx <- tx[1:i,];

  # Target
  vd <- d[i+1,  ];
  vx <- d[i+1,-D];
  vy <- d[i+1, D];


  # Append last row to validation entry 
  if (i > m) {
    for (k in 1:D) {
      for (l in 1:m) {
        vx[1,D-1+k] <- d[i-l,k] + a[l]*d[i-l,k];
      }
      vx[1,D-1+k] <- d[i-l,k] / m
    }
  } else {
    for (k in 1:D) {
      vx[1,D-1+k] <- d[i-1,k];
    }
  }

}
}
