
  # Append last row to training data
  for (k in 1:D) {
    tx[1,D-1+k] <- td[1,k];
  }
  if (i > 1) {
    # Time series
    T[[i]] <- ts(td);
    for (j in 2:i) {
      for (k in 1:D) {
        tx[j,D-1+k] <- td[j-1,k];
      }
    }
  }

  # Target
  vd <- d[i+1,  ];
  vx <- d[i+1,-D];
  vy <- d[i+1, D];

  # Append last row to validation entry 
  for (k in 1:D) {
    vx[1, D-1+k] <- d[i, k];
  }

