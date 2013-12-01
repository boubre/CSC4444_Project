  # Append last class value to training data
  tx[1,D] <- td[1,D];
  if (i > 1) {
    # Time series
    T[[i]] <- ts(td);
    for (j in 2:i) {
      tx[j,D] <- td[j-1,D];
    }
  }

  # Target
  vd <- d[i+1,  ];
  vx <- d[i+1,-D];
  vy <- d[i+1, D];

  # Append last class to validation entry 
  vx[1, D] <- d[i, D];

