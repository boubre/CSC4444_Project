  # Append MA of last m values using a-weights
  for (k in 1:D) {
    tx[1,D-1+k] <- td[1,k];
  }
  if (i > 1) {
    # Time series
    T[[i]] <- ts(td);
    for (j in 2:i) {
      for (k in 1:D) {
        tx[j,D-1+k] <- 0;
        if (j > m) {
          for (l in 1:m) {
            tx[j,D-1+k] <- tx[j,D-1+k] + a[l]*td[j-1-l,k];
          }
          tx[j,D-1+k] <- tx[j,D-1+k] / m;
        } else {
          tx[j,D-1+k] <- td[j-1,k];
        }
      }
    }
  }


  # Target
  vd <- d[i+1,  ];
  vx <- d[i+1,-D];
  vy <- d[i+1, D];


  # Append last row to validation entry 
  for (k in 1:D) {
        if (i > m) {
          for (l in 1:m) {
            vx[1,D-1+k] <- d[i-l,D-1+k] + a[l]*d[i-l,D-1+k];
          }
            vx[1,D-1+k] <- d[i-l,D-1+k] / m
        } else {
          vx[1,D-1+k] <- d[i-1,k];
        }
  }
    
