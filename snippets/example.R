saveGIF({
    ani.options(nmax = 10)
    for (i in 1:10) {
      plot(rnorm(i));
    }
}, interval = 0.05, movie.name = "bm_demo.gif", ani.width = 600, ani.height = 600)
