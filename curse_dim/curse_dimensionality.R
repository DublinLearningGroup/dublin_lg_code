library(pbapply)


Niter <- 1000000
Ndim  <- 1:50

dim_data <- pbsapply(Ndim, function(iterdim) {
    points <- replicate(Niter, runif(iterdim, 0, 1))
    points <- matrix(points, ncol = iterdim, nrow = Niter)

    hits <- apply(points, 1, function(x) sum(x^2) < 1)

    even_idx <- seq(1, Niter, 2)

    one <- points[ even_idx,,drop = FALSE]
    two <- points[-even_idx,,drop = FALSE]


    diff <- apply((one - two), 1, function(x) sqrt(sum(x^2)))

    c(hits = sum(hits) / Niter, avg_dist = mean(diff))
})


ggplot(data.table::melt(vol_frac)) +
    geom_line(aes(x = Var2, y = value)) +
    facet_wrap(~Var1, scales = 'free')

## frac_plot <- ggplot() +
##     geom_line(aes(x = Ndim, y = vol_frac)) +
##     xlab("Dimensions") +
##     ylab("Fraction of Volume") +
##     ggtitle("Fraction of Volume Occupied by Hypersphere in Hypercube vs Dimensionality of Space")

## ggsave(frac_plot, file = "volfrac_plot.pn", height = 10, width = 14)
