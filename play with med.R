library(effectsize)

x <- rnorm(50)
y <- runif(30)

# Do x and y come from the same distribution?
ks.test(x, y)
med(x, y)

# Is the CDS of x larger than that of y?
ks.test(x, y, alternative = "l")
med(x, y, alternative = "l")

plot(ecdf(x), xlim = range(c(x, y)), col = "blue")
lines(ecdf(y), col = "red")

# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x+2, "pgamma", 3, 2)
med(x+2, pdf = "pgamma", shape = 3, rate = 2)

plot(ecdf(x + 2), xlim = range(x + 2), col = "blue")
curve(pgamma(x, 3, 2), col = "red", add = TRUE)