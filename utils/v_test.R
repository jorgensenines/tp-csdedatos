v_test <- function(x, grupo) {
 N <- length(x)
 m <- mean(x)
 s2 <- var(x) * (N - 1) / N
 sapply(levels(grupo), function(g) {
 xg <- x[grupo == g]
 ng <- length(xg)
 (mean(xg) - m) / sqrt(s2 / ng * (N - ng) / (N - 1))
 })
}
