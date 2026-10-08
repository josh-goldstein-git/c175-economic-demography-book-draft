## global.R for the peak-population app (auto-sourced by Shiny).
## Assembled for the C175 textbook draft from the peakpop package (Goldstein, 2026):
## R/leslie.R, R/rescale.R, R/analytic.R, R/plots.R, and the bundled World schedules
## (WPP 2024, reference year 2023), so the app needs only base R and shiny.

world <- as.list(read.csv("world_wpp2024_Fx_Lx.csv"))

get_leslie_matrix <- function(Lx, Fx) {
    n <- length(Lx)
    Lx_next <- c(Lx[-1], 0)
    Fx_next <- c(Fx[-1], 0)

    subdiag <- (Lx_next / Lx)[-n]
    first_row <- (Lx[1] / 2) * (Fx + Fx_next * Lx_next / Lx) * 0.4886

    A <- rbind(first_row, cbind(diag(subdiag), 0))
    dimnames(A) <- NULL
    A
}

project_varying_leslie <- function(nrr_t, e0_t, Lx_zero, Fx_zero) {
    nsteps <- length(nrr_t)
    nages <- length(Lx_zero)

    ## seed with the stable population implied by step 1's rates
    Lx_t <- e0_to_lx(e0_t[1], Lx_zero)
    Fx_t <- nrr_to_fx(nrr_t[1], Fx_zero, Lx_t)
    A_t  <- get_leslie_matrix(Lx_t, Fx_t)
    k0 <- Re(eigen(A_t)$vectors[, 1])
    k0 <- k0 / sum(k0)

    K <- matrix(0, nrow = nages, ncol = nsteps)
    K[, 1] <- k0

    ## time-varying projection: rebuild Lx_t, Fx_t, A_t each step
    for (t in 1:(nsteps - 1)) {
        Lx_t <- e0_to_lx(e0_t[t], Lx_zero)
        Fx_t <- nrr_to_fx(nrr_t[t], Fx_zero, Lx_t)
        A_t  <- get_leslie_matrix(Lx_t, Fx_t)
        K[, t + 1] <- A_t %*% K[, t]
    }
    K
}

nrr_to_fx <- function(nrr, Fx_zero, Lx) {
    nrr_zero <- sum(Fx_zero * Lx * 0.4886)
    Fx_zero * (nrr / nrr_zero)
}

e0_to_lx <- function(e0, Lx_zero) {
    n <- length(Lx_zero)
    young <- 1:50
    old <- 51:n
    ## young ages pass through unchanged; solve for the power applied to old ages
    young_sum <- sum(Lx_zero[young])
    f <- function(theta) young_sum + sum(Lx_zero[old]^theta) - e0
    theta_hat <- uniroot(f, lower = 1e-5, upper = 100)$root
    c(Lx_zero[young], Lx_zero[old]^theta_hat)
}

find_t0 <- function(nrr, time) {
    keep <- !is.na(nrr)
    nrr <- nrr[keep]
    time <- as.numeric(time[keep])
    if (!any(nrr < 1)) return(NA)

    last_above <- max(which(nrr > 1))
    pair <- c(last_above, last_above + 1)
    approx(x = nrr[pair], y = time[pair], xout = 1)$y
}

peak_births_lag <- function(mu){
    -mu / 2
}

peak_pop_lag <- function(mu, A, k, k2 = 0, rho = 0, sigma2 = 0) {
    coale <- -mu / 2 + A
    longevity <- rho * mu / (-k)
    quad_extra <- -2 * k2 / k * sigma2 / 2
    list(coale = coale,
         longevity = longevity,
         quad_extra = quad_extra,
         total = coale + longevity + quad_extra)
}

growth_factor_decomp <- function(N_peak, N_t0, lag, k, mu, sigma2, rho) {
    observed <- N_peak / N_t0
    coale <- exp(-lag^2 * k / (2 * mu) * (1 + (k / mu) * sigma2))
    longevity <- exp(rho * lag)
    list(observed = observed,
         coale = coale,
         longevity = longevity,
         unexplained = observed - coale - longevity)
}

plot_nrr <- function(time, nrr, t0 = NULL,
                     col = "black", lwd = 2,
                     xlab = "Year", ylab = "NRR", ...) {
    plot(time, nrr, type = "l", col = col, lwd = lwd,
         xlab = xlab, ylab = ylab, ...)
    abline(h = 1, col = "grey60")
    if (!is.null(t0)) abline(v = t0, lty = 2, col = "red")
}

plot_births <- function(time, births, tB = NULL,
                        col = "darkred", lwd = 2,
                        xlab = "Year", ylab = "Births", ...) {
    plot(time, births, type = "l", col = col, lwd = lwd,
         xlab = xlab, ylab = ylab, ...)
    if (!is.null(tB)) abline(v = tB, lty = 2, col = "red")
}

plot_pop <- function(time, pop, tN = NULL,
                     col = "darkblue", lwd = 2,
                     xlab = "Year", ylab = "Population", ...) {
    plot(time, pop, type = "l", col = col, lwd = lwd,
         xlab = xlab, ylab = ylab, ...)
    if (!is.null(tN)) abline(v = tN, lty = 2, col = "red")
}

