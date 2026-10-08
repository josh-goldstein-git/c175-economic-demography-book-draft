library(shiny)
## Model functions and the bundled `world` schedules live in global.R
## (auto-sourced by Shiny), so no package install is needed.

## Baseline schedules from the bundled World dataset (WPP2024 reference year).
Fx <- world$Fx
Lx <- world$Lx
baseline_e0 <- sum(Lx)

## Mean age of childbearing and first/second moments of the stationary
## age distribution, computed once from the baseline schedules.
x <- seq_along(Fx) - 1
mu <- sum((x + 0.5) * Lx * Fx) / sum(Lx * Fx)
A  <- sum((x + 0.5) * Lx) / sum(Lx)
A2 <- sum((x + 0.5)^2 * Lx) / sum(Lx)
sigma2 <- A2 - A^2

peak_of <- function(t, ft) {
    dft <- (c(ft[-1], NA) - c(NA, ft[-length(ft)])) / 2
    approx(x = dft, y = t, xout = 0)$y
}

draw_sandbox <- function(k1, k2, rho, t, myt,
                         nrr_slope, show_prediction, curvature) {
    k1 <- as.numeric(k1); k2 <- as.numeric(k2); rho <- as.numeric(rho)
    radix <- 1000

    ## line widths and cex scaling used throughout
    lw_line <- 5   # projection lines
    lw_pred <- 3   # predicted-peak dashed lines
    cex_base  <- 1.6
    cex_main  <- 1.5
    cex_sub   <- 1.2
    cex_axis  <- 1.3
    pt_cex    <- 2

    nrr_t <- exp(k1 * t + k2 * t^2)
    e0_t  <- baseline_e0 * exp(rho * t)

    K <- project_varying_leslie(nrr_t = nrr_t, e0_t = e0_t,
                                Lx_zero = Lx, Fx_zero = Fx)
    Nt <- colSums(K)
    Nt <- radix * Nt / Nt[t == 0]
    Bt <- radix * K[1, ] / (colSums(K)[t == 0])

    tB_exact <- round(peak_of(t[t < 0],  Bt[t < 0]),  2)
    tN_exact <- round(peak_of(t,         Nt),          2)

    if (show_prediction) {
        tB_hat <- -mu / 2
        lag    <- peak_pop_lag(mu = mu, A = A, k = k1, k2 = k2,
                               rho = rho, sigma2 = sigma2)
        tN_hat <- lag$total
    }

    par(mfrow = c(2, 2), cex = cex_base, las = 1,
        mar = c(4.1, 4.1, 5.6, 2.1))
    xlim <- c(-50, 50)
    s <- t <= myt
    i_now <- which(t == myt)

    ## --- NRR ---
    plot_nrr(t, nrr_t,
             col = "grey", lwd = lw_line,
             xlim = xlim, ylim = c(0, 2.7), axes = FALSE,
             xlab = "time", ylab = "")
    lines(t[s], nrr_t[s], lwd = lw_line, col = "turquoise")
    points(myt, nrr_t[i_now], pch = 19, cex = pt_cex, col = "turquoise")
    abline(v = 0); grid()
    axis(1, at = seq(-50, 50, 25),
         labels = c("-50", "", "0", "", "50"),
         cex.axis = cex_axis)
    axis(2, at = 0:3, cex.axis = cex_axis)
    title("Net reproduction rate", cex.main = cex_main)
    title(line = 0.8, cex.main = cex_sub,
          if (curvature) bquote(NRR(t) == e^{k[1]*t + k[2]*t^2})
          else           bquote(NRR(t) == e^{k*t}))
    text(-20, 1, "replacement", pos = 1, cex = 0.95)

    ## --- e0 ---
    e0_hi <- max(100, ceiling(max(e0_t) + 5))
    e0_lo <- min(60,  floor(min(e0_t) - 5))
    plot(t, e0_t, type = "l", lwd = lw_line,
         xlim = xlim, ylim = c(e0_lo, e0_hi), axes = FALSE,
         xlab = "time", ylab = "")
    points(myt, e0_t[i_now], pch = 19, cex = pt_cex)
    axis(1, at = seq(-50, 50, 25),
         labels = c("-50", "", "0", "", "50"),
         cex.axis = cex_axis)
    axis(2, cex.axis = cex_axis); grid()
    title("Life expectancy", cex.main = cex_main)
    title(line = 0.8, cex.main = cex_sub,
          bquote(e[0](t) == .(round(baseline_e0)) * e^{rho*t}))

    ## --- Births ---
    plot_births(t, Bt,
                col = NA,  # draw frame only; reveal overlay below
                xlim = xlim, ylim = radix * c(0.005, 0.02),
                axes = FALSE, xlab = "time", ylab = "")
    lines(t[s], Bt[s], lwd = lw_line, col = "red")
    points(myt, Bt[i_now], pch = 19, cex = pt_cex, col = "red")
    axis(1, at = seq(-50, 50, 25),
         labels = c("-50", "", "0", "", "50"),
         cex.axis = cex_axis)
    axis(2, cex.axis = cex_axis); grid()
    abline(v = 0)
    if (nrr_slope) {
        segments(x0 = myt - 30, x1 = myt,
                 y0 = Bt[t == myt - 30], y1 = Bt[t == myt],
                 lty = 2, col = "turquoise", lwd = lw_pred)
    }
    if (myt >= round(tB_exact)) {
        segments(x0 = tB_exact, y0 = 0, y1 = max(Bt),
                 lty = 2, lwd = lw_pred)
        text(x = tB_exact, y = 7, pos = 2, round(tB_exact, 1),
             cex = cex_sub)
    }
    if (show_prediction && myt >= round(tB_exact)) {
        segments(x0 = tB_hat, y0 = 0, y1 = max(Bt),
                 lty = 2, col = "orange", lwd = lw_pred)
    }
    title("Births", cex.main = cex_main)
    title(line = 0.8, cex.main = cex_sub,
          bquote(t[B] - t[0] == -mu / 2))

    ## --- Population ---
    plot_pop(t, Nt,
             col = NA,  # draw frame only; reveal overlay below
             xlim = xlim, ylim = radix * c(0.2, 1.3),
             axes = FALSE, xlab = "time", ylab = "")
    lines(t[s], Nt[s], lwd = lw_line, col = "blue")
    points(myt, Nt[i_now], pch = 19, cex = pt_cex, col = "blue")
    axis(1, at = seq(-50, 50, 25),
         labels = c("-50", "", "0", "", "50"),
         cex.axis = cex_axis)
    axis(2, cex.axis = cex_axis); grid()
    abline(v = 0)
    if (myt >= round(tN_exact)) {
        segments(x0 = tN_exact, y0 = 0, y1 = max(Nt),
                 lty = 2, lwd = lw_pred)
        text(x = tN_exact, y = 400, pos = 4, round(tN_exact, 1),
             cex = cex_sub)
    }
    if (show_prediction && myt >= round(tN_exact)) {
        segments(x0 = tN_hat, y0 = 0, y1 = max(Nt),
                 lty = 2, col = "orange", lwd = lw_pred)
    }
    title("Population", cex.main = cex_main)
    title(line = 0.8, cex.main = cex_sub,
          bquote(t[N] - t[0] == A - mu / 2))
}

## ---- Custom-NRR tab ---------------------------------------------------
## Build an arbitrary NRR(t) by interpolating user-set anchor points, then
## reuse the same projector.  The closed-form ("Coale") prediction is drawn
## alongside the exact peak so students can see WHERE it breaks: it assumes a
## smooth single decline, so a stall/rebound/kink makes the two diverge.

nrr_from_anchors <- function(t, ta, va, method = c("spline", "linear")) {
    method <- match.arg(method)
    if (method == "linear") {
        out <- approx(ta, va, xout = t, rule = 2)$y      # flat beyond anchors
    } else {
        ## natural spline; extrapolates linearly beyond the anchors, so it
        ## stays smooth (C1) at the end knots -- no kink at +/-45.
        out <- splinefun(ta, va, method = "natural")(t)
    }
    pmax(out, 0.05)                                        # keep positive
}

## crossing of y through `level`, nearest t = 0 (returns NA if none)
crossing_near0 <- function(t, y, level = 1) {
    d <- y - level
    cr <- t[abs(d) < 1e-8]                                 # exact hits at nodes
    j <- which(d[-1] * d[-length(d)] < 0)                 # crossings between nodes
    if (length(j))
        cr <- c(cr, t[j] + (level - y[j]) / (y[j + 1] - y[j]) * (t[j + 1] - t[j]))
    if (!length(cr)) return(NA_real_)
    cr[which.min(abs(cr))]
}

draw_custom <- function(t, nrr_t, ta, va, show_prediction) {
    radix <- 1000
    e0_t  <- rep(baseline_e0, length(t))                  # constant mortality

    K  <- project_varying_leslie(nrr_t = nrr_t, e0_t = e0_t,
                                 Lx_zero = Lx, Fx_zero = Fx)
    Nt <- colSums(K); Nt <- radix * Nt / Nt[t == 0]
    Bt <- radix * K[1, ] / (colSums(K)[t == 0])

    tB_exact <- t[which.max(Bt)]
    tN_exact <- t[which.max(Nt)]

    ## leading-order Coale prediction, centered on t0 (NRR crosses replacement)
    t0     <- crossing_near0(t, nrr_t)
    tB_hat <- if (is.na(t0)) NA else t0 - mu / 2
    tN_hat <- if (is.na(t0)) NA else t0 - mu / 2 + A

    par(mfrow = c(3, 1), cex = 1.25, las = 1, mar = c(4.2, 4.6, 3.2, 2.1))
    xlim <- c(-50, 50); lw <- 5; lwp <- 3
    inwin <- t >= -50 & t <= 50

    ## NRR
    plot(t, nrr_t, type = "l", lwd = lw, col = "turquoise",
         xlim = xlim, ylim = c(0, max(2.7, max(nrr_t[inwin]))),
         xlab = "time", ylab = "NRR")
    abline(h = 1, col = "grey50", lty = 2); abline(v = 0); grid()
    if (!is.na(t0)) abline(v = t0, col = "grey50", lty = 3)
    points(ta, va, pch = 21, bg = "darkgreen", col = "black", cex = 1.8)
    title("Net reproduction rate  (set anchors at left)")

    ## Births
    plot(t, Bt, type = "l", lwd = lw, col = "red",
         xlim = xlim, xlab = "time", ylab = "births")
    abline(v = 0); grid(); abline(v = tB_exact, lty = 2, lwd = lwp)
    if (show_prediction && !is.na(tB_hat))
        abline(v = tB_hat, lty = 2, lwd = lwp, col = "orange")
    title(cex.main = 1.0,
          sprintf("Births   |   exact peak = %g    Coale (t0 - mu/2) = %s",
                  tB_exact, if (is.na(tB_hat)) "NA" else round(tB_hat, 1)))

    ## Population
    plot(t, Nt, type = "l", lwd = lw, col = "blue",
         xlim = xlim, xlab = "time", ylab = "population")
    abline(v = 0); grid(); abline(v = tN_exact, lty = 2, lwd = lwp)
    if (show_prediction && !is.na(tN_hat))
        abline(v = tN_hat, lty = 2, lwd = lwp, col = "orange")
    title(cex.main = 1.0,
          sprintf("Population   |   exact peak = %g    Coale (t0 - mu/2 + A) = %s",
                  tN_exact, if (is.na(tN_hat)) "NA" else round(tN_hat, 1)))
}

## Projection range: -80:80 rather than -100:100 because bundled Lx
## schedules end at age 100, which caps the feasible e0 that
## e0_to_lx() can reach to ~98. At rho=0.003, |t|<=86 stays inside
## that range; -80:80 leaves buffer. xlim=c(-50,50) is unchanged so
## the visible window is identical to the original app.
shinyServer(function(input, output) {
    output$peakPlot <- renderPlot({
        draw_sandbox(k1 = input$k1, k2 = input$k2, rho = input$rho,
                     t = -80:80, myt = input$myt,
                     nrr_slope = input$nrr_slope,
                     show_prediction = input$show_prediction,
                     curvature = input$curvature)
    })

    output$customPlot <- renderPlot({
        ta <- c(-45, -30, -15, 0, 15, 30, 45)
        va <- c(input$a_m45, input$a_m30, input$a_m15, 1.0,   # t = 0 pinned
                input$a_15, input$a_30, input$a_45)
        tt <- -80:80
        nrr_t <- nrr_from_anchors(tt, ta, va, input$interp)
        draw_custom(tt, nrr_t, ta, va, input$show_prediction2)
    })
})
