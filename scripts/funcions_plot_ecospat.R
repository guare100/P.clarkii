## code adapted from:
## Lustenhouwer and Parker 2022. Beyond tracking climate: niche evolution during native range expansion and its implications for novel invasions. Journal of Biogeography 49(8), 1481-1493

myplot <- function (z1, z2, quant, title = "", name.axis1 = "Axis 1", name.axis2 = "Axis 2", 
                    interest = 1, colz1 = "#00FF0050", colz2 = "#FF000050", colinter = "#0000FF50", 
                    colZ1 = "green3", colZ2 = "red3",...) 
{
  if (is.null(z1$y)) {
    R <- length(z1$x)
    x <- z1$x
    xx <- sort(rep(1:length(x), 2))
    y1 <- z1$z.uncor/max(z1$z.uncor)
    Y1 <- z1$Z/max(z1$Z)
    if (quant > 0) {
      Y1.quant <- quantile(z1$Z[which(z1$Z > 0)], probs = seq(0, 
                                                              1, quant))[2]/max(z1$Z)
    }
    else {
      Y1.quant <- 0
    }
    Y1.quant <- Y1 - Y1.quant
    Y1.quant[Y1.quant < 0] <- 0
    yy1 <- sort(rep(1:length(y1), 2))[-c(1:2, length(y1) * 
                                           2)]
    YY1 <- sort(rep(1:length(Y1), 2))[-c(1:2, length(Y1) * 
                                           2)]
    y2 <- z2$z.uncor/max(z2$z.uncor)
    Y2 <- z2$Z/max(z2$Z)
    if (quant > 0) {
      Y2.quant <- quantile(z2$Z[which(z2$Z > 0)], probs = seq(0, 
                                                              1, quant))[2]/max(z2$Z)
    }
    else {
      Y2.quant = 0
    }
    Y2.quant <- Y2 - Y2.quant
    Y2.quant[Y2.quant < 0] <- 0
    yy2 <- sort(rep(1:length(y2), 2))[-c(1:2, length(y2) * 
                                           2)]
    YY2 <- sort(rep(1:length(Y2), 2))[-c(1:2, length(Y2) * 
                                           2)]
    plot(x, y1, type = "n", xlab = name.axis1, ylab = "density of occurrence",...)
    polygon(x[xx], c(0, y1[yy1], 0, 0), col = colz1, border = 0)
    polygon(x[xx], c(0, y2[yy2], 0, 0), col = colz2, border = 0)
    polygon(x[xx], c(0, apply(cbind(y2[yy2], y1[yy1]), 1, 
                              min, na.exclude = TRUE), 0, 0), col = colinter, border = 0)
    lines(x[xx], c(0, Y2.quant[YY2], 0, 0), col = colZ2, 
          lty = "dashed")
    lines(x[xx], c(0, Y1.quant[YY1], 0, 0), col = colZ1, 
          lty = "dashed")
    lines(x[xx], c(0, Y2[YY2], 0, 0), col = colZ2)
    lines(x[xx], c(0, Y1[YY1], 0, 0), col = colZ1)
    segments(x0 = 0, y0 = 0, x1 = max(x[xx]), y1 = 0, col = "white")
    segments(x0 = 0, y0 = 0, x1 = 0, y1 = 1, col = "white")
    seg.cat <- function(inter, cat, col.unf, col.exp, col.stab) {
      if (inter[3] == 0) {
        my.col = 0
      }
      if (inter[3] == 1) {
        my.col = col.unf
      }
      if (inter[3] == 2) {
        my.col = col.stab
      }
      if (inter[3] == -1) {
        my.col = col.exp
      }
      segments(x0 = inter[1], y0 = -0.01, y1 = -0.01, x1 = inter[2], 
               col = my.col, lwd = 4, lty = 2)
    }
    cat <- ecospat.niche.dyn.index(z1, z2, intersection = quant)$dyn
    inter <- cbind(z1$x[-length(z1$x)], z1$x[-1], cat[-1])
    apply(inter, 1, seg.cat, col.unf = "#00FF0050", col.exp = "#FF000050", 
          col.stab = "#0000FF50")
  }
  if (!is.null(z1$y)) {
    z <- t(as.matrix(z1$w + 2 * z2$w))[, nrow(as.matrix(z1$z.uncor)):1]
    z1$Z <- t(as.matrix(z1$Z))[, nrow(as.matrix(z1$Z)):1]
    z2$Z <- t(as.matrix(z2$Z))[, nrow(as.matrix(z2$Z)):1]
    if (interest == 1) {
      image(x = z1$x, y = z1$y, z = z, col = c("#FFFFFF00", colz1, colz2, colinter), # edit 1: switched order of this image and the one below
            xlab = name.axis1, ylab = name.axis2)
      image(x = z1$x, y = z1$y, z = t(as.matrix(z1$z.uncor))[, 
                                                             nrow(as.matrix(z1$z.uncor)):1], col = paste(gray(100:0/100),"99", sep=""), # edit 2: added "99" to hex color codes for transparancy
            zlim = c(1e-05, cellStats(z1$z.uncor, "max")), 
            add=T)
    }
    if (interest == 2) {
      image(x = z2$x, y = z2$y, z = z, col = c("#FFFFFF00", colz1, colz2, colinter), # same as edit 1
            xlab = name.axis1, ylab = name.axis2)
      image(x = z2$x, y = z2$y, z = t(as.matrix(z2$z.uncor))[, 
                                                             nrow(as.matrix(z2$z.uncor)):1], col = paste(gray(100:0/100),"99",sep=""), # same as edit 2
            zlim = c(1e-05, cellStats(z2$z.uncor, "max")), 
            add = T)
    }
    title(title)
    contour(x = z1$x, y = z1$y, z1$Z, add = TRUE, levels = quantile(z1$Z[z1$Z > 
                                                                           0], c(0, quant)), drawlabels = FALSE, lty = c(1,2), col = colZ1)
    contour(x = z2$x, y = z2$y, z2$Z, add = TRUE, levels = quantile(z2$Z[z2$Z > 0], 
                                                                    c(0, quant)), drawlabels = FALSE, lty = c(1,2), col = colZ2)
  }
}


## edits to ecospat.shift.centroids {ecospat} ##
# added points for the centroids of the past and present niche
# changed the linetype and selected individuals colors for the climate and species arrow

my.centroid <- function (sp1, sp2, clim1, clim2, col.sp = "red", col.clim = "black") # added 'col.sp' and 'col.clim' instead of one 'col' argument
{
  if (ncol(as.matrix(sp1)) == 2) {
    points(median(sp1[, 1]), median(sp1[, 2]), cex=2, pch=16, col="#e29f60") # add centroid of past niche
    points(median(sp2[, 1]), median(sp2[, 2]), cex=2, pch=16, col="#52C6F5") # add centroid of present niche
    arrows(median(sp1[, 1]), median(sp1[, 2]), median(sp2[, 
                                                          1]), median(sp2[, 2]), col = col.sp, lwd = 2, length = 0.1) # col = col.sp
    arrows(median(clim1[, 1]), median(clim1[, 2]), median(clim2[, 
                                                                1]), median(clim2[, 2]), lty = 1, col = col.clim, lwd = 2, # lty= 1 instead of 11, col = col.clim
           length = 0.1)
  }
  else {
    arrows(median(sp1), 0.025, median(sp2), 0.025, col = col.sp, # col = col.sp 
           lwd = 2, length = 0.1)
    arrows(median(clim1), -0.025, median(clim2), -0.025, 
           lty=1, col = col.clim, lwd = 2, length = 0.1) # lty = 1 instead of 11, col = col.clim
  }
}
