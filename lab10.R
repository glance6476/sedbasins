# attach dataframes
attach(burial)

# simple plot
par(xaxs = "i", yaxs = "i")
plot(age_from, depth_baseU1_startdep, type = "l", lwd = 2, ylim = rev(range(depth_baseU1_startdep)), xlim = rev(range(age_from)),
     las = 1, ylab = "depth (m)", xlab = "", xaxt = "n")
axis(3)
mtext("age (Ma)", side = 3, line = 2.5)

# add lines
lines(age_from, depth_baseU2_startdep, type = "l", lwd = 2, col = "red")
lines(age_from, depth_baseU3_startdep, type = "l", lwd = 2, col = "blue")
lines(age_from, depth_baseU4_startdep, type = "l", lwd = 2, col = "darkgreen")
lines(age_from, depth_baseU5_startdep, type = "l", lwd = 2, col = "purple")
lines(age_from, depth_baseU6_startdep, type = "l", lwd = 2, col = "orange")

legend(420, 2500, c("base U1", "base U2", "base U3", "base U4", "base U5", "base U6"),
      col = c("black", "red", "blue", "darkgreen", "purple", "orange"),
      lty = 1, cex = 1, lwd = 2, box.lty = 0, y.intersp = 1)