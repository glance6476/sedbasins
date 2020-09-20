###### Lab 4: Subsidence in Extensional Settings ######

#### Part 1: Syn-rift subsidence as a function of stretch factor ----

## assigning values to parameters

# constants
yl = 125000 #m #initial lithospheric thickness
yc = 35000 #m #initial crustal thickness
pm = 3330 #kgm-3 #mantle density
pc = 2800 #kgm-3 #crustal density
ps = 2400 #kgm-3 #sediment density
Ta = 1333 #degreeC #asthenospheric temperature
a = 0.0000328 #degreeC-1 #volumetric coefficient of thermal expansion

# stretch factors
B = c(1.1, 1.2, 1.3, 1.4, 1.5, 2, 3, 4)

## determining the magnitude of subsidence (ys) for a range of stretch factors (B)

# equation for amount of subsidence (s)
ys = yl * (1 - 1/B) * (((pm - pc) * (yc / yl) * (1 - 1/2 * a * Ta * (yc / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - ps))

## plotting the magnitude of subsidence

plot(ys~B, ylim = rev(c(0, 9000)), las = 1, ylab = "Magnitude of Subsidence (m)", xlab = "Stretch Factor",
     col = "blue", lwd = 2, main = "Syn-rift Subsidence as a Function of Stretch Factor")

## question 2, 3, 4
# > B = c(1.1, 1.2, 1.3, 1.4, 1.5, 2, 3, 4)
# > ys
# [1]  990.1905 1815.3493 2513.5605 3112.0273 3630.6986 5446.0478
# [7] 7261.3971 8169.0718


# question 2
# > pchange2
# [1] 450
pchange2 = (5446.0478 - 990.1905) / 990.1905 * 100

# question 3
# > pchange3
# [1] 12.5
pchange3 = (8169.0718 - 7261.3971) / 7261.3971 * 100





#### Part 2: Comparing sediment-filled vs water-filled basin ----

## assigning values to parameters

# constants
yl = 125000 #m #initial lithospheric thickness
yc = 35000 #m #initial crustal thickness
pm = 3330 #kgm-3 #mantle density
pc = 2800 #kgm-3 #crustal density
ps = 2400 #kgm-3 #sediment density
pw = 1030 #kgm-3 #water density
Ta = 1333 #degreeC #asthenospheric temperature
a = 0.0000328 #degreeC-1 #volumetric coefficient of thermal expansion

# stretch factors
B = c(1.1, 1.2, 1.3, 1.4, 1.5, 2, 3, 4)

## determining the magnitude of subsidence (ys) for a range of stretch factors (B)

# equation for amount of subsidence (s)
ys = yl * (1 - 1/B) * (((pm - pc) * (yc / yl) * (1 - 1/2 * a * Ta * (yc / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - ps))
ysw = yl * (1 - 1/B) * (((pm - pc) * (yc / yl) * (1 - 1/2 * a * Ta * (yc / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - pw))

# > ys
# [1]  990.1905 1815.3493 2513.5605 3112.0273 3630.6986 5446.0478
# [7] 7261.3971 8169.0718
# > ysw
# [1]  381.1030  698.6889  967.4154 1197.7524 1397.3778 2096.0666
# [7] 2794.7555 3144.0999

## plotting the magnitude of subsidence

# sediment density
plot(ys~B, ylim = rev(c(0, 9000)), las = 1, ylab = "Magnitude of Subsidence (m)", xlab = "Stretch Factor",
     col = "blue", lwd = 2, main = "Syn-rift Subsidence as a Function of Stretch Factor")

# water density
lines(ysw~B, type = "p", col = "red", lwd = 2, pch = 0)

# legend
legend("topright", legend=c("sediment-filled", "water-filled"), bty="n",  pch = c(1, 0), 
       cex=c(0.8, 0.8), col=c("blue", "red"))

## question 5
d = ysw / ys
#> d
#[1] 0.3848785 0.3848785 0.3848785 0.3848785 0.3848785 0.3848785
#[7] 0.3848785 0.3848785

## question 6
sub = 19 / 100 * 1000000
#> sub
#[1] 190000


#### Part 3: With different crustal/lithospheric ratios ----

## assigning values to parameters

# constants
yl = 125000 #m #initial lithospheric thickness
pm = 3330 #kgm-3 #mantle density
pc = 2800 #kgm-3 #crustal density
ps = 2400 #kgm-3 #sediment density
Ta = 1333 #degreeC #asthenospheric temperature
a = 0.0000328 #degreeC-1 #volumetric coefficient of thermal expansion

# stretch factors and crustal thickness
#yc = c(16000, 22000, 26000, 31000, 35000) #a range of constants
B = c(1.1, 1.2, 1.3, 1.4, 1.5, 2, 3, 4) #x variable

## determining the magnitude of subsidence (ys) for a range of stretch factors (B)
## the stupid way
# equation for amount of subsidence (s)
ys1 = yl * (1 - 1/B) * (((pm - pc) * (16000 / yl) * (1 - 1/2 * a * Ta * (16000 / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - ps))
ys2 = yl * (1 - 1/B) * (((pm - pc) * (22000 / yl) * (1 - 1/2 * a * Ta * (22000 / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - ps))
ys3 = yl * (1 - 1/B) * (((pm - pc) * (26000 / yl) * (1 - 1/2 * a * Ta * (26000 / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - ps))
ys4 = yl * (1 - 1/B) * (((pm - pc) * (31000 / yl) * (1 - 1/2 * a * Ta * (31000 / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - ps))
ys5 = yl * (1 - 1/B) * (((pm - pc) * (35000 / yl) * (1 - 1/2 * a * Ta * (35000 / yl)) - (1/2 * pm * a * Ta)) / (pm * (1 - 1/2 * a * Ta) - ps))

## plotting the magnitude of subsidence
plot(ys1~B, ylim = rev(c(-2000, 8500)), las = 1, ylab = "Magnitude of Subsidence (m)", xlab = "Stretch Factor",
     col = "purple", lwd = 2, pch = 6, main = "Syn-rift Subsidence as a Function of Stretch Factor")
lines(ys2~B, type = "p", col = "red", lwd = 2, pch = 0)
lines(ys3~B, type = "p", col = "green", lwd = 2, pch = 2)
lines(ys4~B, type = "p", col = "yellow", lwd = 2, pch = 5)
lines(ys5~B, type = "p", col = "blue", lwd = 2, pch = 1)

# legend
legend("topright", legend=c("crustal thickness = 16000 m", "crustal thickness = 22000 m", "crustal thickness = 26000 m", "crustal thickness = 31000 m", "crustal thickness = 35000 m"),
       bty="n",  pch = c(6, 0, 2, 5, 1), 
       cex=c(0.5, 0.5), col=c("purple", "red", "green", "yellow", "blue"))

