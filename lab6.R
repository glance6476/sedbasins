#### lab 6: flexural subsidence, rigidity, and elastic thickness

## parameters
rho_w <- 1030  #kgm-3 #density of water
rho_m <- 3300  #kgm-3 #density of mantle
V0 <- 1.0 * 10^13 #kgs-2 #vertical load
g <- 9.81 #ms-2 #gravity

## variables
D1 <- 1.0 * 10^22 #Nm
D2 <- 1.0 * 10^23 #Nm
D3 <- 1.0 * 10^24 #Nm
# D <- c(D1, D2, D3)

## sub equations with respect to different rigidities
a1 <- (4 * D1 / ((rho_m - rho_w) * g))^(1/4) #flexural parameter
A1 <- V0 * a1^3 / (8 * D1)                  #max deflection

a2 <- (4 * D2 / ((rho_m - rho_w) * g))^(1/4)
A2 <- V0 * a2^3 / (8 * D2)

a3 <- (4 * D3 / ((rho_m - rho_w) * g))^(1/4)
A3 <- V0 * a3^3 / (8 * D3)


## equations
x <- c(seq(0, 300000, by=10000))

deflection1 <- function(x) {
  A1 * exp(-x / a1) * (cos(x / a1) + sin(x / a1))
}

deflection2 <- function(x) {
  A2 * exp(-x / a2) * (cos(x / a2) + sin(x / a2))
}

deflection3 <- function(x) {
  A3 * exp(-x / a3) * (cos(x / a3) + sin(x / a3))
}

## plotting
plot(deflection1(x)~x, type = "l", ylim = rev(c(-500, 5000)), ylab = "Subsidence (m)", xlim = (c(0, 300000)), xlab = "Distance (m)", main = "Flexural subsidence vs distance")
lines(deflection2(x)~x, col = "red")
lines(deflection3(x)~x, col = "blue")
#abline(h=0, lwd=0.5, col="darkgray")
#legend("bottomright", legend = c("D = 10^22 Nm", "D = 10^23 Nm", "D = 10^24 Nm"), bty = "o", lty = c(1, 1), col = c("black", "red", "blue"))


#### lab questions

### question 2
####equation to calculate distance to the crest of forebulge
xb1 = 3 * pi * a1 / 4
xb2 = 3 * pi * a2 / 4
xb3 = 3 * pi * a3 / 4

### question 4

## parameters
E = 70 * 10^9 #Pa
v = 0.25
Te1 = (12 * D1 * (1 - v^2) / E) ^ (1/3)
Te2 = (12 * D2 * (1 - v^2) / E) ^ (1/3)
Te3 = (12 * D3 * (1 - v^2) / E) ^ (1/3)


### question 6 subsidence ~100 m
#D4 = 10^29 #changing D4
#a4 <- (4 * D4 / ((rho_m - rho_w) * g))^(1/4)
#A4 <- V0 * a4^3 / (8 * D4)
#deflection4 <- function(x) {
#  A4 * exp(-x / a4) * (cos(x / a4) + sin(x / a4))
#}
#lines(deflection4(x)~x, col = "orange") #and see how plot changes with respect to sub = 100 m


### question 7 resulting Te4
#Te4 = (12 * D4 * (1 - v^2) / E) ^ (1/3)

### question 8 basin filled with sediment
rho_s = 2650 #kgm-3 #sediment grain density
a5 <- (4 * D2 / ((rho_m - rho_s) * g))^(1/4)
A5 <- V0 * a5^3 / (8 * D2)
deflection5 <- function(x) {
  A5 * exp(-x / a5) * (cos(x / a5) + sin(x / a5))
}
lines(deflection5(x)~x, col = "green")

sub_w = deflection2(0) #max deflection with water 3449 m
sub_s = deflection5(0) #max deflection with sediment 8811 m








