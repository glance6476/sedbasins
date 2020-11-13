## import data

## create a plot of depositional age vs. cooling age

plot(DATA$age_dep~DATA$age_cooling, 
     type = "p", 
     pch = 19, 
     las = 1, 
     ylim = rev(c(0, 140)), 
     xlim = c(0, 140), 
     ylab = "depositional age (Ma)", 
     xlab = "AHe cooling age (Ma)")

## deleting AHe-7

DATA = DATA[-c(7),]

## add lag time to data frame

DATA$lag_time <- DATA$age_cooling - DATA$age_dep

## plot lag time against depo age

plot(DATA$lag_time~DATA$age_dep,
     type = "b", 
     pch = 19, 
     las = 1, 
     ylim = c(0, 60),
     xlim = c(0, 120),
     ylab = "lag time (Ma)", 
     xlab = "depositional age (Ma)")

## calculate the closure temperature depth in km

# import parameters
G = 25 #dC/km
Tc = 90 #dC
Ts = 10 #dC

# calculations
Tc_depth = (Tc - Ts) / G #=3.2 km

## calculate the exhumation rate in km/Ma

DATA$exhu_rate = (Tc - Ts) / (G * DATA$lag_time)

## plot exhumation rate vs depositional age

plot(DATA$exhu_rate~DATA$age_dep,
     type = "b", 
     pch = 19, 
     las = 1, 
     ylim = c(0, 1.2),
     xlim = c(0, 120),
     ylab = "exhumation rate (km/Ma)", 
     xlab = "depositional age (Ma)")











