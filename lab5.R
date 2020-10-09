# Step 1: set working directory & import data

# Step 2: calculate and add columns for thickness and midpoint depth of each unit
ROM$thickness_km <- ROM$depth_base_km - ROM$depth_top_km
ROM$mean_depth_km <- (ROM$depth_base_km + ROM$depth_top_km) / 2

# Step 3: set parameters
pm <- 3330 # mantle density kgm-3
psg <- 2650 # sediment grain density kgm-3
pw <- 1030 # seawater density kgm-3
phi_s <- 0.65 # porosity at surface
c <- 0.35 # compaction constant
Wd <- 0 # water depth at deposition km
dSL <- 0 # change in sea level km

# Step 4: calculate and add columns for porosity at depth (phi_d) of each unit using Eq. 1
ROM$phi_d <- phi_s * exp(-c * ROM$mean_depth_km)

# Step 5: calculate and add columns for de-compacted thicknesses of each unit using Eq. 2
ROM$decomp_thickness_km <- ROM$thickness_km * (1 - ROM$phi_d) / (1 - phi_s)

# Step 6: calculate and add columns for backstripped mean depth and average backstripped porosity
ROM$bkstr_mean_depth_km <- ROM$decomp_thickness_km / 2
ROM$bkstr_phi <- phi_s * exp(-c * ROM$bkstr_mean_depth_km)

# Step 7: calculate and add columns for backstripped bulk density for each unit using Eq . 3
ROM$bkstr_bulk_dens_kgm3 <- (ROM$bkstr_phi * pw) + ((1 - ROM$bkstr_phi) * psg)

#--- Problem set starts

# Step 8: solve for tectonic subsidence for Unit 1 using Eq. 4
U1_Y <- Wd + ROM$decomp_thickness_km[ROM$unit==1] * ((pm - ROM$bkstr_bulk_dens_kgm3[ROM$unit==1]) / (pm - pw)) - dSL * pm *(pm - pw)

# Step 9: solve for tectonic subsidence for Unit 2 3 4
# constants
pm <- 3330 # mantle density kgm-3
psg <- 2650 # sediment grain density kgm-3
pw <- 1030 # seawater density kgm-3
phi_s <- 0.65 # porosity at surface
c <- 0.35 # compaction constant
Wd <- 0 # water depth at deposition km
dSL <- 0 # change in sea level km

# Step 2-2: compacted thickness and mean depth for Unit 1 and 2
thickness_km_2_u1u2 <- ROM$thickness_km[ROM$unit==1] + ROM$thickness_km[ROM$unit==2]
mean_depth_km_2_u1 <- 1/2 * ROM$thickness_km[ROM$unit==1] + ROM$thickness_km[ROM$unit==2]
mean_depth_km_2_u2 <- 1/2 * ROM$thickness_km[ROM$unit==2]

# Step 2-4: compacted porosity of Unit 1 and 2
phi_d_2_u1 <- phi_s * exp(-c * mean_depth_km_2_u1) #Unit 1 is compacted in this case
phi_d_2_u2 <- phi_s * exp(-c * mean_depth_km_2_u2)

# Step 2-5: decompacted thickness of Unit 2
decomp_thickness_km_2_u2 <- ROM$thickness_km[ROM$unit==2] * (1 - phi_d_2_u2) / (1 - phi_s)

# Step 2-6: backstripped mean depth and average backstripped porosity for Unit 2
bkstr_mean_depth_km_2_u2 <- decomp_thickness_km_2_u2 / 2
bkstr_phi_2_u2 <- phi_s * exp(-c * bkstr_mean_depth_km_2_u2)

# Step 2-7: compacted bulk density for Unit 1; backstripped bulk density for Unit 2
bulk_dens_kmg3_2_u1 <- (phi_d_2_u1 * pw) + ((1 - phi_d_2_u1) * psg)
bkstr_bulk_dens_kgm3_2_u2 <- (bkstr_phi_2_u2 * pw) + ((1 - bkstr_phi_2_u2) * psg)

# Step 2-8: solve for tectonic subsidence using bulk density of Unit 1 and Unit 2
U2_Y <- Wd + ROM$thickness_km[ROM$unit==1] * ((pm - bulk_dens_kmg3_2_u1) / (pm - pw)) + decomp_thickness_km_2_u2 * ((pm - bkstr_bulk_dens_kgm3_2_u2) / (pm - pw))- dSL * pm *(pm - pw)


# Step 3-2: compacted thickness and mean depth for Unit 1 2 and 3
mean_depth_km_3_u1 <- 1/2 * ROM$thickness_km[ROM$unit==1] + ROM$thickness_km[ROM$unit==2] + ROM$thickness_km[ROM$unit==3]
mean_depth_km_3_u2 <- 1/2 * ROM$thickness_km[ROM$unit==2] + ROM$thickness_km[ROM$unit==3]
mean_depth_km_3_u3 <- 1/2 * ROM$thickness_km[ROM$unit==3]

# Step 3-4: compacted porosity of Unit 1 2 and 3
phi_d_3_u1 <- phi_s * exp(-c * mean_depth_km_3_u1) #Unit 1 is compacted in this case
phi_d_3_u2 <- phi_s * exp(-c * mean_depth_km_3_u2) #Unit 2 is compacted in this case
phi_d_3_u3 <- phi_s * exp(-c * mean_depth_km_3_u3)

# Step 3-5: decompacted thickness of Unit 3
decomp_thickness_km_3_u3 <- ROM$thickness_km[ROM$unit==3] * (1 - phi_d_3_u3) / (1 - phi_s)

# Step 3-6: backstripped mean depth and average backstripped porosity for Unit 3
bkstr_mean_depth_km_3_u3 <- decomp_thickness_km_3_u3 / 2
bkstr_phi_3_u3 <- phi_s * exp(-c * bkstr_mean_depth_km_3_u3)

# Step 3-7: compacted bulk density for Unit 1 and 2; backstripped bulk density for Unit 3
bulk_dens_kmg3_3_u1 <- (phi_d_3_u1 * pw) + ((1 - phi_d_3_u1) * psg)
bulk_dens_kmg3_3_u2 <- (phi_d_3_u2 * pw) + ((1 - phi_d_3_u2) * psg)
bkstr_bulk_dens_kgm3_3_u3 <- (bkstr_phi_3_u3 * pw) + ((1 - bkstr_phi_3_u3) * psg)

# Step 3-8: solve for tectonic subsidence using bulk density of Unit 1 2 and 3
U3_Y <- Wd + ROM$thickness_km[ROM$unit==1] * ((pm - bulk_dens_kmg3_3_u1) / (pm - pw)) + ROM$thickness_km[ROM$unit==2] * ((pm - bulk_dens_kmg3_3_u2) / (pm - pw)) + decomp_thickness_km_3_u3 * ((pm - bkstr_bulk_dens_kgm3_3_u3) / (pm - pw))- dSL * pm *(pm - pw)


# Step 4-2: compacted thickness and mean depth for Unit 1 2 3 and 4
mean_depth_km_4_u1 <- 1/2 * ROM$thickness_km[ROM$unit==1] + ROM$thickness_km[ROM$unit==2] + ROM$thickness_km[ROM$unit==3] + ROM$thickness_km[ROM$unit==4]
mean_depth_km_4_u2 <- 1/2 * ROM$thickness_km[ROM$unit==2] + ROM$thickness_km[ROM$unit==3] + ROM$thickness_km[ROM$unit==4]
mean_depth_km_4_u3 <- 1/2 * ROM$thickness_km[ROM$unit==3] + ROM$thickness_km[ROM$unit==4]
mean_depth_km_4_u4 <- 1/2 * ROM$thickness_km[ROM$unit==4]

# Step 4-4: compacted porosity of Unit 1 2 3 and 4
phi_d_4_u1 <- phi_s * exp(-c * mean_depth_km_4_u1) #Unit 1 is compacted in this case
phi_d_4_u2 <- phi_s * exp(-c * mean_depth_km_4_u2) #Unit 2 is compacted in this case
phi_d_4_u3 <- phi_s * exp(-c * mean_depth_km_4_u3) #Unit 3 is compacted in this case
phi_d_4_u4 <- phi_s * exp(-c * mean_depth_km_4_u4)

# Step 4-5: decompacted thickness of Unit 4
decomp_thickness_km_4_u4 <- ROM$thickness_km[ROM$unit==4] * (1 - phi_d_4_u4) / (1 - phi_s)

# Step 4-6: backstripped mean depth and average backstripped porosity for Unit 4
bkstr_mean_depth_km_4_u4 <- decomp_thickness_km_4_u4 / 2
bkstr_phi_4_u4 <- phi_s * exp(-c * bkstr_mean_depth_km_4_u4)

# Step 4-7: compacted bulk density for Unit 1 2 and 3; backstripped bulk density for Unit 4
bulk_dens_kmg3_4_u1 <- (phi_d_4_u1 * pw) + ((1 - phi_d_4_u1) * psg)
bulk_dens_kmg3_4_u2 <- (phi_d_4_u2 * pw) + ((1 - phi_d_4_u2) * psg)
bulk_dens_kmg3_4_u3 <- (phi_d_4_u3 * pw) + ((1 - phi_d_4_u3) * psg)
bkstr_bulk_dens_kgm3_4_u4 <- (bkstr_phi_4_u4 * pw) + ((1 - bkstr_phi_4_u4) * psg)

# Step 4-8: solve for tectonic subsidence using bulk density of Unit 1 2 and 3
U4_Y <- Wd + ROM$thickness_km[ROM$unit==1] * ((pm - bulk_dens_kmg3_4_u1) / (pm - pw)) + ROM$thickness_km[ROM$unit==2] * ((pm - bulk_dens_kmg3_4_u2) / (pm - pw)) + ROM$thickness_km[ROM$unit==3] * ((pm - bulk_dens_kmg3_4_u3) / (pm - pw)) + decomp_thickness_km_4_u4 * ((pm - bkstr_bulk_dens_kgm3_4_u4) / (pm - pw))- dSL * pm *(pm - pw)

# add columns
ROM$tectonic_sub_km <- c(U4_Y, U3_Y, U2_Y, U1_Y, NA)

#### Part 3
# Total subsidence
U1_T = ROM$decomp_thickness_km[ROM$unit==1]
U2_T = ROM$thickness_km[ROM$unit==1] + ROM$decomp_thickness_km[ROM$unit==2]
U3_T = ROM$thickness_km[ROM$unit==1] + ROM$thickness_km[ROM$unit==2] + ROM$decomp_thickness_km[ROM$unit==3]
U4_T = ROM$thickness_km[ROM$unit==1] + ROM$thickness_km[ROM$unit==2] + ROM$thickness_km[ROM$unit==3] + ROM$decomp_thickness_km[ROM$unit==4]
T <- c(U4_T, U3_T, U2_T, U1_T)

ROM$total_sub_km <- c(U4_T, U3_T, U2_T, U1_T, NA)

# Q1 plot subsidence against time
Y <- c(U4_Y, U3_Y, U2_Y, U1_Y)
plot(Y~ROM$mean_age_Ma[1:4], type = "o", ylim = rev(c(0, 14)), ylab = "Depth (km)", xlim = rev(c(0, 190)), xlab = "Age (Ma)", main = "Tectonic vs Sediment Loading Subsidence")
lines(T~ROM$mean_age_Ma[1:4], type = "o", col = "red")
legend("topright", legend = c("Tectonic Subsidence", "Total Subsidence"), bty = "n", lty = c(1, 1), col = c("black", "red"))

# Q2 export csv
write.csv(ROM, file = "lab5_ROM.csv")

# Q4
d75_q4 = 0.75 * U4_T
# polynomial function calculated in Excel for five data points: y = -1.2367x2 + 7.3961x + 166.5
# with y being the time and x being the depth
# substitute depth = d75_q4
# time = 126 Ma

# Q5
# 160 Ma
# p160_q5 = ROM$tectonic_sub_km[ROM$unit==1] / ROM$total_sub_km[ROM$unit==1] * 100
# p120_q5 = ROM$tectonic_sub_km[ROM$unit==2] / ROM$total_sub_km[ROM$unit==2] * 100
# p80_q5 = ROM$tectonic_sub_km[ROM$unit==3] / ROM$total_sub_km[ROM$unit==3] * 100
# p30_q5 = ROM$tectonic_sub_km[ROM$unit==4] / ROM$total_sub_km[ROM$unit==4] * 100

p160_q5 = (ROM$total_sub_km[ROM$unit==1] - ROM$tectonic_sub_km[ROM$unit==1]) / ROM$total_sub_km[ROM$unit==1] * 100
p120_q5 = (ROM$total_sub_km[ROM$unit==2] - ROM$tectonic_sub_km[ROM$unit==2]) / ROM$total_sub_km[ROM$unit==2] * 100
p80_q5 = (ROM$total_sub_km[ROM$unit==3] - ROM$tectonic_sub_km[ROM$unit==3]) / ROM$total_sub_km[ROM$unit==3] * 100
p30_q5 = (ROM$total_sub_km[ROM$unit==4] - ROM$tectonic_sub_km[ROM$unit==4]) / ROM$total_sub_km[ROM$unit==4] * 100

ROM$sediment_total_sub = c(p30_q5, p80_q5, p120_q5, p160_q5, NA)


