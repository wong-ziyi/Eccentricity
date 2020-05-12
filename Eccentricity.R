list.of.packages <- c("EMD", "pracma")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
library(EMD)
library(pracma)
# First import your data to the variable named res
res<-read.csv("2018.3.1 image1-3 non cut_T0_Z0.csv")
name <- "2018.3.1 image1-3 non cut_T0_Z0"
# Hilbert–Huang transform
try<-emd(c(as.numeric(unlist(res[, 2]))), c(as.numeric(unlist(res[, 1]))), boundary = "wave")
# Make plot space
par(mfrow = c(3,1))
# Plot the original wave
plot(c(as.numeric(unlist(res[, 1]))), c(as.numeric(unlist(res[, 2]))), type = "l", ylab = "Intensity", xlab = "Distance (nm)",
     main = name)
# Plot the last intrinsic mode function (IMF)
plot(c(as.numeric(unlist(res[, 1]))), try$imf[, try$nimf],type = "l", ylab = "Intensity", xlab = "Distance (nm)", main = paste(try$nimf, "-th IMF"))
# Plot the residue
plot(c(as.numeric(unlist(res[, 1]))), try$residue,type = "l", ylab = "Intensity", xlab = "Distance (nm)", main = "Residue")
# Sort the peaks for calculation of mean interval
index<-sort(c(findpeaks(try$imf[, try$nimf])[, 2], findpeaks(-try$imf[, try$nimf])[, 2]))
D<-c() # Array of distance difference bettween every extreme points
for(i in 2:length(index)) {
  temp<-c(as.numeric(unlist(res[, 1])))[index[i]]-c(as.numeric(unlist(res[, 1])))[index[i-1]] # Distance difference between every extreme point
  D<-c(D, temp)
}
# Number of extreme points
extrema(try$imf[, try$nimf])$nextreme
# Number of extreme points per distance
extrema(try$imf[, try$nimf])$nextreme/max(c(as.numeric(unlist(res[, 1]))))
# Calculate the mean interval
mean(D)
# Distance range
max(c(as.numeric(unlist(res[, 1]))))
# Claculate the eccentricity
abs(
  c(as.numeric(unlist(res[, 1])))[match(max(try$residue), try$residue)] - median(c(as.numeric(unlist(res[, 1]))))
) / max(c(as.numeric(unlist(res[, 1]))))
# Show all IMF and residue in .pdf file
pdf(file = "Result.pdf")
plot(c(as.numeric(unlist(res[, 1]))), c(as.numeric(unlist(res[, 2]))), type = "l", ylab = "Intensity", xlab = "Distance (nm)",
     main = name)
lines(c(as.numeric(unlist(res[, 1]))), try$imf[, try$nimf]+10, col = "red")
lines(c(as.numeric(unlist(res[, 1]))), try$residue, col = "blue")
for (i in 1:(try$nimf)) {
  plot(c(as.numeric(unlist(res[, 1]))), try$imf[, i],type = "l", ylab = "Intensity", xlab = "Distance (nm)", main = paste(i, "- IMF"))
}
plot(c(as.numeric(unlist(res[, 1]))), try$residue, ylab = "Intensity", xlab = "Distance (nm)", main = "Residue")
dev.off()