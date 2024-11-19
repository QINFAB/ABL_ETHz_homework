#-------------------------------------------------------------------------------
#-- Premininaries. E.g., define constans

g  = 9.81 # acceleration due to gravity (m s-2)
vK = 0.4  # von Karman constant

#-------------------------------------------------------------------------------
#-- Read data and define relevant variables (as the variable names are given
#-- in the header, we could simply use the command "attach(...)". The chosen
#-- option gives, however, more flexibility)

FilePath = "D:\\Week9_20241115/data/" # Please indicate here the folder where you placed the data,
                   # with the full Path if necessary.

FileName = paste0(FilePath, "SurfaceData_Cabauw_05-10-May-2008.txt")
SFCData  = read.table(FileName, na.strings = "NaN", header = TRUE)

# #-- Inspect data structure
# head(SFCData)

z2  = 2
z5  = 5
z10 = 10

hour  = SFCData$time

T2    = SFCData$Ta002 + 273.15
U10   = SFCData$U010
ustar = SFCData$ust005
wT    = SFCData$wT005

#-------------------------------------------------------------------------------
#-- Evaluate requested quantities

L   = -ustar^3/(vK*9.81/T2*wT)
zoL = z5/L

#-------------------------------------------------------------------------------
#-- Plot results

par(mar = c(3, 3, 1, 1))
par(mgp = c(1.3, 0.2, 0))
par(tck = 0.012)
par(cex.axis = 0.9)
par(cex.lab = 1.0)

plot(hour, L, pch = 21, bg = "tomato",
     xlab = "hour", ylab = "L (m)",
     xlim = c(0, 24), ylim = c(-150, 200),
     xaxt = "n")
axis(1, at = seq(0, 24, 6), labels = as.character(seq(0, 24, 6)))

abline(h = 0, lty = 5)

abline(v = 4, lty = 5)
abline(v = 19, lty = 5)
text(3.5, -120, "sunrise", srt = 90)
text(19.5, -120, "sunset", srt = 90)
abline(v = 6, lty = 1)
abline(v = 16.5, lty = 1)

par(mfrow = c(1, 1))


