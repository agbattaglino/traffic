library("MASS")
dat = read.csv("NYPD_Motor_Vehicle_Collisions.csv")

###################################
#                                 #
#   A L L   C O L L I S I O N S   #
#                                 #
###################################


## Get observations with valid lat/lon in Manhattan
manhAll = which(!is.na(dat[,"LONGITUDE"]) & dat[,"BOROUGH"] == "MANHATTAN")
#plot(dat[manhAll, "LONGITUDE"], dat[manhAll, "LATITUDE"], pch = 16, cex = .4)

## Lower Manhattan
lowerDanger = findDangerLower(dat[manhAll, "LONGITUDE"], dat[manhAll, "LATITUDE"], 500)
#png("VisExample2.png", width = 720, height = 720)
plot(lowerDanger[1,], lowerDanger[2,], xlab = 'Longitude', ylab = 'Latitude')
theCenters = rbind(c(-73.99287, 40.75831), c(-73.98953, 40.75773), c(-73.98650, 40.75675), c(-73.99025, 40.75078),
                   c(-73.9669, 40.761), c(-73.964, 40.76), c(-73.975, 40.747), c(-73.994, 40.72), c(-73.995, 40.76))
lowerDangerCenter = kmeans(t(lowerDanger), theCenters, iter.max = 10)
#points(theCenters[,1], theCenters[,2], pch = 16, col = rgb(1, 0, 0))
points(lowerDangerCenter$centers[,1], lowerDangerCenter$centers[,2], pch = 16, col = rgb(0, 0, 1))
#dev.off()

lowerCoordsOut = cbind(lowerDangerCenter$centers[,2], lowerDangerCenter$centers[,1])
write.table(lowerCoordsOut, "coords/lowerDangerAll.csv", sep = ",", row.names = FALSE, col.names = FALSE)


## Upper Manhattan
upperDanger = findDangerUpper(dat[manhAll, "LONGITUDE"], dat[manhAll, "LATITUDE"], 280)
plot(upperDanger[1,], upperDanger[2,])
theCenters = rbind(c(-73.958, 40.816), c(-73.956, 40.814), c(-73.951, 40.81), c(-73.945, 40.809),
                   c(-73.9365, 40.806), c(-73.9335, 40.805), c(-73.931, 40.849),
                   c(-73.947, 40.785), c(-73.945, 40.784))
upperDangerCenter = kmeans(t(upperDanger), theCenters, iter.max = 10)
#points(theCenters[,1], theCenters[,2], pch = 16, col = rgb(1, 0, 0))
points(upperDangerCenter$centers[,1], upperDangerCenter$centers[,2], pch = 16, col = rgb(1, 0, 0))

upperCoordsOut = cbind(upperDangerCenter$centers[,2], upperDangerCenter$centers[,1])
write.table(upperCoordsOut, "coords/upperDangerAll.csv", sep = ",", row.names = FALSE, col.names = FALSE)


#################################################
#                                               #
#   P E D E S T R I A N   C O L L I S I O N S   #
#                                               #
#################################################


manhPeds = which(!is.na(dat[,"LONGITUDE"]) & dat[,"BOROUGH"] == "MANHATTAN" &
                 (dat[,"NUMBER.OF.PEDESTRIANS.INJURED"] != 0 | dat[,"NUMBER.OF.PEDESTRIANS.KILLED"] != 0))
#plot(dat[manhPeds, "LONGITUDE"], dat[manhPeds, "LATITUDE"], pch = 16, cex = .4)

## Lower Manhattan
lowerDanger = findDangerLower(dat[manhPeds, "LONGITUDE"], dat[manhPeds, "LATITUDE"], 500)
plot(lowerDanger[1,], lowerDanger[2,])
theCenters = rbind(c(-73.99287, 40.75831), c(-73.98953, 40.75773), c(-73.98650, 40.75675), c(-73.99025, 40.75078),
                   c(-73.98762, 40.75010), c(-73.98477, 40.74801), c(-73.98254, 40.73141), c(-73.98853, 40.71898))
lowerDangerCenter = kmeans(t(lowerDanger), theCenters, iter.max = 10)
#points(theCenters[,1], theCenters[,2], pch = 16, col = rgb(1, 0, 0))
points(lowerDangerCenter$centers[,1], lowerDangerCenter$centers[,2], pch = 16, col = rgb(1, 0, 0))

lowerCoordsOut = cbind(lowerDangerCenter$centers[,2], lowerDangerCenter$centers[,1])
write.table(lowerCoordsOut, "coords/lowerDangerPeds.csv", sep = ",", row.names = FALSE, col.names = FALSE)


## Upper Manhattan
upperDanger = findDangerUpper(dat[manhPeds, "LONGITUDE"], dat[manhPeds, "LATITUDE"], 750)
plot(upperDanger[1,], upperDanger[2,])
theCenters = rbind(c(-73.97137, 40.79445), c(-73.93972, 40.79847), c(-73.93549, 40.79605), c(-73.93803, 40.80432),
                   c(-73.94818, 40.80880), c(-73.95585, 40.81337), c(-73.94067, 40.81425))
upperDangerCenter = kmeans(t(upperDanger), theCenters, iter.max = 10)
#points(theCenters[,1], theCenters[,2], pch = 16, col = rgb(1, 0, 0))
points(upperDangerCenter$centers[,1], upperDangerCenter$centers[,2], pch = 16, col = rgb(1, 0, 0))

upperCoordsOut = cbind(upperDangerCenter$centers[,2], upperDangerCenter$centers[,1])
write.table(upperCoordsOut, "coords/upperDangerPeds.csv", sep = ",", row.names = FALSE, col.names = FALSE)


###########################################
#                                         #
#   C Y C L I S T   C O L L I S I O N S   #
#                                         #
###########################################


manhCycs = which(!is.na(dat[,"LONGITUDE"]) & dat[,"BOROUGH"] == "MANHATTAN" &
                 (dat[,"NUMBER.OF.CYCLIST.INJURED"] != 0 | dat[,"NUMBER.OF.CYCLIST.KILLED"] != 0))
#plot(dat[manhPeds, "LONGITUDE"], dat[manhPeds, "LATITUDE"], pch = 16, cex = .4)

## Lower Manhattan
lowerDanger = findDangerLower(dat[manhCycs, "LONGITUDE"], dat[manhCycs, "LATITUDE"], 380)
plot(lowerDanger[1,], lowerDanger[2,])
theCenters = rbind(c(-74.01270, 40.71677), c(-73.993, 40.72), c(-73.99, 40.72), c(-73.986, 40.719),
                   c(-73.99, 40.735), c(-73.993, 40.742), c(-73.99, 40.741), c(-73.987, 40.74),
                   c(-73.98, 40.739), c(-73.975, 40.746), c(-73.973, 40.764))
lowerDangerCenter = kmeans(t(lowerDanger), theCenters, iter.max = 10)
#points(theCenters[,1], theCenters[,2], pch = 16, col = rgb(1, 0, 0))
points(lowerDangerCenter$centers[,1], lowerDangerCenter$centers[,2], pch = 16, col = rgb(1, 0, 0))

lowerCoordsOut = cbind(lowerDangerCenter$centers[,2], lowerDangerCenter$centers[,1])
write.table(lowerCoordsOut, "coords/lowerDangerCycs.csv", sep = ",", row.names = FALSE, col.names = FALSE)


## Upper Manhattan
upperDanger = findDangerUpper(dat[manhCycs, "LONGITUDE"], dat[manhCycs, "LATITUDE"], 420)
plot(upperDanger[1,], upperDanger[2,])
theCenters = rbind(c(-73.952, 40.77), c(-73.9495, 40.781), c(-73.965, 40.791), c(-73.96, 40.80),
                   c(-73.946, 40.797), c(-73.954, 40.806), c(-73.9523, 40.804),
                   c(-73.953, 40.81), c(-73.951, 40.81), c(-73.946, 40.82))
upperDangerCenter = kmeans(t(upperDanger), theCenters, iter.max = 10)
#points(theCenters[,1], theCenters[,2], pch = 16, col = rgb(1, 0, 0))
points(upperDangerCenter$centers[,1], upperDangerCenter$centers[,2], pch = 16, col = rgb(1, 0, 0))

upperCoordsOut = cbind(upperDangerCenter$centers[,2], upperDangerCenter$centers[,1])
write.table(upperCoordsOut, "coords/upperDangerCycs.csv", sep = ",", row.names = FALSE, col.names = FALSE)