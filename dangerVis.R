library("MASS")
dat = read.csv("NYPD_Motor_Vehicle_Collisions.csv")

###################################
#                                 #
#   A L L   C O L L I S I O N S   #
#                                 #
###################################


## All collisions in Manhattan with a valid lat/lon
manhAll = which(!is.na(dat[,"LONGITUDE"]) & dat[,"BOROUGH"] == "MANHATTAN")

## Rotate so that the avenues are pointing east/west.  This makes computation a bit easier.
origCoords = rbind(dat[manhAll, "LONGITUDE"], dat[manhAll, "LATITUDE"])
alpha = -pi/3.35
rotMat = matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)), ncol = 2)
rotCoords = rotMat %*% origCoords
#plot(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)

## Esimate the density of crashes and find the peaks
lowerDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.98, -10.88, 83.73, 83.78), h = c(.003, .003))
lowerDensity[[3]] = max(lowerDensity[[3]]) - lowerDensity[[3]]
lowerDangerInd1d = which(lowerDensity[[3]] < 500)
lowerDangerInd2d = sapply(lowerDangerInd1d, getRowCol, 200)

## Visualize density with most dangerous areas
png("viz/LowerManhAll.png", width = 960, height = 620)
image(lowerDensity, xaxt = 'n', yaxt = 'n')
points(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)
lowerDanger = read.csv("coords/lowerDangerAll.csv", header = FALSE)
rotLowerDanger = rotMat %*% rbind(t(lowerDanger)[2,], t(lowerDanger)[1,])
points(rotLowerDanger[1,], rotLowerDanger[2,], cex = 1.5, pch = 16, col = rgb(0, 0, 1))
dev.off()


## Esimate the density of crashes and find the peaks
upperDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.88, -10.77, 83.74, 83.78), h = c(.003, .003))
upperDensity[[3]] = max(upperDensity[[3]]) - upperDensity[[3]]
upperDangerInd1d = which(upperDensity[[3]] < 280)
upperDangerInd2d = sapply(upperDangerInd1d, getRowCol, 200)

## Visualize density with most dangerous areas
png("viz/UpperManhAll.png", width = 960, height = 620)
image(upperDensity, xaxt = 'n', yaxt = 'n')
points(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)
upperDanger = read.csv("coords/upperDangerAll.csv", header = FALSE)
rotUpperDanger = rotMat %*% rbind(t(upperDanger)[2,], t(upperDanger)[1,])
points(rotUpperDanger[1,], rotUpperDanger[2,], cex = 1.5, pch = 16, col = rgb(0, 0, 1))
dev.off()


#################################################
#                                               #
#   P E D E S T R I A N   C O L L I S I O N S   #
#                                               #
#################################################


manhPeds = which(!is.na(dat[,"LONGITUDE"]) & dat[,"BOROUGH"] == "MANHATTAN" &
                   (dat[,"NUMBER.OF.PEDESTRIANS.INJURED"] != 0 | dat[,"NUMBER.OF.PEDESTRIANS.KILLED"] != 0))

origCoords = rbind(dat[manhPeds, "LONGITUDE"], dat[manhPeds, "LATITUDE"])
alpha = -pi/3.35
rotMat = matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)), ncol = 2)
rotCoords = rotMat %*% origCoords
#plot(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)

## Esimate the density of crashes and find the peaks
lowerDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.98, -10.88, 83.73, 83.78), h = c(.003, .003))
lowerDensity[[3]] = max(lowerDensity[[3]]) - lowerDensity[[3]]
lowerDangerInd1d = which(lowerDensity[[3]] < 500)
lowerDangerInd2d = sapply(lowerDangerInd1d, getRowCol, 200)

## Visualize density with most dangerous areas
png("viz/LowerManhPeds.png", width = 960, height = 620)
image(lowerDensity, xaxt = 'n', yaxt = 'n')
points(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)
lowerDanger = read.csv("coords/lowerDangerPeds.csv", header = FALSE)
rotLowerDanger = rotMat %*% rbind(t(lowerDanger)[2,], t(lowerDanger)[1,])
points(rotLowerDanger[1,], rotLowerDanger[2,], cex = 1.5, pch = 16, col = rgb(0, 0, 1))
dev.off()

## Esimate the density of crashes and find the peaks
upperDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.88, -10.77, 83.74, 83.78), h = c(.003, .003))
upperDensity[[3]] = max(upperDensity[[3]]) - upperDensity[[3]]
upperDangerInd1d = which(upperDensity[[3]] < 750)
upperDangerInd2d = sapply(upperDangerInd1d, getRowCol, 200)

## Visualize density with most dangerous areas
png("viz/UpperManhPeds.png", width = 960, height = 620)
image(upperDensity, xaxt = 'n', yaxt = 'n')
points(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)
upperDanger = read.csv("coords/upperDangerPeds.csv", header = FALSE)
rotUpperDanger = rotMat %*% rbind(t(upperDanger)[2,], t(upperDanger)[1,])
points(rotUpperDanger[1,], rotUpperDanger[2,], cex = 1.5, pch = 16, col = rgb(0, 0, 1))
dev.off()


###########################################
#                                         #
#   C Y C L I S T   C O L L I S I O N S   #
#                                         #
###########################################


manhCycs = which(!is.na(dat[,"LONGITUDE"]) & dat[,"BOROUGH"] == "MANHATTAN" &
                   (dat[,"NUMBER.OF.CYCLIST.INJURED"] != 0 | dat[,"NUMBER.OF.CYCLIST.KILLED"] != 0))

origCoords = rbind(dat[manhCycs, "LONGITUDE"], dat[manhCycs, "LATITUDE"])
alpha = -pi/3.35
rotMat = matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)), ncol = 2)
rotCoords = rotMat %*% origCoords
#plot(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)

## Esimate the density of crashes and find the peaks
lowerDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.98, -10.88, 83.73, 83.78), h = c(.003, .003))
lowerDensity[[3]] = max(lowerDensity[[3]]) - lowerDensity[[3]]
lowerDangerInd1d = which(lowerDensity[[3]] < 380)
lowerDangerInd2d = sapply(lowerDangerInd1d, getRowCol, 200)

## Visualize density with most dangerous areas
png("viz/LowerManhCycs.png", width = 960, height = 620)
image(lowerDensity, xaxt = 'n', yaxt = 'n')
points(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)
lowerDanger = read.csv("coords/lowerDangerCycs.csv", header = FALSE)
rotLowerDanger = rotMat %*% rbind(t(lowerDanger)[2,], t(lowerDanger)[1,])
points(rotLowerDanger[1,], rotLowerDanger[2,], cex = 1.5, pch = 16, col = rgb(0, 0, 1))
dev.off()

## Esimate the density of crashes and find the peaks
upperDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.88, -10.77, 83.74, 83.78), h = c(.003, .003))
upperDensity[[3]] = max(upperDensity[[3]]) - upperDensity[[3]]
upperDangerInd1d = which(upperDensity[[3]] < 420)
upperDangerInd2d = sapply(upperDangerInd1d, getRowCol, 200)

## Visualize density with most dangerous areas
png("viz/UpperManhCycs.png", width = 960, height = 620)
image(upperDensity, xaxt = 'n', yaxt = 'n')
points(rotCoords[1,], rotCoords[2,], pch = 16, cex = .4)
upperDanger = read.csv("coords/upperDangerCycs.csv", header = FALSE)
rotUpperDanger = rotMat %*% rbind(t(upperDanger)[2,], t(upperDanger)[1,])
points(rotUpperDanger[1,], rotUpperDanger[2,], cex = 1.5, pch = 16, col = rgb(0, 0, 1))
dev.off()
