## Given a 1-d matrix index, return 2-d index
getRowCol <- function(index, numRow) {
  row = 0
  column = 0
  if (index %% numRow == 0) {
    row = numRow
    column = index / numRow
  } else {
    row = index %% numRow
    column = floor(index / numRow) + 1
  }
  
  return(c(row, column))
}

## Rotate coordinates so the avenues are facing east/west
rotateCoords <- function(x, y) {
  origCoords = rbind(x, y)
  alpha = -pi/3.35
  rotMat = matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)), ncol = 2)
  rotCoords = rotMat %*% origCoords
  return(rotCoords)
}

## Find coordinates for the most dangerous intersections (lower Manhattan)
findDangerLower <- function(x, y, lambda) {
  origCoords = rbind(x, y)
  alpha = -pi/3.35
  rotMat = matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)), ncol = 2)
  rotCoords = rotMat %*% origCoords
  
  lowerDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.98, -10.88, 83.73, 83.78), h = c(.003, .003))
  lowerDensity[[3]] = max(lowerDensity[[3]]) - lowerDensity[[3]]
  lowerDangerInd1d = which(lowerDensity[[3]] < lambda)
  lowerDangerInd2d = sapply(lowerDangerInd1d, getRowCol, 200)
  
  lowerRotDanger = rbind(lowerDensity[[1]][lowerDangerInd2d[1,]], lowerDensity[[2]][lowerDangerInd2d[2,]])
  antirotM = matrix(c(cos(-alpha),sin(-alpha),-sin(-alpha),cos(-alpha)), ncol = 2)
  lowerDanger = antirotM %*% lowerRotDanger
  
  return(lowerDanger)
}

## Find coordinates for the most dangerous intersections (upper Manhattan)
findDangerUpper <- function(x, y, lambda) {
  origCoords = rbind(x, y)
  alpha = -pi/3.35
  rotMat = matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)), ncol = 2)
  rotCoords = rotMat %*% origCoords
  
  upperDensity = kde2d(rotCoords[1,], rotCoords[2,], n = c(200, 100), lims = c(-10.88, -10.77, 83.74, 83.78), h = c(.003, .003))
  upperDensity[[3]] = max(upperDensity[[3]]) - upperDensity[[3]]
  upperDangerInd1d = which(upperDensity[[3]] < lambda)
  upperDangerInd2d = sapply(upperDangerInd1d, getRowCol, 200)
  
  upperRotDanger = rbind(upperDensity[[1]][upperDangerInd2d[1,]], upperDensity[[2]][upperDangerInd2d[2,]])
  antirotM = matrix(c(cos(-alpha),sin(-alpha),-sin(-alpha),cos(-alpha)), ncol = 2)
  upperDanger = antirotM %*% upperRotDanger
  
  return(upperDanger)
}