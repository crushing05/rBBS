#' RemoveOutliers
#'
#' Removes routes that are likely outliers (far outside "likely" breeding range)
#' 
#' Estimates for each route \code{i} the mean distance of each route to its \code{k} nearest neighbors (knn-distance). Any route with knn-distance greater than \code{tresh}*sd above the mean(knn-distance) is considered an outlier and removed from the data 
#' @param counts Species count data frame obtained from GetSppCounts
#' @param thresh Number of standard deviations beyond mean knn distance to be considered outlier (default = 5)
#' @param k Number k on nearest neighbors
#' @return Data frame with same format as \code{counts} but with outlier routes removed
#' @export

RemoveOutliers <- function(counts, thresh = 5, k = 5){
  route_xy <- as.matrix(counts[!duplicated(counts$routeID), c("Longitude", "Latitude")])
  dist.mat <- geosphere::distm(route_xy)
  nn <- apply(dist.mat, 1, function(x) sort(x)[2:(k + 1)])
  nn.dist <- colMeans(nn)
  mu.nn <- mean(nn.dist)
  sd.nn <- sd(nn.dist)
  
  cutoff <- mu.nn + thresh * sd.nn
  
  counts2 <- counts[which(nn.dist < cutoff),]
}
