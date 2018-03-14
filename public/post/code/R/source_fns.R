### function to calculate the clusters given a list of distance objects

three.clust <- function(distance.vector, method = "complete", k=6) {
  
  fdata      <- NULL
  data       <- NULL
  distance   <- distance.vector
  method     <- method
  k          <- k
  
  # browser()
  
  for(i in 1:length(distance)) {
  
  dist <- distance[[i]]
    
  hclust.fn <- function(dist, method, k) {
    dtwarp.2.hclust  <- cutree(hclust(dist, method), k=k)
    h.data <- data.table(as.matrix(dtwarp.2.hclust))
     return(h.data)
  }
  
  kmeans.fn <- function(dist, k) {
    dtwarp.1.kmeans  <- kmeans(dist, k)$cluster  
    k.data  <- data.table(as.matrix(dtwarp.1.kmeans))
     return(k.data)
  }
  
  
  pam.fn <- function(dist, k) {
    dtwarp.1.pamclus <- pam(dist, k = 6)$clustering
    p.data  <- data.table(as.matrix(dtwarp.1.pamclus))
     return(p.data)
  }
  
  h.data <- hclust.fn(dist, method, k)
  k.data <- kmeans.fn(dist, k)
  p.data <- pam.fn(dist, k)

  data   <- cbind(h.data, k.data, p.data)
  colnames(data) <- c("hclust","kmeans","pam")
  
  fdata  <- if(!is.null(fdata)) { cbind(fdata, data) } else { fdata <- data }
  
  }
  
  
  fwrite(fdata, file = "./result/clust_result.csv")
    
}




  
  
  simple.summary <- function(dtwarp.2.pamclus.all) {
    
    summary.all <- summary(dtwarp.2.pamclus.all)
    
    summary.clus  <- list(head = head(summary.all$clustering),tail = tail(summary.all$clustering))
    summary.sil.w <- list(silhoutte_width_head = head(summary.all$silinfo$widths), silhoutte_width_tail = head(summary.all$silinfo$widths),
                          silhoutte_clus_avg_width = summary.all$silinfo$clus.avg.widths, 
                          silhoutte_avg_width = summary.all$silinfo$avg.width)

    summary     <- list(call = summary.all$call, medoids = summary.all$medoids, clustering = summary.clus, objective = summary.all$objective, 
                        clustering_info = summary.all$clusinfo,
                        isolation = summary.all$isolation, 
                        dissimilarity = summary.all$diss, 
                        id_med = summary.all$id.med,
                        summary.sil.w)
    return(summary)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  

