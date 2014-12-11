connec_boundaries = function(bw)
{
  stopifnot(is.matrix(bw))
  stopifnot(sum(bw*(1-bw))==0)
  
  L = connec_label(bw)
  num_labels = max(L)
  boundary = L
  boundary[,]=0
  B = matrix(rep(0, num_labels), nrow = num_labels)
  for (n in 1:num_labels){
    segment = L
    segment[which(!segment==n)]=0
    all.points = which(segment==n, arr.ind=T)
    row.limit = c(min(all.points[,1]), max(all.points[,1]))
    col.limit = c(min(all.points[,2]), max(all.points[,2]))
    for (i in row.limit[1]:(row.limit[2])){
      for(j in col.limit[1]:(col.limit[2])){
        if(j==1 && segment[i,j]==n){
          boundary[i,j] = 1         
        }else if(j==ncol(segment)&&segment[i,j]==n){
          boundary[i,j] = 1  
        }else if(segment[i,j]>segment[i,j-1]){
          boundary[i,j] = 1
        }else if (segment[i,j]>segment[i,j+1]){
          boundary[i,j] = 1
        }
      }
      
    }
    for (j in col.limit[1]:(col.limit[2])){
      for(i in row.limit[1]:(row.limit[2])){
        if(i==1 && segment[i,j]==n){
          boundary[i,j] = 1         
        }else if(i==nrow(segment) && segment[i,j]==n){
          boundary[i,j] = 1
        }else if(segment[i,j]>segment[i-1,j]){
          boundary[i,j] = 1
        }else if (segment[i,j]>segment[i+1,j]){
          boundary[i,j] = 1
        }
      } 
    }    
  }
  
}



bw = matrix(cbind(rbind(matrix(rep(1,25), ncol = 5), 
                        matrix(rep(0,80*5), ncol =5),
                        matrix(rep(1,50), ncol = 5),
                        matrix(rep(0,25), ncol = 5)),
                  rbind(matrix(rep(0,50), ncol = 10),
                        matrix(rep(1,100), ncol = 10),
                        matrix(rep(0, 85*10), ncol =10)),
                  rbind(matrix(rep(0,100*70), ncol = 70)
                  ),
                  rbind(matrix(rep(1, 5*10), ncol =10),
                        matrix(rep(0, 80*10), ncol =10),
                        matrix(rep(1, 100), ncol =10),
                        matrix(rep(0, 50), ncol =10)
                  ),
                  rbind(matrix(rep(0, 5*95), ncol = 5),
                        matrix(rep(1,25), ncol =5))
), nrow=100)
