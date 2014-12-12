conBoundaries = function(bw,N=4)
{
  #Check whether the input is valid.
  #Components are either 4-connected or 8-connected.
  
  stopifnot(is.matrix(bw))
  stopifnot(sum(bw*(1-bw))==0)
  stopifnot(N==4||N==8)
  
  #Label the regions in the image.
  L = connec_label(bw,N)
  num_labels = max(L)
  
  #Set up a zeros matrix for saving boundaries of the whole image later.
  BOUNDARY = L
  BOUNDARY[,]=0
  
  #Find boundaries for each region.
  for (n in 1:num_labels){
    segment = L
    #Set up a zeros matrix for saving boundaries of an individual region.
    boundary = L
    boundary[,]=0
    #Subset the region we are looking at. Extract the maxmimum and minimum rows and columns of that region.
    segment[which(!segment==n)]=0
    all.points = which(segment==n, arr.ind=T)
    row.limit = c(min(all.points[,1]), max(all.points[,1]))
    col.limit = c(min(all.points[,2]), max(all.points[,2]))
    
    #For each row, search the positions where the elements change from zero to the label or vice versa.
    for (i in row.limit[1]:row.limit[2]){
      for(j in col.limit[1]:col.limit[2]){
        #When we are in the first column of the image, and if the element is in the region, this is the boundary point. 
        if(j==1 && segment[i,j]==n){
          boundary[i,j] = n
        } else if(j==1 && segment[i,j]!=n){
          boundary[i,j] = 0
        #When we are in the last column of the image, and if the element is in the region, this is the boundary point.
        } else if(j==ncol(segment)&&segment[i,j]==n){
          boundary[i,j] = n
        } else if(j==ncol(segment)&&segment[i,j]!=n){
          boundary[i,j] = 0    
        #When the value of two adjacent elements are different, the bigger one is the boundary point (the background is labeled with zero).
        } else if(segment[i,j]>segment[i,j-1]){
          boundary[i,j] = n
        } else if (segment[i,j]>segment[i,j+1]){
          boundary[i,j] = n
        }
      }
      
    }
    #For each column, search the positions where the elements change from zero to the label or vice versa.
    for (j in col.limit[1]:col.limit[2]){
      for(i in row.limit[1]:row.limit[2]){
        #When we are in the first row of the image, and if the element is in the region, this is the boundary point. 
        if(i==1 && segment[i,j]==n){
          boundary[i,j] = n
        } else if(i==1 && segment[i,j]!=n){
          boundary[i,j] = 0
        #When we are in the last row of the image, and if the element is in the region, this is the boundary point.
        } else if(i==nrow(segment) && segment[i,j]==n){
          boundary[i,j] = n
        } else if(i==nrow(segment) && segment[i,j]!=n){
          boundary[i,j] = 0
        #When the value of two adjacent elements are different, the bigger one is the boundary point (the background is labeled with zero).
        } else if(segment[i,j]>segment[i-1,j]){
          boundary[i,j] = n
        } else if (segment[i,j]>segment[i+1,j]){
          boundary[i,j] = n
        }
      } 
    }   
    #Add all the individual region boundaries.
    BOUNDARY = BOUNDARY+boundary
  }
  return(BOUNDARY)
}
