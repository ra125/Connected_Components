conLabel = function(mat, N=4)
{
  #Employ single-pass algorithm
  #Check whether the input is valid.
  #Components are either 4-connected or 8-connected.
  
  stopifnot(is.matrix(mat))
  stopifnot(sum(mat*(1-mat))==0)
  stopifnot(N==4||N==8)
  
  m=nrow(mat)
  n=ncol(mat)
  
  #Set up a zeros matrix for saving labels later.
  connec=matrix(rep(0,m*n),nrow=m,ncol=n)
  
  #Initialize the region label. 
  mark=1
  
  #Search by each row and column.
  for(i in 1:m)
  {
    for(j in 1:n)
    {
      if(mat[i,j]==1)
      {
        #Position of the first detected element in the region.
        index=(j-1)*m+i
        
        #Label the first element.
        connec[index]=mark
        while(length(index)!=0)
        {
          #Set visited element equal to zero, that we will not visit again.
          mat[index]=0
          neighbors=list()
          for(k in 1:length(index))
          {
            #If the element is in the bottom of the mat, it will only have left, right, and upper neighbors.
            if(index[k]%%m==0 && index[k]!=m*n)
            {
              if (N==4){
                offset=c(-1,m,-m)
              }else{
                offset=c(-1,m,-m, m-1,-m-1)
              }
              #If the element is in the top of the mat, it will only have left, right, and lower neighbors.
            } else if(index[k]%%m==1 && index[k]!=(n-1)*m+1)
            {
              if (N==4){
                offset = c(1,m,-m)
              }else{
                offset = c(1,m,-m, m+1, -m+1)
              }
              #If the element is in the very right of the mat, it will only have left, upper and lower neighbors.
            } else if(index[k]%/%m==n-1 && index[k]!=(n-1)*m+1 && index[k]!=m*n)
            {
              if(N==4){
                offset=c(1,-1,-m)
              }else{
                offset=c(1,-1,-m, -m+1,-m-1)
              }
              #If the element is in the bottom right corner of the mat, it will only have left and upper neighbors.
            } else if(index[k]==m*n)
            {
              if(N==4){
                offset=c(-1,-m)
              }else{
                offset= c(-1,-m, -m-1)
              }
              #If the element is in the topright corner of the mat, it will only have left and lower neighbors.
            } else if(index[k]==(n-1)*m+1)
            {
              if(N==4){
                offset=c(1,-m)
              }else{
                offset = c(1,-m,-m+1)
              }
              #Otherwise, the element has all four neighbors.
            } else {
              if(N==4){
                offset=c(-1,m,1,-m)
              }else{
                offset = c(-1,m,1,-m, m+1, m-1, -m-1, -m+1)
              }
            }
            
            neighbors=unlist(c(neighbors,index[k]+offset))
          }
          neighbors=unique(neighbors)
          
          #Get rid of all the neighbors that are out of the boundaries.
          neighbors=neighbors[neighbors>0]
          
          #Extract the position of all the neighbor elements that are in the region and label the element. 
          index=neighbors[which(mat[neighbors]==1)]
          connec[index]=mark
        }
        
        #Update the label before moving to the next region.
        mark=mark+1
      }
    }
  }
  
  return(connec)
  
}