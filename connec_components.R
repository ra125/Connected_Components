connec_label=function(image)
{
  stopifnot(is.matrix(image))
  stopifnot(sum(image*(1-image))==0)
  
  m=nrow(image)
  n=ncol(image)
  
  connec=matrix(rep(0,m*n),nrow=m,ncol=n)
  mark=1
  diff=1
  offset=c(-1,m,1,-m)
  nof=0
  
  for(i in 1:m)
  {
    for(j in 1:n)
    {
      if(image[i,j]==1)
      {
        nof=nof+1
        index=(j-1)*m+i
        connec[index]=mark
        while(length(index)!=0)
        {
          image[index]=0
          neighbors=list()
          for(k in 1:length(index))
          {
            if(index[k]%%m==0 && index[k]!=m*n)
            {
              offset=c(-1,m,-m)
            } else if(index[k]%%m==1 && index[k]!=(n-1)*m+1)
            {
              offset=c(1,m,-m)
            } else if(index[k]%/%m==n-1 && index[k]!=(n-1)*m+1 && index[k]!=m*n)
            {
              offset=c(1,-1,-m)
            } else if(index[k]==m*n)
            {
              offset=c(-1,-m)
            } else if(index[k]==(n-1)*m+1)
            {
              offset=c(1,-m)
            } else {offset=c(-1,m,1,-m)}
            
            neighbors=unlist(c(neighbors,index[k]+offset))
          }
          neighbors=unique(neighbors)
          neighbors=neighbors[neighbors>0]
          index=neighbors[which(image[neighbors]==1)]
          connec[index]=mark
        }
        mark=mark+diff
      }
    }
  }
  
  return(connec)
  
}