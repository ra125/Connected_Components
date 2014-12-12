imgMat = function(png)
{
  #Read the PNG file
  img = readPNG(png)
  
  #Sum up all the layers and normalize the image data
  scl_img=img[,,1]+img[,,2]+img[,,3]
  scl_img=scl_img/max(scl_img)
  
  #Set white pixels equal to 0 and black to 1
  scl_img=1-scl_img
  scl_img[which(scl_img>0.1)]=1
  scl_img[which(scl_img<=0.1)]=0
  return(scl_img)
}