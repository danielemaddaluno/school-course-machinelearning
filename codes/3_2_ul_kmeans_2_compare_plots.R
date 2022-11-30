# Clean enviroment
rm(list=ls())

# Load libraries
library(jpeg)
library(png)
library(countcolors)
library(plyr)

# Function to download image from an url
dowloadFile <- function(url, destfile) {
  if(!file.exists(destfile)){
    download.file(url = url, destfile=destfile)
  }
}

# Function to read an image from a path (just png and jpg)
readImage = function(path){
  if(endsWith(path, ".png")){
    return(readPNG(path))
  } else if(endsWith(path, ".jpg")){
    return(readJPEG(path))
  } else{
    return(NULL)
  }
}

#
# Segmenter
# 
segment_image = function(img, n){
  # create a flat, segmented image data set using kmeans
  # Segment an RGB image into n groups based on color values using Kmeans
  df = data.frame(
    red = matrix(img[,,1], ncol=1),
    green = matrix(img[,,2], ncol=1),
    blue = matrix(img[,,3], ncol=1)
  )
  K = kmeans(df,n)
  df$label = K$cluster
  
  # compute rgb values and color codes based on Kmeans centers
  colors = data.frame(
    label = 1:nrow(K$centers), 
    R = K$centers[,"red"],
    G = K$centers[,"green"],
    B = K$centers[,"blue"],
    color=rgb(K$centers)
  )
  
  # merge color codes on to df but maintain the original order of df
  df$order = 1:nrow(df)
  # merge is like a join
  df = merge(df, colors)
  # df = df[order(df$order),]
  # https://stackoverflow.com/a/21865778/3138238
  df = arrange(df, order)
  df$order = NULL
  
  return(df)
  
}


#
# Reconstitue the segmented images to RGB matrix
#
build_segmented_image = function(df, img){
  # reconstitue the segmented images to RGB array
  
  # get mean color channel values for each row of the df.
  R = matrix(df$R, nrow=dim(img)[1])
  G = matrix(df$G, nrow=dim(img)[1])
  B = matrix(df$B, nrow=dim(img)[1])
  
  # reconsitute the segmented image in the same shape as the input image
  img_segmented = array(dim=dim(img))
  img_segmented[,,1] = R
  img_segmented[,,2] = G
  img_segmented[,,3] = B
  
  return(img_segmented)
}





# download images
img_folder = "data/images"

images = data.frame(
  name = c(
    "Mandrill", 
    "RGB", 
    "Hats", 
    "Barn"
  ),
  path = c(
    paste(img_folder, "/" ,"mandrill.png", sep = ""),
    paste(img_folder, "/" ,"RGB_illumination.jpg", sep = ""),
    paste(img_folder, "/" ,"kodim03.png", sep = ""),
    paste(img_folder, "/" ,"kodim22.png", sep = "")
  ),
  url = c(
    "https://i1.wp.com/rwalk.xyz/wp-content/uploads/2016/01/mandrill-1.png?resize=300%2C300&ssl=1",
    "https://upload.wikimedia.org/wikipedia/commons/2/28/RGB_illumination.jpg",
    "http://r0k.us/graphics/kodak/kodak/kodim03.png",
    "http://r0k.us/graphics/kodak/kodak/kodim22.png"
  )
)

View(images)


# download images
num_images = dim(images)[1]
for (index in 1:num_images){
  path = images$path[index]
  url = images$url[index]
  dowloadFile(url = url, destfile=path)
}


num_clusters = 5
# sets the number of rows/cols for the plot
par(mfrow=c(num_images, num_clusters + 1))

for (index in 1:num_clusters){
  imgName = images$name[index]
  imgMatrix = readImage(images$path[index])
  
  plotArrayAsImage(imgMatrix, main = imgName)
  
  for(K in 1: num_clusters){
    clusteringResult = segment_image(imgMatrix, K)
    segmentedImage = build_segmented_image(clusteringResult, imgMatrix)
    
    kName = paste(imgName, " K=", K, sep = "")
    plotArrayAsImage(segmentedImage, main = kName)
  }
}
