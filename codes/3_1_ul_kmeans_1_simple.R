# Clean enviroment
rm(list=ls())

# Load libraries
library(jpeg)
library(png)
library(countcolors)
library(rgl)

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
# Kmeans based segmenter
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
  df = merge(df, colors)
  df = df[order(df$order),]
  df$order = NULL
  
  return(df)
  
}

#
# reconstitue the segmented images to RGB matrix
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


##############################################################
# DEMO
##############################################################
# some interesting sample images -- download them if they aren't in the current working directory
mandrillFile  = "data/images/mandrill.png"
rgbFile       = "data/images/RGB_illumination.jpg"
hatsFile      = "data/images/kodim03.png"
barnFile      = "data/images/kodim22.png"

dowloadFile(url = "https://i1.wp.com/rwalk.xyz/wp-content/uploads/2016/01/mandrill-1.png?resize=300%2C300&ssl=1", destfile=mandrillFile)
dowloadFile(url = "https://upload.wikimedia.org/wikipedia/commons/2/28/RGB_illumination.jpg", destfile=rgbFile)
dowloadFile(url = "http://r0k.us/graphics/kodak/kodak/kodim03.png", destfile=hatsFile)
dowloadFile(url = "http://r0k.us/graphics/kodak/kodak/kodim22.png", destfile=barnFile)

# we can work with both JPEGs and PNGS. For simplicty, we'll always write out to PNG though.
mandrill  <- readImage(mandrillFile)
rgb       <- readImage(rgbFile)
hats      <- readImage(hatsFile)
barn      <- readImage(barnFile)



##############################################################
# Segment those 4 images
##############################################################

# segment -- tune the number of segments for each image
mandrill.df = segment_image(mandrill, 7)
rgb.df      = segment_image(rgb, 12)
hats.df     = segment_image(hats, 8)
barn.df     = segment_image(barn, 10)

# create segmented image data structure and write to disk
mandrill.segmented = build_segmented_image(mandrill.df, mandrill)
rgb.segmented = build_segmented_image(rgb.df, rgb)
hats.segmented = build_segmented_image(hats.df, hats)
barn.segmented = build_segmented_image(barn.df, barn)

# write the segmented images to disk
writePNG(mandrill.segmented, "data/images/mandrill_segmented.png" )
writePNG(rgb.segmented, "data/images/rgb_illumination_segmented.png")
writePNG(hats.segmented, "data/images/kodim03_segmented.png")
writePNG(barn.segmented, "data/images/kodim22_segmented.png")

# Plots all three channels R/G/B together
par(mfrow = c(2,2))
plotArrayAsImage(mandrill.segmented, main = "Mandrill K = 7")
plotArrayAsImage(rgb.segmented, main = "Mandrill K = 12")
plotArrayAsImage(hats.segmented, main = "Mandrill K = 8")
plotArrayAsImage(barn.segmented, main = "Mandrill K = 10")
par(mfrow = c(1,1))



# 3d Rendering
df = mandrill.df
# df = rgb.df
# df = hats.df
# df = barn.df


# color space plot of mandrill
options(rgl.printRglwidget = TRUE)
# open3d()
plot3d(df$red, df$green, df$blue, 
       col=rgb(df$red, df$green, df$blue),
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE, axes=TRUE)
# play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )

# color space plot of segmented mandrill
# open3d()
plot3d(df$red, df$green, df$blue, 
       col=rgb(df$R, df$G, df$B),
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE)
# play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )

# Use 
# movie3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# instead of play3d to generate GIFs (requires imagemagick)
