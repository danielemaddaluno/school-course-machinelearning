# Clean enviroment
rm(list=ls())

# Set the seed
set.seed(12345678)

# Load libraries
library(jpeg)
library(png)
library(countcolors)
library(rgl)
library(rmarkdown)

# Function to download image from an url
dowloadFile = function(url, destfile) {
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

# # Function to compute euclidean distance between two points x1, x2
# euclideanDist = function(x1, x2){
#   return (dist(rbind(x1, x2)))
# }

##############################################################
# DEMO
##############################################################
# some interesting sample images -- download them if they aren't in the current working directory
# imageFile    = "data/images/mandrill.png"
# dowloadFile(url = "https://i1.wp.com/rwalk.xyz/wp-content/uploads/2016/01/mandrill-1.png?resize=300%2C300&ssl=1", destfile=imageFile)
imageFile    = "data/images/tulliobuzzi.jpg"


# we can work with both JPEGs and PNGS.
image = readImage(imageFile)
class(image)
dim(image)



##############################################################
# Plot R G B separated
##############################################################

### EX 1: show the full RGB image
plotArrayAsImage(image)




### EX 2: show the 3 channels in separate images
# copy the image three times
imageR = image
imageG = image
imageB = image

# zero out the non-contributing channels for each image copy
imageR[,,c(2,3)] = 0
imageG[,,c(1,3)] = 0
imageB[,,c(1,2)] = 0

# Plots all three channels R/G/B together
par(mfrow = c(1,3))
plotArrayAsImage(imageR, main = "R Channel")
plotArrayAsImage(imageG, main = "G Channel")
plotArrayAsImage(imageB, main = "B Channel")
par(mfrow = c(1,1))




# EX 3: Call Kmeans
# create a flat, segmented image data set using kmeans
# Segment an RGB image into n groups based on color values using Kmeans
df = data.frame(
  red = matrix(image[,,1], ncol=1),
  green = matrix(image[,,2], ncol=1),
  blue = matrix(image[,,3], ncol=1)
)
# View(df)

# Set the number of clusters
K = 3
# Call the kmeans algorithm with K=3
kmeansRes = kmeans(df, K)
# Centroids
kmeansRes$centers
# Cluster assignment (1, 2 or 3 for each pixel considered)
kmeansRes$cluster




# EX 4: Compose a new image with the clusters obtained
clusteredImage = image

# Generate levels of grays to use in semented image:
grays = seq(0, 0.9, length.out = K) # 0.9 and not 1.0 because i don't want a complete white
# black, gray and white if cluster is 1, 2 or 3
grays

# Create new image with clusters
rows = dim(clusteredImage)[1]
cols = dim(clusteredImage)[2]
clustersInMatrix = matrix(kmeansRes$cluster, ncol=cols) # bring back clusters from vector to matrix 300x300

for (i in 1:rows) {
  for (j in 1:cols) {
    clust_ij = clustersInMatrix[i,j]
    col_ij = grays[clust_ij]
    # set the same color at the three
    clusteredImage[i,j,1] = col_ij
    clusteredImage[i,j,2] = col_ij
    clusteredImage[i,j,3] = col_ij
  }
}

# par(mfrow = c(1,2))
# New plots
plotArrayAsImage(clusteredImage)
# Compare with:
plotArrayAsImage(image)
# par(mfrow = c(1,1))

# write the segmented images to disk
writePNG(clusteredImage, "data/images/image_gray_segmented.png" )




# EX 5: 3d Rendering
x = image[,,1] # R original pixel
y = image[,,2] # G original pixel
z = image[,,3] # B original pixel
colors = matrix(clusteredImage[,,1], ncol=rows*cols) # new color for the pixel


# Init 3D Visualization Using OpenGL
if(Sys.info()["sysname"] == "Windows"){
  # Use this on Windows
  open3d()
} else {
  # Use on Mac (normally == "Darwin")
  options(rgl.printRglwidget = TRUE)
}


# # just gray points
# plot3d(x, y, z, 
#        col=rgb(0.5, 0.5, 0.5),
#        xlab="Red", ylab="Green", zlab="Blue",
#        size=3, box=FALSE, axes=TRUE)

# color space plot of image
plot3d(x, y, z, 
       col=rgb(x, y, z),
       xlab="Red", ylab="Green", zlab="Blue",
       size=3, box=FALSE, axes=TRUE)

# color space plot of segmented image
plot3d(x, y, z, 
       col=rgb(colors, colors, colors),
       xlab="Red", ylab="Green", zlab="Blue",
       size=3, box=FALSE, axes=TRUE)

# add centers
points3d(kmeansRes$centers, col='red', size=10)

# This works only on windows (or macs pre BigSur)
# play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )