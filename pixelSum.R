# pixelSum.R
# Author: Bo Liu, Jiayi Chen, Steven Shi
# Load acturator img files, standardize size, and calculate pixel sum 

# load necessary library
library(tidyverse)
library(imager)
library(magick)
library(ggplot2)

# load home dataset with the necessary 
home3 <- read_csv("data/Cleaned_data_04_15.csv") %>%  
    select(Pixel.Sum, Feedrate, Psi, Piezo.Batch, Apperture.Batch, Tube.Batch, 
            Testing.Fluid, FR_Y, Column, Row, Batch.File) %>% 
    na.omit()

# example image processing pipeline of a single batch
setwd("~/dev/OVR")
# step 1. read in the image
fpath <- "data/b38-5.png"
act <- load.image(fpath)
plot(act)


# step 2. Gray scale
gray.im <- grayscale(act)
dim(gray.im)

# step 3. crop image
plot(gray.im) #original
cp.im <- imsub(gray.im, 1300>x) %>% 
                imsub(x>600)    %>% 
                imsub(y<880)    %>% 
                imsub(y>280)
plot(cp.im) #corpped
dim(cp.im)

# step 4. resize
re.im <- cp.im %>% resize(round(width(cp.im)/30),round(height(cp.im)/30))
plot(re.im)
dim(re.im)

# step 5. as data frame
df <- as.data.frame(re.im)
View(df)

#step 6. as heat-map
ggplot(df, aes(x, y)) +
    geom_tile(aes(fill=value)) +
    geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient(low = "black", high = "white")

#set 7. get pixel sum
pixel_sum <- sum(df)
pixel_sum

# define pixel sum function
get_pixel_sum <- function(im) {
    gray.im <- grayscale(im)
    cp.im <- imsub(gray.im, 1300>x) %>% 
        imsub(x>600)    %>% 
        imsub(y<880)    %>% 
        imsub(y>280)
    re.im <- cp.im %>% 
        resize(round(width(cp.im)/30),round(height(cp.im)/30))
    df <- as.data.frame(re.im)
    pixel_sum <- sum(df$value)
    return(pixel_sum)
}

# iterate through all pictures
alphabet <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
new_pixel <- NULL

for (i in 1:length(alphabet)) {
    impath <- paste("~/dev/OVR/data/Batch_38/Row_",
                  alphabet[i],
                  sep = "")
    setwd(impath)
    files <- list.files(path = impath,
                        pattern = "*.png", full.names = FALSE, recursive = FALSE)
    images <- lapply(files, load.image) 
    
    #if (length(images) == 0) next
    
    for (j in 1:length(images)) {
        #gray.im <- grayscale(images[[j]])
        
        curr_sum <- get_pixel_sum(images[[j]])
        
        #df$value[df$value < 0.3] <- 0
        #get pixel sum
        new_pixel <- c(new_pixel, curr_sum)
        print(j)
    }
    
    # show progress
    print(alphabet[i])
}

new_pixel <- data.frame(new_pixel)
new_pixel
home3 <- home3 %>% filter(Batch.File == "Batch 38 Data Sheet")
home3$number <- row.names(home3)
new_pixel$number <- row.names(new_pixel)
data3 <- merge(home3, new_pixel, by = "number", all = TRUE)
data3 <- data3[,c(1,2,13,4,5,6,7,8,9,10,11,12,3)]

# find correlations
cor(data3$Pixel.Sum,data3$new_pixel)
#plot(data3$Pixel.Sum,data3$new_pixel)
cor(data3$FR_Y,data3$new_pixel)
cor(data3$FR_Y,data3$Pixel.Sum)
plot(data3$FR_Y,data3$new_pixel)


data3 %>% ggplot(aes(new_pixel, FR_Y)) +
    geom_point() +
    geom_smooth(method='lm', se=FALSE, color='turquoise4') +
    theme_minimal() +
    labs(x='Pixel Sum', y='Y-Real', title='Pixel Sum vs. Y-Real') +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))

data4 <- data3 %>% mutate(Feedrate = as.factor(Feedrate),
                          Psi = as.factor(Psi),
                          Apperture.Batch = as.factor(Apperture.Batch),
                          Testing.Fluid = as.factor(Testing.Fluid))

cor(data4$new_pixel, data4$Psi)

data4 <- data4 %>% select(new_pixel, Psi)
data4

data4 %>% ggplot(aes(new_pixel, Psi)) +
    geom_point() +
    geom_smooth(method='lm', se=FALSE, color='turquoise4') +
    theme_minimal() +
    labs(x='Pixel Sum', y='Y-Real', title='Pixel Sum vs. Y-Real') +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))




