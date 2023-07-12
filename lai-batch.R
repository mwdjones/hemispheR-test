library(tidyverse)
library(dplyr)
library(ggplot2)
library(hemispheR)

batchLAI <- function(link){
  
  #Data frame for all LAI data
  canopyLAI <- data.frame()
  
  #List all photos in the linked folder
  photos <- list.files(path = link)
  
  for(i in c(1:length(photos))) 
  {
    photo_link = paste(link, "/", photos[i], sep = "")
    
    #import
    img <- import_fisheye(photo_link,
                          channel = 3,
                          circ.mask=list(xc=3000,yc=2000,rc=1450),
                          circular=TRUE,
                          gamma=2.2,
                          stretch=FALSE,
                          display=FALSE,
                          message=FALSE)
    #Threshold Image
    img.bw <- binarize_fisheye(img,
                               method='Otsu',
                               zonal=FALSE,
                               manual=NULL,
                               display=FALSE,
                               export=FALSE)
    
    #Apply Rings
    gap.frac <- gapfrac_fisheye(img.bw,
                                maxVZA = 90,
                                lens = "Sigma-4.5",
                                startVZA = 0,
                                endVZA = 70,
                                nrings = 7,
                                nseg = 8,
                                display=FALSE,
                                message = FALSE)
                                
    #Calculate LAI
    canopy <- canopy_fisheye(gap.frac)
    
    #Convert to data frame
    canopydf <- data.frame(canopy)
    
    #Concatenate with past photos
    canopyLAI <- rbind(canopyLAI, canopydf)
  }
  
  #Return completed data frame
  return(canopyLAI)
  
}