#K-means image analysis by Jordan Dowell 12-13-21



#libaraies used * install as necessary

# Load the package
library(jpeg)
library(ggplot2)
#import image 

#get list of file names in inputimages 

listofFiles<-list.files("Inputimages/")
i<-1
#begin for loop to run over all files in inputimages
for (i in 1:length(listofFiles)) {
  

Filetouse<-listofFiles[i]
#print which file currently being worked on 
print(paste0("Inputimages/",Filetouse))
#import image
Leaf <- readJPEG(paste0("Inputimages/",Filetouse))


# get the dimensions of the image 
Leaf.xy <- dim(Leaf)
# Convert pixels to a data frame 
#keep xy position for each value 
Leaf.RGB <- data.frame(
  x = rep(1:Leaf.xy[2], each = Leaf.xy[1]),
  y = rep(Leaf.xy[1]:1, Leaf.xy[2]),
  R = as.vector(Leaf[,,1]),
  G = as.vector(Leaf[,,2]),
  B = as.vector(Leaf[,,3])
)



# ggplot theme to be used
plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}

# Plot the image 
#larges images take a long time to plot 
Orig.Leaf<-ggplot(data = Leaf.RGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(Leaf.RGB[c("R", "G", "B")])) +
  labs(title = "Original Image") +
  xlab("x") +
  ylab("y") +
  plotTheme()



#apply K-means clustering 
#ROT use 2-3x the number of potenial labels in the image

kClusters <- 12

#convert RGB to HSV to decorrelate luminance
Plant.hsv <- t(rgb2hsv(t(Leaf.RGB[c("R", "G", "B")])))

#run kmeans on hsv data
kMeans <- kmeans(Plant.hsv[, c("h", "s", "v")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])


#replace the colors in the original leaf with the mean color of each cluster 
Clus.Leaf<-ggplot(data = Leaf.RGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()

#create a historgram of each category colored by 
#their individual colors mean colors 

#count frequency of each color and convert to dataframe 

Leaf.colors<- as.data.frame(kColours)
color.freq<-as.data.frame(table(Leaf.colors))

#create plot 
Color.hist<-ggplot(as.data.frame(color.freq), aes(factor(Leaf.colors), Freq, fill = Leaf.colors)) +     
  geom_col(position = 'dodge')+
  scale_fill_manual(values=unique(kColours))+
  #hash below to remove number of pixel labels
  geom_text(aes(label=Freq))+
  #hash below to keep column names as the color hexcode
  scale_x_discrete(labels= c(1:nrow(color.freq)))

#print image comparisons if necessary for records
#high resolution images take longer. 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#can change this to png to save memory and increase speed at the cost of low resolution
pdf(file=paste0("Outputimages/Ouput.",Filetouse,".pdf"))
multiplot(Orig.Leaf,Clus.Leaf,Color.hist, cols=1)
dev.off()

#export table
write.csv(color.freq,paste0("Outputimages/Ouput.",Filetouse,".csv"))
#end for loop
print(paste0(Filetouse," is fininshed hurray!"))
}
#when you select your pixel categories just sum them 
#and convert them to your desired measurements

