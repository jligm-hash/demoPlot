# T09-dataVisualize.r
# data visualization: circos, beewarm, violin
# by Jiabao LI
# DATE: 2021-03-29

#==========================================================
# # install the package
# install.packages("circlize") # package for circos
# install.packages('BioCircos')
# install.packages("beeswarm") # package for beeswarm
# install.packages('ggbeeswarm')
# install.packages("ggplot2") 
# install.packages("tidyverse")
# install.packages("RColorBrewer")
# install.packages("grDevices")

# load the library
library(BioCircos)
library(circlize)
library(beeswarm)
library(ggbeeswarm)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(grDevices)

#==========================================================
# basic dataframe in R

# read the document of data frame
help("data.frame")

# create a data frame
x = data.frame("No" = 1:2, "Age" = c(21,15), "Name" = c("John","Dora"))
# structure of x
str(x)

# Check if a variable is a data frame or not
x
typeof(x)    # data frame is a special case of  list
class(x)

# Functions of data frame
names(x)
ncol(x)
nrow(x)
length(x) # returns length of the list, same as ncol()

# How to access Components of a Data Frame
x["Name"] # use square brackets []
x$Name # use dollar sign $
x[["Name"]]
x[[3]]
# use the trees dataset
help(trees) # to read the document of trees dataset
str(trees)
head(trees,n=3)
trees[2:3,]    # select 2nd and 3rd row
trees[trees$Height > 82,]    # selects rows with Height greater than 82
trees[10:12,2]
trees[10:12,2, drop = FALSE] # to avoid that the returned type is a vector since the extracted data is from a single column

# Summary of Data in Data Frame
print(summary(x))
print(summary(trees)) 

# modify the data
x # before
x[1,"Age"] = 20
x # after
rownames(x) = c("myRowName", "youCanUseTheRowNameAsYouLike")
x
colnames(x) = c("myNo", "myAge", "myName")
x

# Adding Components
rbind(x,list(1,16,"Paul"))
cbind(x,State=c("NY","FL"))
x
x = cbind(x,State=c("NY","FL")) # reassignment to x
x # after

# Deleting Component
x$State <- NULL # delete the 'State col
x
x = x[-1,] # delete the first row
x

#==========================================================
# draw violin plot
ToothGrowth$dose = as.factor(ToothGrowth$dose)
head(ToothGrowth)
# Basic violin plot
p = ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE) # axis from 0
# Choose which items to display 
p + scale_x_discrete(limits=c("0.5", "2")) # Warning message: Removed 20 rows containing non-finite values (stat_ydensity). 
# violin plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=23, size=2) # Warning message: `fun.y` is deprecated. Use `fun` instead.
p + stat_summary(fun=mean, geom="point", shape=23, size=2)
# violin plot with median points
p + stat_summary(fun.y=median, geom="point", size=2, color="red") # Warning message: `fun.y` is deprecated. Use `fun` instead. 
p + stat_summary(fun=median, geom="point", size=2, color="red")
# Add median and quartile
p + geom_boxplot(width=0.1)
# Add mean and standard deviation
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
geom_violin(trim=FALSE)
p + stat_summary(fun.data="mean_sdl", 
                 geom="crossbar", width=0.1 )
p + stat_summary(fun.data=mean_sdl, 
                 geom="pointrange", color="red")

# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1, binwidth=0.9)# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

# Change violin plot line colors by groups
p = ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
  geom_violin(trim=FALSE)
p
# Use custom color palettes
p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic()
# change fill colors
# Use single color
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()
# Change violin plot colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_violin(trim=FALSE)
p
# Violin plot with multiple groups
# Change violin plot colors by groups
ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_violin()
# Change the position
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_violin(position=position_dodge(1))
p
#
# Basic violin plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(title="Plot of length  by dose",x="Dose (mg)", y = "Length")+
  geom_boxplot(width=0.1)+
  theme_classic()
# Change color by groups
ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of length  by dose",x="Dose (mg)", y = "Length")+
  guides(fill=F)+
  theme_classic()

#==========================================================
# draw beeswarm
help("beeswarm")
head(OrchardSprays)
beeswarm(decrease ~ treatment, data = OrchardSprays, 
         log = TRUE, pch = 16, col = rainbow(8),
         main = 'beeswarm')
data(breast)
beeswarm(time_survival ~ ER, data = breast,
         pch = 16, pwcol = 1 + as.numeric(event_survival),
         xlab = "", ylab = "Follow-up time (months)",
         labels = c("ER neg", "ER pos"))
legend("topright", legend = c("Yes", "No"),
       title = "Censored", pch = 16, col = 1:2)

# Compare the four methods for arranging points
set.seed(123) # Generate some random data
distro = list(runif = runif(100, min = -3, max = 3), 
               rnorm = rnorm(100))

m = "swarm"# c("swarm", "center", "hex", "square"))
beeswarm(distro, 
           col = 2:3, pch = 16,
           method = m, 
           main = paste('method = "', m, '"', sep = ''),
         xlab = '', ylab = '')
# use ggplot
ggplot(iris,aes(Species, Sepal.Length)) + geom_jitter() # jitter plot
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom()
# default geom_quasirandom
ggplot(mpg,aes(class, hwy)) + geom_quasirandom()
# use geom_beeswarm
help("geom_beeswarm")
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_beeswarm() + 
  ggtitle("Beeswarm")

ggplot(breast, aes(ER, time_survival, col=ER)) + 
  geom_beeswarm(size=2,priority='descending', groupOnX = T) + 
  ggtitle('Beeswarm') 

#==========================================================
# draw circos
help("BioCircos") # document
BioCircos()

#----------------------------------------------------------
# Genome configuration
BioCircos(genome = "hg19", yChr = FALSE, genomeFillColor = "Reds", chrPad = 0, 
          displayGenomeBorder = FALSE, genomeTicksDisplay = FALSE, genomeLabelDy = 0)
# change the basic settings
BioCircos(genome = "hg19", 
          yChr = T, # show the Y chr
          genomeFillColor = "Greens", #Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
          chrPad = 0.03, # Distance between chromosomes
          displayGenomeBorder = T, 
          genomeTicksDisplay = T, 
          genomeLabelDy = 0)
# use your own 'genome'
myGenome = list("A" = 10560, # name, length
                "B" = 8808,"C" = 12014,"D" = 7664,"E" = 9403,"F" = 8661)
BioCircos(genome = myGenome, genomeFillColor = c("tomato2", "darkblue"),
          genomeTicksScale = 4e+3)

#----------------------------------------------------------
# Text track
tracklist = BioCircosTextTrack('myTextTrack', # track name
                               'Some text', # The text to be displayed
                               size = "2em", # Font size
                               opacity = 0.5, #Font opacity
                               x = -0.67, y = -0.5) # Coordinates of the lower left corner of the annotation
BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0, displayGenomeBorder = FALSE, 
          genomeTicksLen = 2, genomeTicksTextSize = 0, genomeTicksScale = 1e+8,
          genomeLabelTextSize = "9pt", genomeLabelDy = 0)

#----------------------------------------------------------
# Background track
tracklist = BioCircosBackgroundTrack("myBackgroundTrack", 
                                     minRadius = 0.5, maxRadius = 0.8,
                                     # Where the track should begin and end, in proportion of the inner radius of the plot
                                     borderColors = "#AAAAAA", 
                                     borderSize = 0.6, 
                                     fillColors = "#FFBBBB")  
BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0.05, displayGenomeBorder = FALSE, 
          genomeTicksDisplay = FALSE,  genomeLabelTextSize = "9pt", genomeLabelDy = 0)

#----------------------------------------------------------
# SNP track
# Chromosomes on which the points should be displayed
points_chromosomes = c('X', '2', '7', '13', '9')
# Coordinates on which the points should be displayed
points_coordinates = c(102621, 140253678, 98567307, 28937403, 20484611) 
# Values associated with each point, used as radial coordinate 
#  on a scale going to minRadius for the lowest value to maxRadius for the highest value
points_values = 0:4
tracklist = BioCircosSNPTrack('mySNPTrack', 
                              points_chromosomes, # A vector containing the chromosomes on which each SNP are found
                              points_coordinates, # A vector containing the coordinates on which each SNP are found
                              points_values, # A vector of numerical values associated with each SNPs, used to determine the radial coordinates of each point on the visualization
                              colors = c("tomato2", "darkblue"), 
                              minRadius = 0.5, maxRadius = 0.9)
# Background are always placed below other tracks
tracklist = tracklist + BioCircosBackgroundTrack("myBackgroundTrack", 
                                                 minRadius = 0.5, maxRadius = 0.9,
                                                 borderColors = "#AAAAAA", borderSize = 0.6, fillColors = "#B3E6FF")  
BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0.05, displayGenomeBorder = FALSE, yChr =  FALSE,
          genomeTicksDisplay = FALSE,  genomeLabelTextSize = 18, genomeLabelDy = 0)

#----------------------------------------------------------
# Arc track
arcs_chromosomes = c('X', 'X', '2', '9') # Chromosomes on which the arcs should be displayed
arcs_begin = c(1, 45270560, 140253678, 20484611)
arcs_end = c(155270560, 145270560, 154978472, 42512974)
tracklist = BioCircosArcTrack('myArcTrack', 
                              arcs_chromosomes, # A vector containing the chromosomes on which each arc is found
                              arcs_begin, arcs_end, # Vectors containing the coordinates on which each arc begins or ends
                              minRadius = 1.18, maxRadius = 1.25, 
                              opacities = c(0.4, 0.4, 1, 0.8))
BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0.02, displayGenomeBorder = FALSE, yChr =  FALSE,
          genomeTicksDisplay = FALSE,  genomeLabelTextSize = 0)

#----------------------------------------------------------
# Link track
links_chromosomes_1 = c('X', '2', '9') # Chromosomes on which the links should start
links_chromosomes_2 = c('3', '18', '9') # Chromosomes on which the links should end
links_pos_1 = c(155270560, 154978472, 42512974)
links_pos_2 = c(102621477, 140253678, 20484611)
links_labels = c("Link 1", "Link 2", "Link 3")
tracklist = BioCircosBackgroundTrack("myBackgroundTrack", minRadius = 0, maxRadius = 0.55,
                                     borderSize = 0, fillColors = "#EEFFEE")  
tracklist = tracklist + BioCircosLinkTrack('myLinkTrack', 
                                           gene1Chromosomes=links_chromosomes_1, 
                                           gene1Starts=links_pos_1,
                                           gene1Ends=links_pos_1 + 50000000, # Vectors with the chromosomes, genomic coordinates of beginning and end
                                           gene2Chromosomes=links_chromosomes_2, 
                                           gene2Starts=links_pos_2, 
                                           gene2Ends=links_pos_2 + 750000,
                                           maxRadius = 0.55, 
                                           labels = links_labels) #A vector of character objects to label each link
BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0.02, displayGenomeBorder = FALSE, yChr =  FALSE,
          genomeTicksDisplay = FALSE,  genomeLabelTextSize = "8pt", genomeLabelDy = 0)

#----------------------------------------------------------
# bar track
# Define a custom genome
genomeChr = LETTERS
lengthChr = 5*1:length(genomeChr)
names(lengthChr) = genomeChr
tracks = BioCircosTracklist()
# Add one track for each chromosome
for (i in 1:length(genomeChr)){
  # Define histogram/bars to be displayed
  nbBars = lengthChr[i] - 1
  barValues = sapply(1:nbBars, function(x) 10 + nbBars%%x) # use your own bar value
  barColor = colorRampPalette(brewer.pal(8, "YlOrBr"))(length(genomeChr))[i]
  # Add a track with bars on the i-th chromosome
  tracks = tracks + BioCircosBarTrack(paste0("bars", i), 
                                      chromosome = genomeChr[i], 
                                      starts = (1:nbBars) - 1, ends = 1:nbBars, 
                                      values = barValues, #A vector of numerical values associated with each bin, used to determine the height of each bar on the track
                                      color = barColor, 
                                      range = c(5,75))
}
# Add background
tracks = tracks + BioCircosBackgroundTrack("bars_background", colors = "#2222EE")
BioCircos(tracks, genomeFillColor = "YlOrBr", genome = as.list(lengthChr), 
          genomeTicksDisplay = F, genomeLabelDy = 0)

#----------------------------------------------------------
# CNV tracks
# Arcs coordinates
snvChr = rep(4:9, 3)
snvStart = c(rep(1,6), rep(40000000,6), rep(100000000,6))
snvEnd = c(rep(39999999,6), rep(99999999,6), 191154276, 180915260, 171115067, 159138663, 146364022, 141213431)
# Values associated with each point, used as radial coordinate 
#   on a scale going to minRadius for the lowest value to maxRadius for the highest value
snvValues = (1:18%%5)+1 # use the snv values by your self
# Create CNV track
tracks = BioCircosCNVTrack('cnv_track', as.character(snvChr), 
                           snvStart, snvEnd, snvValues, 
                           color = "#CC0000", range = c(0,6))
# Add background
tracks = tracks + BioCircosBackgroundTrack("arcs_background", colors = "#2222EE")
BioCircos(tracks, genomeFillColor = "YlOrBr", genomeTicksDisplay = F, genomeLabelDy = 0)

#----------------------------------------------------------
# Heatmap tracks
# Define a custom genome
genomeChr = LETTERS[1:10]
lengthChr = 5*1:length(genomeChr)
names(lengthChr) = genomeChr
# Define boxes positions
boxPositions = unlist(sapply(lengthChr, seq))
boxChromosomes = rep(genomeChr, lengthChr)
# Define values for two heatmap tracks
boxVal1 = boxPositions %% 13 / 13
boxVal2 = (7 + boxPositions) %% 17 / 17
tracks = BioCircosHeatmapTrack("heatmap1", boxChromosomes, 
                               boxPositions - 1, boxPositions,
                               boxVal1, # A vector of numerical values associated with each box, used to determine the height of each bar on the track.
                               minRadius = 0.6, maxRadius = 0.65)
tracks = tracks + BioCircosHeatmapTrack("heatmap1", boxChromosomes, boxPositions - 1, 
                                        boxPositions, boxVal2, minRadius = 0.75, maxRadius = 0.9, 
                                        color = c('#ffffbf','#fc8d59'))
BioCircos(tracks, genome = as.list(lengthChr), genomeTicksDisplay = F, genomeLabelDy = 0, 
          HEATMAPMouseOverColor = "black") #'#F3C73A'


#----------------------------------------------------------
# Line tracks
chrVert =  rep(c(1, 3, 5), c(20,10,5))
posVert = c(249250621*log(c(20:1, 10:1, 5:1), base = 20))
tracks = BioCircosLineTrack('LineTrack', 
                            as.character(chrVert), # chr
                            posVert, # position
                            values = cos(posVert))# value
tracks = tracks + BioCircosLineTrack('LineTrack2', # what if the position is out of the range? - will move to the next
                                     as.character(chrVert+1), 0.95*posVert,
                                     values = sin(posVert), color = "#40D4B9")
tracks = tracks + BioCircosBackgroundTrack('Bg', fillColors = '#FFEEBB', borderSize = 0)
BioCircos(tracks, chrPad = 0.05, displayGenomeBorder = FALSE, LINEMouseOutDisplay = FALSE, 
          LINEMouseOverTooltipsHtml01 = "Pretty lines<br/>This tooltip won't go away!")

#----------------------------------------------------------
# Removing track
# Create a tracklist with a text annotation and backgrounds
tracklist = BioCircosTextTrack('t1', 'hi')
tracklist = tracklist + BioCircosBackgroundTrack('b1')
BioCircos(tracklist)
# Remove the text annotation and display the result
BioCircos(tracklist - 't1')

#----------------------------------------------------------
# Multi-track example
# Fix random generation for reproducibility
set.seed(3)
# SNP tracks
tracks = BioCircosSNPTrack("testSNP1", as.character(rep(1:10,10)), 
                           round(runif(100, 1, 135534747)), 
                           runif(100, 0, 10), colors = "Spectral", minRadius = 0.3, maxRadius = 0.45)
tracks = tracks + BioCircosSNPTrack("testSNP2", as.character(rep(1:15,5)), 
                                    round(runif(75, 1, 102531392)), 
                                    runif(75, 2, 12), colors = c("#FF0000", "#DD1111", "#BB2222", "#993333"), 
                                    maxRadius = 0.8, range = c(2,12))
# Overlap point of interest on previous track, fix range to use a similar scale
tracks = tracks + BioCircosSNPTrack("testSNP3", "7", 1, 9, maxRadius = 0.8, size = 6,
                                    range = c(2,12))
# Background and text tracks
tracks = tracks + BioCircosBackgroundTrack("testBGtrack1", minRadius = 0.3, maxRadius = 0.45,
                                           borderColors = "#FFFFFF", borderSize = 0.6)    
tracks = tracks + BioCircosBackgroundTrack("testBGtrack2", borderColors = "#FFFFFF", 
                                           fillColor = "#FFEEEE", borderSize = 0.6, maxRadius = 0.8)
tracks = tracks + BioCircosTextTrack("testText", 'BioCircos!', weight = "lighter", 
                                     x = - 0.17, y = - 0.87, size = "0.8em")
# Arc track
arcsEnds = round(runif(7, 50000001, 133851895))
arcsLengths = round(runif(7, 1, 50000000))
tracks = tracks + BioCircosArcTrack("fredTestArc", as.character(sample(1:12, 7, replace=T)), 
                                    starts = arcsEnds - arcsLengths, ends = arcsEnds, labels = 1:7, 
                                    maxRadius = 0.97, minRadius = 0.83)
# Link tracks
linkPos1 = round(runif(5, 1, 50000000))
linkPos2 = round(runif(5, 1, 50000000))
chr1 = sample(1:22, 5, replace = T)
chr2 = sample(1:22, 5, replace = T)
linkPos3 = round(runif(5, 1, 50000000))
linkPos4 = round(runif(5, 1, 50000000))
chr3 = sample(1:22, 5, replace = T)
chr4 = sample(1:22, 5, replace = T)
tracks = tracks + BioCircosLinkTrack("testLink", gene1Chromosomes = chr1, 
                                     gene1Starts = linkPos1, gene1Ends = linkPos1+1, gene2Chromosomes = chr2, axisPadding = 6,
                                     color = "#EEEE55", width = "0.3em", labels = paste(chr1, chr2, sep = "*"), displayLabel = F,
                                     gene2Starts = linkPos2, gene2Ends = linkPos2+1, maxRadius = 0.42)
tracks = tracks + BioCircosLinkTrack("testLink2", gene1Chromosomes = chr3, 
                                     gene1Starts = linkPos3, gene1Ends = linkPos3+5000000, axisPadding = 6, displayLabel = F,
                                     color = "#FF6666", labels = paste(chr3, chr4, sep = "-"), gene2Chromosomes = chr4,
                                     gene2Starts = linkPos4, gene2Ends = linkPos4+2500000, maxRadius = 0.42)
# Display the BioCircos visualization
BioCircos(tracks, genomeFillColor = "Spectral", yChr = T, chrPad = 0, displayGenomeBorder = F, 
          genomeTicksLen = 3, genomeTicksTextSize = 0, genomeTicksScale = 50000000,
          genomeLabelTextSize = 18, genomeLabelDy = 0)





# # use the circlize package
# # load the data
# set.seed(999) # set the seed 
# n = 1000
# df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#                 x = rnorm(n), y = runif(n))
# circos.par("track.height" = 0.1)
# circos.initialize(df$sectors, x = df$x)
# circos.track(df$sectors, y = df$y,
#              panel.fun = function(x, y) {
#                circos.text(CELL_META$xcenter, 
#                            CELL_META$cell.ylim[2] + mm_y(5), 
#                            CELL_META$sector.index)
#                circos.axis(labels.cex = 0.6)
#              })
# col = rep(c("#FF0000", "#00FF00"), 4)
# circos.trackPoints(df$sectors, df$x, df$y, col = col, pch = 16, cex = 0.5)
# circos.text(-1, 0.5, "text", sector.index = "a", track.index = 1)
# bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
# circos.trackHist(df$sectors, df$x, bin.size = 0.2, bg.col = bgcol, col = NA)
# circos.track(df$sectors, x = df$x, y = df$y,
#              panel.fun = function(x, y) {
#                ind = sample(length(x), 10)
#                x2 = x[ind]
#                y2 = y[ind]
#                od = order(x2)
#                circos.lines(x2[od], y2[od])
#              })
# circos.update(sector.index = "d", track.index = 2, 
#               bg.col = "#FF8080", bg.border = "black")
# circos.points(x = -2:2, y = rep(0.5, 5), col = "white")
# circos.text(CELL_META$xcenter, CELL_META$ycenter, "updated", col = "white")
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#   xlim = CELL_META$xlim
#   ylim = CELL_META$ylim
#   breaks = seq(xlim[1], xlim[2], by = 0.1)
#   n_breaks = length(breaks)
#   circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks - 1),
#               breaks[-1], rep(ylim[2], n_breaks - 1),
#               col = rand_color(n_breaks), border = NA)
# })

