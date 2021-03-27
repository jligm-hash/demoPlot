# T09-dataVisualize.r
# data visualization: circos, beewarm, violin
# by Jiabao LI
# DATE: 2021-03-29

#==========================================================
# install the package
install.packages("circlize") # package for circos
install.packages('BioCircos')
install.packages("beeswarm") # package for beeswarm
install.packages("ggplot2") 
install.packages("tidyverse")

# load the library
library(BioCircos)
library(circlize)
library(beeswarm)
library(ggplot2)
library(tidyverse)

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
# draw circos
help("BioCircos") # document
BioCircos()

BioCircos(genome = "hg19", yChr = FALSE, genomeFillColor = "Reds", chrPad = 0, 
          displayGenomeBorder = FALSE, genomeTicksDisplay = FALSE, genomeLabelDy = 0)

BioCircos(genome = "hg19", 
          yChr = T, # show the Y chr
          genomeFillColor = "Greens", #Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
          chrPad = 0.03, # Distance between chromosomes
          displayGenomeBorder = T, 
          genomeTicksDisplay = T, 
          genomeLabelDy = 0)

myGenome = list("A" = 10560,
                "B" = 8808,
                "C" = 12014,
                "D" = 7664,
                "E" = 9403,
                "F" = 8661)

BioCircos(genome = myGenome, genomeFillColor = c("tomato2", "darkblue"),
          genomeTicksScale = 4e+3)

tracklist = BioCircosTextTrack('myTextTrack', 'Some text', size = "2em", opacity = 0.5, 
                               x = -0.67, y = -0.5)

BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0, displayGenomeBorder = FALSE, 
          genomeTicksLen = 2, genomeTicksTextSize = 0, genomeTicksScale = 1e+8,
          genomeLabelTextSize = "9pt", genomeLabelDy = 0)


tracklist = BioCircosBackgroundTrack("myBackgroundTrack", minRadius = 0.5, maxRadius = 0.8,
                                     borderColors = "#AAAAAA", borderSize = 0.6, fillColors = "#FFBBBB")  

BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0.05, displayGenomeBorder = FALSE, 
          genomeTicksDisplay = FALSE,  genomeLabelTextSize = "9pt", genomeLabelDy = 0)

# SNP track
# Chromosomes on which the points should be displayed
points_chromosomes = c('X', '2', '7', '13', '9')
# Coordinates on which the points should be displayed
points_coordinates = c(102621, 140253678, 98567307, 28937403, 20484611) 
# Values associated with each point, used as radial coordinate 
#   on a scale going to minRadius for the lowest value to maxRadius for the highest value
points_values = 0:4

tracklist = BioCircosSNPTrack('mySNPTrack', points_chromosomes, points_coordinates, 
                              points_values, colors = c("tomato2", "darkblue"), minRadius = 0.5, maxRadius = 0.9)

# Background are always placed below other tracks
tracklist = tracklist + BioCircosBackgroundTrack("myBackgroundTrack", 
                                                 minRadius = 0.5, maxRadius = 0.9,
                                                 borderColors = "#AAAAAA", borderSize = 0.6, fillColors = "#B3E6FF")  

BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0.05, displayGenomeBorder = FALSE, yChr =  FALSE,
          genomeTicksDisplay = FALSE,  genomeLabelTextSize = 18, genomeLabelDy = 0)


links_chromosomes_1 = c('X', '2', '9') # Chromosomes on which the links should start
links_chromosomes_2 = c('3', '18', '9') # Chromosomes on which the links should end

# link track
links_pos_1 = c(155270560, 154978472, 42512974)
links_pos_2 = c(102621477, 140253678, 20484611)
links_labels = c("Link 1", "Link 2", "Link 3")

tracklist = BioCircosBackgroundTrack("myBackgroundTrack", minRadius = 0, maxRadius = 0.55,
                                     borderSize = 0, fillColors = "#EEFFEE")  

tracklist = tracklist + BioCircosLinkTrack('myLinkTrack', links_chromosomes_1, links_pos_1,
                                           links_pos_1 + 50000000, links_chromosomes_2, links_pos_2, links_pos_2 + 750000,
                                           maxRadius = 0.55, labels = links_labels)

BioCircos(tracklist, genomeFillColor = "PuOr",
          chrPad = 0.02, displayGenomeBorder = FALSE, yChr =  FALSE,
          genomeTicksDisplay = FALSE,  genomeLabelTextSize = "8pt", genomeLabelDy = 0)

# bar track
# Define a custom genome
genomeChr = LETTERS
lengthChr = 5*1:length(genomeChr)
names(lengthChr) <- genomeChr

tracks = BioCircosTracklist()
# Add one track for each chromosome
for (i in 1:length(genomeChr)){
  # Define histogram/bars to be displayed
  nbBars = lengthChr[i] - 1
  barValues = sapply(1:nbBars, function(x) 10 + nbBars%%x)
  barColor = colorRampPalette(brewer.pal(8, "YlOrBr"))(length(genomeChr))[i]
  # Add a track with bars on the i-th chromosome
  tracks = tracks + BioCircosBarTrack(paste0("bars", i), chromosome = genomeChr[i], 
                                      starts = (1:nbBars) - 1, ends = 1:nbBars, values = barValues, color = barColor, 
                                      range = c(5,75))
}

# Add background
tracks = tracks + BioCircosBackgroundTrack("bars_background", colors = "#2222EE")

BioCircos(tracks, genomeFillColor = "YlOrBr", genome = as.list(lengthChr), 
          genomeTicksDisplay = F, genomeLabelDy = 0)

# CNV tracks
# Arcs coordinates
snvChr = rep(4:9, 3)
snvStart = c(rep(1,6), rep(40000000,6), rep(100000000,6))
snvEnd = c(rep(39999999,6), rep(99999999,6), 
           191154276, 180915260, 171115067, 159138663, 146364022, 141213431)
# Values associated with each point, used as radial coordinate 
#   on a scale going to minRadius for the lowest value to maxRadius for the highest value
snvValues = (1:18%%5)+1
# Create CNV track
tracks = BioCircosCNVTrack('cnv_track', as.character(snvChr), snvStart, snvEnd, snvValues, 
                           color = "#CC0000", range = c(0,6))

# Add background
tracks = tracks + BioCircosBackgroundTrack("arcs_background", colors = "#2222EE")

BioCircos(tracks, genomeFillColor = "YlOrBr", genomeTicksDisplay = F, genomeLabelDy = 0)
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

