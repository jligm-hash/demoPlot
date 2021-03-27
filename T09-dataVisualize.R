# T09-dataVisualize.r
# data visualization: circos, beewarm, violin
# by Jiabao LI
# DATE: 2021-03-29

#==========================================================
# install the package
install.packages("circlize") # package for circos
install.packages("beeswarm") # package for beeswarm
install.packages("ggplot2") 
install.packages("tidyverse")

# load the library
library(circlize)
library(beeswarm)
library(ggplot2)
library(tidyverse)

#==========================================================
# draw circos
# load the data
set.seed(999) # set the seed 
n = 1000
df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
                x = rnorm(n), y = runif(n))
circos.par("track.height" = 0.1)
circos.initialize(df$sectors, x = df$x)
circos.track(df$sectors, y = df$y,
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(df$sectors, df$x, df$y, col = col, pch = 16, cex = 0.5)
circos.text(-1, 0.5, "text", sector.index = "a", track.index = 1)
