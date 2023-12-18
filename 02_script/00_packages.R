### Packages R Script ###
### Clear environment

# use (and install if necessary) pacman package 
library(pacman)

# Nach Updates für Pakete schauen
p_update(update = FALSE)
p_update()

# load and install (if necessary) required packages for this course
pacman::p_load(cNORM, haven, ggplot2, ggpubr, cluster, rio, plotrix, 
               haven, Hmisc, gganimate, RColorBrewer, colorspace, 
               knitr, kableExtra, reshape2, summarytools, vegan, MCMCpack, 
               corrplot, ade4, cssTools, WeightedCluster, factoextra,
               tidyverse, effects, margins, psych, devtools, foreign, 
               broom, nnet, descr, here, magrittr, TraMineR, TraMineRextras, 
               graphics, tidyverse, ggpubr, glue, ggseqplot, ggh4x, 
               seqhandbook, patchwork, foreign, dplyr)

