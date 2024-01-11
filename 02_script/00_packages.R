### Packages R Script ###
### Clear environment

# use (and install if necessary) pacman package 
#install.packages("pacman")
library(pacman)

# Nach Updates für Pakete schauen
p_update(update = FALSE)
p_update()

# load and install (if necessary) required packages for this course
pacman::p_load(
  ade4, 
  broom, 
  cNORM, 
  cluster, 
  colorspace, 
  corrplot, 
  cssTools, 
  devtools, 
  descr, 
  dplyr, 
  effects, 
  factoextra, 
  foreign,
  forcats,
  gganimate, 
  ggplot2, 
  ggh4x, 
  ggpubr, 
  ggseqplot, 
  glue, 
  graphics, 
  haven,
  here,
  Hmisc, 
  kableExtra, 
  knitr,
  lmtest,
  magrittr, 
  MCMCpack, 
  margins, 
  nnet, 
  patchwork, 
  plotrix, 
  psych, 
  reshape2, 
  rio, 
  RColorBrewer,
  sandwich,
  sjlabelled, 
  summarytools, 
  tidyr, 
  tidyverse, 
  TraMineR, 
  TraMineRextras, 
  tsibble, 
  vegan, 
  WeightedCluster,
  foreign,      # New package
  readstata13,  # New package
  car,          # New package
  prettyR,      # New package
  devEMF        # New package
)