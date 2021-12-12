
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(rsconnect)

#import data
skills999 <- read.csv("skills999.csv", header=T)
topskill <- read.csv("SkillsData9.csv", header=T)

# Compute distances
distances <- dist(topskill[2:36], method = "euclidean")

# Hierarchical clustering
clustertopskill <- hclust(distances, method = "ward.D")

#ClusterGroups
clusterGroups = cutree(clustertopskill, k = 25)

clusterGroups

cluster1 = subset(topskill, clusterGroups == 1)
# Look at the first 10 titles in this cluster:
cluster1$jobID[1:10]

cluster2 = subset(topskill, clusterGroups == 2)
# Look at the first 10 titles in this cluster:
cluster2$jobID[1:10]

cluster3 = subset(topskill, clusterGroups == 3)
# Look at the first 10 titles in this cluster:
cluster3$jobID[1:10]

