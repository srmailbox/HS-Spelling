################################################################################
# High School Spelling Data
# Results from various reading and spelling tasks from ~400 high school 
# students.
# 
# Grade-based IRT's to help with item selection for the CCSecondary
#
# Created: 2026-11-05
# Author: Serje Robidoux
# CHANGELOG:
#


# 0. Setup ####
include(psych)
if (!all(sapply(c("DiST.i", "DiST.n"), exists))) 
  source('HSSpelling.DataImport.R')

# 1.0 DiST.i ####
## 1.1 Data split ####
DiST.i.grade = by(DiST.i, DiST.i$Grade, data.frame)
names(DiST.i.grade)=paste0("Gr",names(DiST.i.grade))

## 1.2 IRT ####
DiSTi.irt = lapply(
  DiST.i.grade
  , function(x) 
    irt.fa(
      x %>% select(6:40) %>% drop_na()
      , 1, plot=FALSE
      )
)

## 1.3 Plots ####
for (nm in names(DiSTi.irt)){
  png(paste0("Images/DiSTi.irt.", nm, ".png"), height=900, width=1200)
  par(mfrow=c(3,1))
  plot(DiSTi.irt[[nm]], type="ICC", main=paste("Item Curves -", nm))
  abline(h=.5, col="darkgrey")
  plot(DiSTi.irt[[nm]])
  abline(h=2, col="darkred")
  plot(DiSTi.irt[[nm]], type="test")
  abline(h=1/(1-.7), col="darkred")
  abline(h=1/(1-.8), col="darkgrey")
  abline(h=1/(1-.9), col="darkgreen")
  text(x=-3, y=1/(1-c(.7,.8,.9))+.5, labels=paste("Reliability ", c(.7,.8,.9))
       , col=c("darkred", "darkgrey", "darkgreen"))
  dev.off()
  par(mfrow=c(1,1))
}


# 2.0 DiST.n ####
## 2.1 Data split ####
DiST.n.grade = by(DiST.n, DiST.n$Grade, data.frame)
names(DiST.n.grade)=paste0("Gr",names(DiST.n.grade))

## 2.2 IRT ####
DiSTn.irt = lapply(
  DiST.n.grade
  , function(x) 
    irt.fa(
      x %>% select(6:40) %>% drop_na()
      , 1, plot=FALSE
    )
)

## 2.3 Plots ####
for (nm in names(DiSTn.irt)){
  png(paste0("Images/DiSTn.irt.", nm, ".png"), height=900, width=1200)
  par(mfrow=c(3,1))
  plot(DiSTn.irt[[nm]], type="ICC", main=paste("Item Curves -", nm))
  abline(h=.5, col="darkgrey")
  plot(DiSTn.irt[[nm]])
  abline(h=2, col="darkgreen")
  plot(DiSTn.irt[[nm]], type="test")
  abline(h=1/(1-.7), col="darkred")
  abline(h=1/(1-.8), col="darkgrey")
  abline(h=1/(1-.9), col="darkgreen")
  text(x=-3, y=1/(1-c(.7,.8,.9))+.5, labels=paste("Reliability ", c(.7,.8,.9))
       , col=c("darkred", "darkgrey", "darkgreen"))
  dev.off()
  par(mfrow=c(1,1))
}
