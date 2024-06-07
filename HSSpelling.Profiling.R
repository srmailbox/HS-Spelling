################################################################################
# High School Spelling Data
# Results from various reading and spelling tasks from ~400 high school 
# students.
# 
# Analyses - Profiles of adolescent spelling.
#
# Created: 2023/11/08
# Author: Serje Robidoux
# CHANGELOG:
#


# 0. Setup ####
if (!all(sapply(c("DiST.i", "DiST.n", "MoST.spell", "HSSpelling"), exists))) 
  source('HSSpelling.DataImport.R')


# 1.0 Clustering, Spelling + CC2 ####

## 1.1 Set up dataset for clustering ####

# Ditch Grade and use age, since they are nearly identically distributed
HSSpell.clst = HSSpelling %>% 
  select(ID, AgeM, starts_with("DiST"), starts_with("MoST"), starts_with("CC2")
         #, starts_with("TOWRE"), starts_with("WIAT"), LetPos # Very few obs
  ) %>% drop_na()

## 1.1.1 Adjust for Age ###

HSSpell.age = HSSpell.clst %>% 
  mutate(across(DiST.Irr:CC2.Non
                , function(x) 
                  rstandard(lm(x~AgeM, HSSpell.clst))
  )
  )

## 1.2 Calculate distances (use euclidean) ####
HSSpell.dist = HSSpell.age %>% 
  select(-ID, -AgeM) %>% 
  dist

## 1.3 try all clustering methods ####
clst.methods = as.list(c("complete", "ward.D", "ward.D2", "single", "average"
                         , "mcquitty", "median", "centroid"))
names(clst.methods) = c("complete", "ward.D", "ward.D2", "single", "average"
                        , "mcquitty", "median", "centroid")

HSSpell.hclst = lapply(clst.methods
                       , function(x) hclust(HSSpell.dist, method=x[1]))

### 1.3.1 Plot the results ####
# lapply(HSSpell.hclst,
#        function (x) {
#          png(paste0("Images/HClust.", x$method,".png"), height=640, width=800)
#          plot(x, labels=F, main=x$method)
#          dev.off()
#        })
# dev.off() # lapply always seems to leave one open

## 1.4 Look at some options ####

table(cutree(HSSpell.hclst$ward.D2, 6))
## Looks like complete, ward D & D2 are all producing reasonable clusters. 
# Complete produces 5 (one small)
# Ward D  produces 9
# Ward D2 produces 6/7


HSSpell.age$hclust.cmplt = cutree(HSSpell.hclst$complete, 5)
HSSpell.age$hclust.wardD = cutree(HSSpell.hclst$ward.D, 9)
HSSpell.age$hclust.wardD2 = cutree(HSSpell.hclst$ward.D2, 6)

## 1.5 Plot results ####

# colnames(HSSpell.clst)

## 1.5.1 Complete linkage: 5 grps ####
cmplt.ggp = HSSpell.age %>% 
  select(AgeM:CC2.Non, hclust.cmplt) %>% 
  rename(Group = hclust.cmplt) %>% 
  mutate(AgeM = scale(AgeM)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:CC2.Non
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:12])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.cmplt, HSSpell.age, length)
             , mapping=aes(x=hclust.cmplt, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Complete Linkage - Spelling + CC2", "Age-adjusted scores")+
  scale_fill_manual(values=c("darkgrey", "#ff0000", "#aa0000"#, "#770000"
                              , "#006600", "#00ff00", "#008800", "#00aa00", "#00cc00"
                              , "#000088", "#0000ff", "#0000aa"))

# This looks to me like group 
# 1 is generally average readers and spellers, 
# 2 generally poor readers/spellers
# 5 poor spellers, very poor readers
# 4: average (but low performance on _e MoST items)
# 3: a small group of very very poor readers/spellers


## 1.5.2 Ward.D: 9 grps ####
wardD.ggp = HSSpell.age %>% 
  select(AgeM:CC2.Non, hclust.wardD) %>% 
  rename(Group = hclust.wardD) %>% 
  mutate(across(AgeM, scale)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:CC2.Non
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:12])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.wardD, HSSpell.age, length)
             , mapping=aes(x=hclust.wardD, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Ward D - Spelling + CC2", "Age-adjusted scores")+
  scale_fill_manual(values=c("darkgrey", "#ff0000", "#aa0000"#, "#770000"
                             , "#006600", "#00ff00", "#008800", "#00aa00", "#00cc00"
                             , "#000088", "#0000ff", "#0000aa"))

# This looks to me like group 
# 1 is generally good readers and spellers, 
# 2 average, except DiST.Irr, 2-consonant, and y2i items
# 3 slightly above average
# 4 gemerally good except for f2v (so similar to 1)
# 5: average speller, somewhat above average readers
# 6: slightly below average, but especially at spelling
# 7: poor readers, ok spellers... unusual
# 8: slightly below average across the board
# 9: Poor across the board

## 1.5.3 Ward.D2: 9 grps ####
wardD2.ggp = HSSpell.age %>% 
  select(AgeM:CC2.Non, hclust.wardD2) %>% 
  rename(Group = hclust.wardD2) %>% 
  mutate(across(AgeM, scale)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:CC2.Non
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:12])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.wardD2, HSSpell.age, length)
             , mapping=aes(x=hclust.wardD2, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Ward D2 - Spelling + CC2", "Age-adjusted scores")+
  scale_fill_manual(values=c("darkgrey", "#ff0000", "#aa0000"#, "#770000"
                             , "#006600", "#00ff00", "#008800", "#00aa00", "#00cc00"
                             , "#000088", "#0000ff", "#0000aa"))

# This looks to me like group 
# 1 is generally goood reading and spelling
# 2 slightly below average
# 3: slightly above average
# 4: poor-ish spellers, poor readers
# 5: average but very poor at the _e items
# 6/7: very poor readers and spellers

### 1.5.4 Create Images ####

# png("Images/HClust.complete.groups.png", height=640, width=800)
# cmplt.ggp
# dev.off()
# 
# png("Images/HClust.wardD.groups.png", height=640, width=800)
# wardD.ggp
# dev.off()
# 
# png("Images/HClust.wardD2.groups.png", height=640, width=800)
# wardD2.ggp
# dev.off()

# 2.0 Clustering, Spelling only ####

# No real need to do this, just have to drop the CC2 scores from the other
# spots
# HSSpell.clst = HSSpelling %>% 
#   select(ID, AgeM, starts_with("DiST"), starts_with("MoST")
#          #, starts_with("CC2")
#          #, starts_with("TOWRE"), starts_with("WIAT"), LetPos # Very few obs
#   ) %>% drop_na()

## 2.1.1 Adjust for Age ###

# HSSpell.age = HSSpell.clst %>% 
#   mutate(across(DiST.Irr:MoST_y2i
#                 , function(x) 
#                   rstandard(lm(x~AgeM, HSSpell.clst))
#   )
#   )

## 2.2 Calculate distances (use euclidean) ####
HSSpellOnly.dist = HSSpell.age %>% 
  select(-ID, -AgeM, -starts_with("CC2")) %>% 
  dist

## 2.3 try all clustering methods ####
# clst.methods = as.list(c("complete", "ward.D", "ward.D2", "single", "average"
#                          , "mcquitty", "median", "centroid"))
# names(clst.methods) = c("complete", "ward.D", "ward.D2", "single", "average"
#                         , "mcquitty", "median", "centroid")

HSSpellOnly.hclst = lapply(clst.methods
                           , function(x) hclust(HSSpellOnly.dist, method=x[1]))

### 2.3.1 Plot the results ####
# lapply(HSSpellOnly.hclst,
#        function (x) {
#          png(paste0("Images/HClust.SpellOnly.", x$method,".png"), height=640, width=800)
#          plot(x, labels=F, main=x$method)
#          dev.off()
#        })
# dev.off() # lapply always seems to leave one open

## 2.4 Look at some options ####

table(cutree(HSSpellOnly.hclst$ward.D2, 7))
## Looks like complete, ward D & D2 are all producing reasonable clusters. 
# Complete produces 5 (one small)
# Ward D  produces 10
# Ward D2 produces 7


HSSpell.age$hclust.cmplt.SpellOnly = cutree(HSSpellOnly.hclst$complete, 5)
HSSpell.age$hclust.wardD.SpellOnly = cutree(HSSpellOnly.hclst$ward.D, 10)
HSSpell.age$hclust.wardD2.SpellOnly = cutree(HSSpellOnly.hclst$ward.D2, 7)

## 2.5 Plot results ####

# colnames(HSSpell.clst)

## 2.5.1 Complete linkage: 5 grps ####
cmplt.SpellOnly.ggp = HSSpell.age %>% 
  select(AgeM:MoST_y2i, hclust.cmplt.SpellOnly) %>% 
  rename(Group = hclust.cmplt.SpellOnly) %>% 
  mutate(AgeM = scale(AgeM)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:MoST_y2i
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:12])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.cmplt.SpellOnly, HSSpell.age, length)
             , mapping=aes(x=hclust.cmplt.SpellOnly, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Complete Linkage - Spelling Only", subtitle = "Age-adjusted")+
  scale_fill_manual(values=c("darkgrey", "#ff0000", "#aa0000"
                             , "#006600", "#00ff00", "#008800", "#00aa00", "#00cc00"
                             , "#000088", "#0000ff", "#0000aa"))

# This looks to me like group 
# 1 slightly above average readers and spellers, 
# 2 slightly below readers/spellers
# 3 average, except for MoST _e items
# 4: small group of very poor performers
# 5: group of quite poor performers


## 2.5.2 Ward.D: 10 grps ####
wardD.SpellOnly.ggp = HSSpell.age %>% 
  select(AgeM:MoST_y2i, hclust.wardD.SpellOnly) %>% 
  rename(Group = hclust.wardD.SpellOnly) %>% 
  mutate(across(AgeM, scale)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:MoST_y2i
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:12])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.wardD.SpellOnly, HSSpell.age, length)
             , mapping=aes(x=hclust.wardD.SpellOnly, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Ward D - Spelling Only", "Age-adjusted")+
  scale_fill_manual(values=c("darkgrey", "#ff0000", "#aa0000"
                             , "#006600", "#00ff00", "#008800", "#00aa00", "#00cc00"
                             , "#000088", "#0000ff", "#0000aa"))

# This looks to me like group 
# 1 is generally good readers and spellers, 
# 2 below average, especially f2v
# 3 slightly above average
# 4 generally good except for f2v (so similar to 1)
# 5: poor on DiST and some MoST
# 6: average, but bad at _e
# 7: small group, very, very poor
# 8: slighlty above average, except for DiST.Non
# 9: Poor Spellers
# 10: poor spellers, very poor at f2v, and y2i

## 2.5.3 Ward.D2: 9 grps ####
wardD2.SpellOnly.ggp = HSSpell.age %>% 
  select(AgeM:MoST_y2i, hclust.wardD2.SpellOnly) %>% 
  rename(Group = hclust.wardD2.SpellOnly) %>% 
  mutate(across(AgeM, scale)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:MoST_y2i
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:12])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.wardD2.SpellOnly, HSSpell.age, length)
             , mapping=aes(x=hclust.wardD2.SpellOnly, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Ward D2 - Spelling Only", "Age-adjusted")+
  scale_fill_manual(values=c("darkgrey", "#ff0000", "#aa0000"
                             , "#006600", "#00ff00", "#008800", "#00aa00", "#00cc00"
                             , "#000088", "#0000ff", "#0000aa"))

# This looks to me like group 
# 1 good spellers
# 2 slightly below average (bad at f2v)
# 3:very average/slightly above average
# 4: good spellers except f2v (MoST)
# 5: poor spellers, excpt _f2v
# 6: average but very low on _e
# 7: small group of very poor spellers.

### 2.5.4 Create Images ####

# png("Images/HClust.SpellOnly.complete.groups.png", height=640, width=800)
# cmplt.SpellOnly.ggp
# dev.off()
# 
# png("Images/HClust.SpellOnly.wardD.groups.png", height=640, width=800)
# wardD.SpellOnly.ggp
# dev.off()
# 
# png("Images/HClust.SpellOnly.wardD2.groups.png", height=640, width=800)
# wardD2.SpellOnly.ggp
# dev.off()

# 3.0 Latent Profile Analyses ####
include(mclust)

## 3.1 Spelling+Reading ####

HSSpell.mclust = mclustBIC(HSSpell.age %>% select(DiST.Irr:CC2.Non))

# plot(HSSpell.mclust)
# Technically VVE,5 is the best, but it shows a strong spike from ,4 to ,5 and 
# drop again to ,6, so not sure that's the best.
# VVV,4 ,5 ,6 are pretty similar, as is EVE 5

# Feels like LPA really wants to say "5"
# Let's look at some plots.

## 3.1.1 Distribution Plots ####
HSSpell.VVE5 = Mclust(HSSpell.age %>% select(DiST.Irr:CC2.Non)
                      , x=HSSpell.mclust, modelNames="VVE", G=5)

plot(HSSpell.VVE5, what = "classification")
# MoST _e items seem to be driving a lot here

HSSpell.VVV5 = Mclust(HSSpell.age %>% select(DiST.Irr:CC2.Non)
                      , x=HSSpell.mclust, modelNames="VVV", G=5)
plot(HSSpell.VVV5, what = "classification")

HSSpell.VVV4 = Mclust(HSSpell.age %>% select(DiST.Irr:CC2.Non)
                      , x=HSSpell.mclust, modelNames="VVV", G=4)
plot(HSSpell.VVV4, what = "classification")


## 3.1.2 Means plots ####

gridExtra::grid.arrange(
  HSSpell.VVV5$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X5, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVV 5", "Age-adjusted")+
    guides(fill="none")+
    scale_fill_manual(
      values=c("#0000ff", "#0000aa", "#000088" 
               , "#ff0000", "#aa0000"
               , "#006600", "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )
  , HSSpell.VVV4$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X4, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVV 4", "Age-adjusted")+
    guides(fill="none")+
    scale_fill_manual(
      values=c("#0000ff", "#0000aa", "#000088" 
               , "#ff0000", "#aa0000"
               , "#006600", "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )

  , HSSpell.VVE5$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X5, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVE 5", "Age-adjusted")+
    scale_fill_manual(
      values=c("#0000ff", "#0000aa", "#000088" 
               , "#ff0000", "#aa0000"
               , "#006600", "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )
  , layout_matrix=matrix(c(1,1,2,2,NA,3,3,NA), nrow=2, byrow=T)
)

## 3.2 Spelling Only ####

HSSpellOnly.mclust = mclustBIC(HSSpell.age %>% select(DiST.Irr:MoST_y2i))

# plot(HSSpellOnly.mclust)
# 

# Feels like LPA really wants to say "4" in this case, which is odd since
# the reading data didn't seem to define much in the previous versions
# Let's look at some plots.

## 3.2.1 Distribution Plots ####
HSSpellOnly.VVV4 = Mclust(HSSpell.age %>% select(DiST.Irr:MoST_y2i)
                      , x=HSSpellOnly.mclust, modelNames="VVV", G=4)

plot(HSSpellOnly.VVV4, what = "classification")
# MoST _e items seem to be driving a lot here

HSSpellOnly.EVE4 = Mclust(HSSpell.age %>% select(DiST.Irr:MoST_y2i)
                      , x=HSSpellOnly.mclust, modelNames="EVE", G=4)
plot(HSSpellOnly.EVE4, what = "classification")

HSSpellOnly.EVE5 = Mclust(HSSpell.age %>% select(DiST.Irr:MoST_y2i)
                      , x=HSSpellOnly.mclust, modelNames="EVE", G=5)
plot(HSSpellOnly.EVE5, what = "classification")


## 3.2.2 Means plots ####

gridExtra::grid.arrange(
  HSSpellOnly.VVV4$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X4, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVV 5", "Age-adjusted")+
    guides(fill="none")+
    scale_fill_manual(
      values=c(#"#0000ff", "#0000aa", "#000088" 
                "#ff0000", "#aa0000"
               , "#006600", "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )
  , HSSpellOnly.EVE4$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X4, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVV 4", "Age-adjusted")+
    guides(fill="none")+
    scale_fill_manual(
      values=c(#"#0000ff", "#0000aa", "#000088" 
                "#ff0000", "#aa0000"
               , "#006600", "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )
  
  , HSSpellOnly.EVE5$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X5, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVE 5", "Age-adjusted")+
    scale_fill_manual(
      values=c(#"#0000ff", "#0000aa", "#000088" 
                "#ff0000", "#aa0000"
               , "#006600", "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )
  , layout_matrix=matrix(c(1,1,2,2,NA,3,3,NA), nrow=2, byrow=T)
)

## 3.3 Spelling + Reading, no Subscales ####

HSSpellnoSubs.mclust = mclustBIC(HSSpell.age %>% 
                                   select(starts_with("DiST")
                                          , starts_with("CC2")
                                          , MoST_FullItem))

# plot(HSSpellnoSubs.mclust)
# 

# Getting rid of those subscales really changes the results... The groups
# are far less distinctive.


## 3.3.1 Distribution Plots ####
HSSpellnoSubs.VEE3 = Mclust(HSSpell.age %>% 
                              select(DiST.Irr:CC2.Non
                                     , -starts_with("MoST")
                                     , MoST_FullItem)
                          , x=HSSpellnoSubs.mclust, modelNames="VEE", G=3)

plot(HSSpellnoSubs.VEE3, what = "classification")
# MoST _e items seem to be driving a lot here

HSSpellnoSubs.VEE4 = Mclust(HSSpell.age %>% 
                              select(DiST.Irr:CC2.Non
                                     , -starts_with("MoST")
                                     , MoST_FullItem)
                            , x=HSSpellnoSubs.mclust, modelNames="VEE", G=4)

plot(HSSpellnoSubs.VEE4, what = "classification")

HSSpellnoSubs.VEE5 = Mclust(HSSpell.age %>% 
                              select(DiST.Irr:CC2.Non
                                     , -starts_with("MoST")
                                     , MoST_FullItem)
                            , x=HSSpellnoSubs.mclust, modelNames="VEE", G=5)

plot(HSSpellnoSubs.VEE5, what = "classification")


## 3.3.2 Means plots ####

gridExtra::grid.arrange(
  HSSpellnoSubs.VEE3$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X3, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVV 5", "Age-adjusted")+
    guides(fill="none")+
    scale_fill_manual(
      values=c("#0000ff", "#0000aa", "#000088" 
        , "#ff0000", "#aa0000"
        , "#00ff00"#, "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )
  , HSSpellnoSubs.VEE4$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X4, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVV 4", "Age-adjusted")+
    guides(fill="none")+
    scale_fill_manual(
      values=c("#0000ff", "#0000aa", "#000088" 
             , "#ff0000", "#aa0000"
             , "#00ff00"#, "#008800", "#00aa00", "#00ff00", "#00cc00"
    )
  )

  , HSSpellnoSubs.VEE5$parameters$mean %>% 
    data.frame %>% 
    mutate(Measure = rownames(.)) %>% 
    pivot_longer(X1:X5, names_to="Group", values_to = "Mean") %>% 
    ggplot(aes(x=Group, y=Mean, fill=Measure))+
    geom_col(position=position_dodge())+
    ggtitle("LPA - Spelling and Reading, VVE 5", "Age-adjusted")+
    scale_fill_manual(
      values=c("#0000ff", "#0000aa", "#000088" 
               , "#ff0000", "#aa0000"
               , "#00ff00"#, "#008800", "#00aa00", "#00ff00", "#00cc00"
      )
    )
  , layout_matrix=matrix(c(1,1,2,2,NA,3,3,NA), nrow=2, byrow=T)
)

# 4.0 PCA & FA ####

## 4.1 PCA ####

### 4.1.1 Reading + Spelling ####

HSSpell.pca = princomp(HSSpell.age %>% select(DiST.Irr:CC2.Non))

loadings(HSSpell.pca)
summary(HSSpell.pca)

# As usual, only the first 1 and maybe second component matter
# These are a general skill measure, followed by a messy one that, at a stretch
# might be "words vs nonwords, but with lots of _f2v for some reason)

# I think it's clear here that the _e and _f2v is defining a lot

### 4.1.2 Spelling Only (no CC2) ####

HSSpellOnly.pca = princomp(HSSpell.age %>% select(DiST.Irr:MoST_y2i))

loadings(HSSpellOnly.pca)
summary(HSSpellOnly.pca)

# Basically the same deal: general abilities, then a more clearly "_e" vs "_f2v"
# scale, but only the first has any real value.

### 4.1.3 No Subscales ####

HSSpellnoSubs.pca = princomp(HSSpell.age %>% select(starts_with("DiST")
                                                    , starts_with("CC2")
                                                    , MoST_FullItem))

loadings(HSSpellnoSubs.pca)
summary(HSSpellnoSubs.pca)

# Again, only 1 useful component here, and it's a general skill component.
# The second component is more clearly words vs nonwords, but with an
# eigenvalue of just .88...


## 4.2 Factor Analysis ####

### 4.2.1 Reading + Spelling ####

HSSpell.fa = factanal(HSSpell.age %>% select(DiST.Irr:CC2.Non), 3)


loadings(HSSpell.fa)

# 3 factors,
# 1: mostly CC2 with a bit of spelling, but not very cleanly
# 2: Irregs emphasised (both DiST and CC2, but with a few other)
# 3:  Nonword Spelling, but again, not very clean

### 4.2.2 Spelling Only (no CC2) ####

HSSpellOnly.fa = factanal(HSSpell.age %>% select(DiST.Irr:MoST_y2i), 2)

loadings(HSSpellOnly.fa)


# only 2 factors:
# 1: kind of just general spelling skill
# 2: emphasising nonword spelling (but doesn't explain a lot of variance)

### 4.2.3 No Subscales ####

HSSpellnoSubs.fa = factanal(HSSpell.age %>% 
                              select(starts_with("DiST"), starts_with("CC2")
                                     , MoST_FullItem), 3)

loadings(HSSpellnoSubs.fa)

# 3 factors here (max possible)
# 1: somewhat biased towards reading scores, but really a general skill
# 2: nonword spelling
# 3: Irregs (CC2.Irr, DiST.Irr, and CC2.Reg - which is always unclear)

