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
# 2024-06-09: Updated for new MoST scoring schemes.


# 0. Setup ####
if (!all(sapply(c("DiST.i", "DiST.n", "MoST.spell", "HSSpelling"), exists))) 
  source('HSSpelling.DataImport.R')

### Note that MoST.base and MoST.rule both correlate with MoST.full at .9.
# they only correlate with each other at .75

# Should this analysis exclude MoST.full.
# Hm.
# cor(HSSpell.age$MoST.base+HSSpell.age$MoST.rule_total, HSSpell.age$MoST.full)
# cor(HSSpell.age %>% select(Mo, Most.base, Most.suffix, Most.rule))

# 1.0 Clustering, Spelling + CC2 ####

## 1.1 Set up dataset for clustering ####

# Ditch Grade and use age, since they are nearly identically distributed
HSSpell.clst = HSSpelling %>% 
  select(ID, AgeM, starts_with("DiST"), starts_with("MoST"), starts_with("CC2")
         #, starts_with("TOWRE"), starts_with("WIAT"), LetPos # Very few obs
         , -contains("notNA"), -ends_with("sc")
  ) %>% 
  drop_na()

## 1.1.1 Adjust for Age ###

HSSpell.age = HSSpell.clst %>% 
  mutate(across(c(-ID, -AgeM)
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
lapply(HSSpell.hclst,
       function (x) {
         png(paste0("Images/HClust.", x$method,".png"), height=640, width=800)
         plot(x, labels=F, main=x$method)
         dev.off()
       })
# dev.off() # lapply always seems to leave one open

## 1.4 Look at some options ####

table(cutree(HSSpell.hclst$mcquitty, 2))
## Looks like complete, ward D & D2 are all producing reasonable clusters. 
# I'm aiming for at least 30 kids in a group (7.5% of the sample)
# Complete produces 4
# Ward D  produces 8 (one small, 16)
# Ward D2 produces 6 (one small, 16; one just shy, 27)
# McQuitty produces only 2


HSSpell.age$hclust.cmplt = cutree(HSSpell.hclst$complete, 4)
HSSpell.age$hclust.wardD = cutree(HSSpell.hclst$ward.D, 8)
HSSpell.age$hclust.wardD2 = cutree(HSSpell.hclst$ward.D2, 6)
HSSpell.age$hclust.mcquitty = cutree(HSSpell.hclst$mcquitty, 2)

## 1.5 Plot results ####
# colnames(HSSpell.clst)
## 1.5.1 Complete linkage: 4 grps ####
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
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:16])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.cmplt, HSSpell.age, length)
             , mapping=aes(x=hclust.cmplt, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Complete Linkage - Spelling + CC2", "Age-adjusted scores")+
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                        ))

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
  mutate(AgeM = scale(AgeM)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:CC2.Non
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:16])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.wardD, HSSpell.age, length)
             , mapping=aes(x=hclust.wardD, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Ward D Linkage - Spelling + CC2", "Age-adjusted scores")+
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                      ))

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
  mutate(AgeM = scale(AgeM)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:CC2.Non
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:16])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.wardD2, HSSpell.age, length)
             , mapping=aes(x=hclust.wardD2, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("Ward D2 Linkage - Spelling + CC2", "Age-adjusted scores")+
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                      ))

# This looks to me like group 
# 1 is generally goood reading and spelling
# 2 slightly below average
# 3: slightly above average
# 4: poor-ish spellers, poor readers
# 5: average but very poor at the _e items
# 6/7: very poor readers and spellers

## 1.5.4 McQuitty: 2 grps ####
mcquitty.ggp = HSSpell.age %>% 
  select(AgeM:CC2.Non, hclust.mcquitty) %>% 
  rename(Group = hclust.mcquitty) %>% 
  mutate(AgeM = scale(AgeM)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:CC2.Non
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:16])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.mcquitty, HSSpell.age, length)
             , mapping=aes(x=hclust.mcquitty, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("McQuitty Linkage - Spelling + CC2", "Age-adjusted scores")+
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                      ))

# A very average group, and 24 very poor reader/spellers

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
#   mutate(across(DiST.Irr:MoST.y_i
#                 , function(x) 
#                   rstandard(lm(x~AgeM, HSSpell.clst))
#   )
#   )

## 2.2 Calculate distances (use euclidean) ####
HSSpellOnly.dist = HSSpell.age %>% 
  select(-ID, -AgeM, -starts_with("CC2"), -starts_with("hclust")) %>% 
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

table(cutree(HSSpellOnly.hclst$mcquitty, 3))
## Looks like complete, ward D & D2 are all producing reasonable clusters. 
# Complete produces 4 (one small)
# Ward D  produces 8 (one small)
# Ward D2 produces 9 (one small)


HSSpell.age$hclust.cmplt.SpellOnly = cutree(HSSpellOnly.hclst$complete, 4)
HSSpell.age$hclust.wardD.SpellOnly = cutree(HSSpellOnly.hclst$ward.D, 8)
HSSpell.age$hclust.wardD2.SpellOnly = cutree(HSSpellOnly.hclst$ward.D2, 9)
HSSpell.age$hclust.mcquitty.SpellOnly = cutree(HSSpellOnly.hclst$mcquitty, 3)

## 2.5 Plot results ####

# colnames(HSSpell.clst)

## 2.5.1 Complete linkage: 5 grps ####
cmplt.SpellOnly.ggp = HSSpell.age %>% 
  select(AgeM:MoST.y_i, hclust.cmplt.SpellOnly) %>% 
  rename(Group = hclust.cmplt.SpellOnly) %>% 
  mutate(AgeM = scale(AgeM)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:MoST.y_i
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
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                      ))
# This looks to me like group 
# 1 slightly above average readers and spellers, 
# 2 slightly below readers/spellers
# 3 average, except for MoST _e items
# 4: small group of very poor performers
# 5: group of quite poor performers


## 2.5.2 Ward.D: 10 grps ####
wardD.SpellOnly.ggp = HSSpell.age %>% 
  select(AgeM:MoST.y_i, hclust.wardD.SpellOnly) %>% 
  rename(Group = hclust.wardD.SpellOnly) %>% 
  mutate(across(AgeM, scale)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:MoST.y_i
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
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                      ))


## 2.5.3 Ward.D2: 9 grps ####
wardD2.SpellOnly.ggp = HSSpell.age %>% 
  select(AgeM:MoST.y_i, hclust.wardD2.SpellOnly) %>% 
  rename(Group = hclust.wardD2.SpellOnly) %>% 
  mutate(across(AgeM, scale)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:MoST.y_i
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
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                      ))

## 2.5.4 McQuitty: 9 grps ####
mcquitty.SpellOnly.ggp = HSSpell.age %>% 
  select(AgeM:MoST.y_i, hclust.mcquitty.SpellOnly) %>% 
  rename(Group = hclust.mcquitty.SpellOnly) %>% 
  mutate(across(AgeM, scale)) %>% 
  group_by(Group) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(
    AgeM:MoST.y_i
    , names_to = "Measure"
    , values_to = "Mean"
  ) %>% 
  mutate(Measure = factor(Measure, levels=colnames(HSSpell.age)[2:12])
         , Group = factor(Group)) %>% 
  ggplot(aes(x=Group, fill = Measure, y=Mean))+
  geom_col(position=position_dodge())+
  geom_label(data=aggregate(ID~hclust.mcquitty.SpellOnly, HSSpell.age, length)
             , mapping=aes(x=hclust.mcquitty.SpellOnly, y=1, label=paste("N =", ID), fill=NULL))+
  ggtitle("McQuitty - Spelling Only", "Age-adjusted")+
  scale_fill_manual(values=
                      c("darkgrey", # Age
                        "#ff0000", "#aa0000"#Dist*, "#770000"
                        , "#006600", "#00ff00", "#008800" # MoST*
                        , "#420042", "#640064", "#960096", "#c800c8", "#f900f9"# MoST Rule
                        , "#000088", "#0000ff", "#0000aa" # CC2
                      ))

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

HSSpell.mclust = mclustBIC(HSSpell.age %>% select(DiST.Irr:CC2.Non)
                           , modelNames = mclust.options("emModelNames")[1:6])

plot(HSSpell.mclust)
# For some reason, models "EEE" "VEE" "EVE" "VVE" "EEV" "VEV" "EVV" "VVV" are 
# all identical - 1 group, same BIC. I suspect it's something about too many 
# free parameters for too many variables with not enough participants so I have
# excluded those.

# I think the VEI and VVI models, with around 7 groups are the most parsimonious
# but best choices 

# Let's look at some plots.
VarLvls = c("AgeM", "MoST.full", "MoST.base", "MoST.suffix", "MoST.rule_total"
            , "MoST.dbl_cons", "MoST.drop_e", "MoST.f_v", "MoST.y_i"
            , "DiST.Irr", "DiST.Non", "CC2.Irr", "CC2.Non", "CC2.Reg")


### 3.1.1 Distribution Plots ####

#### 3.1.1.1 EEE 1 ####

### This plot is completely pointless, since with only 1 group, all scores must
# be the mean score of0, and all SE's must be equal to basically 1/sqrt(403)


#### 3.1.1.2 VEI 7 ####
HSSpell.VEI7 = Mclust(HSSpell.age %>% select(DiST.Irr:CC2.Non)
                      , x=HSSpell.mclust, modelNames="VEI", G=7)

# plot(HSSpell.VEI6, what = "classification")
# There are really too many variables to use the matrix plotting of scatter
# plots. Instead, I'm going to generate some barplots.

plotDat.VEI7 = merge(
  HSSpell.VEI7$parameters$mean %>% data.frame # means
  , (apply(HSSpell.VEI7$parameters$variance$sigma,3, diag) %>% data.frame %>% 
    sqrt)/sqrt(table(HSSpell.VEI7$classification))
  , by=0, suffixes=c(".Mean", ".SE")
) %>% rename(Var = Row.names) %>% data.frame

plotDat.VEI7 %>% data.frame %>% 
  pivot_longer(-Var, values_to="Value", names_to=c("Group", "Measure")
               , names_sep="\\.") %>% 
  pivot_wider(id_cols=Var:Group, names_from = Measure, values_from=Value) %>% 
  mutate(Group = gsub("X", "grp", Group)) %>% data.frame %>% 
  mutate(Var = factor(Var, levels=VarLvls)) %>% 
  ggplot(aes(x=Group, y=Mean, fill=Var, group=Var, ymin=Mean-SE, ymax=Mean+SE))+
  geom_col(position="dodge")+
  geom_errorbar(position="dodge")+
  geom_vline(xintercept = 1:6+.5, col="grey")+
  annotate(geom="label", x=1:7, y=1.75
           , label=paste("N:", table(HSSpell.VEI7$classification)))+
  theme_minimal()+theme(panel.grid.major.x=element_blank())+
  scale_fill_manual(values=
                        c(#"darkgrey", # Age
                          "#006600", "#009900", "#00bb00", "#00ff00" # MoST*
                          , "#640064", "#960096", "#c800c8", "#f900f9"# MoST subrules
                          , "#aa0000", "#ff0000" #Dist*, "#770000"
                          , "#000066", "#0000aa", "#0000ff" # CC2
                        ))+
  ggtitle("LPA classificiations", subtitle="VEI, 7 groups")+
  ylab("Mean and SE's (sample z-scores)")


#### 3.1.1.3 VVI 7 ####
HSSpell.VVI7 = Mclust(HSSpell.age %>% select(DiST.Irr:CC2.Non)
                      , x=HSSpell.mclust, modelNames="VVI", G=7)

# plot(HSSpell.VVI6, what = "classification")
# There are really too many variables to use the matrix plotting of scatter
# plots. Instead, I'm going to generate some barplots.

plotDat.VVI7 = merge(
  HSSpell.VVI7$parameters$mean %>% data.frame # means
  , (apply(HSSpell.VVI7$parameters$variance$sigma,3, diag) %>% data.frame %>% 
       sqrt)/sqrt(table(HSSpell.VVI7$classification))
  , by=0, suffixes=c(".Mean", ".SE")
) %>% rename(Var = Row.names) %>% data.frame

plotDat.VVI7 %>% data.frame %>% 
  pivot_longer(-Var, values_to="Value", names_to=c("Group", "Measure")
               , names_sep="\\.") %>% 
  pivot_wider(id_cols=Var:Group, names_from = Measure, values_from=Value) %>% 
  mutate(Group = gsub("X", "grp", Group)) %>% data.frame %>% 
  mutate(Var = factor(Var, levels=VarLvls)) %>% 
  ggplot(aes(x=Group, y=Mean, fill=Var, group=Var, ymin=Mean-SE, ymax=Mean+SE))+
  geom_col(position="dodge")+
  geom_errorbar(position="dodge")+
  geom_vline(xintercept = 1:6+.5, col="grey")+
  theme_minimal()+theme(panel.grid.major.x=element_blank())+
  scale_fill_manual(values=
                      c(#"darkgrey", # Age
                        "#007700", "#009900", "#00bb00", "#00ff00" # MoST*
                        , "#740074", "#970097", "#c800c8", "#f900f9"# MoST subrules
                        , "#aa0000", "#ff0000" #Dist*, "#770000"
                        , "#000077", "#0000aa", "#0000ff" # CC2
                      ))+
  annotate(geom="label", x=1:7, y=1.75
           , label=paste("N:", table(HSSpell.VVI7$classification)))+
  ggtitle("LPA classificiations", subtitle="VVI, 7 groups")+
  ylab("Mean and SE's (sample z-scores)")


## 3.2 Spelling Only ####

HSSpellOnly.mclust = mclustBIC(HSSpell.age %>% 
                             select(starts_with("DiST"), starts_with("MoST"))
                           , modelNames = mclust.options("emModelNames")[1:6]
                           )

plot(HSSpellOnly.mclust)
# For the most complex models ("EEE" "VEE" "EVE" "VVE" "EEV" "VEV" "EVV" "VVV")
# there are too many free parameters to fit more than 1 group, which is 
# trivially uninteresting.

# As in the full data, the VEI and VVI models with around 6 groups are the most 
# parsimonious but best choices. more groups produce trivial differences.

# Let's look at some plots.

### 3.2.1 Distribution Plots ####

#### 3.2.1.1 EEE 1 ####

### This plot is completely pointless, since with only 1 group, all scores must
# be the mean score of0, and all SE's must be equal to basically 1/sqrt(403)


#### 3.2.1.2 VEI 6 ####
HSSpellOnly.VEI6 = Mclust(HSSpell.age %>% 
                            select(starts_with("DiST"), starts_with("MoST"))
                      , x=HSSpellOnly.mclust, modelNames="VEI", G=6)

# plot(HSSpellOnly.VEI6, what = "classification")
# There are really too many variables to use the matrix plotting of scatter
# plots. Instead, I'm going to generate some barplots.

plotDat.VEI6 = merge(
  HSSpellOnly.VEI6$parameters$mean %>% data.frame # means
  , (apply(HSSpellOnly.VEI6$parameters$variance$sigma,3, diag) %>% data.frame %>% 
       sqrt)/sqrt(table(HSSpellOnly.VEI6$classification))
  , by=0, suffixes=c(".Mean", ".SE")
) %>% rename(Var = Row.names) %>% data.frame

plotDat.VEI6 %>% data.frame %>% 
  pivot_longer(-Var, values_to="Value", names_to=c("Group", "Measure")
               , names_sep="\\.") %>% 
  pivot_wider(id_cols=Var:Group, names_from = Measure, values_from=Value) %>% 
  mutate(Group = gsub("X", "grp", Group)) %>% data.frame %>% 
  mutate(Var = factor(Var, levels=VarLvls)) %>% 
  ggplot(aes(x=Group, y=Mean, fill=Var, group=Var, ymin=Mean-SE, ymax=Mean+SE))+
  geom_col(position="dodge")+
  geom_errorbar(position="dodge")+
  geom_vline(xintercept = 1:5+.5, col="grey")+
  annotate(geom="label", x=1:6, y=1.75
           , label=paste("N:", table(HSSpellOnly.VEI6$classification)))+
  theme_minimal()+theme(panel.grid.major.x=element_blank())+
  scale_fill_manual(values=
                      c(#"darkgrey", # Age
                        "#006600", "#009900", "#00bb00", "#00ff00" # MoST*
                        , "#640064", "#960096", "#c800c8", "#f900f9"# MoST subrules
                        , "#aa0000", "#ff0000" #Dist*, "#770000"
                        , "#000066", "#0000aa", "#0000ff" # CC2
                      ))+
  ggtitle("LPA classificiations (Spelling Measures Only)"
          , subtitle="VEI, 6 groups")+
  ylab("Mean and SE's (sample z-scores)")


#### 3.2.1.3 VVI 6 ####
HSSpellOnly.VVI6 = Mclust(HSSpell.age %>% 
                            select(starts_with("DiST"), starts_with("MoST"))
                      , x=HSSpellOnly.mclust, modelNames="VVI", G=6)

# plot(HSSpellOnly.VVI6, what = "classification")
# There are really too many variables to use the matrix plotting of scatter
# plots. Instead, I'm going to generate some barplots.

plotDat.VVI6 = merge(
  HSSpellOnly.VVI6$parameters$mean %>% data.frame # means
  , (apply(HSSpellOnly.VVI6$parameters$variance$sigma,3, diag) %>% data.frame %>% 
       sqrt)/sqrt(table(HSSpellOnly.VVI6$classification))
  , by=0, suffixes=c(".Mean", ".SE")
) %>% rename(Var = Row.names) %>% data.frame

plotDat.VVI6 %>% data.frame %>% 
  pivot_longer(-Var, values_to="Value", names_to=c("Group", "Measure")
               , names_sep="\\.") %>% 
  pivot_wider(id_cols=Var:Group, names_from = Measure, values_from=Value) %>% 
  mutate(Group = gsub("X", "grp", Group)) %>% data.frame %>% 
  mutate(Var = factor(Var, levels=VarLvls)) %>% 
  ggplot(aes(x=Group, y=Mean, fill=Var, group=Var, ymin=Mean-SE, ymax=Mean+SE))+
  geom_col(position="dodge")+
  geom_errorbar(position="dodge")+
  geom_vline(xintercept = 1:5+.5, col="grey")+
  theme_minimal()+theme(panel.grid.major.x=element_blank())+
  scale_fill_manual(values=
                      c(#"darkgrey", # Age
                        "#006600", "#009900", "#00bb00", "#00ff00" # MoST*
                        , "#640064", "#960096", "#c800c8", "#f900f9"# MoST subrules
                        , "#aa0000", "#ff0000" #Dist*, "#770000"
                        , "#000066", "#0000aa", "#0000ff" # CC2
                      ))+
  annotate(geom="label", x=1:6, y=1.75
           , label=paste("N:", table(HSSpellOnly.VVI6$classification)))+
  ggtitle("LPA classificiations (Spelling Measures Only)"
          , subtitle="VVI, 6 groups")+
  ylab("Mean and SE's (sample z-scores)")

# 4.0 PCA & FA ####

## 4.1 PCA ####
# Note that rule_total is, by definition, the sum of the subrules. So we should
# only every use rule_total, or the sub_rules.

### 4.1.1 Reading + Spelling ####

HSSpell.pca = princomp(HSSpell.age %>% select(DiST.Irr:CC2.Non
                                              , -MoST.rule_total))

loadings(HSSpell.pca)
summary(HSSpell.pca)

# As usual, only the first 1 and maybe second component matter
# These are a general skill measure, followed by a messy one that, at a stretch
# might be "words vs nonwords, but with lots of _f2v for some reason)

# I think it's clear here that the _e and _f2v is defining a lot

### 4.1.2 Spelling Only (no CC2) ####

HSSpellOnly.pca = princomp(HSSpell.age %>% select(DiST.Irr:MoST.y_i
                                                  , -MoST.rule_total))

loadings(HSSpellOnly.pca)
summary(HSSpellOnly.pca)

# Basically the same deal: general abilities, then a more clearly "_e" vs "_f2v"
# scale, but only the first has any real value.

### 4.1.3 No Subscales ####

HSSpellnoSubs.pca = princomp(HSSpell.age %>% select(starts_with("DiST")
                                                    , starts_with("CC2")
                                                    , MoST.full))

loadings(HSSpellnoSubs.pca)
summary(HSSpellnoSubs.pca)

# Again, only 1 useful component here, and it's a general skill component.
# The second component is more clearly words vs nonwords, but with an
# eigenvalue of just .88...


## 4.2 Factor Analysis ####
# The new coding scheme is making factor analysis very difficult.
# First I'll go with just the rule_total, no sub rules. (f_v, etc.)

### 4.2.1 Reading + Spelling ####

HSSpell.fa = fa(HSSpell.age %>% select(DiST.Irr:CC2.Non, -MoST.dbl_cons
                                       , -MoST.f_v, -MoST.y_i, -MoST.drop_e
                                       , -MoST.full)
                , nfactors=4
                      , rotate = "oblimin")


loadings(HSSpell.fa)

# 4 factors, in order of Prop Var
# MR3: Mostly the MoST suffix scores (rule + suffix)
# MR1: Reading, with an emphasis on Words (mostly Regs)
# MR4: Irreg spelling.
# MR2: Nonword Spelling, mostly DiST.

# For MR2, notice that I have removed the MOST.fullword spelling here due to
# extremely high collinearity with base and rule subscores.

#### 4.2.1.1 ... remove rule_total, and use sub_rules ####
HSSpell.fa = fa(HSSpell.age %>% select(DiST.Irr:CC2.Non, -MoST.rule_total
                                       , -MoST.full)
                , nfactors=5
                , rotate = "oblimin")


loadings(HSSpell.fa)
# 5 factors, in order of Prop Var (the last is really not doing much)
# MR1: Reading, with an emphasis on Words (mostly Regs)
# MR3: MoST.base+ a couple of rules.
# MR5: Word spelling (DiST.Irr), + Irreg Reading (Orthographic representations?)
# MR4: MoST suffixes, ignoring rules.
# MR2: (not a great factor) drop_e rule.

### 4.2.2 Spelling Only (no CC2) ####

HSSpellOnly.fa = factanal(HSSpell.age %>% select(DiST.Irr:MoST.y_i), 2)

loadings(HSSpellOnly.fa)


# only 2 factors:
# 1: kind of just general spelling skill
# 2: emphasising nonword spelling (but doesn't explain a lot of variance)

### 4.2.3 No Subscales ####

HSSpellnoSubs.fa = factanal(HSSpell.age %>% 
                              select(starts_with("DiST"), starts_with("CC2")
                                     , MoST.full), 3)

loadings(HSSpellnoSubs.fa)

# 3 factors here (max possible)
# 1: somewhat biased towards reading scores, but really a general skill
# 2: nonword spelling
# 3: Irregs (CC2.Irr, DiST.Irr, and CC2.Reg - which is always unclear)


### 4.2.4 Remove MoST.full ####
# MoST.full is highly correlated with base word accuracy, rule accuracy, and
# particularly the combination of base words and rules.
# So let's remove it, and see if we get something interesting.

HSSpellnoMoSTfull.fa = factanal(HSSpell.age %>% 
                              select(starts_with("DiST"), starts_with("CC2")
                                     , starts_with("MoST"), -MoST.rule_total), 1)

loadings(HSSpellnoSubs.fa)

# 3 factors here (max possible)
# 1: somewhat biased towards reading scores, but really a general skill
# 2: nonword spelling
# 3: Irregs (CC2.Irr, DiST.Irr, and CC2.Reg - which is always unclear)

