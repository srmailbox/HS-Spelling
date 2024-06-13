################################################################################
# High School Spelling Data
# Results from various reading and spelling tasks from ~400 high school 
# students.
# 
# Analyses - Age "norming"
#
# Created: 2024/06/10
# Author: Serje Robidoux
# CHANGELOG:
#

# 0. Setup ####
include(quantreg)
include(gridExtra)
if (!all(sapply(c("DiST.i", "DiST.n", "MoST.spell", "HSSpelling"), exists))) 
  source('HSSpelling.DataImport.R')

# Really just need the mostWide here, merged with the age in months
# Note that one kid has an age of 1390 months, which is obviously nonsense.
# there's also a 9 yr old in gr 9, which makes me think the 9 was duplicated.

demog %>% filter(AgeM<1000) %>% 
  ggplot(aes(y=AgeM/12))+
  geom_boxplot()+
  facet_grid(.~Grade)+
  scale_y_continuous(breaks=0:18)

mostAge = mostWide %>% select(-contains("notNA")) %>% 
  pivot_longer(cols = starts_with("MoST"), values_to="Score", names_to="Scale") %>% 
  merge(demog %>% select(ID, AgeM, Gender), by="ID") %>% 
  filter(AgeM <1000, AgeM>120) %>% 
  mutate(Scale = factor(Scale
                        , levels=c("MoST.full", "MoST.base", "MoST.suffix"
                                   , "MoST.rule_total", "MoST.dbl_cons"
                                   , "MoST.drop_e", "MoST.f_v", "MoST.y_i"
                                   , "MoST.full.sc", "MoST.base.sc"
                                   , "MoST.suffix.sc", "MoST.rule_total.sc"))
         )

# 1. Basic scatterplots with smooths ####

## 1.1 Full word, Base word, Suffix, and Rule ####

ggplot(mostAge %>% 
         filter(Scale %in% paste0("MoST."
                                  , c("full", "base", "suffix", "rule_total"))
                )
       , aes(x=AgeM/12, y=Score))+
  geom_point()+geom_smooth()+
  facet_grid(Scale~.)+
  theme_minimal()+
  xlab("Age in Yrs")+
  scale_x_continuous(breaks=10:18)
  
# Looks like full and base are pretty solid, suffix has a bit of a ceiling
# effect, as does rule (though less pronounced)

# 2.0 Quantile Regressions ####

# This seems like a perfect use for quantile regression, because we want to know
# what is "good, typical, and poor" performance.

# With 473 datapoints, we can pretty reasonably fit 5%ile intervals.
taus = c(seq(.05, .5, .05), seq(.6, .9, .1))

mostQRs = by(mostAge, mostAge$Scale
             , FUN = 
               function(x) 
                 rq(Score~I(AgeM/12), x, tau=taus)
             )[-c(5:8)]

# Note this works really poorly for the "sub rules" since those don't have
# enough possible scores to offer any resolution.

lapply(mostQRs[1:4], summary)

# It seems there are pretty clear increases in performance at quantiles 
# .7 or less in rules, .5 or less in suffixes (ceiling issue), all quantiles
# for base, and all quantiles for full word (though .9 is just barely sign.)

mostQRs.plots = lapply(
  mostQRs[1:4]
  , function(x)
    data.frame(x$x, x$y, predict(x)) %>% 
    rename(AgeInYrs=I.AgeM.12.
           , Score = x.y) %>% 
    rename_at(vars(X1:X14), ~paste0("q", taus*100)) %>% 
    pivot_longer(cols=q5:q90
                 , names_to = "Quantile", values_to = "FittedScore") %>% 
    mutate(Quantile = factor(Quantile, levels=paste0("q", taus*100))) %>% 
    ggplot(aes(x=AgeInYrs))+
    geom_point(aes(y=Score), colour="grey")+ # observed values
    geom_line(aes(y=FittedScore, col=Quantile
                  , linewidth=I(ifelse(Quantile %in% c("q15", "q50", "q80"),2,1))
                  , linetype=I(ifelse(Quantile %in% c("q15", "q50", "q80")
                                      ,"solid","dashed"))
                  )
              )+
    xlab("Age in Years")+
    theme_minimal()+
    scale_color_discrete(guide=guide_legend(reverse=TRUE))
  
)

## 2.1 Full Word ####
(QRggp.full = mostQRs.plots$MoST.full + 
  ggtitle("Full Word Quantile Regression"))

### 2.1.1 do DiST.non ####

## 2.2 Base Word ####

(QRggp.base = mostQRs.plots$MoST.base + 
  ggtitle("Base Word Quantile Regression"))

## 2.3 Suffix Word ####

(QRggp.suffix = mostQRs.plots$MoST.suffix + 
  ggtitle("Suffix Quantile Regression", subtitle = "Note: ceiling"))

## 2.4 Rule Word ####

(QRggp.rule = mostQRs.plots$MoST.rule_total + 
  ggtitle("Rules Quantile Regression"))

## 2.5 All four together ####
grid.arrange(
  QRggp.full, QRggp.base, QRggp.suffix, QRggp.rule
)
