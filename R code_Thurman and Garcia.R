## R Code for paper "Asymmetric competition shapes amphibian response to rapid environmental change"
## Authors: Lindsey L. Thurman and Tiffany S. Garcia


#### Libraries ####
library(ggplot2)
library(nlme)
library(FSA)
library(reshape2)
library(plyr)
library(effects)
library(RVAideMemoire)
library(dplyr)


#### Read in Larvaldata.csv ####
dec = "."
COMP<-read.csv("Larvaldata.csv", header=T, dec=dec, stringsAsFactors=FALSE)
View(COMP)
str(COMP)

# Species codes:
# PSRE = Pseudacris regilla
# RACA = Rana cascadae
# ANBO = Anaxyrus boreas

# Competition Treatments:
# A: PSRE
# B: RACA
# C: ANBO
# D: PSRE - RACA
# E: RACA - ANBO
# F: PSRE - ANBO
# G: PSRE - RACA - ANBO

# Hydroperiod Treatments:
# Permanent
# Drying

# Why you no numeric Gosner!? because of stringsasfactors=FALSE
COMP$Gosner <- as.numeric(as.character(COMP$Gosner))
COMP$TreatHydro<-as.factor(COMP$TreatHydro)
COMP$TreatComp<-as.factor(COMP$TreatComp)
COMP$Species<-as.factor(COMP$Species)
str(COMP)

COMP$Block<-as.factor(COMP$Block)


# Create a single factor that merges both treatments
COMP$HydroComp<-interaction(COMP$TreatHydro, COMP$TreatComp)

# Default
levels(COMP$HydroComp)
#[1] "Drying.A"    "Permanent.A" "Drying.B"    "Permanent.B" "Drying.C"    "Permanent.C" "Drying.D"   
#[8] "Permanent.D" "Drying.E"    "Permanent.E" "Drying.F"    "Permanent.F" "Drying.G"    "Permanent.G"
# Control treatment = Permanent.A


# Subset Larval data
# By species
PSRE <- Subset(COMP, Species=="PSRE")
RACA <- Subset(COMP, Species=="RACA")
ANBO <- Subset(COMP, Species=="ANBO")

# Day 30
LARV30 <- Subset(COMP, SamplingPeriod == 4) # day 30 of larval data
PLARV30 <- Subset(LARV30, Species=="PSRE") # PSRE larval data at day 30 
RLARV30 <- Subset(LARV30, Species=="RACA") # RACA larval data at day 30 
ALARV30 <- Subset(LARV30, Species=="ANBO") # ANBO larval data at day 30 




#### Read in Emergencedata.csv ####
dec = "."
EMERG<-read.csv("Emergencedata.csv", header=T, dec=dec)
str(EMERG)

EMERG$Block<-as.factor(EMERG$Block)

View(EMERG)

# Create a single factor that merges both treatments
EMERG$HydroComp<-interaction(EMERG$TreatHydro, EMERG$TreatComp)

# Default
levels(EMERG$HydroComp)
[1] "Ephemeral.A" "Permanent.A" "Ephemeral.B" "Permanent.B" "Ephemeral.C"
[6] "Permanent.C" "Ephemeral.D" "Permanent.D" "Ephemeral.E" "Permanent.E"
[11] "Ephemeral.F" "Permanent.F" "Ephemeral.G" "Permanent.G"


# Subset Emergence data
# By species
P.EMERG <- Subset(EMERG, Species=="PSRE")
R.EMERG <- Subset(EMERG, Species=="RACA")
A.EMERG <- Subset(EMERG, Species=="ANBO")




#### Read in Survivaldata.csv ####
SURV<-read.csv("Survivaldata.csv", header=T)
str(SURV)

S$Block<-as.factor(SURV$Block)

# Create a single factor that merges both treatments
SURV$HydroComp<-interaction(SURV$TreatHydro, SURV$TreatComp)


# Subset survival data by species
ASURV<-Subset(SURV, Species=="ANBO")
PSURV<-Subset(SURV, Species=="PSRE")
RSURV<-Subset(SURV, Species=="RACA")





#### Larval Analyses ####

# The models presented below were applied to two response variables: Weight at day 30 ("IndivLarvWt")
# and Gosner stage at day 30 ("Gosner")

# Example code for ANBO weight at day 30

# Summarize response by treatments
ddply(ALARV30, ~HydroComp, summarise, mean(Gosner), sd=sd(Gosner))
#HydroComp    ..1        sd
#1 Ephemeral.C 36.502 0.8936274
#2 Permanent.C 36.680 0.6963117
#3 Ephemeral.E 35.898 0.4361995
#4 Permanent.E 35.940 0.7920859
#5 Ephemeral.F 36.196 0.7788967
#6 Permanent.F 36.226 0.5235743
#7 Ephemeral.G 35.378 1.2623470
#8 Permanent.G 36.352 0.8561367


ddply(ALARV30, ~TreatComp, summarise, mean(Gosner), sd=sd(Gosner))
#TreatComp    ..1        sd
#1         C 36.591 0.7610585
#2         E 35.919 0.6032403
#3         F 36.211 0.6258763
#4         G 35.865 1.1390859

ddply(ALARV30, ~TreatHydro, summarise, mean(Gosner), sd=sd(Gosner))
#TreatHydro     ..1        sd
#1  Ephemeral 35.9935 0.9232054
#2  Permanent 36.2995 0.7214129


# Need to make sure that Permanent.A (control treatment) is the reference level 
ALARV30 <- within(ALARV30, HydroComp <- relevel(HydroComp, ref = "Permanent.C"))

# Full/reduced Model comparison
# Full model includes treatment combinations
mod1 <- lme(Gosner ~ HydroComp, random=~1|Block/Tub, 
            method="ML",data=ALARV30) # use ML
# Reduced model includes treatments as main effects only
mod2 <- lme(Gosner ~ TreatHydro+TreatComp, random=~1|Block/Tub, 
            method="ML", data=ALARV30)
anova(mod1, mod2)
#Model      df      AIC      BIC    logLik   Test  L.Ratio p-value
#mod1     1 11 102.4321 121.0097 -40.21603                        
#mod2     2  8 100.4290 113.9401 -42.21452 1 vs 2 3.996982  0.2618

# Main effects
summary(mod2)
#Linear mixed-effects model fit by maximum likelihood
#Data: ALARV30 
#    AIC      BIC    logLik
#100.429 113.9401 -42.21452

#Random effects:
#  Formula: ~1 | Block
#(Intercept)
#StdDev:   0.4075989

#Formula: ~1 | Tub %in% Block
#(Intercept)  Residual
#StdDev:   0.5882788 0.2380341

#Fixed effects: Gosner ~ TreatHydro + TreatComp 
#                     Value Std.Error DF   t-value p-value
#(Intercept)         36.438 0.3090426 31 117.90609  0.0000
#TreatHydroPermanent  0.306 0.2145380 31   1.42632  0.1638
#TreatCompE          -0.672 0.3034025 31  -2.21488  0.0342 ** competition with RACA
#TreatCompF          -0.380 0.3034025 31  -1.25246  0.2198
#TreatCompG          -0.726 0.3034025 31  -2.39286  0.0230 ** three-spp competition

# Interactions
summary(mod1)
#Linear mixed-effects model fit by maximum likelihood
#Data: ALARV30 
#     AIC      BIC    logLik
#102.4321 121.0097 -40.21603

#Random effects:
#  Formula: ~1 | Block
#(Intercept)
#StdDev:   0.4142097

#Formula: ~1 | Tub %in% Block
#(Intercept)  Residual
#StdDev:   0.5548012 0.2268587

#Fixed effects: Gosner ~ HydroComp 
#                      Value Std.Error DF   t-value p-value
#(Intercept)          36.680 0.3642935 28 100.68805  0.0000
#HydroCompDrying.C    -0.178 0.4238333 28  -0.41998  0.6777
#HydroCompDrying.E    -0.782 0.4238333 28  -1.84507  0.0756
#HydroCompPermanent.E -0.740 0.4238333 28  -1.74597  0.0918
#HydroCompDrying.F    -0.484 0.4238333 28  -1.14196  0.2631
#HydroCompPermanent.F -0.454 0.4238333 28  -1.07118  0.2932
#HydroCompDrying.G    -1.302 0.4238333 28  -3.07196  0.0047 ** drying and three-spp. competition
#HydroCompPermanent.G -0.328 0.4238333 28  -0.77389  0.4455


# For plotting and interpretation:
ef1 <- effect("HydroComp", mod1) # have to use mod1 for interactions
summary(ef1)
xA<-as.data.frame(ef1)
xA
#    HydroComp    fit       se   lower   upper
#1 Permanent.C 36.680 0.325834 36.0163 37.3437
#2    Drying.C 36.502 0.325834 35.8383 37.1657
#3    Drying.E 35.898 0.325834 35.2343 36.5617
#4 Permanent.E 35.940 0.325834 35.2763 36.6037
#5    Drying.F 36.196 0.325834 35.5323 36.8597
#6 Permanent.F 36.226 0.325834 35.5623 36.8897
#7    Drying.G 35.378 0.325834 34.7143 36.0417
#8 Permanent.G 36.352 0.325834 35.6883 37.0157



#### Metamorph/Emergence Analyses ####


## Example code for ANBO weight at emergence

# Summarize response by treatments
aggregate(IndivMetaWt ~ HydroComp, data = A.EMERG, 
          function(x) c(mean = mean(x), sd = sd(x)), na.action=na.omit)  
#    HydroComp      IndivMetaWt.mean      IndivMetaWt.sd
#1 Ephemeral.C            0.12460000          0.03418040
#2 Permanent.C            0.15420000          0.03191708
#3 Ephemeral.E            0.11600000          0.04037326
#4 Permanent.E            0.14700000          0.03866523
#5 Ephemeral.F            0.12900000          0.03130495
#6 Permanent.F            0.14250000          0.02598076
#7 Ephemeral.G            0.14375000          0.02495830
#8 Permanent.G            0.18000000          0.04315669

aggregate(IndivMetaWt ~ TreatHydro, data = A.EMERG, 
          function(x) c(mean = mean(x), sd = sd(x)), na.action=na.omit)
#  TreatHydro      IndivMetaWt.mean      IndivMetaWt.sd
#1     Drying            0.12752632          0.03227722
#2  Permanent            0.15663158          0.03617275

aggregate(IndivMetaWt ~ TreatComp, data = A.EMERG, 
          function(x) c(mean = mean(x), sd = sd(x)), na.action=na.omit)
#  TreatComp      IndivMetaWt.mean      IndivMetaWt.sd
#1         C            0.13940000          0.03486227
#2         E            0.13150000          0.04069193
#3         F            0.13500000          0.02817357
#4         G            0.16388889          0.03911344


# Need to make sure that Permanent.A (control treatment) is the reference level 
A.EMERG <- within(A.EMERG, HydroComp <- relevel(HydroComp, ref = "Permanent.C"))


# Full/reduced Model comparison
mod1 <- lme(IndivMetaWt ~ HydroComp, random=~1|Block/Tub, 
            method="ML", na.action = na.exclude, data=A.EMERG) # use ML
mod2 <- lme(IndivMetaWt ~ TreatHydro+TreatComp, random=~1|Block/Tub, 
            method="ML", na.action = na.exclude, data=A.EMERG)
anova(mod1, mod2)

summary(mod1 <- lme(IndivMetaWt ~ HydroComp, random=~1|Block/Tub, 
                    method="REML", na.action = na.exclude, data=A.EMERG)  )
#Linear mixed-effects model fit by REML
#Data: A.EMERG 
#      AIC       BIC   logLik
#-81.82032 -66.40715 51.91016

#Random effects:
#  Formula: ~1 | Block
#(Intercept)
#StdDev: 0.005446424

#Formula: ~1 | Tub %in% Block
#(Intercept)   Residual
#StdDev:  0.03286419 0.01037465

#Fixed effects: IndivMetaWt ~ HydroComp 
#                           Value  Std.Error DF   t-value p-value
#(Intercept)           0.15420000 0.01560354 26  9.882374  0.0000
#HydroCompDrying.C    -0.02960000 0.02179622 26 -1.358033  0.1861
#HydroCompDrying.E    -0.03820000 0.02179622 26 -1.752597  0.0915
#HydroCompPermanent.E -0.00720000 0.02179622 26 -0.330332  0.7438
#HydroCompDrying.F    -0.02520000 0.02179622 26 -1.156164  0.2581
#HydroCompPermanent.F -0.01088202 0.02314571 26 -0.470153  0.6422
#HydroCompDrying.G    -0.01090770 0.02314571 26 -0.471262  0.6414
#HydroCompPermanent.G  0.02580000 0.02179622 26  1.183691  0.2472


A.EMERG <- within(A.EMERG, TreatHydro <- relevel(TreatHydro, ref = "Permanent"))

summary(mod2)
#Linear mixed-effects model fit by maximum likelihood
#Data: A.EMERG 
#      AIC       BIC   logLik
#-139.5872 -126.4865 77.79361

#Random effects:
#  Formula: ~1 | Block
#(Intercept)
#StdDev: 0.005303561

#Formula: ~1 | Tub %in% Block
#(Intercept)    Residual
#StdDev:  0.02926802 0.009667285

#Fixed effects: IndivMetaWt ~ TreatHydro + TreatComp 
#                       Value  Std.Error DF   t-value p-value
#(Intercept)       0.15344880 0.01203687 29 12.748226  0.0000 
#TreatHydroDrying -0.02809759 0.01077146 29 -2.608523  0.0142 **
#TreatCompE       -0.00790000 0.01479205 29 -0.534071  0.5974
#TreatCompF       -0.00240562 0.01521851 29 -0.158072  0.8755
#TreatCompG        0.02269920 0.01521851 29  1.491552  0.1466



## Example code for ANBO time to emergence
 
# # Full/reduced model comparison
mod1 <- lme(AvgDays ~ HydroComp, random=~1|Block/Tub, 
            method="ML", na.action=na.exclude, data=A.EMERG) # use ML
mod2 <- lme(AvgDays ~ TreatHydro+TreatComp, random=~1|Block/Tub, 
            method="ML", na.action=na.exclude, data=A.EMERG)
anova(mod1, mod2)

mod1 <- lme(AvgDays ~ HydroComp, random=~1|Block/Tub, 
            method="REML", na.action=na.exclude, data=A.EMERG) 

summary(mod1)
#Linear mixed-effects model fit by REML
#Data: A.EMERG 
#     AIC      BIC    logLik
#220.2387 236.0126 -99.11935

#Random effects:
#  Formula: ~1 | Block
#(Intercept)
#StdDev:    1.392677

#Formula: ~1 | Tub %in% Block
#(Intercept) Residual
#StdDev:    4.376938 1.620116

#Fixed effects: AvgDays ~ HydroComp 
#                        Value Std.Error DF   t-value p-value
#(Intercept)          50.05200  2.178161 27 22.979022  0.0000
#HydroCompDrying.C     0.06800  2.951770 27  0.023037  0.9818
#HydroCompDrying.E     7.81400  2.951770 27  2.647225  0.0134 **
#HydroCompPermanent.E  7.67800  2.951770 27  2.601151  0.0149 **
#HydroCompDrying.F     2.35200  2.951770 27  0.796810  0.4325
#HydroCompPermanent.F -0.30485  3.140351 27 -0.097076  0.9234
#HydroCompDrying.G     9.73400  2.951770 27  3.297682  0.0027 **
#HydroCompPermanent.G  2.75800  2.951770 27  0.934355  0.3584

summary(mod2)
#Linear mixed-effects model fit by maximum likelihood
#Data: A.EMERG 
#     AIC      BIC   logLik
#244.7981 258.1066 -114.399

#Random effects:
#  Formula: ~1 | Block
#(Intercept)
#StdDev:    1.116734

#Formula: ~1 | Tub %in% Block
#(Intercept) Residual
#StdDev:    4.165051  1.51142

#Fixed effects: AvgDays ~ TreatHydro + TreatComp 
#                    Value Std.Error DF   t-value p-value
#(Intercept)      48.86306  1.765705 30 27.673397  0.0000
#TreatHydroDrying  2.44587  1.522770 30  1.606199  0.1187
#TreatCompE        7.71200  2.122221 30  3.633929  0.0010 **
#TreatCompF        1.01626  2.184374 30  0.465240  0.6451 
#TreatCompG        6.21200  2.122221 30  2.927123  0.0065 **




#### Survivorship Analyses ####

## Species comparison (overall survivorship)

library(lme4)
mod1a<-glmer(Surv.YN~Species+(1|Block/Tub), data=SURV, family=binomial)
summary(mod1a)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: Surv.YN ~ Species + (1 | Block/Tub)
#Data: SURV

#   AIC      BIC   logLik deviance df.resid 
#2710.5   2738.8  -1350.3   2700.5     2097 

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-1.9230 -0.8220 -0.4742  0.9256  2.5485 

#Random effects:
#Groups    Name        Variance Std.Dev.
#Tub:Block (Intercept) 0.52137  0.7221  
#Block     (Intercept) 0.09165  0.3027  
#Number of obs: 2102, groups:  Tub:Block, 70; Block, 5

#Fixed effects:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.02951    0.18803   0.157 0.875279    
#SpeciesPSRE -0.28618    0.14854  -1.927 0.054029 .  
#SpeciesRACA -0.56855    0.15110  -3.763 0.000168 ***

# Fixed-effect coefficients and confidence intervals, convert from log-odds scale
# so that you can interpret the odds ratios
cc <- exp(confint(mod1a,parm="beta_", method="Wald", level=0.95))  # beta parameters (ie fixed effects only) 
ctaba <- cbind(est=exp(fixef(mod1a)),cc)
ctaba
#                   est     2.5 %    97.5 %
# (Intercept) 1.0299521 0.7124718 1.4889029 # (intercept = ANBO)
# SpeciesPSRE 0.7511250 0.5613998 1.0049678
# SpeciesRACA 0.5663456 0.4211800 0.7615444



## Example code for ANBO - survival measured as proportion of individuals surviving to emergence (metamorphosing)

# Change reference level to control treatment
ASURV <- within(ASURV, HydroComp <- relevel(HydroComp, ref = "Permanent.C"))  

mod1<-glmer(Surv.YN~HydroComp+(1|Block/Tub), data=ASURV, family=binomial)
summary(mod1)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: Surv.YN ~ HydroComp + (1 | Block/Tub)
#Data: ASURV

#  AIC      BIC   logLik deviance df.resid 
#914.7    960.2   -447.4    894.7      690 

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-2.1966 -0.8425  0.4552  0.8596  2.0270 

#Random effects:
#Groups    Name        Variance Std.Dev.
#Tub:Block (Intercept) 0.61998  0.7874  
#Block     (Intercept) 0.05681  0.2384  
#Number of obs: 700, groups:  Tub:Block, 40; Block, 5

#Fixed effects:
#                     Estimate Std. Error z value Pr(>|z|)  
#(Intercept)           0.50744    0.41192   1.232   0.2180  
#HydroCompDrying.C    -0.59042    0.55994  -1.054   0.2917  
#HydroCompDrying.E    -1.24188    0.58810  -2.112   0.0347 *
#HydroCompPermanent.E -0.68759    0.58689  -1.172   0.2414  
#HydroCompDrying.F     0.18417    0.59500   0.310   0.7569  
#HydroCompPermanent.F -0.58725    0.59138  -0.993   0.3207  
#HydroCompDrying.G    -0.59473    0.60748  -0.979   0.3276  
#HydroCompPermanent.G -0.03782    0.61667  -0.061   0.9511  

# check for overdispersion
library(RVAideMemoire)
overdisp.glmer(mod1)
Residual deviance: 820.082 on 690 degrees of freedom (ratio: 1.189)
# The model assumes a theoretical value of 1. Apart from bias in the estimate, 
# the residuals are constrained by the model to have magnitudes that are 
# consistent with this theoretical value
qqnorm(resid(mod1))
hist(resid(mod1))
plot(fitted(mod1),resid(mod1))

cc <- exp(confint(mod1,parm="beta_", method="Wald", level=0.95))  # beta parameters (ie fixed effects only)
ctaba <- cbind(est=exp(fixef(mod1)),cc)
ctaba
#                           est      2.5 %   97.5 %
#(Intercept)          1.6610263 0.74088517 3.723935
#HydroCompDrying.C    0.5540919 0.18490971 1.660366
#HydroCompDrying.E    0.2888418 0.09121544 0.914643
#HydroCompPermanent.E 0.5027843 0.15915565 1.588332
#HydroCompDrying.F    1.2022255 0.37455701 3.858815
#HydroCompPermanent.F 0.5558545 0.17441244 1.771515
#HydroCompDrying.G    0.5517130 0.16773417 1.814700
#HydroCompPermanent.G 0.9628859 0.28751637 3.224683



ASURV <- within(ASURV, TreatHydro <- relevel(TreatHydro, ref = "Permanent"))  

summary(mod2<-glmer(Surv.YN~TreatHydro + TreatComp +(1|Block/Tub), data=ASURV, family=binomial))
#Fixed effects:
#  Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: Surv.YN ~ TreatHydro + TreatComp + (1 | Block/Tub)
#Data: ASURV

#  AIC      BIC   logLik deviance df.resid 
#912.2    944.0   -449.1    898.2      693 

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-2.0443 -0.7977  0.4892  0.8331  2.1511 

#Random effects:
#  Groups    Name        Variance Std.Dev.
#Tub:Block (Intercept) 0.69907  0.8361  
#Block     (Intercept) 0.04617  0.2149  
#Number of obs: 700, groups:  Tub:Block, 40; Block, 5

#Fixed effects:
#                    Estimate Std. Error z value Pr(>|z|)
#(Intercept)          0.09016    0.34697   0.260    0.795
#TreatHydroPermanent  0.24249    0.31760   0.764    0.445
#TreatCompE          -0.67183    0.43331  -1.550    0.121
#TreatCompF           0.09135    0.43625   0.209    0.834
#TreatCompG          -0.02287    0.44980  -0.051    0.959

cc <- exp(confint(mod2,parm="beta_", method="Wald", level=0.95))  # beta parameters (ie fixed effects only)
ctaba <- cbind(est=exp(fixef(mod2)),cc)
ctaba
#                          est     2.5 %   97.5 %
#(Intercept)         1.0943548 0.5543959 2.160212
#TreatHydroPermanent 1.2744159 0.6838652 2.374936
#TreatCompE          0.5107740 0.2184710 1.194164
#TreatCompF          1.0956567 0.4659510 2.576373
#TreatCompG          0.9773867 0.4047621 2.360114


