# development of meta-regression model
# Luncheng You and Gerard Ros (august 2025)

# Load libraries
library(data.table); library(metafor);library(metagear)
library(nlme); library(MuMIn)

# clear environment
rm(list=ls())

# read in the database
d1 <- readRDS('products/250804 cropyield database.rds')

# model development

# make a full model to predict the yield change as function of treatment, site conditions and management
# this is to see which properties do have a significant effect on SMD
m1 <-lme(smd.yi ~ man +
              crop_type + n_dose + p_dose + k_dose +
              mat + map + ph + soc + tn + bd + clay +
              cmp_biochar + cmp_ferttype + cmp_cropresidue + cmp_tillage + cmp_covercrop +
              cmp_croprotation + cmp_rfp + cmp_rft + cmp_rfr +
              GEnZ-1,
         random = ~ 1|study_id/duration, weights = varFunc(~ smd.vi),
         data=d2,na.action=na.omit)

# get stats for the model
anova(m1); r.squaredGLMM(m1)

# build the final model with various properties that are either relevant or statistically relevant
m1 <-lme(yi ~ man + crop_type +
              map + I(map^2)+ mat+
              n_dose + I(n_dose^2) + p_dose + k_dose +
              ph : clay+
              cmp_croprotation +cmp_rfp + cmp_rft + cmp_rfr-1,
         random = ~ 1 |study_id/duration, weights = varFunc(~ vi),
         data=d2,na.action=na.omit)

# get stats for the final model
anova(m1);r.squaredGLMM(m1)

