# prepare explainers

# require packages
require(data.table); require(ggplot2); require(patchwork)
require(DALEX);require(ingredients); require(auditor); require(MASS)
library(nlme); library(MuMIn)

# clean all
rm(list=ls())

# read in the database
d2 <- readRDS('products/250804 cropyield database.rds')

# source all plot functions
source('../06 papers/paper 6/yield_paper/Code_yield/cropyield_plotfunctions.R')

# scale all numeric X variables to unit variance (needed for plotting ALE)
cols.num <- c('mat','map','ph','soc','tn','bd','clay','n_dose','p_dose','k_dose',
              'sand','cec','pet_mean')
d2[,c(cols.num) := lapply(.SD,scale),.SDcols = cols.num]

# use the simple model (copy this from the final model developed in cropyield_meta_regression.R)
m1 <-lme(smd.yi ~ man + crop_type +
           map + I(map^2)+ mat+
           n_dose + I(n_dose^2) + p_dose + k_dose +
           ph : clay+
           cmp_croprotation +cmp_rfp + cmp_rft + cmp_rfr-1,
         random = ~ 1 |study_id/duration, weights = varFunc(~ smd.vi),
         data=d2,na.action=na.omit)

# get stats for the model
anova(m1); r.squaredGLMM(m1);AIC(m1)

# prepare explainers

  # parameters meta-regression model
  cols.lm <- c('man','crop_type','n_dose','p_dose','k_dose','mat','map',
               'ph','soc','cec','tn','bd','clay',
               'cmp_biochar','cmp_ferttype','cmp_cropresidue','cmp_tillage','cmp_covercrop',
                'cmp_croprotation','cmp_rfp','cmp_rft','cmp_rfr','GEnZ','duration','study_id'
               )

  # make the model explainer for the training dataset
  explainer.yield <- explain(model = m1,
                             data = as.data.frame(d2[, .SD, .SDcols = c(cols.lm)]),
                             y = d2$smd.yi,
                             label = "")

  # make 1-to-1 plot (observed vs predicted) and a histograms of residuals
  yield.res <- auditor::model_residual(explainer.yield)
  plot.res <- ggplot_hist(yield.res) + ggtitle('Residual meta-regression model')
  plot.mp <- ggplot_onetoone(yield.res) + ggtitle("Residual plot")
  plot.resmp <- plot.res | plot.mp
  ggsave(plot = plot.resmp, filename = 'products/yield_residuals.png',width = 30, height = 15, units='cm')

  # make a Variance Importance Plot (VIP) on training sets
  imp.yield <- ingredients::feature_importance(explainer.yield,
                                               loss_function = loss_root_mean_square,
                                               type = "difference")
  # make the VIP plot
  p1 <- ggplot_vip(imp.yield,vexclude=c('study_id','duration','cmp_biochar'))
  ggsave(plot=p1,filename ='products/yield_vip.png',width = 15, height = 18,units='cm')

  # make ALE for numeric site properties (only four selected here)
  # note that sometimes the accumulated_dependency gives an error.
  # i therefore start with setting a seed. if the error occurs, just redo the line.
  set.seed(152)
  ale.soc <- ingredients::accumulated_dependency(explainer.yield, 'soc')
  set.seed(152)
  ale.ph <- ingredients::accumulated_dependency(explainer.yield, 'ph')
  set.seed(152)
  ale.tn <- ingredients::accumulated_dependency(explainer.yield, 'tn')
  set.seed(152)
  ale.prec <- ingredients::accumulated_dependency(explainer.yield, 'map')

  p1 <- ggplot_ale(ale.soc,ale.ph,ale.tn,ale.prec,pncol = 1 ,tsc=0.5)
  ggsave(plot=p1,filename ='products/yield_ale_num.png',width = 18, height = 18,units='cm')

  # make ALE for categorial site properties
  # here as an example for variable 'man'. You can add more variables (see e.g. for numeric ones) and plot them together.
  set.seed(152)
  ale.man <- ingredients::accumulated_dependency(explainer.yield, 'man')
  p1 <- ggplot_ale(ale.man,pncol = 1 ,tsc=0.5)
  ggsave(plot=p1,filename ='products/yield_ale_cat.png',width = 18, height = 14,units='cm')


