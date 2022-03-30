# brisk
# This code is modified to be used locally or on a cluster. 

local=FALSE
# Change this for cluster or local:

if(local){
    setwd('~/Dropbox/QualityControlImpactsFMRI')
    save.input.data = TRUE
    getOption("mc.cores")
    options(mc.cores=8)
    seed=1
  } else {
    setwd('~/risk_share/QualityControlImpacts')
    save.input.data = FALSE
    options(mc.cores=1)
    seed = seedID
}
 
a = .libPaths()
.libPaths(c('/home/benjamin.risk/Rlibs',a))

.libPaths()

getOption("mc.cores")
set.seed(seed, "L'Ecuyer-CMRG")

tic = proc.time()
library(drtmle)
library(SuperLearner)
library(earth)
library(quadprog)
library(gam)
library(nloptr)
library(future)
library(future.apply)
library(xgboost)
library(ranger)
library(visdat)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(e1071)
library(glmnet)
library(readxl)
library(ROCit)
# KKI: Lenient
# Ciric: Strict

dat=read.csv('./Data/Master_HeadMotion_wholeGroup_partialCorrelations_ic30_20210803.csv',header=T)

# sort by ID: the simplifies the later indexing, as a merge with 
# propensities sorts by ID:
dat = dat[order(dat$ID),]

table(dat$PrimaryDiagnosis)
table(dat$Race,dat$Sex,useNA='always') # Used in Protection of Human Subjects table in grant

# note: create a "caucasian/other" race variable that allocates the four children without race to caucasian
dat$Race2 = dat$Race
dat$Race2[dat$Race2%in%c('Hispanic','Other','Unknown')]='Caucasian'
table(dat$Race2)


# Add ADHD variable:
dat$ADHD_Secondary = ifelse(dat$ADHD.Subtype=='No dx',0,1)
table(dat$ADHD_Secondary,dat$ADHD.Subtype,useNA='always') # there should be no strange categories. 2/3/2021: Looks good
table(dat$PrimaryDiagnosis[dat$KKI_criteria=='Pass'])
table(dat$PrimaryDiagnosis[dat$Ciric_length=='Pass'])

# subset to signal components:
ic_class = read_excel('./DeconfoundedFMRI/componentLabels_pca85_ica30.xlsx')
#ic_class = read_excel('./componentLabels_pca85_ica30.xlsx')

# create names we want to exclude:
artifacts = c(1:nrow(ic_class))[ic_class$signal==0]
badICs = paste0('ic',artifacts)
allICs = paste0('ic',1:nrow(ic_class))
badEdges=NULL
for (i in 1:nrow(ic_class)) {
  for (j in 1:length(badICs)) {
    badEdges = c(badEdges,paste0(allICs[i],'.',badICs[j]))
  }
}
for (i in 1:length(badICs)) {
  for (j in 1:nrow(ic_class)) {
    badEdges = c(badEdges,paste0(badICs[i],'.',allICs[j]))
  }
}

dat2 = dat[,!names(dat)%in%badEdges]

# Variables containing Fisher z partial correlations 

startEdgeidx = which(names(dat2)=='ic1.ic2')
endEdgeidx = which(names(dat2)=='ic29.ic30')
names(dat2)[startEdgeidx:endEdgeidx] #18*17/2 = nEdges
(nEdges = endEdgeidx - startEdgeidx+1)

# note: check whether all "PASS" have correlations. Should be all FALSE:
table(is.na(dat2[,startEdgeidx]) & dat2$KKI_criteria=='Pass')
# No missing. Cool.

# Motion variables:
# MeanFramewiseDisplacement.KKI
# MaxFramewiseDisplacement.KKI
# FramesWithFDLessThanOrEqualTo250microns
# Here, we control for sex and motion effects. We do not control for age, but 
# age is included in the propensity and outcome models, resulting
# in the age distribution for each group (including fails), which is 
# approximately equal between groups. 

# Create a variable r.ic1.ic2 for each signal pairing that 
# contains the residuals from the three motion variables
temp = dat2[,c('PrimaryDiagnosis','MeanFramewiseDisplacement.KKI','MaxFramewiseDisplacement.KKI',"FramesWithFDLessThanOrEqualTo250microns","Sex","Race2","SES.Family","ic1.ic2")]
completeCases = complete.cases(temp)

#nrow(temp[is.na(temp$ic1.ic2) ,])

t.values.lm=NULL
t.values.naive=NULL

lm.variables=c('MeanFramewiseDisplacement.KKI')
# NOTE: for better interpretation of the plot of mean changes, center all variables:
for (i in c(startEdgeidx:endEdgeidx)) {
  model.temp = lm(dat2[,i]~scale(dat2$MeanFramewiseDisplacement.KKI,center = TRUE, scale=FALSE)+scale(dat2$MaxFramewiseDisplacement.KKI,center=TRUE,scale=FALSE)+scale(dat2$FramesWithFDLessThanOrEqualTo250microns,center=TRUE,scale=FALSE)+dat2$Sex+dat2$Race2+scale(dat2$SES.Family,center=TRUE,scale=FALSE)+dat2$PrimaryDiagnosis)
  # create the motion-adjusted fconn data:
  dat2[completeCases,paste0('r.',names(dat2)[i])]=residuals(model.temp)+coef(model.temp)["(Intercept)"]+coef(model.temp)["dat2$PrimaryDiagnosisNone"]*(dat2$PrimaryDiagnosis[completeCases]=='None')
    
    # Audits:
    # check ordering is same:
    trash.audit = residuals(model.temp)+fitted(model.temp)
    trash = abs(dat2[completeCases,i]-trash.audit)<1e-16
    if(!all(trash)) stop('Ordering mismatch -- check variables in complete cases')     # check that t-statistic on residuals is approximately equal to t-statistic from lm:
    t.values.lm = c(t.values.lm,coef(summary(model.temp))['dat2$PrimaryDiagnosisNone',3])
    t.values.naive =   c(t.values.naive,t.test(dat2[completeCases & dat2$PrimaryDiagnosis=='None',paste0('r.',names(dat2)[i])],dat2[completeCases & dat2$PrimaryDiagnosis=='Autism',paste0('r.',names(dat2)[i])])$statistic)
}
cor(t.values.lm,t.values.naive)
# nearly equivalent.

# Make all ADOS in TD = 0
dat2$ADOS.Comparable.Total[dat2$PrimaryDiagnosis=='None'] = 0

# specify learners for super learner. gn is the propensity model and Qbar is the outcome model,
my.SL.libs.gn= c("SL.earth","SL.glmnet","SL.gam","SL.glm","SL.ranger","SL.step","SL.step.interaction","SL.xgboost","SL.mean")
my.SL.libs.Qbar= c("SL.earth","SL.glmnet","SL.gam","SL.glm","SL.ranger","SL.ridge","SL.step","SL.step.interaction","SL.svm","SL.xgboost","SL.mean")

# Dataset for propensity model:
#gn.variables = c('KKI_criteria','HeadCoil','YearOfScan','PrimaryDiagnosis','ADHD_Secondary','AgeAtScan','Sex','handedness','CurrentlyOnStimulants','PANESS.TotalOverflowNotAccountingForAge','WISC.GAI','DuPaulHome.InattentionRaw','DuPaulHome.HyperactivityRaw','ADOS.Comparable.Total','ADOS.Comparable.StereotypedBehaviorsRestrictedInterests')

# 7 March 2022: use PANESS not iPANESS
gn.variables = c('KKI_criteria','PrimaryDiagnosis','ADHD_Secondary','AgeAtScan','handedness','CurrentlyOnStimulants','PANESS.TotalOverflowNotAccountingForAge','WISC.GAI','DuPaulHome.InattentionRaw','DuPaulHome.HyperactivityRaw','ADOS.Comparable.Total')

# these indices will be used in the outcome model and drtmle as well:
temp.data = dat2[,c(gn.variables)]

# note: include variables from the linear model here to define a consistent set of
# complete predictor cases for the propensity and outcome models
idx.all.cc = complete.cases(temp.data) & complete.cases(dat2[,c('Sex','SES.Family','Race2')])
temp.data = temp.data[idx.all.cc,]
#temp.data$AgeAtScanXdx = (temp.data$PrimaryDiagnosis=='Autism')*temp.data$AgeAtScan
#AgeAtScan
gn.xmat = data.frame(model.matrix(KKI_criteria~.,data=temp.data)[,-1])

Delta.KKI = ifelse(temp.data$KKI_criteria=='Pass',1,0)
#corrplot::corrplot(cor(gn.xmat),method='number')

# Create a variable equal to one if the observation is used:
# NOTE: due to missing observations in variables in the initial linear
# model, need to include linear model variables in this: 
dat2$CompletePredictorCases = idx.all.cc
sum(dat2$CompletePredictorCases)


# fit with glm and gam: eventually, compare AUCs
glm.prop.model = glm(Delta.KKI~as.matrix(gn.xmat),family=binomial)
propensities.glm = predict(glm.prop.model,type = 'response')
gam.prop.model = mgcv::gam(Delta.KKI~PrimaryDiagnosisNone+ADHD_Secondary+handednessMixed+handednessRight+CurrentlyOnStimulants+s(AgeAtScan)+s(DuPaulHome.InattentionRaw)+s(DuPaulHome.HyperactivityRaw)+s(WISC.GAI)+s(PANESS.TotalOverflowNotAccountingForAge)+s(ADOS.Comparable.Total),method='REML',family=binomial,data=gn.xmat)
propensities.gam=predict(gam.prop.model,type='response')


(propensity.KKI = mcSuperLearner(Y = Delta.KKI, X = gn.xmat, family=binomial(link='logit'),SL.library = my.SL.libs.gn, cvControl = list(V = 10), method='method.CC_nloglik')) # 10-fold CV

# check stability of propensities:
min(propensity.KKI$SL.predict[Delta.KKI==1])

# examine if there is a positive probability of usability among all data (positivity):
min(propensity.KKI$SL.predict)

propensities.SL = propensity.KKI$SL.predict
summary(rocit(score=propensities.glm,class=Delta.KKI,method='nonparametric'))
summary(rocit(score=propensities.gam,class=Delta.KKI,method='nonparametric'))
summary(rocit(score=propensities.SL,class=Delta.KKI,method='nonparametric'))
# For most seeds, much better fit with SuperLearner

# merge propensities back to dataset:
prop.asdtd = data.frame('ID'=dat2$ID[idx.all.cc],'propensities.glm'=propensities.glm,'propensities.gam'=propensities.gam,'propensities.SL'=propensities.SL,Delta.KKI)
dat3 = merge(dat2,prop.asdtd,all = TRUE) # here, we keep all observations

# this merge changes the ordering of ID. Hence, new index vectors are created,
# and new xmat need to be made even when using the same variables in the 
# propensity and outcome models

# save datasets to be loaded for DRTMLE:
# NOTE: These datasets include the propensities, which change with each seed:
if (save.input.data) {
  save(file=paste0('./Data/DataWithPropensities_seed',seed,'.RData'),dat3)
}

# check that the merge is correct:
table(dat3$KKI_criteria,dat3$Delta.KKI)
# these should be all in agreement (0 on off diagonal)
all(idx.all.cc==!is.na(dat3$propensities.SL))

  # Delta should be correlated with propensities: 
  cor(1*(dat3$KKI_criteria=='Pass'),dat3$propensities.SL,use='pairwise.complete.obs')

### Create outcome regression datasets:
# NOTE: Currently using same variables for propensity and outcome model. 
Qn.variables =  gn.variables
#NOTE: KKI_criteria is not used in prediction, but is included as a trick to construct the design matrix

# complete cases pass defined by pass, no missing propensities (behavioral variables), and no missing fconn (driven by the initial linear model with imbalanced variables)
idx.pass.cc = dat3$KKI_criteria=='Pass' & !is.na(dat3$propensities.SL)
# use in naive estimates in for loop:
idx.pass.cc.asd = idx.pass.cc & dat3$PrimaryDiagnosis=='Autism'
idx.pass.cc.td = idx.pass.cc & dat3$PrimaryDiagnosis=='None'

# used in drtmle:
idx.all.cc.asd = idx.all.cc & dat3$PrimaryDiagnosis == 'Autism'
idx.all.cc.td = idx.all.cc & dat3$PrimaryDiagnosis == 'None'

#save(file=paste0('~/Dropbox/QualityControlImpactsFMRI/Data/PropensitiesXmats_seed',seed,'.RData'),Qn.variables,dat3,idx.all.cc,idx.pass.cc,edgeList,idx.all.cc.asd,idx.all.cc.td)

# These datasets necessary to run SuperLearner without error
# (note: ordering of indices differs from the propensity models due to the merge with the propensities)
temp.data = dat3[idx.pass.cc,Qn.variables]
Qn.xmat.fit = data.frame(model.matrix(KKI_criteria~.,data=temp.data))[,-1]

temp.data = dat3[idx.all.cc,c('ID',Qn.variables)]
Qn.xmat.predict = data.frame(model.matrix(KKI_criteria~.,data=temp.data))[,-1]

# Separate ASD and TD datasets are necessary to obtain the DRTMLE estimates:
temp.data = Qn.xmat.predict[Qn.xmat.predict$PrimaryDiagnosisNone==0,]
# check that the order matches idx.all.cc.asd:
all(temp.data$ID==dat3$ID[idx.all.cc.asd])
Qn.xmat.predict.asd = data.frame(model.matrix(numeric(nrow(temp.data))~.,data=temp.data)[,-c(1,2)])

temp.data = Qn.xmat.predict[Qn.xmat.predict$PrimaryDiagnosisNone==1,]
# check that the order matches idx.all.cc.td:
all(temp.data$ID==dat3$ID[idx.all.cc.td])
Qn.xmat.predict.td = data.frame(model.matrix(numeric(nrow(temp.data))~.,data=temp.data))[,-c(1,2)]


# define startEdgeIdx using the residuals:
startEdgeidx=which(names(dat3)=='r.ic1.ic2')

results.df = data.frame('EdgeID'=numeric(nEdges),'EdgeName'=numeric(nEdges),'mean.ASD.naive'=numeric(nEdges),'mean.TD.naive'=numeric(nEdges),'mean.diff.naive'=numeric(nEdges),'z.stat.ASD.naive'=numeric(nEdges),'z.stat.TD.naive'=numeric(nEdges),'z.stat.diff.naive'=numeric(nEdges),'mean.ASD.SL'=numeric(nEdges),'mean.TD.SL'=numeric(nEdges),'mean.diff.SL'=numeric(nEdges),'z.stat.ASD.SL'=numeric(nEdges),'z.stat.TD.SL'=numeric(nEdges),'z.stat.diff.SL'=numeric(nEdges))

###########################################
###########################
# OUTCOME MODEL and DRTMLE Estimates:
for (edgeidx in 1:nEdges) {
  results.df[edgeidx,'EdgeID'] = edgeidx
  dat3.edgeidx = startEdgeidx+edgeidx-1 
  results.df[edgeidx,'EdgeName'] = names(dat3)[dat3.edgeidx]

  # naive estimates:
  results.df[edgeidx,'mean.ASD.naive'] = mean(dat3[idx.pass.cc.asd,dat3.edgeidx])
  results.df[edgeidx,'mean.TD.naive'] = mean(dat3[idx.pass.cc.td,dat3.edgeidx])
  results.df[edgeidx,'mean.diff.naive'] =  mean(dat3[idx.pass.cc.asd,dat3.edgeidx])-mean(dat3[idx.pass.cc.td,dat3.edgeidx])
  results.df[edgeidx,'z.stat.ASD.naive'] = t.test(dat3[idx.pass.cc.asd,dat3.edgeidx])$statistic[[1]]
  results.df[edgeidx,'z.stat.TD.naive'] = t.test(dat3[idx.pass.cc.td,dat3.edgeidx])$statistic[[1]]
  results.df[edgeidx,'z.stat.diff.naive'] = t.test(dat3[idx.pass.cc.asd,dat3.edgeidx],dat3[idx.pass.cc.td,dat3.edgeidx])$statistic[[1]]
  
  outcome.SL = mcSuperLearner(Y = dat3[idx.pass.cc,dat3.edgeidx],X=Qn.xmat.fit,family=gaussian(), SL.library = my.SL.libs.Qbar,cvControl = list(V = 10), method = drtmle:::tmp_method.CC_LS)
      
  Qbar.SL.asd = predict(outcome.SL, newdata = Qn.xmat.predict.asd)[[1]]
  Qbar.SL.td = predict(outcome.SL, newdata = Qn.xmat.predict.td)[[1]]
      # rank deficiency in glm.interaction
      
  mean_fconn_asd.SL <- drtmle(Y = dat3[idx.all.cc.asd,dat3.edgeidx],
                              A = dat3[idx.all.cc.asd,c('Delta.KKI')],
                              W = NULL, # Does not do anything with user-input Qn and gn
                              a_0 = 1, # predict for counterfactual that data are usable
                              Qn = list(Qbar.SL.asd), # pass in fitted values
                              gn = list(dat3$propensities.SL[idx.all.cc.asd]), # pass in fitted values
                              SL_Qr = "SL.npreg",
                              SL_gr = "SL.npreg",
                              maxIter = 1 # between 1 and 3 is probably fine
                              )
  
  mean_fconn_td.SL <- drtmle(Y = dat3[idx.all.cc.td,dat3.edgeidx],
                             A = dat3[idx.all.cc.td,c('Delta.KKI')], 
                             W = NULL, 
                             a_0 = 1, 
                             Qn = list(Qbar.SL.td), 
                             gn = list(dat3$propensities.SL[idx.all.cc.td]), 
                             SL_Qr = "SL.npreg", 
                             SL_gr = "SL.npreg", 
                             maxIter = 1)
      
      results.df[edgeidx,'mean.ASD.SL'] = mean_fconn_asd.SL$drtmle$est
      results.df[edgeidx,'mean.TD.SL'] = mean_fconn_td.SL$drtmle$est
      results.df[edgeidx,'mean.diff.SL'] = mean_fconn_asd.SL$drtmle$est-mean_fconn_td.SL$drtmle$est
      
      results.df[edgeidx,'z.stat.ASD.SL'] = mean_fconn_asd.SL$drtmle$est/sqrt(mean_fconn_asd.SL$drtmle$cov)
      results.df[edgeidx,'z.stat.TD.SL'] = mean_fconn_td.SL$drtmle$est/sqrt(mean_fconn_td.SL$drtmle$cov)
      results.df[edgeidx,'z.stat.diff.SL'] = (mean_fconn_asd.SL$drtmle$est - mean_fconn_td.SL$drtmle$est)/sqrt(mean_fconn_asd.SL$drtmle$cov + mean_fconn_td.SL$drtmle$cov)
      message(paste0('Seed ',seed,': Finished Edge',edgeidx))
}

results.df$seed = seed
save(file=paste0('./Results_noimpute/ic30_pc85_glm_gam_drtmle_seed',seed,'.RData'),results.df)

proc.time()-tic

