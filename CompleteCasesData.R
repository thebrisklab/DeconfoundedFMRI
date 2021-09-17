load("./Data/DataWithPropensities_seed1.RData")

names(dat3)      
# see variable CompletePredictorCases

table(dat3$PrimaryDiagnosis)

# appears in the manuscript:
table(dat3$PrimaryDiagnosis[dat3$CompletePredictorCases==1])

# appears in the manuscript:
table(dat3$PrimaryDiagnosis[dat3$CompletePredictorCases==1 & dat3$KKI_criteria=='Pass'])



hist(dat3$SRS.Score,main='')

# SRS on the subset of participants with complete predictor cases:
hist(dat3$SRS.Score[dat3$CompletePredictorCases==1],main='')

sum(dat3$CompletePredictorCases)
table(!is.na(dat3$SRS.Score[dat3$CompletePredictorCases==1]))


## NOTE: There are two panenss variables. One includes the imputed values.
# this is the imputed:
hist(dat3$iPANESS.TotalOverflowNotAccountingForAge[dat3$CompletePredictorCases==1])

# the imputed version CHANGES with each seed. Thus I suggest using the
# original PANESS.TotalOverflowNotAccountingForAge
# (note: the indicator variable completePredictorCases includes the 19 children with imputed PANESS)
table(dat3$CompletePredictorCases,is.na(dat3$PANESS.TotalOverflowNotAccountingForAge))



## More debugging: Check the mean ASD and mean TD at a few edges:
# In new approach, the variability across edges of the mean ASD is greatly reduced,
# whereas the TD look more variable. This is strange. The z-statistics seem fine. 
mean(dat3$r.ic1.ic13[dat3$CompletePredictorCases & dat3$KKI_criteria=='Pass' & dat3$PrimaryDiagnosis=='Autism'])

mean(dat3$r.ic1.ic13[dat3$CompletePredictorCases & dat3$KKI_criteria=='Pass' & dat3$PrimaryDiagnosis=='None'])

load('~/Dropbox/QualityControlImpactsFMRI/Results/Results_10September2021/ic30_pc85_glm_gam_drtmle_seed1.RData')
results.df.cb = results.df[results.df$EdgeName=='r.ic1.ic13',]

results.df.cb$z.stat.diff.naive


load('~/Dropbox/QualityControlImpactsFMRI/Results/Results_10September2021/ic30_pc85_glm_gam_drtmle_seed1.RData')
