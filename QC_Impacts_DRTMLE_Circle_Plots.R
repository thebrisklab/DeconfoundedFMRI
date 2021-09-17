# BenjaminRisk
# adapted from d lidstone

library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(gridExtra)
library(arsenal)
library(ggmosaic)
library(lemon)
library(ggpubr)
library(grid)
library(ggThemeAssist)
library(visdat)
library(readxl)
library(circlize)
library(png)



#########################
###################
results.all=NULL
for (i in 1:200) {
  load(paste0('../Results/Results_10September2021/ic30_pc85_glm_gam_drtmle_seed',i,'.RData'))
  results.all = rbind(results.all,results.df)
}
rm(results.df)

# second set of seeds:
results.all2=NULL
for (i in 201:400) {
  load(paste0('../Results/Results_10September2021/ic30_pc85_glm_gam_drtmle_seed',i,'.RData'))
  results.all2 = rbind(results.all2,results.df)
}
rm(results.df)

p=ggplot(data=results.all[order(results.all$z.stat.diff.naive),], aes(x=z.stat.diff.naive, y=z.stat.diff.SL, color=EdgeName))+ geom_point() + theme(legend.position='none')+geom_abline(slope=1,intercept=0)+scale_color_manual(values = colorRampPalette(brewer.pal(n=8,name='Accent'))(153))

#+scale_color_manual(values = colorRampPalette(brewer.pal(n=12,name='Set3'))(153))

#library(wesanderson)
#+scale_color_manual(values = colorRampPalette(wes_palette(n=30,name='Zissou1', type='continuous'))(153))

pdf(file='~/Dropbox/QualityControlImpactsFMRI/Results/Stability_Assess.pdf')
p
dev.off()

# Edges of interest from Lombardo:
# edgeList = c('r.ic2.ic17','r.ic8.ic17','r.ic17.ic19')
# results.all[results.all$EdgeName%in%edgeList,]
# three key edges are not significant


# average the z-statistics:
results.ave = results.all%>%group_by(EdgeName)%>%summarize(mean.ASD.naive=mean(mean.ASD.naive), mean.TD.naive=mean(mean.TD.naive),mean.diff.naive=mean(mean.diff.naive),z.stat.ASD.naive=mean(z.stat.ASD.naive),z.stat.TD.naive=mean(z.stat.TD.naive),z.stat.diff.naive=mean(z.stat.diff.naive),mean.ASD.SL=mean(mean.ASD.SL),mean.TD.SL=mean(mean.TD.SL),mean.diff.SL=mean(mean.diff.SL),z.stat.ASD.SL=mean(z.stat.ASD.SL),z.stat.TD.SL=mean(z.stat.TD.SL),z.stat.diff.SL=mean(z.stat.diff.SL))

results.ave2 = results.all2%>%group_by(EdgeName)%>%summarize(mean.ASD.naive=mean(mean.ASD.naive), mean.TD.naive=mean(mean.TD.naive),mean.diff.naive=mean(mean.diff.naive),z.stat.ASD.naive=mean(z.stat.ASD.naive),z.stat.TD.naive=mean(z.stat.TD.naive),z.stat.diff.naive=mean(z.stat.diff.naive),mean.ASD.SL=mean(mean.ASD.SL),mean.TD.SL=mean(mean.TD.SL),mean.diff.SL=mean(mean.diff.SL),z.stat.ASD.SL=mean(z.stat.ASD.SL),z.stat.TD.SL=mean(z.stat.TD.SL),z.stat.diff.SL=mean(z.stat.diff.SL))

# compare two estimates:
plot(results.ave$z.stat.diff.SL~results.ave2$z.stat.diff.SL)
head(cbind(results.ave$z.stat.diff.SL,results.ave2$z.stat.diff.SL))
cor(results.ave$z.stat.diff.SL,results.ave2$z.stat.diff.SL)

### Compare the selected edges at FDR=0.20 and 0.05 in the two sets of seeds:
results.ave$p.SL = 2*(1-pnorm(abs(results.ave$z.stat.diff.SL)))
results.ave$p.SL.fdr = p.adjust(results.ave$p.SL,method='BH')

results.ave2$p.SL = 2*(1-pnorm(abs(results.ave2$z.stat.diff.SL)))
results.ave2$p.SL.fdr = p.adjust(results.ave2$p.SL,method='BH')

# These results appear in the manuscript:

sum(results.ave$p.SL.fdr<0.05)
sum(results.ave$p.SL.fdr<0.20)

sum(results.ave2$p.SL.fdr<0.05)
sum(results.ave2$p.SL.fdr<0.20)

results.ave[results.ave$p.SL.fdr<0.2,c('EdgeName','z.stat.diff.naive','p.naive.fdr','z.stat.diff.SL','p.SL.fdr')]
results.ave2[results.ave2$p.SL.fdr<0.2,c('EdgeName','z.stat.diff.SL','p.SL.fdr')]

results.ave[results.ave$p.SL.fdr<0.05,c('EdgeName')]
results.ave2[results.ave2$p.SL.fdr<0.05,c('EdgeName')]

###########################
# Create results.ave from all seeds:
results.ave = rbind(results.all,results.all2)%>%group_by(EdgeName)%>%summarize(mean.ASD.naive=mean(mean.ASD.naive), mean.TD.naive=mean(mean.TD.naive),mean.diff.naive=mean(mean.diff.naive),z.stat.ASD.naive=mean(z.stat.ASD.naive),z.stat.TD.naive=mean(z.stat.TD.naive),z.stat.diff.naive=mean(z.stat.diff.naive),mean.ASD.SL=mean(mean.ASD.SL),mean.TD.SL=mean(mean.TD.SL),mean.diff.SL=mean(mean.diff.SL),z.stat.ASD.SL=mean(z.stat.ASD.SL),z.stat.TD.SL=mean(z.stat.TD.SL),z.stat.diff.SL=mean(z.stat.diff.SL))

results.ave$p.naive = 2*(1-pnorm(abs(results.ave$z.stat.diff.naive)))
results.ave$p.naive.fdr = p.adjust(results.ave$p.naive,method='BH')
results.ave$p.SL = 2*(1-pnorm(abs(results.ave$z.stat.diff.SL)))
results.ave$p.SL.fdr = p.adjust(results.ave$p.SL,method='BH')

sum(results.ave$p.SL.fdr<0.20)
sum(results.ave$p.SL.fdr<0.05)


# Changes in ASD mean:
temp = results.ave[,c(1,2,8)]
temp$Change=temp$mean.ASD.SL-temp$mean.ASD.naive
results.ASD = temp%>%pivot_longer(cols=c(2,3), names_to = 'Method',values_to='mean.ASD')

p0 = results.ASD%>%ggplot(aes(x=Method, y=mean.ASD, group=EdgeName,color=Change))+geom_point()+geom_line()+ylim(c(-0.2,0.25))+ylab('Mean FC')+scale_x_discrete(labels=c("mean.ASD.naive" = "Naive", "mean.ASD.SL" = "DRTMLE"))+ggtitle('A) ASD')+scale_color_gradient2(limits=c(-0.005,0.005),oob=scales::squish)

# Changed in TD mean: 
temp = results.ave[,c(1,3,9)]
temp$Change=temp$mean.TD.SL-temp$mean.TD.naive

results.TD = temp%>%pivot_longer(cols=c(2,3), names_to = 'Method',values_to='mean.TD')
p1=results.TD%>%ggplot(aes(x=Method, y=mean.TD, group=EdgeName,color=Change))+geom_line()+geom_point()+ylim(c(-0.2,0.25))+ggtitle('B) TD')+scale_x_discrete(labels=c("mean.TD.naive" = "Naive", "mean.TD.SL" = "DRTMLE"))+ylab('Mean FC')+scale_color_gradient2(limits=c(-0.005,0.005))

# Difference in mean between ASD and TD: 
results.ave$diff.ASD.TD.naive = results.ave$mean.ASD.naive - results.ave$mean.TD.naive
results.ave$diff.ASD.TD.SL = results.ave$mean.ASD.SL - results.ave$mean.TD.SL

temp = results.ave[,c(1,18,19)]
temp$Change=temp$diff.ASD.TD.SL-temp$diff.ASD.TD.naive
results.diff = temp%>%pivot_longer(cols=c(2,3), names_to = 'Method',values_to='difference')

p2=results.diff%>%ggplot(aes(x=Method, y=difference, group=EdgeName, color=Change))+geom_line()+geom_point()+ylim(c(-0.015,0.015))+ggtitle('C) ASD-TD')+scale_x_discrete(labels=c("diff.ASD.TD.naive" = "Naive", "diff.ASD.TD.SL" = "DRTMLE"))+ylab('Mean ASD - TD')+scale_color_gradient2()

pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/DeconfoundedGroupMeans.pdf',width=8,height=5)
grid.arrange(p0,p1,p2,ncol=3)
dev.off()

par(mfrow=c(1,2))
p0=ggplot(results.ave,aes(x=p.naive))+geom_histogram(binwidth=0.05)+ggtitle('A) P-values from naive')+ylim(0,20)+xlab('Naive')
p1=ggplot(results.ave,aes(x=p.SL))+geom_histogram(binwidth=0.05)+ggtitle('B) P-values from DRTMLE')+ylim(0,20)+xlab('DRTMLE')

pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/Pvalues_ASD_vs_TD.pdf',width=6,height=3)
grid.arrange(p0,p1,ncol=2)
dev.off()


##########################
####################
# Create circle plots
####################
##########################
source('FunctionsCirclePlots.R')
dev.off()

pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/Naive_zstat_groupdifference_fdr20.pdf')
myChordDiagram(data=results.ave,varname='z.stat.diff.naive',alpha=0.20,alphaname='p.naive.fdr',title='A) Naive Z-Statistic, ASD-TD')
dev.off()

pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/DRTMLE_zstat_groupdifference_fdr20.pdf')
myChordDiagram(data=results.ave,varname='z.stat.diff.SL',alpha=0.20,alphaname='p.SL.fdr',title='B) DRTMLE Z-Statistic, ASD-TD')
dev.off()

results.ave[results.ave$p.SL.fdr<0.20,c("EdgeName","z.stat.diff.SL")]
# 09/17/2021: checked plot looks good

pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/Naive_zstat_groupdifference_fdr05.pdf')
myChordDiagram(data=results.ave,varname='z.stat.diff.SL',alphaname = 'p.naive.fdr',alpha=0.05,title='B) DRTMLE Z-Statistic, ASD-TD')
dev.off()

pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/DRTMLE_zstat_groupdifference_fdr05.pdf')
myChordDiagram(data=results.ave,varname='z.stat.diff.SL',alphaname = 'p.SL.fdr',alpha=0.05,title='B) DRTMLE Z-Statistic, ASD-TD')
dev.off()



##################
#################
# check the change in pvalues:
results.ave[(abs(results.ave$z.stat.diff.naive)>1.96) | (abs(results.ave$z.stat.ASD.SL)>1.96),c('EdgeName','z.stat.diff.naive','z.stat.diff.SL')]

edgeList = c('r.ic2.ic17','r.ic8.ic17','r.ic17.ic19')
results.ave[results.ave$EdgeName%in%edgeList,c('EdgeName','z.stat.diff.naive','z.stat.diff.SL')]
# three key edges are not significant

########################
# Approximate equality between lm approach and naive:
t.values.lm=NULL
t.values.naive=NULL

for (i in c(startEdgeidx:endEdgeidx)) {
  model.temp = lm(dat2[,i]~dat2$MeanFramewiseDisplacement.KKI+dat2$MaxFramewiseDisplacement.KKI+dat2$FramesWithFDLessThanOrEqualTo250microns+dat2$Sex+dat2$Race2+dat2$SES.Family+dat2$PrimaryDiagnosis)
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




################
#####
# OLD UNCORRECTED:
pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/Naive_zstat_groupdifference.pdf')
myChordDiagram(data=results.ave,varname='z.stat.diff.naive',alpha=0.05,alphaname='p.naive',title='A) Naive Z-Statistic, ASD-TD')
dev.off()
# check the correct edges are being plotted:
results.ave[abs(results.ave$z.stat.diff.naive)>1.96,c('EdgeName','z.stat.diff.naive')]


# NAIVE RESULTS: 
sum((results.ave$z.stat.diff.naive)>1.96)
sum((results.ave$z.stat.diff.naive)< -1.96)

# DRTMLE:
sum((results.ave$z.stat.diff.SL)>1.96)
sum((results.ave$z.stat.diff.SL)< -1.96)
