library(ggplot2)
library(wesanderson)
library(tidyverse)
library(effsize)
library(gridExtra)

# stability plots, plots of the data with the naive and deconfounded mean, 
# plots of the missingness

# Examine the stability of z-statistics under different seeds:
results.all=NULL
for (i in 1:200) {
  load(paste0('../Results/Results_20November2021/ic30_pc85_glm_gam_drtmle_seed',i,'.RData'))
  results.all = rbind(results.all,results.df)
}
rm(results.df)

# second set of seeds:
results.all2=NULL
for (i in 201:400) {
  load(paste0('../Results/Results_20November2021/ic30_pc85_glm_gam_drtmle_seed',i,'.RData'))
  results.all2 = rbind(results.all2,results.df)
}
rm(results.df)

p=ggplot(data=results.all[order(results.all$z.stat.diff.naive),], aes(x=z.stat.diff.naive, y=z.stat.diff.SL, color=EdgeName))+ geom_point() + theme(legend.position='none')+geom_abline(slope=1,intercept=0)+scale_color_manual(values = colorRampPalette(brewer.pal(n=8,name='Accent'))(153))

#+scale_color_manual(values = colorRampPalette(brewer.pal(n=12,name='Set3'))(153))

#library(wesanderson)
#+scale_color_manual(values = colorRampPalette(wes_palette(n=30,name='Zissou1', type='continuous'))(153))

#pdf(file='~/Dropbox/QualityControlImpactsFMRI/Results/Stability_Assess.pdf')
p
#dev.off()

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

# this result appears in the manuscript:
cor(results.ave$z.stat.diff.SL,results.ave2$z.stat.diff.SL)

### Compare the selected edges at FDR=0.20 and 0.05 in the two sets of seeds:
results.ave$p.SL = 2*(1-pnorm(abs(results.ave$z.stat.diff.SL)))
results.ave$p.SL.fdr = p.adjust(results.ave$p.SL,method='BH')

results.ave2$p.SL = 2*(1-pnorm(abs(results.ave2$z.stat.diff.SL)))
results.ave2$p.SL.fdr = p.adjust(results.ave2$p.SL,method='BH')

# These results appear in the manuscript:

sum(results.ave$p.SL.fdr<0.05)
sum(results.ave2$p.SL.fdr<0.05)
results.ave[results.ave$p.SL.fdr<0.05,c('EdgeName')]
results.ave2[results.ave2$p.SL.fdr<0.05,c('EdgeName')]
# same two edges at fdr=0.05

sum(results.ave$p.SL.fdr<0.20)
sum(results.ave2$p.SL.fdr<0.20)
results.ave[results.ave$p.SL.fdr<0.2,c('EdgeName','z.stat.diff.SL','p.SL.fdr')]
results.ave2[results.ave2$p.SL.fdr<0.2,c('EdgeName','z.stat.diff.SL','p.SL.fdr')]
# 11 edges in first set, 9 in second

###########################
###########################
# Create results.ave from all seeds:
results.ave = rbind(results.all,results.all2)%>%group_by(EdgeID,EdgeName)%>%summarize(mean.ASD.naive=mean(mean.ASD.naive), mean.TD.naive=mean(mean.TD.naive),mean.diff.naive=mean(mean.diff.naive),z.stat.ASD.naive=mean(z.stat.ASD.naive),z.stat.TD.naive=mean(z.stat.TD.naive),z.stat.diff.naive=mean(z.stat.diff.naive),mean.ASD.SL=mean(mean.ASD.SL),mean.TD.SL=mean(mean.TD.SL),mean.diff.SL=mean(mean.diff.SL),z.stat.ASD.SL=mean(z.stat.ASD.SL),z.stat.TD.SL=mean(z.stat.TD.SL),z.stat.diff.SL=mean(z.stat.diff.SL))

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

#pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/DeconfoundedGroupMeans.pdf',width=8,height=5)
grid.arrange(p0,p1,p2,ncol=3)
#dev.off()


par(mfrow=c(1,2))
p0=ggplot(results.ave,aes(x=p.naive))+geom_histogram()+ggtitle('A) P-values from naive')+ylim(0,20)+xlab('Naive')
p1=ggplot(results.ave,aes(x=p.SL))+geom_histogram()+ggtitle('B) P-values from DRTMLE')+ylim(0,20)+xlab('DRTMLE')

#pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/Pvalues_ASD_vs_TD.pdf',width=6,height=3)
grid.arrange(p0,p1,ncol=2)
#dev.off()


# Calculate Cohen's D in naive estimates

# Create plots to visualize the effect of drtmle on the deconfounded mean
load('~/Dropbox/QualityControlImpactsFMRI/Data/DataWithPropensities_seed1.RData')

nEdges=153
idx.pass.cc = dat3$KKI_criteria=='Pass' & !is.na(dat3$propensities.SL) & !is.na(dat3$r.ic1.ic2)
idx.pass.cc.asd = idx.pass.cc & dat3$PrimaryDiagnosis=='Autism'
idx.pass.cc.td = idx.pass.cc & dat3$PrimaryDiagnosis=='None'

# check cohen's d calculation:
n1 = sum(idx.pass.cc.asd)
n2 = sum(idx.pass.cc.td)
(mean(dat3[idx.pass.cc.asd,'r.ic2.ic27'])-mean(dat3[idx.pass.cc.td,'r.ic2.ic27']))/sqrt(((n1-1)*var(dat3[idx.pass.cc.asd,'r.ic2.ic27'])+(n2-1)*var(dat3[idx.pass.cc.td,'r.ic2.ic27']))/(n1+n2-2))
# they match

startEdgeidx=which(names(dat3)=='r.ic1.ic2')

cohen.d.df = data.frame('EdgeID'=numeric(nEdges),'EdgeName'=numeric(nEdges),'cohensd'=numeric(nEdges),'adj.cohensd'=numeric(nEdges))

# NOTE: Adjusted cohen's d is a crude adjustment to cohen's d based on
# the new means estimated from drtmle. It does not update the variance estimates,
# which is a novel problem. The adjusted cohen's d is not used in the manuscript,
# but I did it here to gain insight into the magnitude of the change in mean:
for (edgeidx in 1:nEdges) {
  cohen.d.df[edgeidx,'EdgeID'] = edgeidx
  dat3.edgeidx = startEdgeidx+edgeidx-1 
  cohen.d.df[edgeidx,'EdgeName'] = names(dat3)[dat3.edgeidx]
  temp = cohen.d(dat3[idx.pass.cc.asd,dat3.edgeidx],dat3[idx.pass.cc.td,dat3.edgeidx])
  cohen.d.df[edgeidx,'cohensd']=temp$estimate
  pooled.sd = sqrt(((n1-1)*var(dat3[idx.pass.cc.asd,dat3.edgeidx])+(n2-1)*var(dat3[idx.pass.cc.td,dat3.edgeidx]))/(n1+n2-2))
  cohen.d.df[edgeidx,'adj.cohensd']=results.ave[edgeidx,'mean.diff.SL']/pooled.sd
}

cohen.d.df
# check that the indexing is correct (that the correct drtmle
# estimate was used):
with(cohen.d.df,cor(cohensd,adj.cohensd))
# high correlation (>0.95) so looks like calcs are correct

#proportion of effect sizes that "increased":
mean(abs(cohen.d.df$cohensd)<abs(cohen.d.df$adj.cohensd))
# there was a tendency to increase

# Ordered edges with smallest p-values in drtmle:
(list.edges = results.ave$EdgeName[results.ave$p.SL.fdr<0.20])
list.pvalues = results.ave$p.SL[results.ave$p.SL.fdr<0.20]
list.edges = list.edges[order(list.pvalues)]
list.pvalues = sort(list.pvalues)
# naive cohen's d of selected edges:
cohen.d.df[cohen.d.df$EdgeName%in%list.edges,]

# These values appear in the manuscript:
max(abs(cohen.d.df$cohensd))
# average of the absolute value of Cohen's D among selected edges:
mean(abs(cohen.d.df$cohensd[cohen.d.df$EdgeName%in%list.edges]))


#####################################
# Visualize propensity scores for a few edges:
# these figures do not appear in the manuscript, as the 
# propensities did not appear to shed insight into drtmle:
pal <- wes_palette("Zissou1", 100, type = "continuous")
#library(colorRamps)
#library(gridExtra)
#pal = matlab.like(100)
set.seed(123)
plot_pcorr_fun_propensities = function(EdgeName,EdgeNamePlot,legend=FALSE) { 
      subdata = dat3[idx.pass.cc,c('PrimaryDiagnosis',EdgeName,'propensities.SL')]
      names(subdata)[2] = 'EdgeName'
      temp = results.ave[results.ave$EdgeName==EdgeName,c('mean.ASD.naive','mean.TD.naive')]
      temp.naive = data.frame('PrimaryDiagnosis'=c('Autism','None'),'EdgeName'=c(temp$mean.ASD.naive,temp$mean.TD.naive))
      temp = results.ave[results.ave$EdgeName==EdgeName,c('mean.ASD.SL','mean.TD.SL')]
      temp.SL = data.frame('PrimaryDiagnosis'=c('Autism','None'),'EdgeName'=c(temp$mean.ASD.SL,temp$mean.TD.SL))
      if(legend==FALSE) {
    outplot = ggplot(subdata, aes(x=PrimaryDiagnosis, y=EdgeName))+ geom_jitter(shape=16,position=position_jitter(0.2),size=2,aes(color=1/propensities.SL))+scale_colour_gradientn(colours=pal,oob = scales::squish,name = expression(1/p[i]),limits=c(1,2.1))+xlab("Primary Diagnosis")+ylab(EdgeNamePlot)+geom_point(data=temp.naive,shape=15,size=2,color="red")+geom_point(data=temp.SL,shape=15,size=2,color='blue',alpha=0.5)+theme(legend.position="none") 
    } else {
        outplot = ggplot(subdata, aes(x=PrimaryDiagnosis, y=EdgeName))+ geom_jitter(shape=16,position=position_jitter(0.2),size=2,aes(color=1/propensities.SL))+scale_colour_gradientn(colours=pal,oob = scales::squish,name = expression(1/p[i]),limits=c(1,2.1))+xlab("Primary Diagnosis")+ylab(EdgeNamePlot)+geom_point(data=temp.naive,shape=15,size=2,color="red")+geom_point(data=temp.SL,shape=15,size=2,color='blue',alpha=0.5)}
    outplot
  }

# list the selected edges order by their uncorrected pvalues:
list.edges

# manually enter the desired edges:
gn.p0=plot_pcorr_fun_propensities(EdgeName='r.ic2.ic27',EdgeNamePlot='IC02-IC27')
gn.p1=plot_pcorr_fun_propensities(EdgeName='r.ic14.ic19',EdgeNamePlot='IC14-IC19')
gn.p2=plot_pcorr_fun_propensities(EdgeName='r.ic13.ic26',EdgeNamePlot='IC13-IC26')
gn.p3=plot_pcorr_fun_propensities(EdgeName='r.ic4.ic17',EdgeNamePlot='IC04-IC17')
gn.p4=plot_pcorr_fun_propensities(EdgeName='r.ic1.ic21',EdgeNamePlot='IC01-IC21')
gn.p5=plot_pcorr_fun_propensities(EdgeName='r.ic14.ic21',EdgeNamePlot='IC14-IC21')
gn.p6=plot_pcorr_fun_propensities(EdgeName='r.ic17.ic24',EdgeNamePlot='IC17-IC24')
gn.p7=plot_pcorr_fun_propensities(EdgeName='r.ic19.ic25',EdgeNamePlot='IC19-IC25')
gn.p8=plot_pcorr_fun_propensities(EdgeName='r.ic8.ic22',EdgeNamePlot='IC08-IC22')

#pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/DataForNineComponentsWithPropensities_naive_drtmle.pdf')
grid.arrange(gn.p0,gn.p1,gn.p2,gn.p3,gn.p4,gn.p5,gn.p6,gn.p7,gn.p8,nrow=3)
#dev.off()

# the edge with the largest change:
results.ave$mag_change.ASD = abs(results.ave$mean.ASD.SL - results.ave$mean.ASD.naive)
results.ave$EdgeName[which.max(results.ave$mag_change.ASD)]
plot_pcorr_fun(EdgeName='r.ic4.ic28',EdgeNamePlot='IC04-IC28')

###################
##################
# Create plots plotting the data, which is a simplified version of the previous plots:
set.seed(123)
plot_pcorr_fun = function(EdgeName,EdgeNamePlot,legend=FALSE) { 
  subdata = dat3[idx.pass.cc,c('PrimaryDiagnosis',EdgeName,'propensities.SL')]
  names(subdata)[2] = 'EdgeName'
  temp = results.ave[results.ave$EdgeName==EdgeName,c('mean.ASD.naive','mean.TD.naive')]
  temp.naive = data.frame('PrimaryDiagnosis'=c('Autism','None'),'EdgeName'=c(temp$mean.ASD.naive,temp$mean.TD.naive))
  temp = results.ave[results.ave$EdgeName==EdgeName,c('mean.ASD.SL','mean.TD.SL')]
  temp.SL = data.frame('PrimaryDiagnosis'=c('Autism','None'),'EdgeName'=c(temp$mean.ASD.SL,temp$mean.TD.SL))
  if(legend==FALSE) {
    outplot = ggplot(subdata, aes(x=PrimaryDiagnosis, y=EdgeName))+ geom_jitter(shape=16,position=position_jitter(0.2),size=2,color='grey50')+xlab("Primary Diagnosis")+ylab(EdgeNamePlot)+geom_point(data=temp.naive,shape=15,size=2,color="red")+geom_point(data=temp.SL,shape=15,size=2,color='blue',alpha=0.5)+theme(legend.position="none") 
  } else {
    outplot = ggplot(subdata, aes(x=PrimaryDiagnosis, y=EdgeName))+ geom_jitter(shape=16,position=position_jitter(0.2),size=2,color='grey50')+xlab("Primary Diagnosis")+ylab(EdgeNamePlot)+geom_point(data=temp.naive,shape=15,size=2,color="red")+geom_point(data=temp.SL,shape=15,size=2,color='blue',alpha=0.5)}
  outplot
}

gn.p0=plot_pcorr_fun(EdgeName='r.ic2.ic27',EdgeNamePlot='IC02-IC27')
gn.p1=plot_pcorr_fun(EdgeName='r.ic14.ic19',EdgeNamePlot='IC14-IC19')
gn.p2=plot_pcorr_fun(EdgeName='r.ic13.ic26',EdgeNamePlot='IC13-IC26')
gn.p3=plot_pcorr_fun(EdgeName='r.ic4.ic17',EdgeNamePlot='IC04-IC17')
gn.p4=plot_pcorr_fun(EdgeName='r.ic1.ic21',EdgeNamePlot='IC01-IC21')
gn.p5=plot_pcorr_fun(EdgeName='r.ic14.ic21',EdgeNamePlot='IC14-IC21')
gn.p6=plot_pcorr_fun(EdgeName='r.ic17.ic24',EdgeNamePlot='IC17-IC24')
gn.p7=plot_pcorr_fun(EdgeName='r.ic19.ic25',EdgeNamePlot='IC19-IC25')
gn.p8=plot_pcorr_fun(EdgeName='r.ic8.ic22',EdgeNamePlot='IC08-IC22',legend=TRUE)


#pdf(file='~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/DataForNineComponents_naive_drtmle.pdf')
grid.arrange(gn.p0,gn.p1,gn.p2,gn.p3,gn.p4,gn.p5,gn.p6,gn.p7,gn.p8,nrow=3)
#dev.off()


## Create a plot of the missing data:
dat.forcompletepredictors = dat3[,c('PrimaryDiagnosis','ADHD_Secondary','AgeAtScan','handedness','CurrentlyOnStimulants','iPANESS.TotalOverflowNotAccountingForAge','WISC.GAI','DuPaulHome.InattentionRaw','DuPaulHome.HyperactivityRaw','ADOS.Comparable.Total', 'Sex', 'SES.Family', 'Race2','CompletePredictorCases')]
sum(complete.cases(dat.forcompletepredictors))
dat.forcompletepredictors$CompletePredictorCases[dat.forcompletepredictors$CompletePredictorCases==0]=NA

names(dat.forcompletepredictors) = c('Primary Diagnosis', 'ADHD Secondary', 'Age at Scan', 'Handedness', 'Prescribed Stimulants', 'iMotor Overflow', 'GAI', 'Inattention', 'Hyperactivity', 'iADOS', 'Sex', 'SES', 'Race', 'Complete Predictor Cases') 

pdf('~/Dropbox/Apps/Overleaf/MotionSelectionBias_rsfMRI/Figures/vis_miss.pdf')
vis_miss(dat.forcompletepredictors,sort_miss = TRUE)
dev.off()

