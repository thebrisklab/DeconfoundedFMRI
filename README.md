# DeconfoundedFMRI
Code to reproduce all analyses, tables, and figures in "Accounting for motion in fMRI: What part of the spectrum are we
characterizing in autism?"

### Our goals are two-fold: 
1. To document the data loss and biases introduced by common motion exclusion practices in functional connectivity research

2. To introduce a framework to treat excluded scans as a missing data problem. We use doubly robust targeted minimum loss based estimation (DRTMLE) with an ensemble of machine learning algorithms to address these data losses and the resulting biases.

### Main functions:
1. covariate_figures.Rmd - code used to run all analyses in sections *2.1.3 Study Sample* to *2.2.4 Functional connectivity as a function of phenotypes*. Outputs:
    - Table 1: Socio-demographic characteristics of complete predictor cases.
    - Fig 3: Motion quality control leads to dramatic reductions in sample size.
    - Fig 4: rs-fMRI exclusion probability changes with phenotype and age.
    - Fig 5: Participants with usable rs-fMRI data differed from participants with unusable rsfMRI data.
    - Table S2: Summary of Mann-Whitney U tests comparing included and excluded participants using the lenient motion QC and stratified by primary diagnosis.
    - Table S3: Summary of Mann-Whitney U tests comparing included and excluded participants using the strict motion QC and stratified by primary diagnosis.
    - Fig 6: Some covariates related to rs-fMRI exclusion probability are also related to functional connectivity.

2. DeconfoundGroupDifference_Tutorial.Rmd - Tutorial described in *section 2.3.1 Theory: Deconfounded group difference*, which simulates a variable W<sub>c</sub> that drives the confounding between data usability (Δ) and functional connectivity (Y) and then shows how DRTMLE can adjust for this confounding. Output:
    - Fig. 2: Illustration of the improvement in functional connectivity from DRTMLE compared to the naive approach from a single simulated dataset.

3. QC_Impacts_Analysis_singleseed_cb.R - R code to run the entire DRTMLE procedure for a single seed as described in *section 2.3.2 Application: Deconfounded group difference in the KKI Dataset*. This includes motor overflow imputation, estimation of the propensity model, estimation of 153 outcome models, and application of DRTMLE to 153 functional connectivity edges.

4. QC_Impacts_loopSeeds.sh - Loops through seeds, creates a copy of QC_Impacts_Analysis_singleseed.sh, and replaces "seedID" with a numeric value.

5. QC_Impacts_DRTMLE_Table_CirclePlotsWhiteBG.Rmd - Code used to remove black background from screenshots of the 18 signal components saved in [Mango](http://ric.uthscsa.edu/mango/), average DRTMLE group means, group differences, and z-statistics across all 400 seeds. Outputs: 
    - Fig. 7: The DRTMLE deconfounded group difference revealed more extensive differences than the naıve approach. 
    - Fig. S2: Comparison of edges showing a group difference using the naive and DRTMLE approaches at FDR=0.05.
    - Table S3: Summary of edges for which DRTMLE indicated a group difference at FDR=0.20. 
   
6. QC_Impacts_Plots_PropensitiesOutcome.R - R code to calculate the correlation between the mean ASD-TD z-statistic from DRTMLE for the two sets of 200 seeds, to compare the number of significant edges indicated by DRTMLE for the two sets of 200 seeds, to calculate Cohen's D for the naive ASD-TD differences, to plot the modified partial correlations, naive means, and DRTMLE means for the nine components with the smallest DRTMLE p-values, and to visualize the missingness of the initial dataset. Outputs:
    - Fig. S1: Missingness of socio-demographic and behavioral variables in the initial dataset.
    - Fig. S3: Plots of modified partial correlations, naive means, and DRTMLE means for the nine components with smallest DRTMLE p-values.


![graphical overview](https://github.com/mbnebel/DeconfoundedFMRI/blob/thebrisklab-main/graphicalOverview.png)


