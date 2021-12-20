# DeconfoundedFMRI
Code to reproduce all analyses, tables, and figures in "Accounting for motion in fMRI: What part of the spectrum are we
characterizing in autism?"

### Our goals are two-fold: 
1. To document the data loss and biases introduced by common motion exclusion practices in functional connectivity research

2. To introduce a framework to treat excluded scans as a missing data problem. We use doubly robust targeted minimum loss based estimation with an ensemble of machine learning algorithms to address these data losses and the resulting biases.

### Main functions:
1. covariate_figures.Rmd - code used to run all analyses in sections 2.1.3 "Study Sample" to 2.2.4 "Functional connectivity as a function of phenotypes"

2. DeconfoundGroupDifference_Tutorial.Rmd - Tutorial described in section 2.3.1 "Theory: Deconfounded group difference" to illustrate the improvement in functional connectivity from DRTMLE compared to the naive approach from a single simulated dataset. Produces Figure 2.

3. QC_Impacts_Analysis_singleseed_cb.R - R code to run the entire DRTMLE procedure described in section 2.3.2 (motor overflow imputation, estimation of the propensity model, estimation of 153 outcome models, and application of DRTMLE to 153 functional connectivity edges) for a single seed.

4. QC_Impacts_loopSeeds.sh - Loops through seeds and creates a copy of QC_Impacts_Analysis_singleseed.sh and replaces "seedID" with a numeric value

5. QC_Impacts_Plots_PropensitiesOutcome.R - code to produce Figure S3 "Plots of modified partial correlations, naive means, and DRTMLE means for the nine components with smallest DRTMLE p-values"

6. QC_Impacts_DRTMLE_Table_CirclePlotsWhiteBG.Rmd - code to produce Figure 7 "The DRTMLE deconfounded group difference revealed more extensive differences
than the naıve approach", Figure S2, Table S3. 

![graphical overview](https://github.com/mbnebel/DeconfoundedFMRI/blob/thebrisklab-main/graphicalOverview.png)


