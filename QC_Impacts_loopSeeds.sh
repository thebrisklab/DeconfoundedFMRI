#!/bin/bash

for seedID in $(seq 10)
do

cd ~/risk_share/QualityControlImpacts/ProgramsBrisk
sed -e s:seedID:"${seedID}":g <~/risk_share/QualityControlImpacts/Programs/QC_Impacts_Analysis_singleseed.R >QC_Impacts_Analysis_seed${seedID}.R

sed -e s:seedID:"${seedID}":g <~/risk_share/QualityControlImpacts/Programs/QC_Impacts_Analysis_singleseed.sh >QC_Impacts_Analysis_seed${seedID}.sh


qsub  -cwd -N seed${subjectID} QC_Impacts_Analysis_seed${subjectID}.sh

done

