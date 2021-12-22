#!/bin/bash

cd ~/risk_share/QualityControlImpacts/ProgramsBrisk

for seedID in $(seq 1 400)
do

sed -e s:seedID:"${seedID}":g <~/risk_share/QualityControlImpacts/ProgramsBrisk/QC_Impacts_Analysis_singleseed_cb.R >QC_Impacts_Analysis_seed${seedID}.R

sed -e s:seedID:"${seedID}":g <~/risk_share/QualityControlImpacts/ProgramsBrisk/QC_Impacts_Analysis_singleseed.sh >QC_Impacts_Analysis_seed${seedID}.sh


qsub  -cwd -q R4.q -N seed${seedID} QC_Impacts_Analysis_seed${seedID}.sh

done

