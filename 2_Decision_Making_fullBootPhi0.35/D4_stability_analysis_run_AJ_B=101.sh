#!/bin/bash
#$ -N stability_analysis_B100
#$ -j y
###$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-100
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=20G

Rscript --vanilla D4_stability_analysis.R ${SGE_TASK_ID} 101