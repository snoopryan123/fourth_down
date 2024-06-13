#!/bin/bash
#$ -N sim_bias_variance_fitModels
#$ -j y
###$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-100
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla sim_bias_variance_fitModels.R ${SGE_TASK_ID}