#!/bin/bash
#$ -N xgb_ParamTuning_WP
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-3
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla T2_param_tuning_xgb.R ${SGE_TASK_ID} FALSE TRUE