#!/bin/bash
#$ -N sim_v2_1
#$ -j y
###$ -m e -M ryguy123@sas.upenn.edu 

## ARRAY JOB
#$ -t 1-25
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log
## MORE RAM
#$ -l m_mem_free=10G

Rscript --vanilla sim_2.R ${SGE_TASK_ID} 1