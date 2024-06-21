#!/bin/bash
#$ -N fit_BootWPModels
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 

#$ -o job_output/$JOB_NAME-$JOB_ID.log
#$ -l m_mem_free=10G

Rscript --vanilla D5_fit_BootWPModels.R