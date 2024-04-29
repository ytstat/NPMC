#!/bin/sh
#
# Simple "Hello World" submit script for Slurm.
#
#SBATCH --account=stats         # Replace ACCOUNT with your group account name
#SBATCH --job-name=loanclub_rf     # The job name.
#SBATCH -c 1                     # The number of cpu cores to use
#SBATCH -t 00-14:00                 # Runtime in D-HH:MM
#SBATCH --mem-per-cpu=4gb         # The memory the job will use per cpu core
#SBATCH --output=/burg/home/yt2661/trash/slurm-%A_%a.out

export OMP_NUM_THREADS=1

module load R/4.1.0
 
#Command to execute Python program
R CMD BATCH --no-save --vanilla loanclub_rf.R /burg/home/yt2661/projects/NPMC/experiment/real_data/loanclub_rf/out/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt
 