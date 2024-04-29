#!/bin/sh
#
# Simple "Hello World" submit script for Slurm.
#
#SBATCH --account=stats         # Replace ACCOUNT with your group account name
#SBATCH --job-name=sat     # The job name.
#SBATCH -c 1                     # The number of cpu cores to use
#SBATCH -t 00-05:30                 # Runtime in D-HH:MM
#SBATCH --mem-per-cpu=2gb         # The memory the job will use per cpu core
#SBATCH --output=/burg/home/yt2661/trash/slurm-%A_%a.out

export OMP_NUM_THREADS=1

module load R/4.1.0
 
#Command to execute Python program
R CMD BATCH --no-save --vanilla sat.R /burg/home/yt2661/projects/NPMC/experiment/real_data/sat/out/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt
 