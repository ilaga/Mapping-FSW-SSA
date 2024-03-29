#!/bin/bash
#SBATCH --partition=defq
#SBATCH --mem=4000
#SBATCH --time=0-01:00:00
#SBATCH --nodes 1
#SBATCH --ntasks-per-node 4
#SBATCH --job-name=Normal_vs_t_LOAO
#SBATCH --array=1-616
#SBATCH --output=/mnt/lustrefs/scratch/ian.laga/output/Normal_vs_t_LOAO-%A_%a.out
#SBATCH --error=/mnt/lustrefs/scratch/ian.laga/error/Normal_vs_t_LOAO-%A_%a_%j.err


echo " "
echo " "
echo "Job started on `hostname` at `date`"

cd ${SLURM_SUBMIT_DIR}

echo ${SLURM_SUBMIT_DIR}

module load Anaconda3/5.3.0
source activate /home/ian.laga/condaenv

Rscript Normal_vs_t_LOAO.R ${SLURM_ARRAY_TASK_ID}


echo "Job Ended at `date`"


