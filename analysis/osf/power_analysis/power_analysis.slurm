#!/bin/bash
#SBATCH --job-name=menagerie_power       # create a short name for your job
#SBATCH --nodes=1                # node count
#SBATCH --ntasks=1               # how many instances of your command are run, total, across all nodes
#SBATCH --cpus-per-task=20       # cpu-cores per task (>1 if multithread tasks)
#SBATCH --mem-per-cpu=6G         # memory per cpu-core (4G is default)
#SBATCH --time=2:30:00          # total run time limit (HH:MM:SS)
#SBATCH --mail-type=begin        # send email when process begins
#SBATCH --mail-type=fail         # send email if job fails
#SBATCH --mail-type=end          # send email when job ends
#SBATCH --mail-user=am9578@princeton.edu

Rscript power_analysis.R