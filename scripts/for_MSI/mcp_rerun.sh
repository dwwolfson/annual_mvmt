#!/bin/bash -l
#SBATCH --time=48:00:00	
#SBATCH --ntasks=24			
#SBATCH --mem=50g
#SBATCH --mail-type=ALL  
#SBATCH --mail-user=wolfs064@umn.edu


cd /home/jfieberg/wolfs064/annual_mvmt
module load JAGS/4.3.0-gcc7.2.0
module load R/4.1.0

Rscript --vanilla mcp_rerun_apr_2023.Rq