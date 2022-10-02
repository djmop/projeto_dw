# Parameters
project_root = 'E:/My drive/PUC Minas/6o_semestre/BUSINESS_INTEL/projeto.dw/'

# Load main script
rscript = base::file.path(project_root, 'src/main.R')
source(rscript, encoding = 'UTF-8')

# Run
run_etl(project_root, download = FALSE, log = TRUE)
