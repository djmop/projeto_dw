# # Parameters
# project_root = '<PATH_TO_PROJECT>/projeto_dw/'
# 
# # Load main script
# rscript = base::file.path(project_root, 'src/main.R')
# source(rscript, encoding = 'UTF-8')
# 
# # Run
# run_etl(project_root, download = FALSE, log = TRUE)
# 


run_etl(here::here())
