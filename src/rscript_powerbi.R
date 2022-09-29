root    <- 'PATH_TO_PROJECT/projeto_dw/src'
rscript <- paste0(root, '/etl.R')

setwd(root)
source(rscript, encoding = 'UTF-8')
