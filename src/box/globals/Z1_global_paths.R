
#  .............................................................................
#  CAMINHOS (DIRs e ARQs)                                                   ####

## vars: Diretórios ( - ) ------------------------------------------------------
directories <- list(
  config     = here::here('config'),
  raw_data   = here::here('data', '1_raw'),
  stage_area = here::here('data', '2_stage'),
  dw         = here::here('data', '3_dw'),
  log        = here::here('log')
)

## proc: Cria diretórios ( - ) -------------------------------------------------
purrr::walk(directories, function(d) {
  if (!dir.exists(d))
    dir.create(d, recursive = TRUE)
})


## env: Caminho de diretórios ( + ) --------------------------------------------
#' @export
#' 
dirs <- as.environment(directories)
lockEnvironment(dirs, bindings = TRUE)


## env: Caminho de arquivos ( + ) ----------------------------------------------
#' @export
#' 
files <- as.environment(list(
  bhtrans_urls = file.path(dirs$config, 'bhtrans_urls.csv'),
  download_log = file.path(dirs$log, 'csv_downloads.log'),
  global_log   = file.path(dirs$log, 'global.log')
))
lockEnvironment(files, bindings = TRUE)



