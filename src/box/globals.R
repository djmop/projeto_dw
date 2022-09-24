#' Árvore de diretórios
directories <- list(
  config     = here::here('config'),
  raw_data   = here::here('data', '1_raw'),
  stage_area = here::here('data', '2_stage'),
  dw         = here::here('data', '3_dw'),
  log        = here::here('log')
)

#' Cria árvore de diretórios
#' 
purrr::walk(directories, function(d) {
  if (!dir.exists(d))
    dir.create(d, recursive = TRUE)
})

#' Caminhos de diretórios
#' @export
dirs <- as.environment(directories)
lockEnvironment(dirs, bindings = TRUE)


#' Caminhos de arquivos
#' @export
files <- as.environment(list(
  bhtrans_urls = file.path(dirs$config, 'bhtrans_urls.csv'),
  download_log = file.path(dirs$log, 'csv_downloads.log')
))
lockEnvironment(files, bindings = TRUE)

#' Padrões REGEX
#' @export
regex_patts <- as.environment(list(
  dict_files = r'(_dict_)'
))
lockEnvironment(regex_patts, bindings = TRUE)


#' Constantes
#' @export
constants <- as.environment(list(
  timezone     = 'America/Sao_Paulo',
  str_translit = 'Latin-ASCII'
))
