#' Carrega CSVs de dados brutos
#' @export
#' @return list. Lista contendo dados carregados e mensagens de erro.
#' 
load_raw_data <- function() {
  box::use(
    cli[cli_process_start, cli_process_done, cli_process_failed],
    fs[dir_ls], glue[glue], janitor[clean_names], purrr[map],
    readr[read_delim, locale, cols, col_character], stringr[str_remove],
    g = ./globals
  )
  
  dir.paths <- dir_ls(g$dirs$raw_data, type = 'directory')
  result    <- list(err = list(), data = list())
  
  for (dir.path in dir.paths) {
    dir.name  <- basename(dir.path)
    csv.files <- dir_ls(dir.path, type = 'file', glob = '*.csv')
    
    tryCatch(
      expr = {
        cli_process_start(glue("Leitura de CSVs: `{dir.name}`"), .auto_close = F)
        
        csv.dfs <- map(
          csv.files,
          ~read_delim(
            .,
            delim          = ';',
            locale         = locale(encoding = 'latin1'),
            show_col_types = FALSE,
            progress       = FALSE,
            col_types      = cols(.default = col_character()),
            name_repair    = 'minimal'
          )
        )
        
        csv.dfs        <- map(csv.dfs, clean_names)
        names(csv.dfs) <- basename(str_remove(csv.files, r'{\.csv}'))
        result$data[[dir.name]] <- csv.dfs
        
        cli_process_done()
      },
      
      error = function(e) {
        cli_process_failed()
        result$err[[dir.name]]  <<- e
      }
    )
  }
  
  return(result)
}
