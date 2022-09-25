#' Carrega CSVs de dados brutos
#' @export
#' @return list. Lista contendo dados carregados e mensagens de erro.
#' 
load_raw_data <- function() {
  box::use(
    cli[cli_process_start, cli_process_done, cli_process_failed], fs[dir_ls],
    glue[glue], janitor[clean_names], purrr[map, map2], stringr[str_remove],
    g = ./zzz_globals
  )
  
  dir.paths <- dir_ls(g$dirs$raw_data, type = 'directory')
  result    <- list(err = list(), data = list())
  
  for (dir.path in dir.paths) {
    dir.name  <- basename(dir.path)
    csv.files <- dir_ls(dir.path, type = 'file', glob = '*.csv')
    csv.names <- basename(str_remove(csv.files, r'{\.csv}'))
    
    tryCatch(
      expr = {
        cli_process_start(glue("Leitura de CSVs: `{dir.name}`"), .auto_close = F)
        
        csv.dfs <- map2(csv.files, csv.names, ~ read_csv_file(.x, .y))
        csv.dfs <- map(csv.dfs, clean_names)
        
        names(csv.dfs) <- csv.names
        
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


read_csv_file <- function(x, filename) {
  box::use(
    readr[read_delim, locale, cols, col_character],
    stringr[str_detect], g = ./zzz_globals
  )
  
  is.dict <- str_detect(filename, g$regex_patts$dict_files)
  
  if (is.dict)
    skip_line = 3
  else
    skip_line = 0
  
  df <- read_delim(
    x,
    delim          = ';',
    locale         = locale(encoding = g$constants$ENCODING),
    skip           = skip_line,
    show_col_types = FALSE,
    progress       = FALSE,
    col_types      = cols(.default = col_character()),
    name_repair    = 'minimal'
  )
  
  return(df)
}
