box::use(
  cli[cli_process_start, cli_process_done, cli_process_failed],
  readr[read_delim, locale, cols, col_character],
  stringr[str_detect, str_remove],
  janitor[clean_names],
  purrr[map, map2],
  glue[glue],
  fs[dir_ls]
)

box::use(G.PATH  = ../globals/Z1_global_paths)
box::use(G.LOG   = ../globals/Z2_global_logging)
box::use(G.CONST = ../globals/Z3_global_constants)



#' Checa erros de carregamento
#' @export
#' 
validate_loaded_data <- function(raw_data) {
  G.LOG$oversee(
    proc_msg = 'Validando dados',
    expr = {
      if (length(raw_data$err) > 0) {
        box::use(purrr[walk2])
        walk2(
          names(raw_data$err), raw_data$err,
          ~cat(paste(.x, .y, sep = '\n'), '\n')
        )
        stop('Falha no carregamento de dados brutos')
      }
    }
  )
}



#' Carrega CSVs de dados brutos
#' @export
#' @return list. Lista contendo dados carregados e mensagens de erro.
#' 
load_raw_data <- function() {
  raw_data_dir <- G.PATH$dirs$raw_data
  dir.paths    <- dir_ls(raw_data_dir, type = 'directory')
  result       <- list(err = list(), data = list())
  
  for (dir.path in dir.paths) {
    dir.name  <- basename(dir.path)
    csv.files <- dir_ls(dir.path, type = 'file', glob = '*.csv')
    csv.names <- basename(str_remove(csv.files, r'{\.csv}'))
    
    G.LOG$oversee(
      proc_msg = glue('Lendo CSVs: {dir.name}'),
      
      expr = {
        csv.dfs <- map2(csv.files, csv.names, ~ read_csv_file(.x, .y))
        csv.dfs <- map(csv.dfs, clean_names)
        names(csv.dfs) <- csv.names
        result$data[[dir.name]] <- csv.dfs
      },
      
      error = function(e) {
        cli_process_failed()
        result$err[[dir.name]] <<- e
      }
    )
  }
  return(result)
}


read_csv_file <- function(x, filename) {
  is.dict <- str_detect(filename, G.CONST$regex_patts$dict_files)
  
  if (is.dict)
    skip_line = 3
  else
    skip_line = 0
  
  df <- read_delim(
    x,
    delim          = ';',
    locale         = locale(encoding = G.CONST$constants$ENCODING),
    skip           = skip_line,
    show_col_types = FALSE,
    progress       = FALSE,
    col_types      = cols(.default = col_character()),
    name_repair    = 'minimal'
  )
  
  return(df)
}
