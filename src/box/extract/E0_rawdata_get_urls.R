box::use(
  cli[cli_process_start, cli_process_failed, cli_process_done],
  stringr[str_remove_all, str_replace, str_replace_all, str_detect],
  readr[read_csv, cols, col_character],
  RSelenium[rsDriver],
  purrr[map, map_chr],
  here[here]
)

box::use(G.PATH  = ../globals/Z1_global_paths)
box::use(G.LOG   = ../globals/Z2_global_logging)
box::use(G.CONST = ../globals/Z3_global_constants)



#' Coleta URLs dos CSVs
#' @export
#' 
get_csv_urls <- function(verbose = F, port = 4813L) {
  log.i <- G.LOG$log_info
  log.e <- G.LOG$log_error
  
  
  G.LOG$oversee(
    proc_msg = 'Extração de URLs',
    expr = {
      while (TRUE) {
        tryCatch(
          exp = {
            r.server <- rsDriver(
              browser = 'firefox',
              verbose = verbose,
              port = port,
              extraCapabilities = list(
                "moz:firefoxOptions" = list(args = list('--headless'))
              )
            )
            break
          },
          error = function(e) {
            cat('\n\n')
            warning(e)
            cat('\n\n')
            port = port + 1L
          },
          finally = { invisible(gc()) }
        )
      }
      
      root.urls <- load_root_urls()
      r.browser <- r.server$client
      
      tryCatch(
        expr = {
          csv.urls <- map(root.urls, extract_csv_urls, browser = r.browser)
          csv.urls
        },
        error = function(e) {
          print(e)
          stop(e)
        },
        finally = {
          r.browser$close()
          r.server$server$process$finalize()
        }
      )
      
    }
  )
}


#' Carrega urls da BHTRANS
#' 
load_root_urls <- function() {
  bhtrans <- read_csv(
    here(G.PATH$files$bhtrans_urls),
    col_type = cols(.default = col_character()),
    progress = FALSE
  )
  urls        <- bhtrans[['url']]
  names(urls) <- bhtrans[['name']]
  return(urls)
}


#' Busca csvs nos elementos da página
#' 
extract_csv_urls <- function(url, browser) {
  browser$navigate(url)
  elems <- browser$findElements(
    using = 'class',
    value = 'resource-url-analytics'
  )
  csv.urls  <- map_chr(elems, ~unlist(.$getElementAttribute('href')))
  csv.names <- map_chr(basename(csv.urls), simplify_csv_name)
  names(csv.urls) <- csv.names
  return(csv.urls)
}


#' Simplifica nome dos CSVs
#' 
simplify_csv_name <- function(x) {
  is.dict <- str_detect(x, 'dicionario')
  x <- str_replace_all(x, '-', '_')
  x <- str_remove_all(x, r'(dicionario_tabela_|_aaaa|si_|\.csv)')
  if (is.dict)
    x <- str_replace(x, '_', G.CONST$regex_patts$dict_files)
  return(x)
}
