#' Coleta URLs dos CSVs
#' @export
#' 
get_csv_urls <- function(verbose = F, port = 4813L) {
  box::use(
    cli[cli_process_start, cli_process_failed, cli_process_done],
    RSelenium[rsDriver],
    purrr[map]
  )
  
  cli_process_start('Extração de links para download', .auto_close = FALSE)
  
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
    },
    error = function(e) {
      cli_process_failed()
      print(e)
    },
    finally = {
      r.browser$close()
      r.server$server$process$finalize()
    }
  )
  
  cli_process_done()
  return(csv.urls)
}


#' Carrega urls da BHTRANS
#' 
load_root_urls <- function() {
  box::use(g = ./zzz_globals, readr[read_csv, cols, col_character], here[here])
  bhtrans <- read_csv(
    here(g$files$bhtrans_urls),
    col_type = cols(.default = col_character())
  )
  urls        <- bhtrans[['url']]
  names(urls) <- bhtrans[['name']]
  return(urls)
}


#' Busca csvs nos elementos da página
#' 
extract_csv_urls <- function(url, browser) {
  box::use(purrr[map_chr])
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
  box::use(
    stringr[str_remove_all, str_replace, str_replace_all, str_detect],
    g = ./zzz_globals
  )
  is.dict <- str_detect(x, 'dicionario')
  x <- str_replace_all(x, '-', '_')
  x <- str_remove_all(x, r'(dicionario_tabela_|_aaaa|si_|\.csv)')
  if (is.dict)
    x <- str_replace(x, '_', g$regex_patts$dict_files)
  return(x)
}
