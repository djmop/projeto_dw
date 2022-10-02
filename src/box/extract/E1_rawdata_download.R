box::use(
  cli[cli_progress_bar, cli_progress_update, cli_progress_done],
  foreach[foreach, `%dopar%`],
  here[here],
  glue[glue]
)

box::use(URL = ./E0_rawdata_get_urls)
box::use(CSV = ./E0_rawdata_get_csvs)

box::use(G.PATH = ../globals/Z1_global_paths)
box::use(G.LOG  = ../globals/Z2_global_logging)


#' Realiza o download dos CSVs
#' @export
#' 
download_raw_data <- function(verbose = F, port = 4813L) {
  csv.urls <- URL$get_csv_urls(verbose = verbose, port = port)
  
  if (!dir.exists(G.PATH$dirs$log))
    dir.create(G.PATH$dirs$log)
  
  myCluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = 'PSOCK',
    outfile = G.PATH$files$download_log
  )
  
  proc.msg <- 'Download de CSVs'
  
  G.LOG$oversee(
    proc_msg = proc.msg,
    
    expr = {
      pb <- cli_progress_bar(
        name = 'Baixando CSVs',
        total = length(csv.urls), type = "tasks"
      )
      
      doParallel::registerDoParallel(myCluster)
      
      download.csv <- CSV$download_csv
      
      for (i in seq_along(csv.urls)) {
        cur.subject <- names(csv.urls)[i]
        cur.names   <- names(csv.urls[[i]])
        cur.urls    <- csv.urls[[i]]
        
        dirpath <- here(G.PATH$dirs$raw_data, cur.subject)
        if (!dir.exists(dirpath)) { dir.create(dirpath) }
        
        foreach(j = seq_along(cur.urls)) %dopar% {
          box::use(here[here], glue[glue])
          csv.url  <- cur.urls[[j]]
          filepath <- here(dirpath, glue('{names(cur.urls[j])}.csv'))
          download.csv(csv.url, filepath)
        }
        cli_progress_update(id = pb)
      }
      cli_progress_done(id = pb)
    },
    
    finally = {
      parallel::stopCluster(myCluster)
      invisible(gc())
    }
  )
}
