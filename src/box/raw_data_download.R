#' Realiza o download dos CSVs
#' @export
#' 
download_raw_data <- function(verbose = F, port = 4813L) {
  box::use(
    cli[cli_progress_bar, cli_progress_update, cli_progress_done],
    here[here], foreach[foreach, `%dopar%`],
    g = ./globals, urls = ./raw_data_urls, dwn = ./download_csv
  )
  
  csv.urls <- urls$get_csv_urls(verbose = verbose, port = port)
  
  if (!dir.exists(g$dirs$log))
    dir.create(g$dirs$log)
  
  myCluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = 'PSOCK',
    outfile = g$files$download_log
  )
  
  tryCatch(
    expr = {
      cli_progress_bar(
        'Download de CSVs', total = length(csv.urls),
        type = "tasks", clear = FALSE
      )
      
      doParallel::registerDoParallel(myCluster)
      
      download_csv <- dwn$download_csv
      
      for (i in seq_along(csv.urls)) {
        cur.subject <- names(csv.urls)[i]
        cur.names   <- names(csv.urls[[i]])
        cur.urls    <- csv.urls[[i]]
        
        dirpath <- here(g$dirs$raw_data, cur.subject)
        if (!dir.exists(dirpath)) { dir.create(dirpath) }
        
        foreach(j = seq_along(cur.urls)) %dopar% {
          box::use(here[here], glue[glue])
          csv.url  <- cur.urls[[j]]
          filepath <- here(dirpath, glue('{names(cur.urls[j])}.csv'))
          download_csv(csv.url, filepath)
        }
        cli_progress_update()
      }
      cli_progress_done()
    },
    
    finally = {
      parallel::stopCluster(myCluster)
      invisible(gc())
    }
  )
}
