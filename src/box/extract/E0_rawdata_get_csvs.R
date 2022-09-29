box::use(
  cli[cli_alert_success, cli_alert_danger, cli_alert_info],
  httr[GET, write_disk, stop_for_status],
  glue[glue]
)

box::use(G.LOG = ../globals/Z2_global_logging)


#' Realiza download de um único CSV
#' @export
#' 
download_csv <- function(csv.url, filepath, max.tries = 3) {
  log.i <- G.LOG$log_info
  log.e <- G.LOG$log_error
  
  df.name <- basename(filepath)
  count = 1
  
  while (TRUE) {
    tryCatch(
      expr = {
        resp <- GET(csv.url, write_disk(filepath, overwrite = TRUE))
        stop_for_status(resp)
        cli_alert_success(log.i(glue('Download concluído: `{df.name}`')))
        break
      },
      
      error = function(e) {
        cli_alert_danger(log.e(glue('Falha de download: `{df.name}`')))
        if (count > max.tries) {
          cli_alert_danger(log.e(glue(
            'Número de tentativas esgotado para `{df.name}`. ',
            'Abortando o download...'
          )))
          break
        }
        Sys.sleep(10 * count)
        count = count + 1
        cli_alert_info(log.i(glue(
          'Iniciando tentativa ({count}) para `{df.name}`...'
        )))
      }
    )
  }
}

