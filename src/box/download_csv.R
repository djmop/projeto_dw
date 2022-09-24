#' Realiza download de um único CSV
#' @export
#' 
download_csv <- function(csv.url, filepath, max.tries = 3) {
  box::use(
    cli[cli_alert_success, cli_alert_danger, cli_alert_info],
    httr[GET, write_disk, stop_for_status],
    glue[glue]
  )
  
  log_msg <- function(type, msg) {
    box::use(stringr[str_pad, str_to_upper], glue[glue])
    x <- glue("{Sys.time()} | PID: {str_pad(Sys.getpid(), 8, pad = '0')}")
    x <- glue('{x} | [{str_to_upper(type)}] {msg}')
    return(x)
  }
  
  df.name <- basename(filepath)
  count = 1
  
  while (TRUE) {
    tryCatch(
      expr = {
        resp <- GET(csv.url, write_disk(filepath, overwrite = TRUE))
        stop_for_status(resp)
        cli_alert_success(log_msg(
          type = 'info',
          msg = glue('Download concluído: `{df.name}`')
        ))
        break
      },
      
      error = function(e) {
        cli_alert_danger(log_msg(
          type = 'erro',
          msg = glue('Falha de download: `{df.name}`')
        ))
        if (count > max.tries) {
          cli_alert_danger(log_msg(
            type = 'erro',
            msg = glue('Número de tentativas esgotado para `{df.name}`. ',
                       'Abortando o download...')
          ))
          break
        }
        Sys.sleep(10 * count)
        count = count + 1
        cli_alert_info(log_msg(
          type = 'info',
          msg = glue('Iniciando nova tentativa ({count}) para `{df.name}`')
        ))
      }
    )
  }
}

