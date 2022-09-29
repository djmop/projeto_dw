#  .............................................................................
#  FUNÇÕES DE LOG                                                           ####

box::use(glue[glue], stringr[str_pad])


## func: Mensagem de log ( - ) -------------------------------------------------
#' @title Mensagem informação para log
#' @param msg `string`. Conteúdo da mensagem.
#' @export
#'
process_msg <- function(msg, npad = 50) {
  res <- paste0(msg, ' ')
  res <- str_pad(res, width = npad, side = 'right', pad = '.')
  return(res)
}


## func: Mensagem de log: INFO ( + ) -------------------------------------------
#' @title Mensagem informação para log
#' @param msg `string`. Conteúdo da mensagem.
#' @export
#'
log_info <- function(msg) {
  return(glue('[INFO ] {process_msg(msg)}'))
}


## func: Mensagem de log: ERROR ( + ) ------------------------------------------
#' @title Mensagem erro para log
#' @param msg `string`. Conteúdo da mensagem.
#' @export
#'
log_error <- function(msg) {
  return(glue('[ERROR] {process_msg(msg)}'))
}


cat_success <- function(msg, time) {
  success <- glue('[INFO ] {process_msg(msg)} done [{time}]')
  cat(success, fill = T)
}

cat_error <- function(msg, caller) {
  error  <- glue('[ERROR] {process_msg(msg)} failed')
  caller <- glue('[ERROR] Caller: {process_msg(caller, 42)} failed')
  cat(error, fill = T)
  cat(caller, fill = T)
}


## func: Mensagem de log: DW ( + ) ---------------------------------------------
#' @title Mensagem de processos do data warehouse para log
#' @export
#' 
log_dw <- function(proc_msg, expr, error) {
  box::use(cli[cli_progress_step])
  box::use(assertthat[assert_that])
  
  .env <- as.environment(list(
    msg = proc_msg,
    caller = sys.call(sys.parent())
  ))
  
  .clock <- Clock$new()
  
  if (missing(error))
    throw_error <- function(e) { cat_error(.env$msg, .env$caller); stop(e) }
  else {
    assert_that(is.function(error))
    throw_error <- function(e) { cat_error(.env$msg, .env$caller); error(e) }
  }
  
  tryCatch(
    expr = {
      .clock$tic()
      x <- eval(expr)
      cat_success(.env$msg, .clock$toc())
      return(x)
    },
    error = throw_error
  )
}


## func: Inicia log global ( + ) -----------------------------------------------
#' @export
start_log <- function() {
  box::use(G.PATH = ./Z1_global_paths)
  logfile <- file(G.PATH$files$global_log, open = 'wt')
  sink(logfile)
  sink(logfile, type = "message")
  return(logfile)
}


## func: Finaliza log global ( + ) ---------------------------------------------
#' @export
close_log <- function(logfile) {
  sink(type = "message")
  sink()
  close(logfile)
}


## func: Timer ( + ) -----------------------------------------------------------
#' @export
Clock <- R6::R6Class(
  classname = 'Clock',
  private = list(
    start = NULL,
    end   = NULL,
    pretty_time = function() {
      box::use(prettyunits[pretty_dt], stringr[str_extract, str_pad])
      
      diff <- private$end - private$start
      diff <- pretty_dt(diff)
      num  <- str_extract(diff, r'{\d+(\.\d+)?}')
      txt  <- str_extract(diff, r'{[a-z]+}')
      diff <- str_pad(paste(num, txt), 6)
      return(diff)
    }
  ),
  public = list(
    tic = function() { private$start <- Sys.time() },
    toc = function() {
      private$end <- Sys.time()
      private$pretty_time()
    }
  )
)

