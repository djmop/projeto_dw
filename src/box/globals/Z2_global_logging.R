box::use(
  checkmate[assert_flag, assert_function, assert_posixct, assert_true],
  checkmate[assert_string, test_posixct, test_null],
  cli[cli_text, cat_bullet, cat_line, cli_div, cli_rule, cli_end],
  lubridate[interval], hms[hms], prettyunits[pretty_sec],
  stringr[str_extract, str_pad], glue[glue]
)
box::use(G.PATH = ./Z1_global_paths)


#  .............................................................................
#  FUNÇÕES DE LOG                                                           ####

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

cat_error <- function(msg) {
  error  <- glue('[ERROR] {process_msg(msg)} failed')
  cat(error, fill = T)
}


## func: Mensagem de log: DW ( + ) ---------------------------------------------
#' @title Mensagem de processos do data warehouse para log
#' @export
#' 
oversee <- function(proc_msg, expr, error, finally) {
  .env   <- as.environment(list(msg = proc_msg))
  
  if (missing(error))
    throw_error <- function(e) { cat_error(.env$msg); stop(e) }
  else {
    assert_function(error)
    throw_error <- function(e) { cat_error(.env$msg); error(e) }
  }
  
  if (missing(finally))
    finally <- {}
  
  tryCatch(
    expr = {
      .clock <- Clock$new()
      
      .clock$tic()
      x <- eval(expr)
      .clock$toc()
      
      cat_success(.env$msg, .clock$duration_pretty())
      
      return(x)
    },
    error = throw_error,
    finally = {
      invisible(eval(finally))
    }
  )
}


## func: Controla log global ( + ) ---------------------------------------------
#' @export
Logger <- R6::R6Class(
  classname = 'Logger',
  
  private   = list(
    sink_path = G.PATH$files$global_log,
    sink_file = NULL,
    do_sink   = NULL,
    clock     = NULL,
    
    start_sink = function() {
      private$sink_file <- file(private$sink_path, open = 'wt')
      sink(private$sink_file)
      sink(private$sink_file, type = "message")
    },
    close_sink = function() {
      sink(type = "message")
      sink()
      close(private$sink_file)
    },
    
    alert_msg = function(msg, type = c('info', 'warn', 'error')) {
      type <- match.arg(type)
      type <- str_pad(toupper(type), 5, side = 'right')
      msg  <- glue('[{type}] PID: {Sys.getpid()} | {msg}', .trim = F)
      cli_text(msg)
    },
    alert_start = function() {
      private$alert_msg(glue('Processo iniciado em: {private$clock$started_at()}'))
    },
    alert_end   = function() {
      private$alert_msg(glue('Processo finalizado em: {private$clock$ended_at()}'))
      dur <- private$clock$duration_hms()
      private$alert_msg(glue('Tempo total decorrido:  {dur}', .trim = F))
    },
    alert_error = function(caller, problem) {
      msg <- glue('Processo ABORTADO em: {private$clock$ended_at()}', .trim = F)
      private$alert_msg(msg, type = 'error')
      private$alert_msg("MENSAGEM:", type = 'error')
      cat_bullet(bullet = 'cross', c(
        glue('Caller:  {paste(caller, collapse = " <- ")}', .trim = F),
        glue('Problem: {as.character(problem)}', .trim = F)
      ))
      cat_line()
    },
    
    add_rule = function(left = '', right = '',
                        type = c('single', 'double')) {
      type <- match.arg(type)
      cli_div(id = 'rl', theme = list(rule = list(`line-type` = type)))
      cli_rule(id = 'rl', left = left, right = right)
      cli_end(id = 'rl')
    },
    start_rule = function(left = '') {
      private$add_rule(left = left, right = 'LOG_START', type = 'double')
      cat_line()
    },
    end_rule = function() {
      cat_line()
      private$add_rule(right = 'LOG_END', type = 'double')
      cat_line()
    }
  ),
  
  public = list(
    initialize = function(output = TRUE) {
      assert_flag(output)
      private$do_sink <- output
      private$clock   <- Clock$new()
    },
    
    start = function(title = '') {
      if (private$do_sink) { private$start_sink() }
      private$clock$tic()
      private$start_rule(left = title)
      private$alert_start()
    },
    
    close = function() {
      private$clock$toc()
      private$alert_end()
      private$end_rule()
      if (private$do_sink) { private$close_sink() }
    },
    
    dump = function(caller, problem) {
      cat_line()
      private$clock$toc()
      private$add_rule('ERROS', type = 'double')
      private$alert_error(caller, problem)
      private$end_rule()
      if (private$do_sink) { private$close_sink() }
    },
    
    add_title = function(title, right = '') {
      cat_line()
      private$add_rule(left = title, type = 'single')
    }
  )
)

## func: Timer ( + ) -----------------------------------------------------------
#' @export
Clock <- R6::R6Class(
  classname = 'Clock',
  private = list(
    start = NULL,
    end   = NULL,
    dsec  = function() {
      assert_posixct(private$start); assert_posixct(private$end)
      secs <- as.numeric(interval(private$start, private$end))
      return(secs)
    }
  ),
  public  = list(
    tic = function() { private$start <- Sys.time() },
    toc = function() { private$end   <- Sys.time() },
    started_at = function() { return(private$start) },
    ended_at   = function() {
      if(test_posixct(private$end))
        return(private$end)
      return(lubridate::NA_POSIXct_)
    },
    duration_pretty = function(npad = 5) {
      secs <- pretty_sec(private$dsec())
      dur  <- str_pad(secs, npad)
      return(dur)
    },
    duration_hms = function(digits = 1) {
      secs <- round(private$dsec(), digits = digits)
      dur  <- hms(seconds = secs)
      return(dur)
    }
  )
)
