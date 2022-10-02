box::use(
  checkmate[makeAssertCollection, makeAssertion],
  purrr[map_depth, map2], glue[glue],
  withr[local_options]
)

#  .............................................................................
#  ASSERTIONS GLOBAIS                                                       ####

## func: Conversão de tipos ( + ) ----------------------------------------------
#' @title Converte tipos de dados em tabelas
#' @param .x Uma lista ou vertor atômico.
#' @param .depth Nível de `.x` a ser avaliado
#' @param .chk Função de checagem [checkmate].
#' @export
#' 
assert_depth <- function(.x, .depth, .chk) {
  checked <- map_depth(.x, .depth, .f = .chk)
  .depth <- ifelse(.depth > 0, .depth - 1, .depth)
  coll <- makeAssertCollection()
  res <- map_depth(
    checked, .depth,
    .f = function(z) { map2(
      .x = z,
      .y = names(z),
      .f = ~ makeAssertion(
        x = .x,
        res = .x,
        var.name = .y,
        collection = coll
      )
    )}
  )
  msgs <- coll$getMessages()
  if (length(msgs) > 0) {
    err = c(
      glue('{length(msgs)} assertion(s) failed'),
      glue('  ✖ {msgs}', .trim = F)
    )
    err = paste(err, collapse = "\n")
    local_options(list(warning.length = 8170))
    stop(err)
  }
}
