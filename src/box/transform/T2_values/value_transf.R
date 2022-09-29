box::use(
  dplyr[mutate, across, case_when, distinct, select, na_if, rename, relocate],
  lubridate[parse_date_time, dmy, date, dyears],
  lubridate[as.duration, interval, NA_Date_],
  stringr[str_squish, str_detect],
  stringi[stri_trans_general],
  purrr[map],
  glue[glue]
)

box::use(LOGR = ./val_logr_transf)
box::use(OCOR = ./val_ocor_transf)
box::use(PESS = ./val_pess_transf)
box::use(VEIC = ./val_veic_transf)

box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)



#' Transformação de valores
#' @param data_set `data.frame` Dataset.
#' @export
#' 
val_transform <- function(data_set, set_name) {
  data_set <- G.LOG$log_dw(
    proc_msg = glue('Removendo whitespaces: {set_name}'),
    expr = { map(data_set, ~ mutate(., across(.fns = str_squish))) }
  )
  
  data_set$data <- G.LOG$log_dw(
    proc_msg = glue('Removendo duplicatas:  {set_name}', .trim = F),
    expr = { distinct(data_set$data) }
  )
  
  data_set$data <- G.LOG$log_dw(
    proc_msg = glue('Modificando valores:   {set_name}', .trim = F),
    expr = {
      val_transformation <- val_func_mapper(set_name)
      val_transformation(data_set$data)
    }
  )
  
  return(data_set)
}


#' Mapeia função de transformação
val_func_mapper <- function(dataset.name) {
  func <- switch(
    dataset.name,
    logradouros         = LOGR$val_transf_logradouros,
    ocorrencias         = OCOR$val_transf_ocorrencias,
    pessoas_envolvidas  = PESS$val_transf_pessoas,
    veiculos_envolvidos = VEIC$val_transf_veiculos
  )
  return(func)
}



