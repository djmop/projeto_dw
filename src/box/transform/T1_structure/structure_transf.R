box::use(
  dplyr[bind_rows, rowwise, mutate, ungroup, rename, any_of],
  janitor[make_clean_names],
  purrr[map, keep, every],
  assertthat[assert_that],
  stringr[str_detect],
  glue[glue]
)

box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)


## Concatena conjuntos de data.frames com colunas idênticas ( + ) ==============
#' 
#' @description Data.frames contendo colunas com nomes e ordens idênticos
#'   são concatenados em um data.frame único. 
#' @param data_sets Lista de data.frames.
#' @export
#' 
bind_by_cols <- function(data_sets, set_names) {
  G.LOG$log_dw(
    proc_msg = glue('Detectando conjuntos:  {set_names}', .trim = F),
    
    expr = {
      assert_that(every(data_sets, is.data.frame))
      
      dtf.sets <- list()
      col.sets <- list()
      csn.builder <- colset_name_builder$new()
      
      for (i in seq_along(data_sets)) {
        df.name <- names(data_sets[i])
        df.data <- data_sets[[i]];
        is.dict <- str_detect(df.name, G.CONST$regex_patts$dict_files)
        col.set <- keep(col.sets, ~ identical(., colnames(df.data)))
        
        if (length(col.set) > 0)
          colset.name <- names(col.set)
        else
          colset.name <- csn.builder$build_name(is.dict)
        
        dtf.sets[[colset.name]][[df.name]] <- df.data
      }
      
      dtf.sets <- map(dtf.sets, bind_rows, .id = 'arquivo_origem')
      dtf.sets
    }    
  )
}


## Gera nome para os colsets ( - ) =============================================
colset_name_builder <- { R6::R6Class(
  'colset_name_builder',
  private = list(
    d.num = 0, s.num = 0,
    get_prefix  = function(is.dict) {
      ifelse(is.dict, 'D', 'S')
    },
    get_counter = function(is.dict) {
      if (is.dict) {
        private$d.num <- private$d.num + 1
        return(private$d.num)
      } else {
        private$s.num <- private$s.num + 1
        return(private$s.num)
      }
    }
  ),
  public = list(
    build_name = function(is.dict) {
      box::use(glue[glue])
      name <- glue(private$get_prefix(is.dict), private$get_counter(is.dict))
      return(name)
    }
  )
)}





## Transformação estrutural ( + ) ==============================================
#' @param data_set `data.frame`. Dataset.
#' @param set_name `string`. Nome do dataset.
#' @export
#' 
str_transform <- function(data_set, set_name) {
  result <- G.LOG$log_dw(
    glue::glue('Ajustando estrutura:   {set_name}', .trim = F),
    expr = { do_transform(data_set) }
  )
  return(result)
}


## Função de transformação ( - ) ===============================================
#' @description Função geral de transformação estrutural
#' 
do_transform <- function(data_set) {
  is.dict <- str_detect(names(data_set), 'D')
  dicts   <- data_set[is.dict]
  dataset <- data_set[!is.dict]
  
# Processing dictionaries ------------------------------------------------------
  dicts <- bind_rows(dicts)
  dicts <- rowwise(dicts)
  dicts <- mutate(dicts, nome_do_campo = make_clean_names(nome_do_campo))
  dicts <- ungroup(dicts)

# Processing datasets ----------------------------------------------------------  
  dataset <- map(dataset, ~ rename(., any_of(G.CONST$constants$COLNAME_CONV)))
  dataset <- bind_rows(dataset)
  
  
  output <- list(data = dataset, dict = dicts)
  return(output)
}

