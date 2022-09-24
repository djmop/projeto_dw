#' Concatena conjuntos de data.frames com colunas idênticas
#' 
#' @description Data.frames contendo colunas com nomes e ordens idênticos
#'   são concatenados em um data.frame único. 
#' @param x Lista de data.frames.
#' @return Lista de data.frames concatenados (Sn = dataset, Dn = dicionário).
#' @export
#' 
bind_by_cols <- function(x) {
  box::use(
    purrr[map, keep, every], glue[glue], dplyr[bind_rows],
    stringr[str_detect], g = ./globals
  )
  assertthat::assert_that(every(x, is.data.frame))
  
  col.sets <- list()
  df.sets  <- list()
  
  s = 0; d = 0
  for (i in seq_along(x)) {
    df <- x[[i]]
    df.name <- names(x[i])
    col.set <- keep(col.sets, ~ identical(., colnames(df)))
    
    if (length(col.set) == 0) {
      is.dict <- str_detect(df.name, g$regex_patts$dict_files)
      if (is.dict) {
        d = d + 1
        col.set.nm <- glue("D{d}")
      } else {
        s = s + 1
        col.set.nm <- glue("S{s}")
      }
      col.sets[[col.set.nm]] <- colnames(df)
    } else {
      col.set.nm <- names(col.set)
    }
    df.sets[[col.set.nm]][[df.name]] <- df
  }
  df.sets <- map(df.sets, bind_rows, .id = 'arquivo_origem')
  return(df.sets)
}



#' Transformação estrutural
#' @param x list. Lista de datasets.
#' @export
#' 
str_transform <- function(datasets) {
  box::use(cli[cli_process_start, cli_process_done], glue[glue])
  datasets <- purrr::map2(
    datasets,
    names(datasets),
    function(.x, .y) {
      cli_process_start(glue('Transformação estrutural: `{.y}`'))
      str_transform <- str_func_mapper(.y)
      cli_process_done()
      return(str_transform(.x))
    }
  )
  
  datasets <- purrr::modify_depth(
    datasets, 2,
    dplyr::mutate,
    across(.fns = stringr::str_squish)
  )
  
  return(datasets)
}

#' Mapeia função de transformação
str_func_mapper <- function(dataset.name) {
  func <- switch(dataset.name,
    logradouros         = str_transf_logradouros,
    ocorrencias         = str_transf_ocorrencias,
    pessoas_envolvidas  = str_transf_pessoas,
    veiculos_envolvidos = str_transf_veiculos
  )
  return(func)
}

#' Transformação estrutural individualizada para Logradouros
str_transf_logradouros <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect]
  )
  
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL
  
  colname.conversions <- c(
    boletim_nk             = 'no_boletim',
    boletim_nk             = 'numero_boletim',
    data_hora_boletim      = 'data_boletim',
    municipio_sk           = 'no_municipio',
    municipio_sk           = 'numero_municipio',
    municipio_nome         = 'nome_municipio',
    logradouro_sk          = 'numero_logradouro',
    logradouro_sk          = 'no_logradouro',
    logradouro_tipo        = 'tipo_logradouro',
    logradouro_nome        = 'nome_logradouro',
    logr_anterior_tipo     = 'tipo_logradouro_anterior',
    logr_anterior_nome     = 'nome_logradoro_anterior',
    logr_anterior_nome     = 'nome_logradouro_anterior',
    bairro_sk              = 'numero_bairro',
    bairro_sk              = 'no_bairro',
    bairro_nome            = 'nome_bairro',
    bairro_tipo            = 'tipo_bairro',
    bairro_tipo_desc       = 'descricao_tipo_bairro',
    imovel_num             = 'numero_imovel',
    imovel_num             = 'no_imovel',
    imovel_prox_num        = 'numero_imovel_proximo',
    imovel_prox_num        = 'no_imovel_proximo',
    logradouros_sequencial = 'sequencia_logradouros',
    logradouros_sequencial = 'seq_logradouros'
  )
  
  dataset <- map(dataset, ~ rename(., any_of(colname.conversions)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}

#' Transformação estrutural individualizada para Ocorrências
str_transf_ocorrencias <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect]
  )
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL
  
  colname.conversions <- c(
    boletim_nk           = 'numero_boletim',
    data_hora_boletim    = 'data_hora_boletim',
    data_hora_inclusao   = 'data_inclusao',
    acidente_tipo_nk     = 'tipo_acidente',
    acidente_tipo_desc   = 'desc_tipo_acidente',
    tempo_sk             = 'cod_tempo',
    tempo_desc           = 'desc_tempo',
    pavimento_sk         = 'cod_pavimento',
    pavimento_desc       = 'pavimento',
    regional_sk          = 'cod_regional',
    regional_desc        = 'desc_regional',
    boletim_origem       = 'origem_boletim',
    local_sinalizado     = 'local_sinalizado',
    velocidade_permitida = 'velocidade_permitida',
    coordenada_x         = 'coordenada_x',
    coordenada_y         = 'coordenada_y',
    hora_informada       = 'hora_informada',
    indicador_fatalidade = 'indicador_fatalidade',
    ups_valor_sk         = 'valor_ups',
    ups_desc             = 'descricao_ups',
    data_alteracao_smsa  = 'data_alteracao_smsa',
    ups_antigo_valor     = 'valor_ups_antiga',
    ups_antigo_desc      = 'descricao_ups_antiga'
  )
  
  dataset <- map(dataset, ~ rename(., any_of(colname.conversions)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}

#' Transformação estrutural individualizada para Pessoas Envolvidas
str_transf_pessoas <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect]
  )
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL
  
  colname.conversions <- c(
    boletim_sk             = 'no_boletim',
    boletim_sk             = 'num_boletim',
    data_hora_boletim      = 'data_hora_boletim',
    pessoa_sequencial      = 'no_envolvido',
    pessoa_sequencial      = 'numero_envolvido',
    severidade_sk          = 'cod_severidade',
    severidade_sk          = 'codigo_severidade',
    severidade_antiga_cod  = 'cod_severidade_antiga',
    severidade_desc        = 'desc_severidade',
    declaracao_obito       = 'declaracao_obito',
    sexo_cod               = 'sexo',
    idade                  = 'idade',
    data_nascimento        = 'nascimento',
    embreagues             = 'embreagues',
    pedestre               = 'pedestre',
    pedestre               = 'indicador_pedestre',
    passageiro             = 'passageiro',
    passageiro             = 'indicador_passageiro',
    condutor               = 'condutor',
    especie_veiculo_desc   = 'especie_veiculo',
    categ_habilitacao_cod  = 'categoria_habilitacao',
    categ_habilitacao_desc = 'descricao_habilitacao',
    usa_capacete           = 'indicador_usa_capacete',
    cinto_seguranca        = 'cinto_seguranca'
  )
  
  dataset <- map(dataset, ~ rename(., any_of(colname.conversions)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}

#' Transformação estrutural individualizada para Veículos Envolvidos
str_transf_veiculos <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect]
  )
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL
  
  colname.conversions <- c(
    'boletim_sk'         = 'no_boletim',
    'boletim_sk'         = 'numero_boletim',
    'data_hora_boletim'  = 'data_hora_boletim',
    'veiculo_sequencial' = 'seq_veic',
    'veiculo_sequencial' = 'sequencial_veiculo',
    'categoria_sk'       = 'cod_categ',
    'categoria_sk'       = 'codigo_categoria',
    'categoria_desc'     = 'descricao_categoria',
    'especie_sk'         = 'cod_especie',
    'especie_sk'         = 'codigo_especie',
    'especie_desc'       = 'descricao_especie',
    'situacao_sk'        = 'cod_situacao',
    'situacao_sk'        = 'codigo_situacao',
    'situacao_desc'      = 'desc_situacao',
    'situacao_desc'      = 'descricao_situacao',
    'tipo_socorro_sk'    = 'tipo_socorro',
    'tipo_socorro_desc'  = 'desc_tipo_socorro',
    'tipo_socorro_desc'  = 'descricao_tipo_socorro'
  )
  
  dataset <- map(dataset, ~ rename(., any_of(colname.conversions)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}
