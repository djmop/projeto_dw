#' Árvore de diretórios
directories <- list(
  config     = here::here('config'),
  raw_data   = here::here('data', '1_raw'),
  stage_area = here::here('data', '2_stage'),
  dw         = here::here('data', '3_dw'),
  log        = here::here('log')
)


#' Cria árvore de diretórios
purrr::walk(directories, function(d) {
  if (!dir.exists(d))
    dir.create(d, recursive = TRUE)
})


#' Caminhos de diretórios
#' @export
#' 
dirs <- as.environment(directories)
lockEnvironment(dirs, bindings = TRUE)




#' Conversão de nomes de colunas
colname.conversions <- {c(
  ##  GLOBAIS
  ##  -------
  boletim_nk        = 'no_boletim',
  boletim_nk        = 'num_boletim',
  boletim_nk        = 'numero_boletim',
  data_hora_boletim = 'data_boletim',
  data_hora_boletim = 'data_hora_boletim',
  
  
  ##  LOGRADOUROS
  ##  -----------
  municipio_sk           = 'no_municipio',
  municipio_sk           = 'numero_municipio',
  municipio_nome         = 'nome_municipio',
  logradouro_sk          = 'no_logradouro',
  logradouro_sk          = 'numero_logradouro',
  logradouro_tipo        = 'tipo_logradouro',
  logradouro_nome        = 'nome_logradouro',
  logr_anterior_tipo     = 'tipo_logradouro_anterior',
  logr_anterior_nome     = 'nome_logradoro_anterior',
  logr_anterior_nome     = 'nome_logradouro_anterior',
  bairro_sk              = 'no_bairro',
  bairro_sk              = 'numero_bairro',
  bairro_nome            = 'nome_bairro',
  bairro_tipo            = 'tipo_bairro',
  bairro_tipo_desc       = 'descricao_tipo_bairro',
  imovel_num             = 'no_imovel',
  imovel_num             = 'numero_imovel',
  imovel_prox_num        = 'no_imovel_proximo',
  imovel_prox_num        = 'numero_imovel_proximo',
  logradouros_sequencial = 'sequencia_logradouros',
  logradouros_sequencial = 'seq_logradouros',
  
  
  ##  OCORRÊNCIAS
  ##  -----------
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
  ups_antigo_desc      = 'descricao_ups_antiga',
  
  
  ##  PESSOAS ENVOLVIDAS
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
  cinto_seguranca        = 'cinto_seguranca',
  
  
  ##  VEÍCULOS ENVOLVIDOS
  ##  -------------------
  veiculo_sequencial = 'seq_veic',
  veiculo_sequencial = 'sequencial_veiculo',
  categoria_sk       = 'cod_categ',
  categoria_sk       = 'codigo_categoria',
  categoria_desc     = 'descricao_categoria',
  especie_sk         = 'cod_especie',
  especie_sk         = 'codigo_especie',
  especie_desc       = 'descricao_especie',
  situacao_sk        = 'cod_situacao',
  situacao_sk        = 'codigo_situacao',
  situacao_desc      = 'desc_situacao',
  situacao_desc      = 'descricao_situacao',
  tipo_socorro_sk    = 'tipo_socorro',
  tipo_socorro_desc  = 'desc_tipo_socorro',
  tipo_socorro_desc  = 'descricao_tipo_socorro'
)}

#' Constantes
#' @export
#' 
constants <- as.environment(list(
  TIMEZONE     = 'America/Sao_Paulo',
  STR_TRANSLIT = 'Latin-ASCII',
  LOCALE       = 'Portuguese_Brazil.utf8',
  ENCODING     = 'latin1',
  COLNAME_CONV = colname.conversions
))
lockEnvironment(constants, bindings = TRUE)



#' Caminhos de arquivos
#' @export
#' 
files <- as.environment(list(
  bhtrans_urls = file.path(dirs$config, 'bhtrans_urls.csv'),
  download_log = file.path(dirs$log, 'csv_downloads.log')
))
lockEnvironment(files, bindings = TRUE)




#' Padrões REGEX
#' @export
#' 
regex_patts <- as.environment(list(
  dict_files = r'(_dict_)'
))
lockEnvironment(regex_patts, bindings = TRUE)
