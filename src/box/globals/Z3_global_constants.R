#  .............................................................................
#  CONSTANTES                                                               ####

## vars: Conversão nomes colunas ( - ) -----------------------------------------
colname.conversions <- {c(
  
  ### _ Global ----
  boletim_nk        = 'no_boletim',
  boletim_nk        = 'num_boletim',
  boletim_nk        = 'numero_boletim',
  data_hora_boletim = 'data_boletim',
  data_hora_boletim = 'data_hora_boletim',
  
  ## _ Logradouros ---- 
  municipio_sk           = 'no_municipio',
  municipio_sk           = 'numero_municipio',
  municipio_nome         = 'nome_municipio',
  logradouro_sk          = 'no_logradouro',
  logradouro_sk          = 'numero_logradouro',
  logradouro_tipo_nk     = 'tipo_logradouro',
  logradouro_nome        = 'nome_logradouro',
  logr_anterior_tipo_nk  = 'tipo_logradouro_anterior',
  logr_anterior_nome     = 'nome_logradoro_anterior',
  logr_anterior_nome     = 'nome_logradouro_anterior',
  bairro_sk              = 'no_bairro',
  bairro_sk              = 'numero_bairro',
  bairro_nome            = 'nome_bairro',
  bairro_tipo_nk         = 'tipo_bairro',
  bairro_tipo_desc       = 'descricao_tipo_bairro',
  imovel_num             = 'no_imovel',
  imovel_num             = 'numero_imovel',
  imovel_prox_num        = 'no_imovel_proximo',
  imovel_prox_num        = 'numero_imovel_proximo',
  logradouros_sequencial = 'sequencia_logradouros',
  logradouros_sequencial = 'seq_logradouros',
  
  
  ## _ Ocorrências ----
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
  
  
  ## _ Pessoas ----
  pessoa_sequencial      = 'no_envolvido',
  pessoa_sequencial      = 'numero_envolvido',
  severidade_sk          = 'cod_severidade',
  severidade_sk          = 'codigo_severidade',
  severidade_antiga_nk   = 'cod_severidade_antiga',
  severidade_desc        = 'desc_severidade',
  declaracao_obito       = 'declaracao_obito',
  sexo_nk                = 'sexo',
  idade                  = 'idade',
  data_nascimento        = 'nascimento',
  embreagues             = 'embreagues',
  pedestre               = 'pedestre',
  pedestre               = 'indicador_pedestre',
  passageiro             = 'passageiro',
  passageiro             = 'indicador_passageiro',
  condutor               = 'condutor',
  especie_veiculo_desc   = 'especie_veiculo',
  categ_habilitacao_nk   = 'categoria_habilitacao',
  categ_habilitacao_desc = 'descricao_habilitacao',
  usa_capacete           = 'indicador_usa_capacete',
  cinto_seguranca        = 'cinto_seguranca',
  
  
  ## _ Veículos ----
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


## env: Constantes ( + ) -------------------------------------------------------
#' @export
#' 
constants <- as.environment(list(
  TIMEZONE     = 'America/Sao_Paulo',         ###_ Timezone            ####
  STR_TRANSLIT = 'Latin-ASCII',               ###_ Transliteração      ####
  LOCALE       = 'Portuguese_Brazil.utf8',    ###_ Locale              ####
  ENCODING     = 'latin1',                    ###_ Encoding            ####
  COLNAME_CONV = colname.conversions          ###_ Conversão colnames  ####
))
lockEnvironment(constants, bindings = TRUE)


## env: Regex ( + ) ------------------------------------------------------------
#' @export
#' 
regex_patts <- as.environment(list(
  dict_files = r'(_dict_)'
))
lockEnvironment(regex_patts, bindings = TRUE)

