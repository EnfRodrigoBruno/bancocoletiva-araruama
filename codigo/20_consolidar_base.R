# ============================================================
# 20_consolidar_base.R — Camada de tratamento
# Versão 1.0.0 — Indicadores municipais consolidados
# ============================================================
# Gera 6 tabelas analíticas em dados_tratados/ + relatório de
# qualidade. Cada tabela é autossuficiente e defensiva:
# detecta nomes alternativos de coluna via intersect().
#
# Entrada:
#   dados_brutos/sim/sim_araruama.parquet
#   dados_brutos/sinasc/sinasc_araruama.parquet
#   dados_brutos/sinasc_micro/sinasc_micro_araruama.parquet
#   dados_brutos/sih/sih_araruama.parquet
#   dados_brutos/sipni/sipni_api_araruama.parquet
#   dados_brutos/populacao/populacao_araruama.parquet
#   dados_brutos/populacao/populacao_araruama_idade.parquet
#   dados_brutos/sinan/{agravo}/sinan_{agravo}_araruama.parquet
#   dados_brutos/sim_preliminar/sim_araruama_{dim}.parquet
#
# Saída:
#   dados_tratados/painel_vital_araruama.parquet
#   dados_tratados/mortalidade_causa_araruama.parquet
#   dados_tratados/mortalidade_mensal_araruama.parquet
#   dados_tratados/painel_agravos_araruama.parquet
#   dados_tratados/painel_imunizacao_araruama.parquet
#   dados_tratados/painel_hospitalar_araruama.parquet
#   registros/qualidade_base.csv
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())

registrar_log("=== Início 20_consolidar_base.R v1.0.0 ===")

t_geral <- Sys.time()

# Limites temporais do painel (interseção SIM + POP + SINASC)
ANO_MIN <- 2000L
ANO_MAX <- 2024L

cabecalho_etapa(
  titulo    = "20_consolidar_base.R — Camada de tratamento",
  subtitulo = c(
    sprintf("Período : %d–%d", ANO_MIN, ANO_MAX),
    sprintf("Município: %s (%s)", MUNICIPIO_NOME, COD_IBGE_ARARUAMA_6),
    "Saída    : 6 tabelas analíticas + qualidade"
  ),
  icone = "▶"
)

# ------------------------------------------------------------
# Helpers locais
# ------------------------------------------------------------

# Snapshot de metadados para anexar a toda tabela
carimbar <- function(df) {
  df |>
    dplyr::mutate(
      consultado_em = Sys.time(),
      versao_pipeline = VERSAO_PIPELINE,
      versao_R = as.character(getRversion())
    )
}

# (relatório de qualidade consolidado no final do script)

# Lê parquet se existir; senão devolve NULL
ler_se_existir <- function(caminho) {
  if (!file.exists(caminho)) return(NULL)
  tryCatch(arrow::read_parquet(caminho), error = function(e) NULL)
}

# Detecta primeira coluna que casar com um dos nomes fornecidos
detectar_col <- function(df, candidatos) {
  hit <- intersect(candidatos, names(df))
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

message("\n[20] Carregando insumos...")

# ------------------------------------------------------------
# 1. POPULAÇÃO (total e por idade)
# ------------------------------------------------------------
pop_total <- ler_se_existir(file.path(dir_brutos_pop,
                                       "populacao_araruama.parquet"))
pop_idade <- ler_se_existir(file.path(dir_brutos_pop,
                                       "populacao_araruama_idade.parquet"))

if (is.null(pop_total)) {
  stop("populacao_araruama.parquet ausente. Rodar 17 antes do 20.")
}

# Filtra ao período e normaliza nomes
pop_total <- pop_total |>
  dplyr::filter(ano >= ANO_MIN, ano <= ANO_MAX) |>
  dplyr::select(ano, populacao) |>
  dplyr::arrange(ano)

message(sprintf("  População total: %d anos (%d–%d)",
                nrow(pop_total), min(pop_total$ano), max(pop_total$ano)))

# ------------------------------------------------------------
# 2. SIM (óbitos)
# ------------------------------------------------------------
message("[20] Processando SIM...")

sim <- ler_se_existir(file.path(dir_brutos_sim, "sim_araruama.parquet"))
if (is.null(sim)) {
  stop("sim_araruama.parquet ausente. Rodar 10 e consolidar.")
}

# Defensivo: normaliza nomes
names(sim) <- tolower(names(sim))

# Detecta colunas críticas
col_dtobito   <- detectar_col(sim, c("dtobito", "dtoiito", "dataobito"))
col_idade     <- detectar_col(sim, c("idade", "idade_obito", "idadeanos"))
col_sexo      <- detectar_col(sim, c("sexo", "cs_sexo"))
col_causabas  <- detectar_col(sim, c("causabas", "causabas_o",
                                      "causa_basica", "diag_princ"))
col_ano       <- detectar_col(sim, c("ano_arquivo", "ano", "ano_obito"))

if (any(is.na(c(col_dtobito, col_idade, col_causabas, col_ano)))) {
  stop(sprintf("SIM: colunas ausentes. Encontrei: dtobito=%s idade=%s causabas=%s ano=%s",
               col_dtobito, col_idade, col_causabas, col_ano))
}

# Processa SIM — decodifica idade, extrai ano/mês, capítulo CID
sim_proc <- sim |>
  dplyr::transmute(
    ano_arquivo = as.integer(.data[[col_ano]]),
    dtobito     = as.character(.data[[col_dtobito]]),
    idade_anos  = decodificar_idade_sim(.data[[col_idade]]),
    sexo        = decodificar_sexo_sim(.data[[col_sexo]]),
    causabas    = toupper(as.character(.data[[col_causabas]])),
    capitulo_cid = atribuir_capitulo_cid(.data[[col_causabas]]),
    faixa_etaria = classificar_faixa_etaria(
                     decodificar_idade_sim(.data[[col_idade]]))
  ) |>
  dplyr::mutate(
    mes_obito = as.integer(substr(dtobito, 3, 4)),
    apvp      = pmax(0, LIMITE_APVP - idade_anos, na.rm = TRUE)
  ) |>
  dplyr::filter(!is.na(ano_arquivo),
                ano_arquivo >= ANO_MIN, ano_arquivo <= ANO_MAX)

message(sprintf("  SIM processado: %s óbitos (%d–%d)",
                fmt_num(nrow(sim_proc)),
                min(sim_proc$ano_arquivo),
                max(sim_proc$ano_arquivo)))

# ------------------------------------------------------------
# 3. TABELA 1 — Painel Vital (1 linha/ano)
# ------------------------------------------------------------
message("[20] Gerando painel_vital...")

# 3.1 SIM — óbitos por ano com classificação infantil
sim_ano <- sim_proc |>
  dplyr::group_by(ano = ano_arquivo) |>
  dplyr::summarise(
    obitos_total        = dplyr::n(),
    obitos_infantis     = sum(idade_anos < 1, na.rm = TRUE),
    obitos_neonatais    = sum(idade_anos < 28 / 365, na.rm = TRUE),
    obitos_posneonatais = sum(idade_anos >= 28 / 365 &
                              idade_anos < 1, na.rm = TRUE),
    apvp_total          = sum(apvp, na.rm = TRUE),
    .groups = "drop"
  )

# 3.2 SINASC — nascidos vivos por ano
# Fonte primária: sinasc_araruama.parquet (agregado)
# Fallback: sinasc_micro_araruama.parquet (microdados, conta NV)
nascidos <- NULL

sinasc_agr_path   <- file.path(dir_brutos_sinasc, "sinasc_araruama.parquet")
sinasc_micro_path <- file.path(dir_brutos_sinasc_micro,
                               "sinasc_micro_araruama.parquet")

if (file.exists(sinasc_agr_path)) {
  # --- Caminho 1: agregado TabNet ---
  message("  SINASC: usando agregado (TabNet)")
  nascidos <- arrow::read_parquet(sinasc_agr_path)
  names(nascidos) <- tolower(names(nascidos))
  
  col_ano_nv <- detectar_col(nascidos, c("ano_arquivo", "ano"))
  col_nv     <- detectar_col(nascidos, c("nascidos_vivos", "nv", "total"))
  
  if (!is.na(col_ano_nv) && !is.na(col_nv)) {
    nascidos <- nascidos |>
      dplyr::transmute(
        ano = as.integer(.data[[col_ano_nv]]),
        nascidos_vivos = as.numeric(.data[[col_nv]])
      ) |>
      dplyr::filter(ano >= ANO_MIN, ano <= ANO_MAX,
                    !is.na(nascidos_vivos)) |>
      dplyr::group_by(ano) |>
      dplyr::summarise(nascidos_vivos = sum(nascidos_vivos, na.rm = TRUE),
                       .groups = "drop")
  } else {
    warning("SINASC agregado: colunas ano ou NV não encontradas")
    nascidos <- NULL
  }
}

if (is.null(nascidos) && file.exists(sinasc_micro_path)) {
  # --- Caminho 2 (fallback): microdados, conta nascimentos ---
  message("  SINASC: agregado ausente → usando microdados")
  sinasc_micro <- arrow::read_parquet(sinasc_micro_path)
  names(sinasc_micro) <- tolower(names(sinasc_micro))
  
  col_ano_micro <- detectar_col(sinasc_micro,
                                c("ano_arquivo", "ano",
                                  "ano_nasc", "anonasc"))
  
  # Se não tiver coluna de ano, extrai da data de nascimento
  if (is.na(col_ano_micro)) {
    col_dtnasc <- detectar_col(sinasc_micro,
                               c("dtnasc", "dt_nasc", "dtnascim",
                                 "data_nasc"))
    if (!is.na(col_dtnasc)) {
      # Formatos esperados: ddmmaaaa (SIM/SINASC clássico)
      sinasc_micro$ano_arquivo <- as.integer(
        substr(as.character(sinasc_micro[[col_dtnasc]]), 5, 8)
      )
      col_ano_micro <- "ano_arquivo"
    }
  }
  
  if (!is.na(col_ano_micro)) {
    nascidos <- sinasc_micro |>
      dplyr::transmute(ano = as.integer(.data[[col_ano_micro]])) |>
      dplyr::filter(!is.na(ano), ano >= ANO_MIN, ano <= ANO_MAX) |>
      dplyr::count(ano, name = "nascidos_vivos")
    
    message(sprintf("  SINASC micro: %s nascimentos em %d anos",
                    fmt_num(sum(nascidos$nascidos_vivos)),
                    nrow(nascidos)))
  } else {
    warning("SINASC micro: não consegui identificar ano de nascimento")
  }
}

if (is.null(nascidos)) {
  warning("SINASC indisponível — painel_vital ficará sem taxas de natalidade/MTI")
}

# 3.3 Monta painel_vital — tolerante a SINASC ausente
painel_vital <- pop_total |>
  dplyr::left_join(sim_ano, by = "ano")

if (!is.null(nascidos)) {
  painel_vital <- painel_vital |> dplyr::left_join(nascidos, by = "ano")
} else {
  painel_vital$nascidos_vivos <- NA_real_
}

painel_vital <- painel_vital |>
  dplyr::mutate(
    taxa_natalidade            = ifelse(!is.na(nascidos_vivos),
                                        1000 * nascidos_vivos / populacao,
                                        NA_real_),
    taxa_mortalidade_geral     = 1000 * obitos_total / populacao,
    taxa_mortalidade_infantil  = ifelse(!is.na(nascidos_vivos) & nascidos_vivos > 0,
                                        1000 * obitos_infantis / nascidos_vivos,
                                        NA_real_),
    taxa_mortalidade_neonatal  = ifelse(!is.na(nascidos_vivos) & nascidos_vivos > 0,
                                        1000 * obitos_neonatais / nascidos_vivos,
                                        NA_real_),
    apvp_taxa_100k             = 1e5 * apvp_total / populacao
  ) |>
  dplyr::arrange(ano) |>
  carimbar()

message(sprintf("  ✓ painel_vital: %d anos", nrow(painel_vital)))

# --- FIX v1.0.2: faltava salvar ---
salvar_parquet(painel_vital,
               file.path(dir_dados_tratados,
                         "painel_vital_araruama.parquet"))

# ------------------------------------------------------------
# 4. TABELA 2 — Mortalidade por capítulo CID
# ------------------------------------------------------------
message("[20] Gerando mortalidade_causa...")

mort_causa <- sim_proc |>
  dplyr::group_by(ano = ano_arquivo, capitulo_cid) |>
  dplyr::summarise(
    obitos = dplyr::n(),
    apvp   = sum(apvp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(pop_total, by = "ano") |>
  dplyr::group_by(ano) |>
  dplyr::mutate(
    proporcao  = obitos / sum(obitos, na.rm = TRUE),
    taxa_100k  = 1e5 * obitos / populacao
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(ano, dplyr::desc(obitos)) |>
  carimbar()

salvar_parquet(mort_causa,
               file.path(dir_dados_tratados,
                         "mortalidade_causa_araruama.parquet"))
message(sprintf("  ✓ mortalidade_causa: %s registros",
                fmt_num(nrow(mort_causa))))

# ------------------------------------------------------------
# 5. TABELA 3 — Mortalidade mensal
# ------------------------------------------------------------
message("[20] Gerando mortalidade_mensal...")

mort_mensal <- sim_proc |>
  dplyr::filter(!is.na(mes_obito), mes_obito >= 1, mes_obito <= 12) |>
  dplyr::count(ano = ano_arquivo, mes = mes_obito, name = "obitos") |>
  dplyr::arrange(ano, mes) |>
  carimbar()

salvar_parquet(mort_mensal,
               file.path(dir_dados_tratados,
                         "mortalidade_mensal_araruama.parquet"))
message(sprintf("  ✓ mortalidade_mensal: %s registros",
                fmt_num(nrow(mort_mensal))))

# ------------------------------------------------------------
# 6. TABELA 4 — Painel de agravos (SINAN)
# ------------------------------------------------------------
message("[20] Gerando painel_agravos...")

sinan_dir <- file.path(dir_dados_brutos, "sinan")
agravos <- list.dirs(sinan_dir, recursive = FALSE, full.names = FALSE)

lista_agravos <- list()
for (ag in agravos) {
  f <- file.path(sinan_dir, ag, sprintf("sinan_%s_araruama.parquet", ag))
  if (!file.exists(f)) next

  d <- tryCatch(arrow::read_parquet(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) next

  names(d) <- tolower(names(d))
  col_ano <- detectar_col(d, c("ano_arquivo", "ano"))
  if (is.na(col_ano)) next

  lista_agravos[[ag]] <- d |>
    dplyr::transmute(ano = as.integer(.data[[col_ano]]),
                     agravo = ag) |>
    dplyr::filter(ano >= ANO_MIN, ano <= ANO_MAX) |>
    dplyr::count(ano, agravo, name = "casos")
}

if (length(lista_agravos) > 0) {
  painel_agravos <- dplyr::bind_rows(lista_agravos) |>
    dplyr::left_join(pop_total, by = "ano") |>
    dplyr::mutate(incidencia_100k = 1e5 * casos / populacao) |>
    dplyr::arrange(agravo, ano) |>
    dplyr::group_by(agravo) |>
    dplyr::mutate(
      variacao_pct = 100 * (casos / dplyr::lag(casos) - 1)
    ) |>
    dplyr::ungroup() |>
    carimbar()

  salvar_parquet(painel_agravos,
                 file.path(dir_dados_tratados,
                           "painel_agravos_araruama.parquet"))
  message(sprintf("  ✓ painel_agravos: %s registros (%d agravos)",
                  fmt_num(nrow(painel_agravos)), length(lista_agravos)))
} else {
  warning("Nenhum agravo SINAN encontrado")
  painel_agravos <- NULL
}

# ------------------------------------------------------------
# 7. TABELA 5 — Painel de imunização (SIPNI)
# ------------------------------------------------------------
message("[20] Gerando painel_imunizacao...")

sipni_path <- file.path(dir_brutos_sipni, "sipni_api_araruama.parquet")
if (file.exists(sipni_path)) {
  sipni <- arrow::read_parquet(sipni_path)
  names(sipni) <- tolower(names(sipni))

  col_ano_sip <- detectar_col(sipni, c("ano_arquivo", "ano", "ano_vac"))
  col_imuno   <- detectar_col(sipni, c("imunobiologico", "imunobiologicos",
                                        "vacina", "ds_vacina", "no_vacina",
                                        "co_imunobiologico"))

  if (!is.na(col_ano_sip)) {
    if (!is.na(col_imuno)) {
      painel_imuno <- sipni |>
        dplyr::transmute(
          ano = as.integer(.data[[col_ano_sip]]),
          imunobiologico = as.character(.data[[col_imuno]])
        ) |>
        dplyr::filter(ano >= ANO_MIN, ano <= ANO_MAX,
                      !is.na(imunobiologico), nzchar(imunobiologico)) |>
        dplyr::count(ano, imunobiologico, name = "doses_aplicadas")
    } else {
      # Sem coluna de imunobiológico — agrega só por ano
      painel_imuno <- sipni |>
        dplyr::transmute(
          ano = as.integer(.data[[col_ano_sip]]),
          imunobiologico = "Todas"
        ) |>
        dplyr::filter(ano >= ANO_MIN, ano <= ANO_MAX) |>
        dplyr::count(ano, imunobiologico, name = "doses_aplicadas")
    }

    painel_imuno <- painel_imuno |>
      dplyr::left_join(pop_total, by = "ano") |>
      dplyr::mutate(doses_por_1000hab = 1000 * doses_aplicadas / populacao) |>
      dplyr::arrange(ano, dplyr::desc(doses_aplicadas)) |>
      carimbar()

    salvar_parquet(painel_imuno,
                   file.path(dir_dados_tratados,
                             "painel_imunizacao_araruama.parquet"))
    message(sprintf("  ✓ painel_imunizacao: %s registros",
                    fmt_num(nrow(painel_imuno))))
  } else {
    warning("SIPNI: coluna de ano não encontrada")
    painel_imuno <- NULL
  }
} else {
  warning("SIPNI não encontrado")
  painel_imuno <- NULL
}

# ------------------------------------------------------------
# 8. TABELA 6 — Painel hospitalar (SIH)
# ------------------------------------------------------------
message("[20] Gerando painel_hospitalar...")

sih_path <- file.path(dir_brutos_sih, "sih_araruama.parquet")
if (file.exists(sih_path)) {
  sih <- arrow::read_parquet(sih_path)
  names(sih) <- tolower(names(sih))

  col_ano_sih  <- detectar_col(sih, c("ano_arquivo", "ano", "ano_cmpt"))
  col_diag_sih <- detectar_col(sih, c("diag_princ", "cid_principal",
                                       "diagnostico_principal", "diagprinc"))
  col_val_sih  <- detectar_col(sih, c("val_tot", "valor_total", "vl_total"))
  col_per_sih  <- detectar_col(sih, c("dias_perm", "permanencia",
                                       "dias_permanencia"))

  if (!is.na(col_ano_sih) && !is.na(col_diag_sih)) {
    sih_proc <- sih |>
      dplyr::transmute(
        ano = as.integer(.data[[col_ano_sih]]),
        capitulo_cid = atribuir_capitulo_cid(.data[[col_diag_sih]]),
        valor = if (!is.na(col_val_sih)) as.numeric(.data[[col_val_sih]]) else NA_real_,
        dias  = if (!is.na(col_per_sih)) as.numeric(.data[[col_per_sih]]) else NA_real_
      ) |>
      dplyr::filter(ano >= ANO_MIN, ano <= ANO_MAX)

    painel_hosp <- sih_proc |>
      dplyr::group_by(ano, capitulo_cid) |>
      dplyr::summarise(
        internacoes       = dplyr::n(),
        valor_total       = sum(valor, na.rm = TRUE),
        permanencia_media = mean(dias, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::left_join(pop_total, by = "ano") |>
      dplyr::mutate(taxa_100k = 1e5 * internacoes / populacao) |>
      dplyr::arrange(ano, dplyr::desc(internacoes)) |>
      carimbar()

    salvar_parquet(painel_hosp,
                   file.path(dir_dados_tratados,
                             "painel_hospitalar_araruama.parquet"))
    message(sprintf("  ✓ painel_hospitalar: %s registros",
                    fmt_num(nrow(painel_hosp))))
  } else {
    warning("SIH: colunas essenciais não encontradas")
    painel_hosp <- NULL
  }
} else {
  warning("SIH não encontrado")
  painel_hosp <- NULL
}

# ------------------------------------------------------------
# 9. Relatório de qualidade (v1.0.1 — robusto a tabelas vazias)
# ------------------------------------------------------------
message("\n[20] Gerando relatório de qualidade...")

# Resumo real, tabela por tabela
qualidade_tabelas <- list()

if (exists("painel_vital") && nrow(painel_vital) > 0) {
  qualidade_tabelas[["painel_vital"]] <- tibble::tibble(
    tabela          = "painel_vital",
    anos_cobertos   = nrow(painel_vital),
    primeira_ano    = min(painel_vital$ano),
    ultimo_ano      = max(painel_vital$ano),
    n_registros     = nrow(painel_vital),
    n_colunas       = ncol(painel_vital),
    tem_nascidos    = "nascidos_vivos" %in% names(painel_vital) &&
      sum(!is.na(painel_vital$nascidos_vivos)) > 0
  )
}

if (exists("mort_causa") && nrow(mort_causa) > 0) {
  qualidade_tabelas[["mortalidade_causa"]] <- tibble::tibble(
    tabela          = "mortalidade_causa",
    anos_cobertos   = length(unique(mort_causa$ano)),
    primeira_ano    = min(mort_causa$ano),
    ultimo_ano      = max(mort_causa$ano),
    n_registros     = nrow(mort_causa),
    n_colunas       = ncol(mort_causa),
    tem_nascidos    = NA
  )
}

if (exists("mort_mensal") && nrow(mort_mensal) > 0) {
  qualidade_tabelas[["mortalidade_mensal"]] <- tibble::tibble(
    tabela          = "mortalidade_mensal",
    anos_cobertos   = length(unique(mort_mensal$ano)),
    primeira_ano    = min(mort_mensal$ano),
    ultimo_ano      = max(mort_mensal$ano),
    n_registros     = nrow(mort_mensal),
    n_colunas       = ncol(mort_mensal),
    tem_nascidos    = NA
  )
}

if (exists("painel_agravos") && !is.null(painel_agravos) &&
    nrow(painel_agravos) > 0) {
  qualidade_tabelas[["painel_agravos"]] <- tibble::tibble(
    tabela          = "painel_agravos",
    anos_cobertos   = length(unique(painel_agravos$ano)),
    primeira_ano    = min(painel_agravos$ano),
    ultimo_ano      = max(painel_agravos$ano),
    n_registros     = nrow(painel_agravos),
    n_colunas       = ncol(painel_agravos),
    tem_nascidos    = NA
  )
}

if (exists("painel_imuno") && !is.null(painel_imuno) &&
    nrow(painel_imuno) > 0) {
  qualidade_tabelas[["painel_imunizacao"]] <- tibble::tibble(
    tabela          = "painel_imunizacao",
    anos_cobertos   = length(unique(painel_imuno$ano)),
    primeira_ano    = min(painel_imuno$ano),
    ultimo_ano      = max(painel_imuno$ano),
    n_registros     = nrow(painel_imuno),
    n_colunas       = ncol(painel_imuno),
    tem_nascidos    = NA
  )
}

if (exists("painel_hosp") && !is.null(painel_hosp) &&
    nrow(painel_hosp) > 0) {
  qualidade_tabelas[["painel_hospitalar"]] <- tibble::tibble(
    tabela          = "painel_hospitalar",
    anos_cobertos   = length(unique(painel_hosp$ano)),
    primeira_ano    = min(painel_hosp$ano),
    ultimo_ano      = max(painel_hosp$ano),
    n_registros     = nrow(painel_hosp),
    n_colunas       = ncol(painel_hosp),
    tem_nascidos    = NA
  )
}

if (length(qualidade_tabelas) > 0) {
  qualidade <- dplyr::bind_rows(qualidade_tabelas)
  readr::write_csv(qualidade,
                   file.path(dir_registros, "qualidade_base.csv"))
  cat("\n  Qualidade por tabela:\n")
  print(qualidade)
} else {
  qualidade <- tibble::tibble()
  warning("Nenhuma tabela gerada — relatório de qualidade vazio")
}

# ------------------------------------------------------------
# 10. Relatório final
# ------------------------------------------------------------
linhas_final <- c(
  sprintf("Painel vital          : %d anos", nrow(painel_vital)),
  sprintf("Mortalidade por causa : %s registros", fmt_num(nrow(mort_causa))),
  sprintf("Mortalidade mensal    : %s registros", fmt_num(nrow(mort_mensal))),
  sprintf("Painel de agravos     : %s",
          if (!is.null(painel_agravos)) fmt_num(nrow(painel_agravos)) else "ausente"),
  sprintf("Painel de imunização  : %s",
          if (!is.null(painel_imuno)) fmt_num(nrow(painel_imuno)) else "ausente"),
  sprintf("Painel hospitalar     : %s",
          if (!is.null(painel_hosp)) fmt_num(nrow(painel_hosp)) else "ausente"),
  sprintf("Qualidade salva em    : registros/qualidade_base.csv")
)

duracao <- rodape_etapa(linhas_final, t_geral)

registrar_log(sprintf("=== Fim 20_consolidar_base.R v1.0.0 (%.1fs) ===",
                      duracao))