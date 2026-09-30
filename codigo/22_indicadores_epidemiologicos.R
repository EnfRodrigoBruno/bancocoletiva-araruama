# ============================================================
# 22_indicadores_epidemiologicos.R — Indicadores com IC95%
# Versão 1.0.0
# ============================================================
# Gera indicadores epidemiológicos clássicos com intervalos de
# confiança de 95%, a partir das tabelas em dados_tratados/.
#
# Métodos estatísticos:
#   - Taxas de incidência/mortalidade: Poisson exato (qgamma)
#   - Proporções (letalidade, Swaroop): Wilson score
#   - APVP: Poisson exato
#
# Saída:
#   dados_tratados/indicadores_vitais_ic.parquet
#   dados_tratados/indicadores_agravos_ic.parquet
#   dados_tratados/indicadores_hospitalares_ic.parquet
#   resultados/tabelas/indicadores_resumo.csv
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())

registrar_log("=== Início 22_indicadores_epidemiologicos.R v1.0.0 ===")
t_geral <- Sys.time()

dir_tab <- file.path(dir_resultados, "tabelas")
dir.create(dir_tab, recursive = TRUE, showWarnings = FALSE)

cabecalho_etapa(
  titulo    = "22_indicadores_epidemiologicos.R",
  subtitulo = c("IC95% por Poisson exato e Wilson score",
                sprintf("RAM livre: %.0f MB", mem_livre_mb())),
  icone = "▶"
)

# ------------------------------------------------------------
# Helpers de IC95%
# ------------------------------------------------------------

#' IC95% Poisson exato para taxas
#'
#' @param casos      contagem de eventos
#' @param denom      denominador (população ou NV)
#' @param multiplicador 1e5 para incidência, 1000 para mortalidade
ic95_poisson <- function(casos, denom, multiplicador = 1e5) {
  n <- length(casos)
  li <- ls <- taxa <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    k <- casos[i]; d <- denom[i]
    if (is.na(k) || is.na(d) || d == 0) next
    taxa[i] <- multiplicador * k / d
    if (k == 0) {
      li[i] <- 0
      ls[i] <- multiplicador * qgamma(0.975, shape = 1, rate = 1) / d
    } else {
      li[i] <- multiplicador * qgamma(0.025, shape = k,     rate = 1) / d
      ls[i] <- multiplicador * qgamma(0.975, shape = k + 1, rate = 1) / d
    }
  }
  tibble::tibble(estimativa = taxa, ic_li = li, ic_ls = ls)
}

#' IC95% Wilson score para proporções
ic95_wilson <- function(sucessos, total, multiplicador = 100) {
  n <- length(sucessos)
  p_hat <- li <- ls <- rep(NA_real_, n)
  z <- 1.959964
  for (i in seq_len(n)) {
    s <- sucessos[i]; t <- total[i]
    if (is.na(s) || is.na(t) || t == 0) next
    p <- s / t
    denom_w <- 1 + z^2 / t
    centro  <- (p + z^2 / (2 * t)) / denom_w
    margem  <- z * sqrt(p * (1 - p) / t + z^2 / (4 * t^2)) / denom_w
    p_hat[i] <- multiplicador * p
    li[i]    <- multiplicador * pmax(0, centro - margem)
    ls[i]    <- multiplicador * pmin(1, centro + margem)
  }
  tibble::tibble(estimativa = p_hat, ic_li = li, ic_ls = ls)
}

# ------------------------------------------------------------
# 1. Indicadores vitais com IC95%
# ------------------------------------------------------------
message("[22] Indicadores vitais com IC95%...")

vital <- arrow::read_parquet(
  file.path(dir_dados_tratados, "painel_vital_araruama.parquet"))

ind_vitais <- vital |>
  dplyr::mutate(
    ic_mort_geral = ic95_poisson(obitos_total, populacao, 1000),
    ic_mort_inf   = ic95_poisson(obitos_infantis, nascidos_vivos, 1000),
    ic_mort_neo   = ic95_poisson(obitos_neonatais, nascidos_vivos, 1000),
    ic_natalidade = ic95_poisson(nascidos_vivos, populacao, 1000)
  ) |>
  tidyr::unnest_wider(ic_mort_geral, names_sep = "_") |>
  tidyr::unnest_wider(ic_mort_inf,   names_sep = "_") |>
  tidyr::unnest_wider(ic_mort_neo,   names_sep = "_") |>
  tidyr::unnest_wider(ic_natalidade, names_sep = "_") |>
  dplyr::rename_with(~ paste0("tx_mort_geral_", .x),
                     dplyr::starts_with("ic_mort_geral_")) |>
  dplyr::rename_with(~ paste0("tx_mort_inf_", .x),
                     dplyr::starts_with("ic_mort_inf_")) |>
  dplyr::rename_with(~ paste0("tx_mort_neo_", .x),
                     dplyr::starts_with("ic_mort_neo_")) |>
  dplyr::rename_with(~ paste0("tx_natalidade_", .x),
                     dplyr::starts_with("ic_natalidade_"))

salvar_parquet(ind_vitais,
               file.path(dir_dados_tratados,
                         "indicadores_vitais_ic.parquet"))
message(sprintf("  ✓ indicadores vitais: %d anos", nrow(ind_vitais)))

# ------------------------------------------------------------
# 2. Indicadores de agravos com IC95%
# ------------------------------------------------------------
message("[22] Indicadores de agravos com IC95%...")

agravos <- arrow::read_parquet(
  file.path(dir_dados_tratados, "painel_agravos_araruama.parquet"))

ind_agravos <- agravos |>
  dplyr::group_by(agravo) |>
  dplyr::group_modify(function(df, key) {
    ic <- ic95_poisson(df$casos, df$populacao, 1e5)
    dplyr::bind_cols(df, ic)
  }) |>
  dplyr::ungroup() |>
  dplyr::arrange(agravo, ano)

salvar_parquet(ind_agravos,
               file.path(dir_dados_tratados,
                         "indicadores_agravos_ic.parquet"))
message(sprintf("  ✓ indicadores de agravos: %s registros",
                fmt_num(nrow(ind_agravos))))

# ------------------------------------------------------------
# 3. Indicadores hospitalares com IC95%
# ------------------------------------------------------------
message("[22] Indicadores hospitalares com IC95%...")

hosp <- arrow::read_parquet(
  file.path(dir_dados_tratados, "painel_hospitalar_araruama.parquet"))

ind_hosp <- hosp |>
  dplyr::group_by(ano, capitulo_cid) |>
  dplyr::group_modify(function(df, key) {
    ic <- ic95_poisson(df$internacoes, df$populacao, 1e5)
    dplyr::bind_cols(df, ic)
  }) |>
  dplyr::ungroup() |>
  dplyr::arrange(ano, dplyr::desc(internacoes))

salvar_parquet(ind_hosp,
               file.path(dir_dados_tratados,
                         "indicadores_hospitalares_ic.parquet"))
message(sprintf("  ✓ indicadores hospitalares: %s registros",
                fmt_num(nrow(ind_hosp))))

# ------------------------------------------------------------
# 4. Swaroop-Uemman (mortalidade proporcional ≥50 anos)
# ------------------------------------------------------------
message("[22] Calculando Swaroop-Uemman...")

sim <- arrow::read_parquet(file.path(dir_brutos_sim, "sim_araruama.parquet"))
names(sim) <- tolower(names(sim))
sim_proc <- sim |>
  dplyr::transmute(
    ano = as.integer(ano_arquivo),
    idade_anos = decodificar_idade_sim(idade)
  ) |>
  dplyr::filter(ano >= 2000, ano <= 2024)

swaroop <- sim_proc |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    obitos_total = dplyr::n(),
    obitos_50mais = sum(idade_anos >= 50, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ic = ic95_wilson(obitos_50mais, obitos_total, 100)
  ) |>
  tidyr::unnest_wider(ic, names_sep = "_") |>
  dplyr::rename(swaroop = estimativa,
                swaroop_li = ic_li,
                swaroop_ls = ic_ls)

salvar_parquet(swaroop,
               file.path(dir_dados_tratados,
                         "indicador_swaroop_araruama.parquet"))
message(sprintf("  ✓ Swaroop-Uemman: %d anos", nrow(swaroop)))

# ------------------------------------------------------------
# 5. Tabela resumo (últimos 5 anos, indicadores-chave)
# ------------------------------------------------------------
message("[22] Gerando tabela resumo...")

resumo <- ind_vitais |>
  dplyr::filter(ano >= 2020) |>
  dplyr::transmute(
    ano,
    `Natalidade (IC95%)` = sprintf("%.1f (%.1f–%.1f)",
                                    tx_natalidade_estimativa,
                                    tx_natalidade_ic_li,
                                    tx_natalidade_ic_ls),
    `Mort. geral (IC95%)` = sprintf("%.1f (%.1f–%.1f)",
                                     tx_mort_geral_estimativa,
                                     tx_mort_geral_ic_li,
                                     tx_mort_geral_ic_ls),
    `TMI (IC95%)` = sprintf("%.1f (%.1f–%.1f)",
                             tx_mort_inf_estimativa,
                             tx_mort_inf_ic_li,
                             tx_mort_inf_ic_ls),
    `Swaroop (%)` = sprintf("%.1f (%.1f–%.1f)",
                             swaroop$swaroop[swaroop$ano == ano],
                             swaroop$swaroop_li[swaroop$ano == ano],
                             swaroop$swaroop_ls[swaroop$ano == ano])
  )

readr::write_csv(resumo, file.path(dir_tab, "indicadores_resumo.csv"))

cat("\n  Resumo (2020–2024):\n")
print(resumo)

# ------------------------------------------------------------
# 6. Relatório final
# ------------------------------------------------------------
linhas_final <- c(
  sprintf("Indicadores vitais      : %d anos", nrow(ind_vitais)),
  sprintf("Indicadores de agravos  : %s registros", fmt_num(nrow(ind_agravos))),
  sprintf("Indicadores hospitalares: %s registros", fmt_num(nrow(ind_hosp))),
  sprintf("Swaroop-Uemman          : %d anos", nrow(swaroop)),
  sprintf("Tabela resumo           : resultados/tabelas/indicadores_resumo.csv")
)

duracao <- rodape_etapa(linhas_final, t_geral)
registrar_log(sprintf("=== Fim 22_indicadores_epidemiologicos.R v1.0.0 (%.1fs) ===",
                      duracao))