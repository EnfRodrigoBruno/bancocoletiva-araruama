# ============================================================
# 11_extrair_sinasc_agregado.R — Nascidos vivos (TabNet)
# Versão 3.0.2 — Tenta múltiplos valores de `linha`
# ============================================================
# Mudanças v3.0.2:
#   - Tenta "Município" (acentuado), "Municipio" e "Município de
#     residência" em sequência. Foi o acento que faltava.
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

registrar_log("=== Início 11_extrair_sinasc_agregado.R v3.0.2 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

LINHAS_TENTAR <- c("Município", "Municipio", "Município de residência")

montar_fontes_sinasc_agr <- function(ano) {
  uf  <- UF_PROJETO
  mun <- "330020 ARARUAMA"

  lapply(LINHAS_TENTAR, function(linha) {
    list(nome = sprintf("tabnet_%s", gsub(" ", "_", linha)),
         expr = sprintf('{
      d <- datasus::sinasc(
        conjunto    = "nascidos_vivos",
        abrangencia = "municipio",
        uf          = "%s",
        linha       = "%s",
        coluna      = "--Não-Ativa--",
        conteudo    = 1,
        periodo     = "%d",
        filtros     = list(municipio = "%s")
      )
      as.data.frame(d)
    }', uf, linha, ano, mun))
  })
}

filtro_sinasc_agr <- function(ano) {
  sprintf('{
    col_dim <- names(d)[1]
    col_val <- names(d)[2]
    d <- d[!grepl("^TOTAL$", d[[col_dim]]), , drop = FALSE]
    d <- d[grepl("^330020", d[[col_dim]]), , drop = FALSE]
    if (nrow(d) == 0) return(d)
    d$nascidos_vivos <- suppressWarnings(as.numeric(d[[col_val]]))
    d$ano_arquivo    <- %d
    d$fonte          <- "SINASC/TabNet"
    d$status         <- if (%d <= 2024) "definitivo" else "preliminar"
    d$consultado_em  <- Sys.time()
    d[, c("ano_arquivo","nascidos_vivos","fonte","status","consultado_em")]
  }', ano, ano)
}

extrair_por_item(
  itens            = ANOS_SIM,
  dir_saida        = dir_brutos_sinasc,
  nome_arquivo_fn  = function(ano) sprintf("sinasc_araruama_%d.parquet", ano),
  montar_fontes_fn = montar_fontes_sinasc_agr,
  filtro_fn        = filtro_sinasc_agr,
  timeout_s        = 60,
  cap_mb           = 2000,
  forcar           = FORCAR,
  titulo           = "11_extrair_sinasc_agregado.R — TabNet",
  min_mb_inicio    = 300
)

arquivos <- list.files(dir_brutos_sinasc,
                       pattern = "^sinasc_araruama_\\d{4}\\.parquet$",
                       full.names = TRUE)
if (length(arquivos) > 0) {
  sinasc_total <- purrr::map(arquivos, arrow::read_parquet) |>
    dplyr::bind_rows() |>
    dplyr::arrange(ano_arquivo)
  salvar_parquet(sinasc_total,
                 file.path(dir_brutos_sinasc, "sinasc_araruama.parquet"))
  cat(sprintf("\n  Consolidado: %s nascidos vivos\n",
              fmt_num(sum(sinasc_total$nascidos_vivos, na.rm = TRUE))))
}

registrar_log("=== Fim 11_extrair_sinasc_agregado.R v3.0.2 ===")