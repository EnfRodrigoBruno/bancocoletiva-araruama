# ============================================================
# 16_extrair_cnes.R — CNES: estabelecimentos de saúde
# Versão 3.1.0 — microdatasus como fonte única
# ============================================================
# Mudanças v3.1.0:
#   - FIX CRÍTICO: datasusr NÃO suporta CNES. Trocado para
#     microdatasus::fetch_datasus() com information_system =
#     "CNES-ST"/"CNES-LT"/"CNES-PF"/"CNES-EQ".
#   - Detecção defensiva da coluna de município (varia entre
#     tipos CNES).
#   - cap_mb mantido em 2000 (arrow.so).
#   - Consolidação tolerante a arquivos corrompidos mantida.
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

registrar_log("=== Início 16_extrair_cnes.R v3.1.0 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "false")

ANOS_CNES  <- 2005:2024
MESES_CNES <- 1:12

# Tipos CNES suportados pelo microdatasus
# Referência: microdatasus::fetch_datasus(information_system = "CNES-XX")
TIPOS_CNES <- c("ST", "LT", "PF", "EQ")

for (tipo in TIPOS_CNES) {
  cat(sprintf("\n═══ CNES tipo %s ═══\n", tipo))

  dir_tipo <- file.path(dir_brutos_cnes, tipo)
  dir.create(dir_tipo, recursive = TRUE, showWarnings = FALSE)

  sis_id <- sprintf("CNES-%s", tipo)

  montar_fontes_cnes <- local({
    tipo_local <- tipo
    sis_local  <- sis_id
    function(item) {
      list(
        list(nome = "microdatasus", expr = sprintf('{
          microdatasus::fetch_datasus(
            year_start  = %d, year_end  = %d,
            month_start = %d, month_end = %d,
            uf          = "%s",
            information_system = "%s",
            stop_on_error = FALSE,
            quiet = TRUE
          )
        }', item$ano, item$ano, item$mes, item$mes,
             UF_PROJETO, sis_local))
      )
    }
  })

  filtro_cnes <- function(item) {
    sprintf('{
      names(d) <- tolower(names(d))
      col_mun <- intersect(
        c("codufmun", "cod_municipio", "codmun",
          "codigo_municipio", "codigo_uf_municipio",
          "co_ufmun", "municipio", "codigo_municipio_ibge"),
        names(d)
      )[1]
      if (is.na(col_mun)) stop("coluna municipio ausente")
      x <- trimws(as.character(d[[col_mun]]))
      x[nchar(x) == 7] <- substr(x[nchar(x) == 7], 1, 6)
      d$ano_arquivo <- %d
      d$mes_arquivo <- %d
      d[x == "%s", , drop = FALSE]
    }', item$ano, item$mes, COD_IBGE_ARARUAMA_6)
  }

  itens <- list()
  for (ano in ANOS_CNES) {
    for (mes in MESES_CNES) {
      itens[[length(itens) + 1]] <- list(
        ano = ano, mes = mes,
        rotulo = sprintf("%s/%d/%02d", tipo, ano, mes)
      )
    }
  }

  extrair_por_item(
    itens            = itens,
    dir_saida        = dir_tipo,
    nome_arquivo_fn  = function(it) sprintf("cnes_%s_araruama_%d_%02d.parquet",
                                             tolower(tipo), it$ano, it$mes),
    montar_fontes_fn = montar_fontes_cnes,
    filtro_fn        = filtro_cnes,
    timeout_s        = 60,
    cap_mb           = 2000,
    forcar           = FORCAR,
    titulo           = sprintf("16_extrair_cnes.R — %s", tipo),
    min_mb_inicio    = 300
  )

  # --- Consolidação tolerante ---
  arquivos <- list.files(dir_tipo,
                         pattern = sprintf("^cnes_%s_araruama_\\d{4}_\\d{2}\\.parquet$",
                                           tolower(tipo)),
                         full.names = TRUE)
  if (length(arquivos) > 0) {
    tamanhos <- file.size(arquivos)
    ruins <- arquivos[tamanhos < 8]
    if (length(ruins) > 0) unlink(ruins)
    arquivos <- arquivos[tamanhos >= 8]

    lista <- list()
    for (f in arquivos) {
      d <- tryCatch(arrow::read_parquet(f), error = function(e) NULL)
      if (is.null(d)) next
      lista[[basename(f)]] <- d
    }

    if (length(lista) > 0) {
      tipos <- purrr::map(lista, ~ purrr::map_chr(.x, ~ class(.x)[1]))
      tipos_consol <- purrr::transpose(tipos) |> purrr::map(~ unique(unlist(.x)))
      mistas <- names(tipos_consol)[purrr::map_int(tipos_consol, length) > 1]
      if (length(mistas) > 0) {
        lista <- purrr::map(lista, function(df) {
          for (col in mistas) if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
          df
        })
      }
      consol <- dplyr::bind_rows(lista)
      rm(lista); limpar_memoria()
      salvar_parquet(consol,
                     file.path(dir_brutos_cnes,
                               sprintf("cnes_%s_araruama.parquet", tolower(tipo))))
      cat(sprintf("\n  Consolidado %s: %s registros | %d colunas\n",
                  tipo, fmt_num(nrow(consol)), ncol(consol)))
    }
  }
}

registrar_log("=== Fim 16_extrair_cnes.R v3.1.0 ===")