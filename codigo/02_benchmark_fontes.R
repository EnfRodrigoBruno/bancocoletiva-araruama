# ============================================================
# 02_benchmark_fontes.R — Diagnóstico de fontes (sandboxed)
# Versão 3.1.0 — Isolamento total por subprocesso
# ============================================================
# Cada fonte roda em callr::r() separado. Se o subprocesso estourar
# a RAM ou travar, o SO devolve memória ao parent e o benchmark
# continua. O parent nunca materializa dados.
#
# Uso:
#   source(here::here("codigo", "02_benchmark_fontes.R"))
#
# Saída:
#   registros/benchmark_fontes.csv
#   registros/fontes_prioridade_sugerida.R
# ============================================================

sys.source(
  getExportedValue("here", "here")("codigo", "00_setup.R"),
  envir = globalenv()
)
sys.source(
  getExportedValue("here", "here")("codigo", "01_utils.R"),
  envir = globalenv()
)

registrar_log("=== Início 02_benchmark_fontes.R v3.1.0 ===")
tempo_inicio <- Sys.time()

cabecalho_etapa(
  titulo    = "02_benchmark_fontes.R — Diagnóstico (sandboxed)",
  subtitulo = c(
    sprintf("RAM sistema : %.0f MB livres", mem_livre_mb()),
    sprintf("RSS parent  : %.0f MB", mem_rss_mb()),
    "Cada teste roda em subprocesso isolado (callr)"
  ),
  icone = "⚙"
)

# ------------------------------------------------------------
# 0. Guardas
# ------------------------------------------------------------
if (!requireNamespace("callr", quietly = TRUE)) {
  stop("Pacote 'callr' é obrigatório para o benchmark sandboxed.\n",
       "Instale com: install.packages('callr')", call. = FALSE)
}

MIN_RAM_PARA_RODAR <- 400    # parent precisa disso
TIMEOUT_POR_TESTE  <- 300    # 5 min por fonte (kill se estourar)
RAM_MIN_SUBPROC    <- 300    # aviso no subprocesso

checar_memoria(MIN_RAM_PARA_RODAR, abortar = TRUE,
               contexto = "início do benchmark")

# ------------------------------------------------------------
# 1. Parâmetros — escopo MÍNIMO
# ------------------------------------------------------------
ANO    <- 2023
MES    <- 1
AGRAVO <- "dengue"

# ------------------------------------------------------------
# 2. Catálogo de testes
# ------------------------------------------------------------
# Cada entrada define:
#   sistema, fonte, descr, expr (string de código R a avaliar)
#
# IMPORTANTE: `expr` roda em subprocesso, sem herdar o ambiente.
# Precisa qualificar TUDO com pacote::funcao e passar constantes
# como strings literais interpoladas aqui.

.uf        <- UF_PROJETO
.cod7      <- COD_IBGE_ARARUAMA_7
.cod6      <- COD_IBGE_ARARUAMA_6

TESTES <- list(

  list(sistema = "SIM", fonte = "healthbR",
       descr = sprintf("SIM-DO %d UF=%s (R2)", ANO, .uf),
       expr = sprintf('{
         d <- healthbR::datasus_data(
           system = "SIM-DO", year = %d, uf = "%s",
           source = "r2", lazy = FALSE
         )
         as.data.frame(d)
       }', ANO, .uf)),

  list(sistema = "SIM", fonte = "ftp_datasusr",
       descr = sprintf("SIM-DO %d (FTP+DBC)", ANO),
       expr = sprintf('{
         url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/DO%s%d.dbc"
         tmp <- tempfile(fileext = ".dbc")
         old <- getOption("timeout"); options(timeout = 300)
         on.exit(options(timeout = old), add = TRUE)
         curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
         on.exit(unlink(tmp), add = TRUE)
         datasusr::read_datasus_dbc(tmp)
       }', .uf, ANO)),

  list(sistema = "SIM", fonte = "microdatasus",
       descr = sprintf("SIM-DO %d (microdatasus)", ANO),
       expr = sprintf('{
         microdatasus::fetch_datasus(
           year_start = %d, year_end = %d,
           uf = "%s", information_system = "SIM-DO"
         )
       }', ANO, ANO, .uf)),

  list(sistema = "SINASC_MICRO", fonte = "healthbR",
       descr = sprintf("SINASC %d UF=%s (R2)", ANO, .uf),
       expr = sprintf('{
         d <- healthbR::datasus_data(
           system = "SINASC", year = %d, uf = "%s",
           source = "r2", lazy = FALSE
         )
         as.data.frame(d)
       }', ANO, .uf)),

  list(sistema = "SINASC_MICRO", fonte = "ftp_datasusr",
       descr = sprintf("SINASC %d (FTP+DBC)", ANO),
       expr = sprintf('{
         url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DN%s%d.dbc"
         tmp <- tempfile(fileext = ".dbc")
         old <- getOption("timeout"); options(timeout = 300)
         on.exit(options(timeout = old), add = TRUE)
         curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
         on.exit(unlink(tmp), add = TRUE)
         datasusr::read_datasus_dbc(tmp)
       }', .uf, ANO)),

  # --- SINAN: testamos só FTP+DBC, pois é o método do script original ---
  list(sistema = "SINAN", fonte = "ftp_datasusr",
       descr = sprintf("SINAN DENGBR %d (FTP+DBC)", ANO),
       expr = sprintf('{
         url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/DENGBR%02d.dbc"
         tmp <- tempfile(fileext = ".dbc")
         old <- getOption("timeout"); options(timeout = 300)
         on.exit(options(timeout = old), add = TRUE)
         curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
         on.exit(unlink(tmp), add = TRUE)
         datasusr::read_datasus_dbc(tmp)
       }', ANO %% 100)),

  list(sistema = "SIH", fonte = "healthbR",
       descr = sprintf("SIH-RD %d/%02d (R2)", ANO, MES),
       expr = sprintf('{
         d <- healthbR::datasus_data(
           system = "SIH-RD", year = %d, month = %d, uf = "%s",
           source = "r2", lazy = FALSE
         )
         as.data.frame(d)
       }', ANO, MES, .uf)),

  list(sistema = "SIH", fonte = "microdatasus",
       descr = sprintf("SIH-RD %d/%02d (microdatasus)", ANO, MES),
       expr = sprintf('{
         microdatasus::fetch_datasus(
           year_start = %d, year_end = %d,
           month_start = %d, month_end = %d,
           uf = "%s", information_system = "SIH-RD"
         )
       }', ANO, ANO, MES, MES, .uf)),

  list(sistema = "CNES", fonte = "healthbR",
       descr = sprintf("CNES-ST %d/%02d (R2)", ANO, MES),
       expr = sprintf('{
         d <- healthbR::datasus_data(
           system = "CNES-ST", year = %d, month = %d, uf = "%s",
           source = "r2", lazy = FALSE
         )
         as.data.frame(d)
       }', ANO, MES, .uf)),

  list(sistema = "CNES", fonte = "datasusr",
       descr = sprintf("CNES-ST %d/%02d (datasusr)", ANO, MES),
       expr = sprintf('{
         datasusr::datasus_fetch(
           source = "CNES", file_type = "ST",
           year = %d, month = %d, uf = "%s", verbose = FALSE
         )
       }', ANO, MES, .uf)),

  # --- SI-PNI: SEMPRE filtra antes de collect ---
  list(sistema = "SIPNI", fonte = "healthbR",
       descr = sprintf("SI-PNI %d/%02d (lazy + filter)", ANO, MES),
       expr = sprintf('{
         ds <- healthbR::sipni_data(
           year = %d, uf = "%s", source = "r2", lazy = TRUE
         )
         ano_str <- as.character(%d)
         mes_str <- sprintf("%%02d", %d)
         cod     <- "%s"
         ds |>
           dplyr::filter(
             .data$ano == !!ano_str,
             .data$mes == !!mes_str,
             .data$co_municipio_paciente == !!cod
           ) |>
           dplyr::collect() |>
           as.data.frame()
       }', ANO, .uf, ANO, MES, .cod6)),

  list(sistema = "POPULACAO", fonte = "brpop",
       descr = sprintf("População IBGE %d (brpop)", ANO),
       expr = sprintf('{
         d <- brpop::ibge_pop()
         as.data.frame(d[d$year == %d, ])
       }', ANO)),

  list(sistema = "POPULACAO", fonte = "sidrar",
       descr = sprintf("População SIDRA 6579 %d", ANO),
       expr = sprintf('{
         as.data.frame(sidrar::get_sidra(
           x = 6579, variable = 9324,
           period = "%d", geo = "N6",
           geo.filter = list(`N6` = %s)
         ))
       }', ANO, .cod7)),

  list(sistema = "POPULACAO", fonte = "popsus",
       descr = sprintf("POPSVS %d (FTP, filtra)", ANO),
       expr = sprintf('{
         url <- sprintf("ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/POPSBR%%02d.zip", %d - 2000)
         tmp <- tempfile(fileext = ".zip")
         tmp_dir <- tempfile("popsvs_")
         on.exit(unlink(c(tmp, tmp_dir), recursive = TRUE), add = TRUE)
         dir.create(tmp_dir, showWarnings = FALSE)
         utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
         arqs <- unzip(tmp, exdir = tmp_dir)
         dbf  <- arqs[grepl("\\\\.dbf$", arqs, ignore.case = TRUE)][1]
         d <- foreign::read.dbf(dbf, as.is = TRUE)
         d[as.character(d$COD_MUN) == "%s", , drop = FALSE]
       }', ANO, .cod7))
)

# ------------------------------------------------------------
# 3. Executor sandboxed
# ------------------------------------------------------------
#' Roda uma fonte em subprocesso e devolve o resultado do teste
#'
#' O subprocesso é morto ao terminar (sucesso ou erro).
#' O SO devolve TODA a RAM alocada dentro dele para o parent.
executar_sandbox <- function(teste) {

  cat(sprintf("  %-13s | %-38s\n", teste$sistema, teste$descr))
  cat(sprintf("               → %-14s ... ", teste$fonte))
  flush.console()

  livre_antes <- mem_livre_mb()
  rss_antes   <- mem_rss_mb()

  # --- Checagem de RAM antes de disparar subprocesso ---
  if (!checar_memoria(MIN_RAM_PARA_RODAR, abortar = FALSE,
                      contexto = sprintf("(%s/%s)",
                                         teste$sistema, teste$fonte))) {
    cat(sprintf("✗ RAM baixa (%.0f MB livres)\n", livre_antes))
    return(tibble::tibble(
      sistema = teste$sistema, fonte = teste$fonte, descr = teste$descr,
      sucesso = FALSE, pulado = TRUE, duracao_s = 0,
      n_rows = NA_integer_, n_cols = NA_integer_,
      rss_antes = rss_antes, livre_antes = livre_antes,
      livre_depois = livre_antes, erro = "RAM insuficiente"
    ))
  }

  t0 <- Sys.time()

  # --- Executa em subprocesso com timeout ---
  resultado <- tryCatch(
    callr::r(
      func = function(expr_str) {
        # Subprocesso limpo. Avalia a expressão e devolve SÓ
        # um resumo — nunca os dados.
        d <- eval(parse(text = expr_str))
        if (!is.data.frame(d)) {
          return(list(ok = FALSE, msg = "não é data.frame"))
        }
        n <- nrow(d)
        # Libera ANTES de devolver o resumo
        rm(d); gc(verbose = FALSE)
        rss <- tryCatch({
          linhas <- readLines("/proc/self/status", warn = FALSE)
          m <- grep("^VmRSS:", linhas, value = TRUE)
          if (length(m) == 0) NA_real_
          else round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
        }, error = function(e) NA_real_)
        list(ok = TRUE, n_rows = n, n_cols = NA_integer_, rss_fim = rss)
      },
      args    = list(expr_str = teste$expr),
      timeout = TIMEOUT_POR_TESTE,
      show    = FALSE,
      spinner = FALSE
    ),
    error = function(e) {
      list(ok = FALSE, msg = conditionMessage(e))
    }
  )

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  # --- Interpreta ---
  sucesso <- isTRUE(resultado$ok)
  n_rows  <- if (sucesso) resultado$n_rows else NA_integer_
  erro    <- if (!sucesso) resultado$msg else NA_character_

  # --- Checagem pós: o parent ainda tem RAM? ---
  limpar_memoria()
  livre_depois <- mem_livre_mb()

  if (sucesso) {
    cat(sprintf("✓ %.0fs | %s linhas | livre %.0f MB\n",
                dt, fmt_num(n_rows), livre_depois))
  } else {
    msg_curta <- if (!is.null(erro)) substr(erro, 1, 55) else "falha"
    cat(sprintf("✗ %.0fs | %s | livre %.0f MB\n",
                dt, msg_curta, livre_depois))
  }

  tibble::tibble(
    sistema      = teste$sistema,
    fonte        = teste$fonte,
    descr        = teste$descr,
    sucesso      = sucesso,
    pulado       = FALSE,
    duracao_s    = dt,
    n_rows       = n_rows,
    n_cols       = NA_integer_,
    rss_antes    = rss_antes,
    livre_antes  = livre_antes,
    livre_depois = livre_depois,
    erro         = erro
  )
}

# ------------------------------------------------------------
# 4. Loop principal
# ------------------------------------------------------------
cat(sprintf("\n  RAM livre: %.0f MB | RSS parent: %.0f MB\n\n",
            mem_livre_mb(), mem_rss_mb()))

resultados <- vector("list", length(TESTES))

for (i in seq_along(TESTES)) {
  # Aborta se o parent está sem RAM (raro — subprocessos não vazam)
  if (!checar_memoria(MIN_RAM_PARA_RODAR, abortar = FALSE,
                      contexto = sprintf("antes do teste %d/%d",
                                         i, length(TESTES)))) {
    cat(sprintf("\n  ⚠ RAM do parent baixa (%.0f MB). Interrompendo.\n",
                mem_livre_mb()))
    break
  }

  resultados[[i]] <- tryCatch(
    executar_sandbox(TESTES[[i]]),
    error = function(e) {
      tibble::tibble(
        sistema = TESTES[[i]]$sistema,
        fonte   = TESTES[[i]]$fonte,
        descr   = TESTES[[i]]$descr,
        sucesso = FALSE, pulado = FALSE, duracao_s = 0,
        n_rows = NA_integer_, n_cols = NA_integer_,
        rss_antes = NA_real_, livre_antes = NA_real_,
        livre_depois = NA_real_,
        erro = conditionMessage(e)
      )
    }
  )

  cat("\n")  # linha em branco entre testes
}

benchmark <- dplyr::bind_rows(resultados)

# ------------------------------------------------------------
# 5. Salvar
# ------------------------------------------------------------
arquivo_csv <- file.path(dir_registros, "benchmark_fontes.csv")
utils::write.csv(benchmark, arquivo_csv, row.names = FALSE, na = "")

# ------------------------------------------------------------
# 6. Sugerir prioridade
# ------------------------------------------------------------
sugerir_ordem <- function(df_sis) {
  df_ok <- df_sis[df_sis$sucesso & !df_sis$pulado, ]
  if (nrow(df_ok) == 0) return(character(0))
  df_ok[order(df_ok$duracao_s), "fonte"]
}

sugestoes <- list()
for (sis in unique(benchmark$sistema)) {
  sugestoes[[sis]] <- sugerir_ordem(benchmark[benchmark$sistema == sis, ])
}

cabecalho_etapa(
  titulo    = "Sugestão de prioridade",
  subtitulo = "Revise e copie para config/fontes_prioridade.R",
  icone     = "★"
)

for (sis in names(sugestoes)) {
  f <- sugestoes[[sis]]
  if (length(f) == 0) {
    cat(sprintf("  %-13s → (nenhuma fonte funcionou)\n", sis))
  } else {
    cat(sprintf("  %-13s → %s\n", sis, paste(f, collapse = " → ")))
  }
}

# ------------------------------------------------------------
# 7. Arquivo de sugestão
# ------------------------------------------------------------
caminho_sugestao <- file.path(dir_registros,
                              "fontes_prioridade_sugerida.R")
linhas <- c(
  "# ============================================================",
  "# fontes_prioridade_sugerida.R — gerado por 02_benchmark",
  sprintf("# Gerado em: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "# Revise e copie para config/fontes_prioridade.R",
  "# ============================================================",
  "",
  "FONTES_PRIORIDADE <- list("
)
for (i in seq_along(sugestoes)) {
  sis <- names(sugestoes)[i]; f <- sugestoes[[sis]]
  vals <- if (length(f) == 0) "character(0)"
          else paste0('"', f, '"', collapse = ", ")
  virgula <- if (i < length(sugestoes)) "," else ""
  linhas <- c(linhas, sprintf("  %s = c(%s)%s", sis, vals, virgula))
}
linhas <- c(linhas, ")")
writeLines(linhas, caminho_sugestao)

# ------------------------------------------------------------
# 8. Sumário
# ------------------------------------------------------------
n_total   <- nrow(benchmark)
n_ok      <- sum(benchmark$sucesso, na.rm = TRUE)
n_falhas  <- sum(!benchmark$sucesso & !benchmark$pulado, na.rm = TRUE)
n_pulados <- sum(benchmark$pulado, na.rm = TRUE)

linhas_saida <- c(
  sprintf("Testes executados : %d", n_total),
  sprintf("Sucessos          : %d", n_ok),
  sprintf("Falhas            : %d", n_falhas),
  sprintf("Pulados (RAM)     : %d", n_pulados),
  sprintf("CSV               : %s", basename(arquivo_csv)),
  sprintf("Sugestão          : %s", basename(caminho_sugestao))
)

duracao <- rodape_etapa(linhas_saida, tempo_inicio)

registrar_log(sprintf(
  "Benchmark v3.1.0: %d testes | %d ok | %d falhas | %d pulados | %.1f s",
  n_total, n_ok, n_falhas, n_pulados, duracao
))
registrar_log("=== Fim 02_benchmark_fontes.R v3.1.0 ===")