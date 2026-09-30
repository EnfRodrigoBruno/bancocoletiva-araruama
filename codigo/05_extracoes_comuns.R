# ============================================================
# 05_extracoes_comuns.R — Framework compartilhado de extração
# Versão 3.2.0 — Cap 2GB + timeout 30s + validação pós-write
# ============================================================
# Mudanças v3.2.0:
#   - cap_mb default 1200 → 2000 (arrow.so precisa de ~1.5GB AS)
#   - timeout_s default 300 → 60
#   - Validação pós-write: parquet precisa ter >= 8 bytes
#   - Retry adicional para OOM transitório
# ============================================================

# ------------------------------------------------------------
# 1. Sandbox por subprocesso
# ------------------------------------------------------------
executar_sandbox_extracao <- function(expr_fonte, expr_filtro, saida,
                                       timeout_s = 60, cap_mb = 2000,
                                       tentativas = 2) {

  rodar_uma_vez <- function() {
    callr::r(
      func = function(expr_fonte, expr_filtro, saida, timeout_s, cap_mb) {
        if (requireNamespace("unix", quietly = TRUE)) {
          tryCatch(
            unix::rlimit_as(as.integer(cap_mb) * 1048576L),
            error = function(e) NULL
          )
        }

        setTimeLimit(elapsed = timeout_s, transient = TRUE)
        on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)

        d <- tryCatch(
          eval(parse(text = expr_fonte)),
          error = function(e) stop("fonte: ", conditionMessage(e), call. = FALSE)
        )
        if (!is.data.frame(d) || nrow(d) == 0) {
          return(list(status = "vazio", n_baixado = 0L,
                      n_salvo = 0L, erro = NA_character_))
        }
        n_baixado <- nrow(d)

        env_filtro <- list2env(list(d = d), parent = globalenv())
        d <- tryCatch(
          eval(parse(text = expr_filtro), envir = env_filtro),
          error = function(e) stop("filtro: ", conditionMessage(e), call. = FALSE)
        )
        if (!is.data.frame(d) || nrow(d) == 0) {
          return(list(status = "vazio", n_baixado = n_baixado,
                      n_salvo = 0L, erro = NA_character_))
        }

        dir.create(dirname(saida), recursive = TRUE, showWarnings = FALSE)
        arrow::write_parquet(d, saida)
        n_salvo <- nrow(d)

        rm(d, env_filtro); gc(verbose = FALSE)

        # Validação pós-write: parquet mínimo tem 8 bytes de footer
        if (!file.exists(saida) || file.size(saida) < 8) {
          stop("parquet escrito inválido (< 8 bytes)")
        }

        list(status = "ok", n_baixado = n_baixado,
             n_salvo = n_salvo, erro = NA_character_)
      },
      args    = list(expr_fonte = expr_fonte, expr_filtro = expr_filtro,
                     saida = saida, timeout_s = timeout_s, cap_mb = cap_mb),
      timeout = timeout_s + 30,
      show    = FALSE,
      spinner = FALSE
    )
  }

  ultimo_erro <- NULL
  for (t in seq_len(tentativas)) {
    res <- tryCatch(rodar_uma_vez(), error = function(e) e)

    if (!inherits(res, "error")) return(res)

    msg <- conditionMessage(res)
    ultimo_erro <- msg

    # Retry em falhas transientes de boot/OOM
    retry_ok <- grepl("could not start R", msg, fixed = TRUE) ||
                grepl("Out of memory", msg, fixed = TRUE) ||
                grepl("memory exhausted", msg, fixed = TRUE)

    if (retry_ok && t < tentativas) {
      Sys.sleep(3)
      next
    }
    return(list(status = "erro", n_baixado = NA, n_salvo = NA,
                erro = msg))
  }

  list(status = "erro", n_baixado = NA, n_salvo = NA,
       erro = paste("após", tentativas, "tentativas:", ultimo_erro))
}

# ------------------------------------------------------------
# 2. Loop genérico de extração por item
# ------------------------------------------------------------
extrair_por_item <- function(itens,
                             dir_saida,
                             nome_arquivo_fn,
                             montar_fontes_fn,
                             filtro_fn,
                             timeout_s      = 60,
                             cap_mb         = 2000,
                             forcar         = FALSE,
                             titulo         = "Extração",
                             min_mb_inicio  = 500) {

  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Pacote 'callr' é obrigatório.", call. = FALSE)
  }

  dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)

  cabecalho_etapa(
    titulo    = titulo,
    subtitulo = c(
      sprintf("Itens    : %d", length(itens)),
      sprintf("Destino  : %s", basename(dir_saida)),
      sprintf("Cap RAM  : %d MB/item", cap_mb),
      sprintf("Timeout  : %ds/item", timeout_s),
      sprintf("RAM livre: %.0f MB", mem_livre_mb())
    ),
    icone = "▶"
  )

  t_geral <- Sys.time()
  resultados <- list()

  for (i in seq_along(itens)) {
    item    <- itens[[i]]
    rotulo  <- if (is.list(item)) item$rotulo %||% i else as.character(item)
    caminho <- file.path(dir_saida, nome_arquivo_fn(item))

    if (file.exists(caminho) && !forcar) {
      ok <- tryCatch({
        if (file.size(caminho) < 8) stop("arquivo muito pequeno")
        arrow::read_parquet(caminho, col_select = 1L); TRUE
      }, error = function(e) FALSE)

      if (ok) {
        cat(sprintf("  [%s] ⏭  existe, pulando\n", rotulo))
        resultados[[length(resultados) + 1]] <-
          tibble::tibble(item = rotulo, status = "pulado",
                         fonte = NA_character_, n = NA_integer_)
        next
      }
      cat(sprintf("  [%s] ⚠  corrompido, rebaixando\n", rotulo))
      unlink(caminho)
    }

    if (!checar_memoria(min_mb_inicio, abortar = FALSE,
                        contexto = sprintf("item %s", rotulo))) {
      cat(sprintf("  [%s] ✗ RAM baixa (%.0f MB), pulando\n",
                  rotulo, mem_livre_mb()))
      resultados[[length(resultados) + 1]] <-
        tibble::tibble(item = rotulo, status = "sem_ram",
                       fonte = NA_character_, n = NA_integer_)
      next
    }

    cat(sprintf("  [%s] baixando...\n", rotulo))
    fontes <- montar_fontes_fn(item)
    filtro <- filtro_fn(item)
    sucesso <- FALSE

    for (fonte in fontes) {
      t0 <- Sys.time()
      res <- tryCatch(
        executar_sandbox_extracao(fonte$expr, filtro, caminho,
                                   timeout_s = timeout_s, cap_mb = cap_mb),
        error = function(e) list(status = "erro", n_baixado = NA,
                                 n_salvo = NA,
                                 erro = conditionMessage(e))
      )
      dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

      if (identical(res$status, "ok")) {
        cat(sprintf("    ✓ %-14s %6.1fs | %s → %s linhas\n",
                    fonte$nome, dt,
                    fmt_num(res$n_baixado), fmt_num(res$n_salvo)))
        registrar_log(sprintf("[%s] ok via %s (%d → %d)",
                              rotulo, fonte$nome,
                              res$n_baixado, res$n_salvo))
        resultados[[length(resultados) + 1]] <-
          tibble::tibble(item = rotulo, status = "ok",
                         fonte = fonte$nome, n = res$n_salvo)
        sucesso <- TRUE
        break
      } else if (identical(res$status, "vazio")) {
        cat(sprintf("    ○ %-14s %6.1fs | vazio após filtro\n",
                    fonte$nome, dt))
      } else {
        cat(sprintf("    ✗ %-14s %6.1fs |\n", fonte$nome, dt))
        cat(sprintf("        %s\n",
                    gsub("\n", "\n        ", res$erro %||% "erro")))
        registrar_log(sprintf("[%s] falha em %s:\n%s",
                              rotulo, fonte$nome,
                              res$erro %||% "erro"), nivel = "AVISO")
      }
    }

    if (!sucesso) {
      cat(sprintf("  [%s] ✗ todas as fontes falharam\n", rotulo))
      resultados[[length(resultados) + 1]] <-
        tibble::tibble(item = rotulo, status = "falhou",
                       fonte = NA_character_, n = NA_integer_)
    }

    limpar_memoria()
    Sys.sleep(1)
  }

  resumo <- dplyr::bind_rows(resultados)
  n_ok     <- sum(resumo$status == "ok",     na.rm = TRUE)
  n_pul    <- sum(resumo$status == "pulado", na.rm = TRUE)
  n_falhou <- sum(resumo$status %in% c("falhou", "sem_ram"), na.rm = TRUE)

  linhas <- c(
    sprintf("Baixados : %d",  n_ok),
    sprintf("Pulados  : %d (cache válido)", n_pul),
    sprintf("Falhas   : %d",  n_falhou)
  )
  rodape_etapa(linhas, t_geral)
  invisible(resumo)
}

# ------------------------------------------------------------
# 3. Helper — checar existência via HEAD HTTP
# ------------------------------------------------------------
testar_ftp_arquivo <- function(url, timeout_s = 10) {
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, nobody = TRUE, connecttimeout = timeout_s,
                        timeout = timeout_s)
    con <- curl::curl(url, "rb", handle = h)
    on.exit(close(con), add = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

# ------------------------------------------------------------
# 4. Helper — download FTP com timeout real
# ------------------------------------------------------------
baixar_ftp_timeout <- function(url, destino,
                               connect_s = 30,
                               total_s   = 90) {
  h <- curl::new_handle()
  curl::handle_setopt(
    h,
    connecttimeout = connect_s,
    timeout        = total_s
  )
  curl::curl_download(url, destino, quiet = TRUE, mode = "wb", handle = h)
  if (!file.exists(destino) || file.size(destino) < 1000) {
    stop("download vazio ou inválido: ", url)
  }
  invisible(destino)
}

# ------------------------------------------------------------
# 5. Helpers de path DATASUS
# ------------------------------------------------------------
.URL_BASE_DATASUS <- "ftp://ftp.datasus.gov.br/dissemin/publicos"

url_sim_dores <- function(ano, uf) {
  sprintf("%s/SIM/CID10/DORES/DO%s%d.dbc",
          .URL_BASE_DATASUS, uf, ano)
}

url_sinasc_nov <- function(ano, uf) {
  sprintf("%s/SINASC/NOV/DNRES/DN%s%d.dbc",
          .URL_BASE_DATASUS, uf, ano)
}

url_sinasc_prelim <- function(ano, uf) {
  sprintf("%s/SINASC/PRELIM/DNRES/DN%s%d.dbc",
          .URL_BASE_DATASUS, uf, ano)
}

# ATENÇÃO: prefixo JÁ inclui "BR" (ex.: DENGBR, TUBEBR).
# Concatenar direto com o ano em 2 dígitos → DENGBR23.dbc
url_sinan <- function(prefixo, ano, subdir = c("FINAIS", "PRELIM")) {
  subdir <- match.arg(subdir)
  sprintf("%s/SINAN/DADOS/%s/%s%02d.dbc",
          .URL_BASE_DATASUS, subdir, prefixo, ano %% 100)
}

url_popsvs <- function(ano) {
  sprintf("%s/IBGE/POPSVS/POPSBR%02d.zip",
          .URL_BASE_DATASUS, ano - 2000)
}

message("[05_extracoes_comuns] Framework de extração carregado.")