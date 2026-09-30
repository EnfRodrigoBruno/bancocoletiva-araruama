# ============================================================
# 21_visualizar_base.R — Relatório exploratório visual
# Versão 1.0.0
# ============================================================
# Gera 5 figuras analíticas a partir das tabelas em dados_tratados/
# e um PDF consolidado. Salva em resultados/figuras/.
#
# Figuras:
#   1. Série temporal — mortalidade geral + TMI + natalidade
#   2. Heatmap sazonal — óbitos por ano × mês
#   3. Top causas — capítulos CID por ano (barras)
#   4. Agravos SINAN — incidência por agravo (facetas)
#   5. Pirâmide de óbitos — faixa etária × sexo
#
# Saída:
#   resultados/figuras/01_serie_temporal.png
#   resultados/figuras/02_heatmap_sazonal.png
#   resultados/figuras/03_causas_capitulo.png
#   resultados/figuras/04_agravos_sinan.png
#   resultados/figuras/05_piramide_obitos.png
#   resultados/figuras/relatorio_exploratorio.pdf
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())

registrar_log("=== Início 21_visualizar_base.R v1.0.0 ===")

t_geral <- Sys.time()

# --- Checagens de dependência ---
if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("Pacote 'patchwork' obrigatório para esta etapa.")
}

dir_fig <- file.path(dir_resultados, "figuras")
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

cabecalho_etapa(
  titulo    = "21_visualizar_base.R — Relatório exploratório",
  subtitulo = c(
    "Fonte   : dados_tratados/",
    "Saída   : resultados/figuras/",
    sprintf("RAM livre: %.0f MB", mem_livre_mb())
  ),
  icone = "▶"
)

# ------------------------------------------------------------
# Tema padrão dos gráficos
# ------------------------------------------------------------
tema_araruama <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = ggplot2::element_text(color = "grey30", size = base_size - 1),
      plot.caption     = ggplot2::element_text(color = "grey50", size = base_size - 3,
                                                hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position  = "bottom",
      strip.text       = ggplot2::element_text(face = "bold"),
      axis.title       = ggplot2::element_text(size = base_size - 1)
    )
}

# Anotações contextuais nos gráficos
ANOTACOES <- c(
  "Descontinuidade IBGE 2022 (Censo)" = 2022,
  "COVID-19" = 2021
)

# ------------------------------------------------------------
# Carrega painéis
# ------------------------------------------------------------
message("\n[21] Carregando painéis...")

vital     <- arrow::read_parquet(
  file.path(dir_dados_tratados, "painel_vital_araruama.parquet"))
mort_causa <- arrow::read_parquet(
  file.path(dir_dados_tratados, "mortalidade_causa_araruama.parquet"))
mort_mensal <- arrow::read_parquet(
  file.path(dir_dados_tratados, "mortalidade_mensal_araruama.parquet"))
agravos   <- arrow::read_parquet(
  file.path(dir_dados_tratados, "painel_agravos_araruama.parquet"))
# Para pirâmide precisamos do SIM processado — recarrega e processa
sim <- arrow::read_parquet(
  file.path(dir_brutos_sim, "sim_araruama.parquet"))
names(sim) <- tolower(names(sim))
sim_proc <- sim |>
  dplyr::transmute(
    ano_arquivo = as.integer(ano_arquivo),
    idade_anos = decodificar_idade_sim(idade),
    sexo       = decodificar_sexo_sim(sexo),
    faixa_etaria = classificar_faixa_etaria(decodificar_idade_sim(idade))
  ) |>
  dplyr::filter(ano_arquivo >= 2000, ano_arquivo <= 2024,
                sexo %in% c("Masculino", "Feminino"))

message("  ✓ painéis carregados")

# ------------------------------------------------------------
# FIGURA 1 — Série temporal
# ------------------------------------------------------------
message("[21] Gerando figura 1 — série temporal...")

# Prepara dados no formato longo
vital_long <- vital |>
  dplyr::select(ano,
                `Mortalidade geral` = taxa_mortalidade_geral,
                `Mortalidade infantil` = taxa_mortalidade_infantil,
                `Natalidade` = taxa_natalidade) |>
  tidyr::pivot_longer(-ano, names_to = "indicador", values_to = "valor") |>
  dplyr::mutate(
    indicador = factor(indicador, levels = c("Natalidade",
                                              "Mortalidade geral",
                                              "Mortalidade infantil"))
  )

fig1 <- ggplot2::ggplot(vital_long,
                        ggplot2::aes(x = ano, y = valor, color = indicador)) +
  ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
  ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
  ggplot2::geom_vline(xintercept = 2020, linetype = "dotted",
                       color = "grey40", alpha = 0.6) +
  ggplot2::geom_vline(xintercept = 2022, linetype = "dotted",
                       color = "grey40", alpha = 0.6) +
  ggplot2::annotate("text", x = 2020.2, y = Inf, label = "COVID",
                    hjust = 0, vjust = 1.5, size = 3, color = "grey30") +
  ggplot2::annotate("text", x = 2022.2, y = Inf, label = "Censo",
                    hjust = 0, vjust = 1.5, size = 3, color = "grey30") +
  ggplot2::scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  ggplot2::scale_color_manual(values = c(
    "Natalidade"           = "#1f77b4",
    "Mortalidade geral"    = "#d62728",
    "Mortalidade infantil" = "#2ca02c"
  )) +
  ggplot2::labs(
    title    = "Indicadores vitais — Araruama (RJ), 2000–2024",
    subtitle = "Taxas por 1.000 habitantes (natalidade, mortalidade geral) ou por 1.000 NV (TMI)",
    x        = NULL, y = "Taxa (por 1.000)",
    color    = NULL,
    caption  = "Fonte: SIM/SINASC/DATASUS. Elaboração própria."
  ) +
  tema_araruama()

ggplot2::ggsave(file.path(dir_fig, "01_serie_temporal.png"),
                fig1, width = 10, height = 6, dpi = 300)

message("  ✓ figura 1 salva")

# ------------------------------------------------------------
# FIGURA 2 — Heatmap sazonal
# ------------------------------------------------------------
message("[21] Gerando figura 2 — heatmap sazonal...")

fig2 <- ggplot2::ggplot(mort_mensal,
                        ggplot2::aes(x = factor(mes), y = factor(ano),
                                     fill = obitos)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.4) +
  ggplot2::scale_fill_viridis_c(option = "rocket", direction = -1,
                                 name = "Óbitos") +
  ggplot2::scale_x_discrete(labels = c("J","F","M","A","M","J",
                                        "J","A","S","O","N","D")) +
  ggplot2::labs(
    title    = "Sazonalidade da mortalidade — Araruama (RJ)",
    subtitle = "Óbitos por mês em cada ano (2000–2024)",
    x        = "Mês", y = NULL,
    caption  = "Fonte: SIM/DATASUS. Elaboração própria."
  ) +
  tema_araruama() +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 8))

ggplot2::ggsave(file.path(dir_fig, "02_heatmap_sazonal.png"),
                fig2, width = 10, height = 7, dpi = 300)

message("  ✓ figura 2 salva")

# ------------------------------------------------------------
# FIGURA 3 — Causas por capítulo CID (top 5)
# ------------------------------------------------------------
message("[21] Gerando figura 3 — causas por capítulo...")

# Top 5 capítulos por óbitos totais
top5 <- mort_causa |>
  dplyr::group_by(capitulo_cid) |>
  dplyr::summarise(total = sum(obitos, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(total)) |>
  dplyr::slice_head(n = 5) |>
  dplyr::pull(capitulo_cid)

mort_top5 <- mort_causa |>
  dplyr::filter(capitulo_cid %in% top5) |>
  dplyr::mutate(capitulo_cid = forcats::fct_reorder(capitulo_cid, obitos,
                                                     .fun = sum, .desc = TRUE))

fig3 <- ggplot2::ggplot(mort_top5,
                        ggplot2::aes(x = ano, y = proporcao,
                                     fill = capitulo_cid)) +
  ggplot2::geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_fill_viridis_d(option = "mako", direction = -1, name = NULL) +
  ggplot2::scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  ggplot2::labs(
    title    = "Composição da mortalidade por capítulo CID-10",
    subtitle = "Proporção dos 5 capítulos mais frequentes em Araruama (2000–2024)",
    x        = NULL, y = "Proporção dos óbitos",
    caption  = "Fonte: SIM/DATASUS. Elaboração própria."
  ) +
  tema_araruama() +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

ggplot2::ggsave(file.path(dir_fig, "03_causas_capitulo.png"),
                fig3, width = 10, height = 6, dpi = 300)

message("  ✓ figura 3 salva")

# ------------------------------------------------------------
# FIGURA 4 — Agravos SINAN facetados
# ------------------------------------------------------------
message("[21] Gerando figura 4 — agravos SINAN...")

fig4 <- ggplot2::ggplot(agravos,
                        ggplot2::aes(x = ano, y = incidencia_100k)) +
  ggplot2::geom_line(color = "#2c7fb8", linewidth = 0.8, na.rm = TRUE) +
  ggplot2::geom_point(color = "#2c7fb8", size = 1.2, na.rm = TRUE) +
  ggplot2::facet_wrap(~ agravo, scales = "free_y", ncol = 3) +
  ggplot2::scale_x_continuous(breaks = seq(2000, 2024, 6)) +
  ggplot2::labs(
    title    = "Incidência de agravos de notificação — Araruama (RJ)",
    subtitle = "Casos por 100.000 habitantes (SINAN)",
    x        = NULL, y = "Incidência (por 100.000)",
    caption  = "Fonte: SINAN/DATASUS. Elaboração própria. Anos sem notificação aparecem como lacuna."
  ) +
  tema_araruama(base_size = 10)

ggplot2::ggsave(file.path(dir_fig, "04_agravos_sinan.png"),
                fig4, width = 12, height = 7, dpi = 300)

message("  ✓ figura 4 salva")

# ------------------------------------------------------------
# FIGURA 5 — Pirâmide de óbitos
# ------------------------------------------------------------
message("[21] Gerando figura 5 — pirâmide de óbitos...")

pir <- sim_proc |>
  dplyr::count(faixa_etaria, sexo, name = "n") |>
  dplyr::mutate(
    n = ifelse(sexo == "Masculino", -n, n)
  )

fig5 <- ggplot2::ggplot(pir,
                        ggplot2::aes(x = faixa_etaria, y = n, fill = sexo)) +
  ggplot2::geom_col(width = 0.85) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    labels = function(x) format(abs(x), big.mark = "."),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Feminino"  = "#d95f8d",
    "Masculino" = "#3b7fc4"
  ), name = NULL) +
  ggplot2::labs(
    title    = "Pirâmide de óbitos por faixa etária e sexo",
    subtitle = "Araruama (RJ), 2000–2024 acumulado",
    x        = NULL, y = "Óbitos",
    caption  = "Fonte: SIM/DATASUS. Elaboração própria."
  ) +
  tema_araruama()

ggplot2::ggsave(file.path(dir_fig, "05_piramide_obitos.png"),
                fig5, width = 10, height = 6, dpi = 300)

message("  ✓ figura 5 salva")

# ------------------------------------------------------------
# PDF consolidado
# ------------------------------------------------------------
message("\n[21] Gerando PDF consolidado...")

caminho_pdf <- file.path(dir_fig, "relatorio_exploratorio.pdf")
grDevices::pdf(caminho_pdf, width = 11, height = 8.5, onefile = TRUE)

# Capa
grid::grid.newpage()
grid::grid.text("Araruama (RJ)\nRelatório Exploratório\nda Saúde Municipal",
                x = 0.5, y = 0.6, gp = grid::gpar(fontsize = 24, fontface = "bold"))
grid::grid.text(sprintf("Gerado em %s", format(Sys.time(), "%d/%m/%Y %H:%M")),
                x = 0.5, y = 0.35, gp = grid::gpar(fontsize = 12, col = "grey40"))
grid::grid.text("Fonte: DATASUS / IBGE / SIM / SINASC / SINAN / SIH / SIPNI",
                x = 0.5, y = 0.25, gp = grid::gpar(fontsize = 10, col = "grey50"))

print(fig1); print(fig2); print(fig3); print(fig4); print(fig5)

grDevices::dev.off()

message(sprintf("  ✓ PDF salvo: %s (%.1f KB)",
                basename(caminho_pdf),
                file.size(caminho_pdf) / 1024))

# ------------------------------------------------------------
# Relatório final
# ------------------------------------------------------------
arquivos_gerados <- list.files(dir_fig, pattern = "\\.(png|pdf)$",
                                full.names = FALSE)

linhas_final <- c(
  sprintf("Figuras PNG : %d", sum(grepl("\\.png$", arquivos_gerados))),
  sprintf("PDF         : %d", sum(grepl("\\.pdf$", arquivos_gerados))),
  sprintf("Diretório   : resultados/figuras/"),
  sprintf("Tamanho total: %.1f MB",
          sum(file.size(file.path(dir_fig, arquivos_gerados))) / 1024^2)
)

duracao <- rodape_etapa(linhas_final, t_geral)
registrar_log(sprintf("=== Fim 21_visualizar_base.R v1.0.0 (%.1fs) ===",
                      duracao))