# Herpetodados: figuras da descritiva em colunas, atributos, curvas juntas, iNEXT explicado e diversidade por ponto.
# - Descritiva em colunas com rótulos: famílias, famílias em cada ponto (uma coluna por família), forma de registro.
# - Um gráfico para cada atributo das espécies.
# - Curva com Observada + Jackknife 1 + Bootstrap juntas.
# - iNEXT com legenda que diz o que é cada linha, e linhas do observado e dos estimadores.
# - Diversidade por ponto em números de Hill e índices com valores escritos.

ATRIBUTOS <- c(endemismo = "Distribuição (endemismo)", habito = "Hábito", dependencia = "Dependência de ambiente florestal",
               sensibilidade = "Sensibilidade a alterações", salve = "Status nacional (SALVE)", iucn = "Status global (IUCN)")

catalogo_novas <- function() c(
  list(
    familias_pontos = list(nome = "Espécies de cada família por ponto", padrao = "colunas", formas = list(
      colunas = "Uma coluna por família, com valores", empilhado = "Colunas empilhadas com valores")),
    curva_jack1_boot = list(nome = "Curvas: Observada, Jackknife 1 e Bootstrap", padrao = "linhas", formas = list(
      linhas = "Três curvas com valores", final = "Só o valor final de cada uma"))
  ),
  setNames(lapply(names(ATRIBUTOS), function(a) list(nome = paste("Atributo:", ATRIBUTOS[[a]]), padrao = "colunas", formas = list(
    colunas = "Colunas com número e %", barras = "Barras deitadas com número e %"))), paste0("atributo_", names(ATRIBUTOS)))
)

# cor fixa para cada família (a mesma em todas as figuras)
cores_familias <- function(fams, pal) {
  u <- sort(unique(fams)); n <- length(u)
  extra <- if (n > length(pal)) grDevices::adjustcolor(pal, red.f = .7, green.f = .7, blue.f = .7) else NULL
  setNames(rep(c(pal, extra), length.out = n), u)
}

desenhar_novas <- function(chave, forma, G, e, gg, pal, k, w, h) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
  pc <- function(x) paste0(fmt1(100 * x, 0), "%")
  D <- G$descritiva; tam <- e$fonte / 3.1
  rotx <- ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))

  if (chave == "familias_pontos" && !is.null(D)) {
    le <- D$especies; pts <- D$pontos$ponto
    d <- do.call(rbind, lapply(pts, function(p) { z <- le[le[[p]] > 0, ]; if (!nrow(z)) return(NULL)
      t <- table(z$familia); data.frame(ponto = p, familia = names(t), n = as.integer(t)) }))
    d$ponto <- factor(d$ponto, levels = pts); cf <- cores_familias(le$familia, pal)
    if (forma == "empilhado") return(gg(ggplot2::ggplot(d, ggplot2::aes(ponto, n, fill = familia)) +
      ggplot2::geom_col(width = .66, colour = k$fundo, linewidth = .4) +
      ggplot2::geom_text(ggplot2::aes(label = n), position = ggplot2::position_stack(vjust = .5), family = e$familia, size = tam, colour = "white", fontface = "bold") +
      ggplot2::stat_summary(fun = sum, ggplot2::aes(label = ggplot2::after_stat(y), group = ponto, fill = NULL), geom = "text", vjust = -.5, family = e$familia, size = tam * 1.1, colour = k$tinta, fontface = "bold") +
      ggplot2::scale_fill_manual(values = cf, name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
      titulos_gg(e, chave, "Número de espécies de cada família por ponto amostral", NULL, "Número de espécies", "Número em cima da coluna = total de espécies do ponto.") + tema_gg(e)))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(ponto, n, fill = familia)) +
      ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single", padding = .12), width = .85) +
      ggplot2::geom_text(ggplot2::aes(label = n, group = familia), position = ggplot2::position_dodge2(width = .85, preserve = "single", padding = .12),
                         vjust = -.4, family = e$familia, size = tam * .95, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cf, name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12)), breaks = function(l) unique(floor(pretty(l)))) +
      titulos_gg(e, chave, "Número de espécies de cada família por ponto amostral", NULL, "Número de espécies") + tema_gg(e), w * 1.15, h))
  }

  if (startsWith(chave, "atributo_") && !is.null(D)) {
    a <- sub("^atributo_", "", chave); t <- D$categorias[[a]]; if (is.null(t) || !nrow(t)) return(invisible())
    t$rot <- paste0(t$especies_n, " (", pc(t$especies_pct), ")")
    t$classe <- factor(t$classe, levels = t$classe[order(-t$especies_n)])
    tt <- paste(ATRIBUTOS[[a]], "das espécies registradas")
    if (forma == "barras") return(gg(ggplot2::ggplot(t, ggplot2::aes(especies_n, stats::reorder(classe, especies_n))) + ggplot2::geom_col(fill = pal[1], width = .6) +
      ggplot2::geom_text(ggplot2::aes(label = rot), hjust = -.12, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .25))) +
      titulos_gg(e, chave, tt, "Número de espécies", NULL) + tema_gg(e), w * .85, max(2.4, .55 * nrow(t) + 1.4)))
    return(gg(ggplot2::ggplot(t, ggplot2::aes(classe, especies_n)) + ggplot2::geom_col(fill = pal[1], width = .55) +
      ggplot2::geom_text(ggplot2::aes(label = rot), vjust = -.5, family = e$familia, size = tam * 1.05, colour = k$tinta, fontface = "bold") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
      titulos_gg(e, chave, tt, NULL, "Número de espécies", sprintf("%d espécies; porcentagem sobre o total.", sum(t$especies_n))) + tema_gg(e) +
      (if (nrow(t) > 4 || max(nchar(as.character(t$classe))) > 18) rotx), w * .85, h * .9))
  }

  if (chave == "curva_jack1_boot" && !is.null(G$acumulacao$poolaccum)) {
    c1 <- G$acumulacao$curva; pool <- G$acumulacao$poolaccum
    d <- rbind(data.frame(serie = "Riqueza observada", pontos = c1$pontos, v = c1$riqueza, li = c1$li, ls = c1$ls),
               data.frame(serie = "Jackknife 1", pontos = pool$pontos[pool$estimador == "jack1"], v = pool$media[pool$estimador == "jack1"], li = pool$li[pool$estimador == "jack1"], ls = pool$ls[pool$estimador == "jack1"]),
               data.frame(serie = "Bootstrap", pontos = pool$pontos[pool$estimador == "boot"], v = pool$media[pool$estimador == "boot"], li = pool$li[pool$estimador == "boot"], ls = pool$ls[pool$estimador == "boot"]))
    d$serie <- factor(d$serie, levels = c("Jackknife 1", "Bootstrap", "Riqueza observada"))
    cs <- c("Jackknife 1" = pal[2], "Bootstrap" = pal[4], "Riqueza observada" = k$tinta)
    fim <- do.call(rbind, lapply(split(d, d$serie), function(z) z[which.max(z$pontos), ]))
    fim$rot <- paste0(fim$serie, ": ", fmt1(fim$v))
    p <- ggplot2::ggplot(d, ggplot2::aes(pontos, v, colour = serie, fill = serie)) +
      { if (isTRUE(e$faixa)) ggplot2::geom_ribbon(ggplot2::aes(ymin = li, ymax = ls), alpha = .1, colour = NA) } +
      ggplot2::geom_line(linewidth = e$linha * 1.2) + ggplot2::geom_point(size = e$ponto * 1.2)
    if (forma == "final") p <- p + ggplot2::geom_label(data = fim, ggplot2::aes(label = rot), hjust = -.08, family = e$familia, size = tam, fill = k$fundo, linewidth = 0, fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_x_continuous(breaks = unique(d$pontos), expand = ggplot2::expansion(mult = c(.04, .35)))
    else {
      d$vj <- ifelse(d$serie == "Riqueza observada", 1.7, ifelse(d$serie == "Jackknife 1", -.8, .5)); d$hj <- ifelse(d$serie == "Bootstrap", -.35, .5)
      p <- p + ggplot2::geom_text(data = d, ggplot2::aes(label = fmt1(v), vjust = vj, hjust = hj), family = e$familia, size = tam * .95, fontface = "bold", show.legend = FALSE) +
        ggplot2::scale_x_continuous(breaks = unique(d$pontos), expand = ggplot2::expansion(mult = c(.05, .12)))
    }
    return(gg(p + ggplot2::scale_colour_manual(values = cs, name = NULL) + ggplot2::scale_fill_manual(values = cs, name = NULL) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.1, .12))) +
      titulos_gg(e, chave, "Riqueza observada e estimada (Jackknife 1 e Bootstrap)", "Número de pontos amostrais", "Número de espécies",
                 "Observada: vegan::specaccum; estimadores: vegan::poolaccum. Faixas = IC 95%.") + tema_gg(e)))
  }
  invisible()
}

# ---- iNEXT legível: curva de riqueza com observado e estimadores ----
inext_claro <- function(chave, forma, G, e, gg, pal, k, w, h) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
  por_ind <- chave == "inext_individuos"; z <- if (por_ind) G$inext$abundancia else G$inext$incidencia
  if (is.null(z)) return(invisible())
  z$parte <- ifelse(z$metodo == "Extrapolation", "Extrapolação (projeção além da amostra)", "Rarefação (dentro da amostra real)")
  # o ponto da amostra real também começa a parte tracejada (sem buraco entre as duas)
  liga <- z[z$metodo == "Observed", ]; if (nrow(liga)) { liga$parte <- "Extrapolação (projeção além da amostra)"; z <- rbind(z, liga) }
  inteiro <- ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l))), expand = ggplot2::expansion(mult = c(.02, .04)))
  leg_v <- ggplot2::theme(legend.box = "vertical", legend.box.just = "left", legend.position = "bottom", legend.title.position = "top", legend.justification = "left")
  xlab <- if (por_ind) "Número de indivíduos amostrados" else "Número de pontos amostrais"
  tam <- e$fonte / 3.1; lt <- c("Rarefação (dentro da amostra real)" = "solid", "Extrapolação (projeção além da amostra)" = "22")
  if (forma == "hill") {
    nq <- c("0" = "Riqueza (q = 0)", "1" = "Espécies comuns: Shannon (q = 1)", "2" = "Espécies dominantes: Simpson (q = 2)")
    z$qn <- factor(nq[as.character(z$q)], levels = nq); ob <- z[z$metodo == "Observed" & z$parte != "Extrapolação (projeção além da amostra)", ]
    return(gg(ggplot2::ggplot(z, ggplot2::aes(m, qD, colour = qn, fill = qn)) +
      { if (isTRUE(e$faixa)) ggplot2::geom_ribbon(ggplot2::aes(ymin = li, ymax = ls, group = qn), alpha = .1, colour = NA) } +
      ggplot2::geom_line(ggplot2::aes(linetype = parte, group = interaction(qn, parte)), linewidth = e$linha * 1.1) +
      ggplot2::geom_point(data = ob, size = e$ponto * 1.8, shape = 21, colour = k$fundo, stroke = .8) +
      ggplot2::geom_label(data = ob, ggplot2::aes(label = fmt1(qD)), vjust = -.6, family = e$familia, size = tam, fill = k$fundo, linewidth = 0, fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = pal[1:3], name = NULL) + ggplot2::scale_fill_manual(values = pal[1:3], name = NULL) + ggplot2::scale_linetype_manual(values = lt, name = NULL) +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, order = 1), linetype = ggplot2::guide_legend(ncol = 1, order = 2), fill = "none") +
      titulos_gg(e, chave, paste("Diversidade em números de Hill por", if (por_ind) "indivíduos" else "pontos"), xlab, "Número efetivo de espécies",
                 "Círculo = valor da amostra real. Faixas = IC 95% (bootstrap, 200 réplicas). iNEXT.") + tema_gg(e) + leg_v + (if (!por_ind) inteiro), w, h * 1.3))
  }
  z <- z[z$q == 0, ]; ob <- z[z$metodo == "Observed" & z$parte != "Extrapolação (projeção além da amostra)", ]
  a <- if (por_ind) G$inext$assintotico_abund else G$inext$assintotico_inc
  est <- G$acumulacao$estimadores
  ref <- data.frame(nome = character(), v = numeric())
  ref <- rbind(ref, data.frame(nome = "Riqueza observada", v = G$riqueza))
  if (!is.null(a)) ref <- rbind(ref, data.frame(nome = if (por_ind) "Chao 1 (iNEXT)" else "Chao 2 (iNEXT)", v = a$estimado[1]))
  if (!por_ind && !is.null(est)) for (n in c("Jackknife 1", "Bootstrap")) if (n %in% est$estimador) ref <- rbind(ref, data.frame(nome = n, v = est$valor[est$estimador == n]))
  ref$rot <- paste0(ref$nome, " = ", fmt1(ref$v))
  ref$rot <- factor(ref$rot, levels = ref$rot)
  cr <- setNames(c(k$tinta, pal[c(2, 4, 3)])[seq_len(nrow(ref))], levels(ref$rot))
  xmax <- max(z$m)
  return(gg(ggplot2::ggplot(z, ggplot2::aes(m, qD)) +
    { if (isTRUE(e$faixa)) ggplot2::geom_ribbon(ggplot2::aes(ymin = li, ymax = ls), fill = pal[1], alpha = .14) } +
    ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = v, colour = rot), linetype = 3, linewidth = e$linha * .9) +
    ggplot2::geom_line(ggplot2::aes(linetype = parte, group = parte), colour = pal[1], linewidth = e$linha * 1.3) +
    ggplot2::geom_point(data = ob, colour = pal[1], size = e$ponto * 2) +
    ggplot2::geom_label(data = ob, ggplot2::aes(label = paste0("amostra real: ", fmt1(qD, 0), " espécies")), vjust = 1.6, hjust = .1, family = e$familia, size = tam, fill = k$fundo, linewidth = 0, colour = pal[1]) +
    ggplot2::scale_colour_manual(values = cr, name = "Linhas pontilhadas") + ggplot2::scale_linetype_manual(values = lt, name = "Curva de riqueza") +
    (if (!por_ind) inteiro) +
    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = 1, order = 1), colour = ggplot2::guide_legend(ncol = 1, order = 2)) +
    titulos_gg(e, chave, paste("Riqueza por rarefação e extrapolação (por", if (por_ind) "indivíduos)" else "pontos)"), xlab, "Número de espécies",
               "Faixa = IC 95% da curva. Pontilhadas = riqueza observada e estimadores. iNEXT (Chao et al. 2014).") + tema_gg(e) + leg_v, w, h * 1.4))
}
