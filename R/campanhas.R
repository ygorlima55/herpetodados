# Quadrante: várias campanhas.
# Com 2 ou mais campanhas, a unidade amostral vira ponto x campanha ("P1 C1", "P1 C2"...),
# a campanha vira o grupo da PERMANOVA (quando a aba de pontos não tem "Grupo amostral")
# e sai uma comparação entre campanhas (espécies em comum, exclusivas, mudança em cada ponto).

codigo_campanhas <- function(camp) { u <- sort(unique(camp)); setNames(paste0("C", seq_along(u)), u) }

unidades_campanha <- function(L) {
  r <- L$registros
  camp_pontos <- L$pontos$campanha
  cod <- codigo_campanhas(c(r$campanha, camp_pontos[!is.na(camp_pontos) & nzchar(camp_pontos)]))
  r$ponto_campo <- r$ponto; r$camp_cod <- unname(cod[r$campanha]); r$ponto <- paste(r$ponto, r$camp_cod)
  L$registros <- r
  P <- L$pontos
  if (is.null(P)) P <- data.frame(ponto = character(), nome = character(), fitofisionomia = character(),
    coordenadas = character(), metodo = character(), grupo_amostral = character(), esforco = numeric())
  L$pontos_campo <- P[!duplicated(P$ponto), , drop = FALSE]
  for (pt in unique(P$ponto[duplicated(P$ponto)])) {
    co <- unique(na.omit(P$coordenadas[P$ponto == pt]))
    L$pontos_campo$coordenadas[L$pontos_campo$ponto == pt] <- if (length(co) == 1) co else NA_character_
  }
  sem_grupo <- all(is.na(P$grupo_amostral) | P$grupo_amostral == "")
  L$pontos <- do.call(rbind, lapply(names(cod), function(cn) {
    especificos <- if ("campanha" %in% names(P)) which(P$campanha == cn) else integer()
    gerais <- if ("campanha" %in% names(P)) which(is.na(P$campanha) | !nzchar(P$campanha)) else seq_len(nrow(P))
    cadastro <- P[c(especificos, gerais), , drop = FALSE]
    locais <- unique(c(r$ponto_campo[r$campanha == cn], P$ponto[especificos]))
    q <- cadastro[match(locais, cadastro$ponto), , drop = FALSE]
    q$ponto <- paste(locais, cod[[cn]])
    q$nome <- paste(ifelse(is.na(q$nome), locais, q$nome), cod[[cn]])
    q$campanha <- cn
    if (sem_grupo) q$grupo_amostral <- cod[[cn]]
    q
  }))
  L$campanhas <- data.frame(codigo = unname(cod), campanha = names(cod), stringsAsFactors = FALSE)
  L
}

comparar_campanhas <- function(reg, excluidas, campanhas, pontos_info = NULL) {
  visitas <- unique(reg[c("ponto_campo", "camp_cod")])
  if (!is.null(pontos_info$campanha)) {
    cadastro <- data.frame(ponto_campo = sub(" C[0-9]+$", "", pontos_info$ponto),
      camp_cod = campanhas$codigo[match(pontos_info$campanha, campanhas$campanha)])
    visitas <- unique(rbind(visitas, cadastro[!is.na(cadastro$camp_cod), , drop = FALSE]))
  }
  r <- reg[!(reg$especie %in% excluidas) & !is.na(reg$total) & reg$total > 0, ]
  cods <- campanhas$codigo[campanhas$codigo %in% visitas$camp_cod]
  if (length(cods) < 2 || !nrow(r)) return(NULL)
  mc <- tapply(r$total, list(factor(r$camp_cod, levels = cods), r$especie), sum); mc[is.na(mc)] <- 0
  pres <- mc > 0
  resumo <- data.frame(codigo = cods, campanha = campanhas$campanha[match(cods, campanhas$codigo)],
    riqueza = rowSums(pres), individuos = rowSums(mc),
    pontos_com_registro = vapply(cods, function(c) length(unique(r$ponto_campo[r$camp_cod == c])), 1L),
    exclusivas = vapply(seq_along(cods), function(i) sum(pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0), 1L),
    stringsAsFactors = FALSE)
  fam <- r$familia[match(colnames(mc), r$especie)]
  esp <- data.frame(especie = colnames(mc), familia = fam, t(mc), check.names = FALSE, stringsAsFactors = FALSE)
  esp$em <- apply(pres, 2, function(v) if (all(v)) "todas" else paste(cods[v], collapse = " e "))
  esp <- esp[order(esp$familia, esp$especie), ]; rownames(esp) <- NULL
  pares <- do.call(rbind, combn(cods, 2, simplify = FALSE, FUN = function(p) {
    a <- pres[p[1], ]; b <- pres[p[2], ]; com <- sum(a & b)
    data.frame(campanha_a = p[1], campanha_b = p[2], comuns = com, so_a = sum(a & !b), so_b = sum(b & !a),
      jaccard = if (any(a | b)) round(com / sum(a | b), 3) else NA_real_, sorensen = if (any(a | b)) round(2 * com / (sum(a) + sum(b)), 3) else NA_real_,
      bray_curtis_sim = if (sum(mc[p, , drop = FALSE]) > 0) round(1 - as.numeric(vegan::vegdist(mc[p, , drop = FALSE], "bray")), 3) else NA_real_, stringsAsFactors = FALSE) }))
  pts <- sort(unique(visitas$ponto_campo)); pts <- pts[order(suppressWarnings(as.integer(sub("\\D+", "", pts))), pts)]
  porponto <- do.call(rbind, lapply(pts, function(p) {
    s <- lapply(cods, function(c) unique(r$especie[r$ponto_campo == p & r$camp_cod == c]))
    linha <- data.frame(ponto = p, stringsAsFactors = FALSE)
    visitado <- cods %in% visitas$camp_cod[visitas$ponto_campo == p]
    for (i in seq_along(cods)) {
      linha[[paste0("S_", cods[i])]] <- if (visitado[i]) length(s[[i]]) else NA_integer_
      linha[[paste0("N_", cods[i])]] <- if (visitado[i]) sum(r$total[r$ponto_campo == p & r$camp_cod == cods[i]]) else NA_real_
    }
    u <- unique(unlist(s)); comum <- Reduce(intersect, s)
    linha$comuns <- if (all(visitado)) length(comum) else NA_integer_
    linha$jaccard <- if (all(visitado) && length(u)) round(length(comum) / length(u), 3) else NA_real_
    linha }))
  texto <- txt(
    "Compara as listas e abundâncias de cada campanha. Para cada par, Jaccard e Sørensen quantificam a semelhança de composição, e Bray–Curtis considera a abundância.",
    paste(sprintf("%s (%s): %d espécies e %d indivíduos", resumo$codigo, resumo$campanha, resumo$riqueza, resumo$individuos), collapse = ". "),
    "A tabela por ponto compara as campanhas com registro de visita. Um campo vazio identifica uma visita sem confirmação. O Jaccard por ponto é a interseção dividida pela união das listas das campanhas.",
    "Diferença entre campanhas mistura estação, clima da noite, esforço e quem amostrou. Sem a data e o esforço de cada campanha, não dá para separar o efeito da estação do efeito do método.")
  list(resumo = resumo, especies = esp, pares = pares, por_ponto = porponto, texto = texto)
}

# ---- figuras ----
catalogo_camp <- function() list(
  campanhas = list(nome = "Campanhas: espécies em comum", padrao = "composicao", formas = list(composicao = "Espécies só numa campanha e em comum")),
  campanhas_pontos = list(nome = "Campanhas: riqueza de cada ponto", padrao = "pontos", formas = list(pontos = "Pontos ligados entre campanhas")),
  campanhas_especies = list(nome = "Campanhas: espécies e abundância", padrao = "especies", formas = list(especies = "Círculos do tamanho da abundância")),
  nmds = list(nome = "Ordenação NMDS", padrao = "grupos", formas = list(grupos = "Pontos com o contorno de cada grupo", rotulos = "Só pontos com nome"))
)

desenhar_camp <- function(chave, forma, G, e, gg, pal, k, w, h) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ","); tam <- e$fonte / 3.1; gr <- G$grupo
  if (chave %in% c("campanhas_pontos", "campanhas_especies")) forma <- sub("campanhas_", "", chave)
  if (startsWith(chave, "campanhas") && !is.null(G$campanhas)) {
    C <- G$campanhas; cods <- C$resumo$codigo
    if (forma == "pontos") {
      d <- do.call(rbind, lapply(cods, function(c) data.frame(ponto = C$por_ponto$ponto, campanha = c, S = C$por_ponto[[paste0("S_", c)]])))
      d$ponto <- factor(d$ponto, levels = rev(C$por_ponto$ponto))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(S, ponto)) + ggplot2::geom_line(ggplot2::aes(group = ponto), colour = k$tinta2, linewidth = e$linha) +
        ggplot2::geom_point(ggplot2::aes(colour = campanha), size = e$ponto * 2) +
        ggplot2::geom_text(ggplot2::aes(label = S, colour = campanha), vjust = -1.1, family = e$familia, size = tam, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = pal, name = "Campanha") + ggplot2::scale_x_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, .12))) +
        titulos_gg(e, chave, paste("Riqueza de cada ponto nas campanhas:", gr), "Espécies", NULL,
                   paste("Jaccard entre campanhas por ponto:", paste(sprintf("%s %s", C$por_ponto$ponto, fmt1(C$por_ponto$jaccard, 2)), collapse = "; "))) + tema_gg(e)))
    }
    if (forma == "especies") {
      E <- C$especies; d <- do.call(rbind, lapply(cods, function(c) data.frame(especie = E$especie, familia = E$familia, campanha = c, n = E[[c]])))
      d <- d[d$n > 0, ]; d$especie <- factor(d$especie, levels = rev(E$especie))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(campanha, especie)) + ggplot2::geom_point(ggplot2::aes(size = n, colour = campanha), show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = n), nudge_x = .28, hjust = 0, family = e$familia, size = tam * .9, colour = k$tinta) +
        ggplot2::scale_size_area(max_size = 9) + ggplot2::scale_colour_manual(values = pal) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")) +
        titulos_gg(e, chave, paste("Espécies registradas em cada campanha:", gr), NULL, NULL, "Tamanho do círculo = número de indivíduos.") + tema_gg(e) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")), w * .8, max(h, .22 * nrow(E) + 1.2)))
    }
    P <- C$pares
    d <- data.frame(par = rep(paste(P$campanha_a, "×", P$campanha_b), 3),
      parte = rep(c("Só na primeira", "Em ambas", "Só na segunda"), each = nrow(P)), v = c(P$so_a, P$comuns, P$so_b))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(par, v, fill = parte)) + ggplot2::geom_col(width = .6) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(v > 0, v, "")), position = ggplot2::position_stack(vjust = .5), family = e$familia, size = tam) +
      ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = setNames(pal[c(2,4,1)], c("Só na primeira", "Em ambas", "Só na segunda")), name = NULL) +
      titulos_gg(e, chave, paste("Espécies em comum entre as campanhas:", gr), "Campanhas", "Número de espécies",
                 paste(sprintf("%s: %s", C$resumo$codigo, C$resumo$campanha), collapse = "; ")) + tema_gg(e), w, max(h * .6, 1.8 + .26 * nrow(P))))
  }
  if (chave == "nmds" && !is.null(G$nmds$pontos)) {
    d <- G$nmds$pontos; if (is.null(d$grupo)) d$grupo <- "Pontos"; d$grupo <- as.character(d$grupo)
    nota <- sprintf("Stress = %s.%s", fmt1(G$nmds$stress, 3), if (!is.null(G$permanova$R2)) sprintf(" PERMANOVA: R² = %s; p = %s.", fmt1(G$permanova$R2, 2), fmt1(G$permanova$p, 3)) else "")
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y))
    if (forma == "grupos") {
      cas <- do.call(rbind, lapply(split(d, d$grupo), function(q) if (nrow(q) >= 3) q[grDevices::chull(q$x, q$y), ] else q))
      p <- p + ggplot2::geom_polygon(data = cas, ggplot2::aes(fill = grupo, colour = grupo), alpha = .12, linewidth = e$linha * .7, show.legend = FALSE)
    }
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = k$grade, linetype = 3) + ggplot2::geom_vline(xintercept = 0, colour = k$grade, linetype = 3) +
      ggplot2::geom_point(ggplot2::aes(colour = grupo, shape = grupo), size = e$ponto * 1.8) +
      ggplot2::geom_text(ggplot2::aes(label = ponto), vjust = -1, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::scale_colour_manual(values = pal, name = NULL) + ggplot2::scale_fill_manual(values = pal) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .15)) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = .15)) +
      ggplot2::labs(shape = NULL) + ggplot2::coord_equal() +
      titulos_gg(e, chave, paste("Ordenação NMDS (Bray-Curtis, raiz):", gr), "NMDS 1", "NMDS 2", nota) + tema_gg(e)
    return(gg(p, w, h * 1.1))
  }
  invisible()
}
