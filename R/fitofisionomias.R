# Herpetodados: análise por fitofisionomia dos pontos (coluna "Fitofisionomia" da aba de pontos).
# Duas leituras:
# - classe como está escrita (ex.: "MG/VR" é uma classe);
# - componente: cada sigla de um ponto misto conta separado (P1 "MG/VR" entra em MG e em VR).
# Para cada uma: pontos, espécies, indivíduos, espécies exclusivas e a lista; similaridade (Jaccard) entre fitofisionomias.

partes_fito <- function(x) { v <- trimws(unlist(strsplit(x, "[/+,;]| e ", perl = TRUE))); v[nzchar(v)] }

# nomes das siglas: arquivo siglas_fitofisionomias.csv na pasta do programa (sigla;nome), editável.
# A aba de pontos também pode trazer uma coluna "Legenda" ou outra aba "Siglas" no futuro; por ora, o arquivo.
SIGLAS_FITO <- local({
  f <- file.path(dirname(dirname(normalizePath(sys.frame(1)$ofile %||% "R/x.R", mustWork = FALSE))), "siglas_fitofisionomias.csv")
  if (!file.exists(f)) f <- "siglas_fitofisionomias.csv"
  if (file.exists(f)) { d <- utils::read.csv2(f, encoding = "UTF-8", fileEncoding = "UTF-8", stringsAsFactors = FALSE); setNames(trimws(d$nome), toupper(trimws(d$sigla))) } else character()
})
nome_fito <- function(x) vapply(x, function(f) { p <- partes_fito(f); if (!length(p)) return(f)
  n <- ifelse(toupper(p) %in% names(SIGLAS_FITO), SIGLAS_FITO[toupper(p)], p); paste(n, collapse = " + ") }, "", USE.NAMES = FALSE)

analisar_fito <- function(m, pontos_info, fam = NULL) {
  if (is.null(pontos_info) || all(is.na(pontos_info$fitofisionomia))) return(NULL)
  fito <- pontos_info$fitofisionomia[match(rownames(m), pontos_info$ponto)]
  if (all(is.na(fito))) return(NULL)
  fito[is.na(fito) | fito == ""] <- "Sem fitofisionomia"
  ok <- rowSums(m) >= 0
  resumo <- function(grupos) {   # grupos: lista nome -> pontos
    nomes <- names(grupos)
    pres <- t(vapply(grupos, function(p) colSums(m[p, , drop = FALSE]) > 0, logical(ncol(m))))
    ab <- t(vapply(grupos, function(p) colSums(m[p, , drop = FALSE]), numeric(ncol(m))))
    if (length(grupos) == 1) { pres <- matrix(pres, 1, dimnames = list(nomes, colnames(m))); ab <- matrix(ab, 1, dimnames = list(nomes, colnames(m))) }
    excl <- vapply(seq_along(nomes), function(i) sum(pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0), 1L)
    tab <- data.frame(fitofisionomia = nomes, nome = nome_fito(nomes), pontos = vapply(grupos, paste, "", collapse = ", "), n_pontos = lengths(grupos),
      riqueza = rowSums(pres), abundancia = rowSums(ab), riqueza_media_por_ponto = round(vapply(grupos, function(p) mean(rowSums(m[p, , drop = FALSE] > 0)), 1), 1),
      exclusivas = excl,
      especies_exclusivas = vapply(seq_along(nomes), function(i) paste(colnames(m)[pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0], collapse = "; "), ""),
      stringsAsFactors = FALSE)
    tab <- tab[order(-tab$riqueza), ]; rownames(tab) <- NULL
    esp <- data.frame(familia = if (!is.null(fam)) unname(fam[colnames(m)]) else NA, especie = colnames(m), t(ab), check.names = FALSE, stringsAsFactors = FALSE)
    esp$em <- apply(pres, 2, function(v) paste(rownames(pres)[v], collapse = ", "))
    esp <- esp[order(esp$familia, esp$especie), ]; rownames(esp) <- NULL
    pares <- if (length(nomes) >= 2) do.call(rbind, combn(nomes, 2, simplify = FALSE, FUN = function(p) {
      a <- pres[p[1], ]; b <- pres[p[2], ]; data.frame(fito_a = p[1], fito_b = p[2], comuns = sum(a & b), so_a = sum(a & !b), so_b = sum(b & !a),
        jaccard = round(sum(a & b) / max(1, sum(a | b)), 3), stringsAsFactors = FALSE) })) else NULL
    if (!is.null(pares)) pares <- pares[order(-pares$jaccard), ]
    list(tabela = tab, especies = esp, pares = pares)
  }
  cls <- split(rownames(m), fito)
  comp_nomes <- unique(unlist(lapply(fito, partes_fito)))
  comp <- setNames(lapply(comp_nomes, function(cn) rownames(m)[vapply(fito, function(f) cn %in% partes_fito(f), TRUE)]), comp_nomes)
  A <- resumo(cls); B <- if (length(comp) >= 2 && !identical(sort(names(comp)), sort(names(cls)))) resumo(comp) else NULL
  t1 <- A$tabela; um <- t1$n_pontos == 1
  txtres <- sprintf("%d fitofisionomias como escritas na planilha (%s). A mais rica foi %s (%d espécies em %d ponto%s) e a com mais espécies só dela foi %s (%d exclusivas).%s",
    nrow(t1), paste(sprintf("%s: %s", t1$nome, t1$pontos), collapse = "; "), t1$nome[1], t1$riqueza[1], t1$n_pontos[1], if (t1$n_pontos[1] > 1) "s" else "",
    t1$nome[which.max(t1$exclusivas)], max(t1$exclusivas),
    if (!is.null(B)) sprintf(" Contando cada sigla separada: %s.", paste(sprintf("%s %d espécies (%d %s)", B$tabela$nome, B$tabela$riqueza, B$tabela$n_pontos, ifelse(B$tabela$n_pontos == 1, "ponto", "pontos")), collapse = "; ")) else "")
  list(classes = A, componentes = B, texto = txt(
    "Junta os pontos pela fitofisionomia da aba de pontos e compara o que cada ambiente tem: espécies, indivíduos, espécies que só aparecem nele (exclusivas) e quanto as listas se parecem (Jaccard). Em pontos mistos (ex.: MG/VR), a segunda leitura conta cada sigla separada, porque o ponto tem os dois ambientes.",
    txtres,
    paste0("Fitofisionomias com espécies exclusivas são as que mais contribuem para a riqueza da área: perder esse ambiente leva espécies que os outros não têm. Ambientes úmidos (veredas, matas de galeria, brejos) costumam concentrar anuros; formações abertas e rupestres podem ter espécies especialistas. Jaccard baixo entre fitofisionomias indica comunidades diferentes (alta diversidade beta entre ambientes).",
      if (any(grepl("antropiz", nome_fito(t1$fitofisionomia), ignore.case = TRUE))) " Em áreas antropizadas, espécies generalistas e tolerantes costumam dominar; espécies exclusivas desses pontos tendem a ser de áreas abertas, e não especialistas de mata." else ""),
    if (any(um)) sprintf("%d das %d fitofisionomias têm um ponto só (%s): a comparação é descritiva, porque não dá para separar o ambiente do ponto. Riqueza depende do número de pontos e do esforço; compare pela riqueza média por ponto. Testes (PERMANOVA) só com 2 ou mais pontos em cada fitofisionomia. Nomes das siglas: arquivo siglas_fitofisionomias.csv.",
      sum(um), nrow(t1), paste(t1$nome[um], collapse = ", ")) else "Riqueza depende do número de pontos e do esforço; compare pela riqueza média por ponto. Nomes das siglas: arquivo siglas_fitofisionomias.csv."))
}

catalogo_fito <- function() list(
  fitofisionomias = list(nome = "Fitofisionomias: espécies e indivíduos", padrao = "colunas", formas = list(
    colunas = "Colunas por fitofisionomia (espécies, exclusivas e indivíduos)", componentes = "Cada sigla separada (pontos mistos contam nas duas)")),
  fitofisionomias_especies = list(nome = "Fitofisionomias: espécies em cada uma", padrao = "matriz", formas = list(matriz = "Espécie x fitofisionomia, círculo do tamanho da abundância"))
)

desenhar_fito <- function(chave, forma, G, e, gg, pal, k, w, h) {
  F <- G$fitofisionomias; if (is.null(F)) return(invisible()); tam <- e$fonte / 3.1
  rotx <- ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1))
  if (chave == "fitofisionomias") {
    X <- if (forma == "componentes" && !is.null(F$componentes)) F$componentes else F$classes; t <- X$tabela
    t$rot <- paste0(gsub(" \\+ ", " +\n", t$nome %||% t$fitofisionomia), "\n(", ifelse(t$n_pontos == 1, t$pontos, paste(t$n_pontos, "pontos")), ")")
    t$rot <- factor(t$rot, levels = t$rot)
    d <- rbind(data.frame(rot = t$rot, medida = "Espécies", v = t$riqueza), data.frame(rot = t$rot, medida = "Espécies exclusivas", v = t$exclusivas),
               data.frame(rot = t$rot, medida = "Indivíduos", v = t$abundancia))
    d$painel <- ifelse(d$medida == "Indivíduos", "Indivíduos", "Espécies (total e só daquela fitofisionomia)")
    d$painel <- factor(d$painel, levels = c("Espécies (total e só daquela fitofisionomia)", "Indivíduos")); d$rot <- factor(d$rot, levels = levels(t$rot)); d$medida <- factor(d$medida, levels = c("Espécies", "Espécies exclusivas", "Indivíduos"))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(rot, v, fill = medida)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75) +
      ggplot2::geom_text(ggplot2::aes(label = v), position = ggplot2::position_dodge(.8), vjust = -.4, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::facet_wrap(~painel, ncol = 1, scales = "free_y") + ggplot2::scale_fill_manual(values = c(pal[1], pal[4], pal[2]), name = NULL) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .18))) +
      titulos_gg(e, chave, if (forma == "componentes") "Espécies e indivíduos por fitofisionomia (cada sigla separada)" else "Espécies e indivíduos por fitofisionomia",
                 NULL, NULL, "Exclusivas = espécies registradas só naquela fitofisionomia.") + tema_gg(e), w * 1.05, h * 1.45))
  }
  if (chave == "fitofisionomias_especies") {
    E <- F$classes$especies; fs <- F$classes$tabela$fitofisionomia; nm <- setNames(gsub(" \\+ ", " +\n", F$classes$tabela$nome %||% fs), fs)
    d <- do.call(rbind, lapply(fs, function(f) data.frame(especie = E$especie, fito = nm[[f]], n = E[[f]])))
    d <- d[d$n > 0, ]; d$especie <- factor(d$especie, levels = rev(E$especie)); d$fito <- factor(d$fito, levels = unname(nm))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(fito, especie)) + ggplot2::geom_point(ggplot2::aes(size = n), colour = pal[1], alpha = .85, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = n), nudge_x = .3, hjust = 0, family = e$familia, size = tam * .85, colour = k$tinta) +
      ggplot2::scale_size_area(max_size = 8) + titulos_gg(e, chave, "Espécies registradas em cada fitofisionomia", NULL, NULL, "Tamanho do círculo e número = indivíduos.") +
      tema_gg(e) + ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")), w * .9, max(h, .2 * nrow(E) + 1.4)))
  }
  invisible()
}
