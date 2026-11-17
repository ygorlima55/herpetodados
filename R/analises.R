# Herpetodados: matrizes e análises de comunidade (vegan + iNEXT).
# Cada análise devolve dados para gráfico/tabela e um texto com: o que mede, resultado, interpretação e limites.

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d, decimal.mark = ","), "%")
txt <- function(mede, resultado, interpretacao, limites) list(mede = mede, resultado = resultado, interpretacao = interpretacao, limites = limites)

# espécies com identificação incompleta que podem ser uma espécie já registrada do mesmo gênero
ambiguas <- function(reg) {
  inc <- unique(reg$especie[grepl(" (sp\\.|cf\\.|aff\\.|gr\\.)", reg$especie)])
  inc[vapply(inc, function(e) {
    gen <- sub(" .*$", "", e)
    any(reg$genero == gen & !grepl(" (sp\\.|cf\\.|aff\\.|gr\\.)", reg$especie))
  }, TRUE)]
}

montar_matriz <- function(reg, pontos_todos) {
  amb <- ambiguas(reg)
  r <- reg[!(reg$especie %in% amb) & !is.na(reg$total), ]
  pts <- unique(c(pontos_todos, r$ponto))
  pts <- pts[order(suppressWarnings(as.integer(sub("\\D+", "", pts))), pts)]
  m <- tapply(r$total, list(factor(r$ponto, levels = pts), r$especie), sum)
  m[is.na(m)] <- 0
  m <- as.matrix(m)
  list(abund = m, pres = (m > 0) * 1, excluidas = amb,
       n_excluido = sum(reg$total[reg$especie %in% amb], na.rm = TRUE))
}

indices_por_ponto <- function(m) {
  S <- rowSums(m > 0); N <- rowSums(m)
  H <- vegan::diversity(m, "shannon"); D <- vegan::diversity(m, "simpson")
  invD <- ifelse(N > 0, 1 / rowSums((m / pmax(N, 1))^2), NA)
  J <- ifelse(S > 1, H / log(S), NA)
  bp <- ifelse(N > 0, apply(m, 1, max) / pmax(N, 1), NA)
  dom <- ifelse(N > 0, colnames(m)[apply(m, 1, which.max)], NA)
  nmin <- min(N[N > 1])
  rar <- if (is.finite(nmin) && nmin >= 2) suppressWarnings(vegan::rarefy(m[N > 1, , drop = FALSE], nmin, se = TRUE)) else NULL
  Srar <- rep(NA, nrow(m)); Srar_se <- rep(NA, nrow(m))
  if (!is.null(rar)) { Srar[N > 1] <- rar[1, ]; Srar_se[N > 1] <- rar[2, ] }
  excl <- sapply(seq_len(nrow(m)), function(i) sum(m[i, ] > 0 & colSums(m[-i, , drop = FALSE] > 0) == 0))
  sing <- rowSums(m == 1)
  data.frame(ponto = rownames(m), S = S, N = N, shannon = round(H, 3), simpson = round(D, 3),
             hill0 = S, hill1 = round(exp(H), 2), hill2 = round(invD, 2), pielou = round(J, 3),
             berger_parker = round(bp, 3), dominante = dom, S_rarefeita = round(Srar, 2), S_rarefeita_ep = round(Srar_se, 2),
             n_rarefacao = nmin, exclusivas = excl, singletons = sing, stringsAsFactors = FALSE)
}

analisar_grupo <- function(reg, nome, pontos_info, consol = NULL, campanhas = NULL, faixa = NULL) {
  passo <- function(f, etapa) if (!is.null(faixa) && exists("progresso")) progresso(faixa[1] + diff(faixa) * f, paste0(nome, ": ", etapa))
  pts_todos <- if (!is.null(pontos_info)) pontos_info$ponto else unique(reg$ponto)
  M <- montar_matriz(reg, pts_todos)
  m <- M$abund; pr <- M$pres
  n_pt <- nrow(m); S <- ncol(m); N <- sum(m)
  res <- list(grupo = nome, n_pontos = n_pt, riqueza = S, n_individuos = N,
              excluidas = I(M$excluidas), n_excluido = M$n_excluido,
              matriz = list(pontos = rownames(m), especies = colnames(m), valores = unname(m)))
  res$descritiva <- tryCatch(descritiva(reg, M, consol), error = function(e) NULL)
  if (S < 2 || n_pt < 2) { res$insuficiente <- TRUE; return(res) }

  # ---------- 1. índices por ponto ----------
  passo(0, "índices por ponto")
  ip <- indices_por_ponto(m)
  if (!is.null(pontos_info)) ip$fitofisionomia <- pontos_info$fitofisionomia[match(ip$ponto, pontos_info$ponto)]
  best <- ip[which.max(ip$shannon), ]; worst <- ip[ip$N > 0, ][which.min(ip$shannon[ip$N > 0]), ]
  pdom <- ip[which.max(ip$berger_parker), ]
  res$indices <- list(tabela = ip, texto = txt(
    "Para cada ponto: riqueza (S, número de espécies), abundância (N, número de indivíduos), Shannon (H', mistura riqueza e equilíbrio entre as espécies; usa log natural), Simpson (1 menos D, chance de dois indivíduos sorteados serem de espécies diferentes), Pielou (J', equitabilidade de 0 a 1), Berger-Parker (fração do ponto que é da espécie mais comum) e números de Hill (q0 = riqueza, q1 = exp(H'), número de espécies 'comuns', q2 = 1/D, número de espécies 'dominantes'). A riqueza rarefeita estima quantas espécies cada ponto teria se todos tivessem o mesmo número de indivíduos.",
    sprintf("Maior diversidade no %s (S = %d, H' = %s, J' = %s); menor no %s (S = %d, H' = %s). Maior dominância no %s, onde %s responde por %s dos indivíduos. Rarefazendo todos os pontos para %d indivíduos, a riqueza esperada vai de %s a %s espécies.",
            best$ponto, best$S, fmt(best$shannon), fmt(best$pielou), worst$ponto, worst$S, fmt(worst$shannon),
            pdom$ponto, pdom$dominante, pct(pdom$berger_parker), ip$n_rarefacao[1],
            fmt(min(ip$S_rarefeita, na.rm = TRUE), 1), fmt(max(ip$S_rarefeita, na.rm = TRUE), 1)),
    "Compare os pontos pela riqueza rarefeita e pelos números de Hill, não pela riqueza bruta: um ponto com mais indivíduos tende a ter mais espécies só por isso. Pielou baixo e Berger-Parker alto indicam um ambiente dominado por poucas espécies, comum em áreas abertas ou alteradas, onde espécies generalistas formam grandes coros. Quando q1 e q2 ficam muito abaixo de q0, a maioria das espécies é rara no ponto.",
    "Os índices dependem do esforço e da forma de contar. Contagens grandes (coros de anuros, bandos de aves, colônias) são estimativas e inflam a dominância. Com uma única amostra por ponto, a diferença entre pontos pode refletir só a noite (chuva, lua, hora) e não o ambiente. Não há teste estatístico entre pontos porque cada ponto tem uma única amostra."))

  # ---------- 2. curva de acumulação por pontos + estimadores de incidência ----------
  passo(0.05, "curva de acumulação e estimadores (999 permutações)")
  sa <- vegan::specaccum(pr[rowSums(pr) > 0, , drop = FALSE], method = "random", permutations = 999)
  curva <- data.frame(pontos = sa$sites, riqueza = round(sa$richness, 3), dp = round(sa$sd, 3),
                      li = round(pmax(sa$richness - 1.96 * sa$sd, 0), 3), ls = round(sa$richness + 1.96 * sa$sd, 3))
  sp <- vegan::specpool(pr[rowSums(pr) > 0, , drop = FALSE])
  est <- data.frame(estimador = c("Observada", "Chao 2", "Jackknife 1", "Jackknife 2", "Bootstrap"),
                    valor = round(c(sp$Species, sp$chao, sp$jack1, sp$jack2, sp$boot), 2),
                    ep = round(c(NA, sp$chao.se, sp$jack1.se, NA, sp$boot.se), 2))
  est$completude <- round(sp$Species / est$valor, 3)
  pa <- tryCatch(vegan::poolaccum(pr[rowSums(pr) > 0, , drop = FALSE], permutations = 199, minsize = 2), error = function(e) NULL)
  pool <- NULL
  if (!is.null(pa)) {
    sm <- summary(pa, display = c("S", "chao", "jack1", "jack2", "boot"))
    pool <- do.call(rbind, lapply(names(sm), function(k) { z <- as.data.frame(sm[[k]]); data.frame(estimador = k, pontos = z[[1]], media = round(z[[2]], 2), li = round(z[[3]], 2), ls = round(z[[4]], 2)) }))
  }
  ult <- tail(curva, 2); ganho <- ult$riqueza[2] - ult$riqueza[1]
  uniq <- sum(colSums(pr) == 1); dup <- sum(colSums(pr) == 2)
  faixa <- range(est$valor[-1])
  res$acumulacao <- list(curva = curva, estimadores = est, poolaccum = pool, unicas = uniq, duplicadas = dup, texto = txt(
    "A curva de acumulação mostra quantas espécies se acumulam conforme se somam pontos amostrais, na média de 999 ordens sorteadas dos pontos (com faixa de 95%). Os estimadores (Chao 2, Jackknife 1 e 2, Bootstrap) usam as espécies raras, vistas em só um ponto (únicas) ou em dois (duplicadas), para estimar quantas espécies existem na área, incluindo as que não foram encontradas.",
    sprintf("Foram %d espécies em %d pontos. O último ponto somado ainda acrescenta %s espécies em média. %d espécies apareceram em um único ponto e %d em dois. Os estimadores indicam de %s a %s espécies para a área; a amostragem registrou entre %s e %s do esperado.",
            S, sum(rowSums(pr) > 0), fmt(ganho, 1), uniq, dup, fmt(faixa[1], 1), fmt(faixa[2], 1),
            pct(min(est$completude[-1])), pct(max(est$completude[-1]))),
    if (ganho >= 1) "A curva não estabilizou: cada ponto novo ainda traz espécies novas. Isso quer dizer que a lista de espécies está incompleta e que outras campanhas (principalmente na outra estação) devem acrescentar espécies. No laudo, apresente a riqueza observada junto com a estimada e diga que a curva não chegou à assíntota."
    else "A curva está perto de estabilizar: novos pontos acrescentariam poucas espécies com o mesmo método e na mesma época. Espécies de outra estação ou de outro método ainda podem faltar.",
    sprintf("Com só %d unidades amostrais, os estimadores são muito instáveis (o Jackknife 2 é o que mais oscila) e a faixa de confiança da curva é estreita no fim por construção, não por precisão: no último ponto todas as ordens dão o mesmo total. Os pontos precisam ser independentes (distantes o bastante para não ouvir o mesmo coro) e ter esforço parecido.", sum(rowSums(pr) > 0))))

  # ---------- 3. iNEXT: números de Hill com interpolação e extrapolação ----------
  passo(0.45, "iNEXT (rarefação e extrapolação)")
  vec <- sort(colSums(m), decreasing = TRUE)
  inx <- tryCatch(iNEXT::iNEXT(list(Total = vec), q = c(0, 1, 2), datatype = "abundance", endpoint = 2 * sum(vec), knots = 40, nboot = 200), error = function(e) NULL)
  inc <- tryCatch(iNEXT::iNEXT(list(Total = c(nrow(pr[rowSums(pr) > 0, , drop = FALSE]), sort(colSums(pr), decreasing = TRUE))), q = c(0, 1, 2), datatype = "incidence_freq",
                               endpoint = 2 * sum(rowSums(pr) > 0), knots = 20, nboot = 200), error = function(e) NULL)
  pega_inext <- function(o) {
    if (is.null(o)) return(NULL)
    z <- o$iNextEst$size_based
    data.frame(m = if ("m" %in% names(z)) z$m else z$t, metodo = as.character(z$Method), q = z$Order.q,
               qD = round(z$qD, 3), li = round(z$qD.LCL, 3), ls = round(z$qD.UCL, 3), cobertura = round(z$SC, 4))
  }
  asy <- function(o) if (is.null(o)) NULL else { a <- o$AsyEst; data.frame(medida = as.character(a$Diversity), observado = round(a$Observed, 2), estimado = round(a$Estimator, 2), ep = round(a$s.e., 2), li = round(a$LCL, 2), ls = round(a$UCL, 2)) }
  cob <- if (!is.null(inx)) inx$DataInfo$SC[1] else NA
  ab_a <- asy(inx); in_a <- asy(inc)
  res$inext <- list(abundancia = pega_inext(inx), incidencia = pega_inext(inc), assintotico_abund = ab_a, assintotico_inc = in_a,
                    cobertura = round(cob, 4), texto = txt(
    "O iNEXT (Chao et al. 2014; Hsieh et al. 2016) calcula a diversidade em números de Hill: q = 0 é a riqueza, q = 1 é a diversidade de Shannon convertida em número de espécies igualmente comuns e q = 2 é a de Simpson, número de espécies dominantes. A linha cheia é a interpolação (rarefação) até a amostra real; a tracejada é a extrapolação até o dobro. A cobertura de amostra diz que fração dos indivíduos da comunidade pertence a espécies já registradas.",
    sprintf("Por indivíduos (N = %d), a cobertura foi de %s. Riqueza estimada (Chao 1): %s espécies (IC 95%% %s a %s). Diversidade de Shannon em número de espécies: %s; de Simpson: %s. Por incidência nos pontos, a riqueza estimada (Chao 2) foi %s (IC 95%% %s a %s).",
            sum(vec), pct(cob, 1),
            fmt(ab_a$estimado[1], 1), fmt(ab_a$li[1], 1), fmt(ab_a$ls[1], 1), fmt(ab_a$observado[2], 1), fmt(ab_a$observado[3], 1),
            if (!is.null(in_a)) fmt(in_a$estimado[1], 1) else "-", if (!is.null(in_a)) fmt(in_a$li[1], 1) else "-", if (!is.null(in_a)) fmt(in_a$ls[1], 1) else "-"),
    "Cobertura alta (acima de 95%) com riqueza estimada maior que a observada quer dizer que as espécies comuns foram bem amostradas e faltam as raras. Quando as curvas de q1 e q2 ficam planas logo cedo, a comunidade é dominada por poucas espécies. A curva por incidência (pontos) é a mais confiável aqui, porque não depende das contagens de coro.",
    "A curva por indivíduos supõe que cada indivíduo é uma amostra independente, o que não vale para coros, bandos ou colônias contados por estimativa: ela tende a superestimar a cobertura e a subestimar espécies raras. A curva por pontos usa só presença e ausência, mas com 5 pontos a extrapolação além de 10 pontos não é confiável e o intervalo fica largo."))

  # rarefação por ponto (q0), para comparar pontos com o mesmo número de indivíduos
  vp <- lapply(seq_len(nrow(m)), function(i) { v <- m[i, m[i, ] > 0]; if (length(v) >= 2 && sum(v) >= 3) sort(v, decreasing = TRUE) else NULL })
  names(vp) <- rownames(m); vp <- vp[!vapply(vp, is.null, TRUE)]
  ipt <- tryCatch(iNEXT::iNEXT(vp, q = 0, datatype = "abundance", knots = 30, nboot = 100, endpoint = 2 * max(sapply(vp, sum))), error = function(e) NULL)
  if (!is.null(ipt)) {
    z <- ipt$iNextEst$size_based
    res$rarefacao_pontos <- data.frame(ponto = z$Assemblage, m = z$m, metodo = as.character(z$Method), qD = round(z$qD, 3), li = round(z$qD.LCL, 3), ls = round(z$qD.UCL, 3))
  }

  # ---------- 4. similaridade: Bray-Curtis e Jaccard ----------
  passo(0.8, "similaridade, NMDS e PERMANOVA")
  mm <- m[rowSums(m) > 0, , drop = FALSE]
  if (nrow(mm) >= 3) {
    bc <- vegan::vegdist(mm, "bray"); bcs <- vegan::vegdist(sqrt(mm), "bray"); ja <- vegan::vegdist(mm, "jaccard", binary = TRUE)
    hc <- function(d) { h <- hclust(d, "average"); list(merge = h$merge, altura = round(h$height, 4), ordem = h$order, rotulos = h$labels,
                                                       cofenetica = round(suppressWarnings(cor(d, cophenetic(h))), 3)) }
    sim <- function(d) { s <- 1 - as.matrix(d); round(s, 3) }
    S_bc <- sim(bc); S_ja <- sim(ja); S_bcs <- sim(bcs)
    par_max <- function(S) { s <- S; diag(s) <- NA; k <- which(s == max(s, na.rm = TRUE), arr.ind = TRUE)[1, ]; c(rownames(s)[k[1]], colnames(s)[k[2]], s[k[1], k[2]]) }
    par_min <- function(S) { s <- S; diag(s) <- NA; k <- which(s == min(s, na.rm = TRUE), arr.ind = TRUE)[1, ]; c(rownames(s)[k[1]], colnames(s)[k[2]], s[k[1], k[2]]) }
    a1 <- par_max(S_bc); a2 <- par_min(S_bc); j1 <- par_max(S_ja); j2 <- par_min(S_ja)
    comp <- outer(seq_len(nrow(mm)), seq_len(nrow(mm)), Vectorize(function(i, j) sum(mm[i, ] > 0 & mm[j, ] > 0)))
    dimnames(comp) <- list(rownames(mm), rownames(mm))
    h_bc <- hc(bc); h_ja <- hc(ja); h_bcs <- hc(bcs)
    res$similaridade <- list(bray = S_bc, bray_raiz = S_bcs, jaccard = S_ja, compartilhadas = comp,
      dendro_bray = h_bc, dendro_bray_raiz = h_bcs, dendro_jaccard = h_ja, texto = txt(
      "Jaccard compara só a lista de espécies (presença e ausência): 1 quer dizer as mesmas espécies e 0 nenhuma em comum. Bray-Curtis compara também as abundâncias: dois pontos com as mesmas espécies mas proporções muito diferentes ficam pouco similares. A versão com raiz quadrada reduz o peso das espécies muito abundantes (coros grandes). O dendrograma (UPGMA, média de grupo) junta primeiro os pontos mais parecidos; a correlação cofenética diz quanto o desenho respeita as distâncias reais (acima de 0,8 é boa).",
      sprintf("Pela composição de espécies (Jaccard), os pontos mais parecidos são %s e %s (%s) e os mais diferentes são %s e %s (%s). Considerando as abundâncias (Bray-Curtis), os mais parecidos são %s e %s (%s) e os mais diferentes %s e %s (%s). Correlação cofenética: Bray-Curtis %s; Jaccard %s.",
              j1[1], j1[2], fmt(as.numeric(j1[3])), j2[1], j2[2], fmt(as.numeric(j2[3])), a1[1], a1[2], fmt(as.numeric(a1[3])), a2[1], a2[2], fmt(as.numeric(a2[3])),
              fmt(h_bc$cofenetica), fmt(h_ja$cofenetica)),
      "Similaridade baixa entre pontos (Jaccard abaixo de 0,3) indica que cada ponto tem um conjunto próprio de espécies, ou seja, a área tem ambientes diferentes (alta diversidade beta) e perder um deles leva espécies que os outros não têm. Quando Jaccard e Bray-Curtis contam histórias diferentes, as mesmas espécies estão presentes mas em proporções diferentes (por exemplo, um coro grande num ponto só). Compare os grupos do dendrograma com as fitofisionomias de cada ponto.",
      "Com poucos pontos e uma noite, a similaridade é muito sensível a uma espécie a mais ou a menos. Bray-Curtis herda o problema das contagens estimadas (coros, bandos); por isso há a versão com raiz. Não há como testar se os grupos do dendrograma são reais: ele descreve, não prova."))

    # NMDS e PERMANOVA só quando os dados sustentam
    if (nrow(mm) >= 6) {
      nm <- tryCatch(vegan::metaMDS(sqrt(mm), "bray", k = 2, trymax = 100, trace = 0, autotransform = FALSE), error = function(e) NULL)
      if (!is.null(nm)) {
        gq <- if (!is.null(pontos_info)) pontos_info$grupo_amostral[match(rownames(nm$points), pontos_info$ponto)] else NA
        res$nmds <- list(pontos = data.frame(ponto = rownames(nm$points), x = round(nm$points[, 1], 4), y = round(nm$points[, 2], 4), grupo = gq), stress = round(nm$stress, 3))
        res$nmds$texto <- txt("O NMDS desenha as unidades amostrais num plano de modo que as mais parecidas em composição e abundância (Bray-Curtis sobre a raiz das contagens) fiquem perto. Os eixos não têm unidade; vale só a distância entre os pontos. O stress diz quanto o desenho distorce as distâncias reais: abaixo de 0,1 é bom, de 0,1 a 0,2 é aceitável, acima de 0,2 não deve ser interpretado.",
          sprintf("Stress = %s (%s), com %d unidades amostrais.", fmt(nm$stress, 3), if (nm$stress < .1) "bom" else if (nm$stress <= .2) "aceitável" else "alto, não interpretar", nrow(mm)),
          "Unidades do mesmo grupo (campanha ou ambiente) juntas e separadas das do outro grupo indicam que o grupo muda a comunidade. Grupos sobrepostos indicam comunidades parecidas. Confirme com a PERMANOVA.",
          "Ordenação é descritiva. Com cerca de 10 unidades, uma espécie muito abundante pode puxar um ponto para longe dos outros.")
      }
    } else res$nmds <- list(pulado = sprintf("NMDS não foi feito: com %d pontos a ordenação em 2 dimensões fica quase sem restrição (stress perto de zero sempre) e não informa nada além do dendrograma. Passa a ser útil com 6 ou mais unidades amostrais, por exemplo juntando campanhas.", nrow(mm)))
    g <- if (!is.null(pontos_info)) pontos_info$grupo_amostral[match(rownames(mm), pontos_info$ponto)] else NA
    if (all(!is.na(g)) && length(unique(g)) >= 2 && min(table(g)) >= 2 && nrow(mm) >= 6) {
      bl <- sub(" C[0-9]+$", "", rownames(mm)); blocos <- any(duplicated(bl)) && all(table(bl) == length(unique(g))) && !any(duplicated(paste(bl, g)))
      perm <- if (blocos) permute::how(blocks = factor(bl), nperm = 999) else 999
      ad <- vegan::adonis2(sqrt(mm) ~ g, method = "bray", permutations = perm)
      bd <- vegan::permutest(vegan::betadisper(vegan::vegdist(sqrt(mm), "bray"), g), permutations = 999)
      res$permanova <- list(R2 = round(ad$R2[1], 3), F = round(ad$F[1], 3), p = ad$`Pr(>F)`[1], p_dispersao = bd$tab$`Pr(>F)`[1],
        grupos = paste(sprintf("%s (%d)", names(table(g)), as.integer(table(g))), collapse = ", "), blocos = blocos, n_perm = if (blocos) factorial(length(unique(g)))^length(unique(bl)) else 999)
      res$permanova$texto <- txt("A PERMANOVA testa se a composição de espécies difere entre grupos de unidades amostrais (aqui, os grupos da aba de pontos ou as campanhas), comparando a variação entre grupos com a variação dentro deles, por permutação. O R² diz quanto da diferença entre unidades é explicado pelo grupo. O teste de dispersão (betadisper) confere o pressuposto de que os grupos têm variação interna parecida.",
        sprintf("Grupos: %s. R² = %s; F = %s; p = %s. Teste de dispersão: p = %s.", res$permanova$grupos, fmt(ad$R2[1]), fmt(ad$F[1]), fmt(ad$`Pr(>F)`[1], 3), fmt(bd$tab$`Pr(>F)`[1], 3)),
        if (ad$`Pr(>F)`[1] < .05) sprintf("A composição difere entre os grupos (p < 0,05); o grupo explica %s da variação.%s", pct(ad$R2[1]), if (bd$tab$`Pr(>F)`[1] < .05) " Atenção: a dispersão também difere, então parte do resultado pode vir de um grupo ser mais variável, e não de ter outras espécies." else "")
        else "Não houve diferença detectável de composição entre os grupos (p >= 0,05). Isso não prova que são iguais: com poucas unidades o teste tem pouco poder.",
        if (blocos) sprintf("Cada ponto entra nas duas campanhas, então as permutações foram feitas dentro de cada ponto (blocos), sem misturar pontos: isso respeita o fato de ser o mesmo lugar medido de novo. Com %d pontos há só %d permutações possíveis, então o menor p possível é cerca de %s.", length(unique(bl)), factorial(length(unique(g)))^length(unique(bl)), fmt(1 / factorial(length(unique(g)))^length(unique(bl)), 3)) else "Com poucas unidades por grupo o número de permutações distintas é limitado e o valor de p é aproximado.")
    } else res$permanova <- list(pulado = "PERMANOVA não foi feita: ela compara grupos de pontos (por exemplo, mata x campo) e precisa de pelo menos 2 grupos com 2 ou mais pontos cada, num total de 6 ou mais pontos. Com 5 pontos o menor valor de p possível já é alto. Para usar, inclua na aba de pontos uma coluna 'Grupo amostral' e junte mais pontos ou campanhas.")
  }

  # ---------- 5. abundância relativa (Whittaker) e constância ----------
  passo(0.92, "dominância")
  tot <- sort(colSums(m), decreasing = TRUE)
  ra <- data.frame(posicao = seq_along(tot), especie = names(tot), n = as.numeric(tot), relativa = round(tot / sum(tot), 4),
                   pontos = as.numeric(colSums(pr)[names(tot)]))
  ra$fo <- round(ra$pontos / sum(rowSums(pr) > 0), 3)
  ra$constancia <- ifelse(ra$fo > 0.5, "Constante", ifelse(ra$fo >= 0.25, "Acessória", "Acidental"))
  top3 <- sum(ra$relativa[1:min(3, nrow(ra))])
  raros <- sum(ra$n <= 2)
  res$abundancia <- list(tabela = ra, texto = txt(
    "Ordena as espécies da mais para a menos abundante (curva de Whittaker, em escala log). Uma curva íngreme indica poucas espécies dominantes e muitas raras; uma curva suave indica comunidade equilibrada. A constância (Dajoz) classifica cada espécie pelo número de pontos em que apareceu: constante (mais de 50%), acessória (25% a 50%) e acidental (menos de 25%).",
    sprintf("As três espécies mais abundantes (%s) somam %s dos indivíduos. %d espécies tiveram 1 ou 2 indivíduos. Constantes: %d; acessórias: %d; acidentais: %d.",
            paste(ra$especie[1:min(3, nrow(ra))], collapse = ", "), pct(top3), raros,
            sum(ra$constancia == "Constante"), sum(ra$constancia == "Acessória"), sum(ra$constancia == "Acidental")),
    paste0(if (nome %in% c("Anuros", "Herpetofauna")) "Dominância de espécies generalistas e de coros grandes, com muitas espécies raras, é o padrão esperado em áreas abertas e alteradas do Cerrado na estação chuvosa." else "Poucas espécies muito abundantes e muitas raras é o padrão mais comum em comunidades naturais; dominância forte de espécies generalistas costuma indicar ambiente alterado.", " As espécies raras e acidentais são as que mais pesam nos estimadores de riqueza e são as primeiras a merecer atenção em campanhas futuras."),
    "A posição das espécies contadas por estimativa (coros, bandos) é aproximada; espécies pouco ativas ou pouco detectáveis no dia da amostragem ficam sub-representadas. Raridade numa amostra não é raridade na área."))

  # ---------- 6. textos de cada estimador, beta, Hutcheson, cobertura por ponto ----------
  passo(0.95, "beta, Hutcheson e textos")
  res$acumulacao$textos <- textos_estimadores(res, if (is.null(pool)) data.frame(estimador = character(), pontos = numeric(), media = numeric()) else pool)
  if (sum(rowSums(pr) > 0) >= 2) {
    b <- beta_baselga(pr)
    res$beta <- c(b, list(texto = txt(
      "A diversidade beta mede o quanto a composição de espécies muda de um ponto para outro. A de Whittaker (gama / alfa média − 1) diz quantas 'comunidades distintas' cabem na área. A partição de Baselga divide a diferença total (Sørensen) em substituição de espécies (turnover: um ponto troca espécies por outras) e aninhamento (um ponto é um subconjunto mais pobre de outro).",
      sprintf("Riqueza total (gama) = %d; média por ponto (alfa) = %s; beta de Whittaker = %s. Diferença total entre os pontos (Sørensen) = %s, sendo %s por substituição (%s do total) e %s por aninhamento.",
              b$gama, fmt(b$alfa_media, 1), fmt(b$whittaker, 2), fmt(b$sorensen, 2), fmt(b$turnover, 2), pct(b$turnover / max(b$sorensen, 1e-9)), fmt(b$aninhamento, 2)),
      if (b$turnover >= b$aninhamento) "Predomina a substituição de espécies: cada ponto abriga espécies que os outros não têm. Para conservação, isso significa que vários ambientes precisam ser mantidos; proteger só o ponto mais rico não protege a comunidade da área."
      else "Predomina o aninhamento: os pontos mais pobres têm um subconjunto das espécies dos mais ricos. Proteger o ponto mais rico conserva boa parte da comunidade, e a perda de espécies nos outros pontos sugere degradação ou ambientes menos favoráveis.",
      "Usa só presença e ausência; com poucos pontos e uma campanha, espécies não detectadas parecem 'substituídas'. Diferenças de esforço entre pontos aumentam o aninhamento aparente.")))
  }
  hu <- tryCatch(hutcheson(m), error = function(e) NULL)
  if (!is.null(hu)) {
    nd <- sum(hu$pares$diferenca == "diferente")
    res$hutcheson <- c(hu, list(texto = txt(
      "O teste t de Hutcheson compara a diversidade de Shannon (H') entre dois pontos, considerando a variância de H' de cada um. É muito usado em relatórios de fauna para dizer se um ponto é mais diverso que outro. Os valores de p foram corrigidos pelo método de Holm por haver várias comparações.",
      sprintf("%d de %d pares de pontos tiveram diversidade de Shannon diferente (p corrigido < 0,05).", nd, nrow(hu$pares)),
      "Par 'diferente' quer dizer que a diferença de H' é maior do que a esperada pelo acaso, dado o número de indivíduos. Mostre junto o H' de cada ponto com o intervalo de confiança.",
      "O teste trata cada indivíduo como independente. Contagens estimadas (coros, bandos) e indivíduos que andam ou vocalizam juntos violam isso e deixam o teste otimista demais (pseudorreplicação). Use como apoio descritivo, não como prova; com uma amostra por ponto, a diferença pode ser do dia e não do ambiente.")))
  }
  if (exists("ipt") && !is.null(ipt)) {
    di <- ipt$DataInfo; res$indices$tabela$cobertura <- round(di$SC[match(res$indices$tabela$ponto, di$Assemblage)], 4)
  }
  if (!is.null(res$similaridade)) {
    res$similaridade$complementaridade <- round(1 - res$similaridade$jaccard, 3)
    rp <- rownames(res$similaridade$jaccard); pp <- combn(rp, 2)
    res$similaridade$pares <- data.frame(par = paste(pp[1, ], "x", pp[2, ]), ponto_a = pp[1, ], ponto_b = pp[2, ],
      distancia_m = NA, compartilhadas = res$similaridade$compartilhadas[t(pp)], jaccard = res$similaridade$jaccard[t(pp)],
      bray_curtis = res$similaridade$bray[t(pp)], bray_curtis_raiz = res$similaridade$bray_raiz[t(pp)],
      complementaridade = res$similaridade$complementaridade[t(pp)], stringsAsFactors = FALSE)
    res$similaridade$pares <- res$similaridade$pares[order(-res$similaridade$pares$jaccard), ]
  }

  # objetos do R guardados para as figuras (não vão para o painel em JSON)
  objs <- list(sa = sa, pa = pa, inx = inx, inc = inc, ipt = if (exists("ipt")) ipt else NULL, m = m, pr = pr)
  if (exists("bc")) { objs$bc <- bc; objs$bcs <- bcs; objs$ja <- ja }
  fam_v <- tapply(reg$familia, reg$especie, function(x) x[!is.na(x)][1])
  res$fitofisionomias <- tryCatch(analisar_fito(m, pontos_info, fam_v), error = function(e) { message("fitofisionomias: ", conditionMessage(e)); NULL })
  if (!is.null(reg$camp_cod) && length(unique(reg$camp_cod)) >= 2 && !is.null(campanhas))
    res$campanhas <- tryCatch(comparar_campanhas(reg, M$excluidas, campanhas), error = function(e) { message("campanhas: ", conditionMessage(e)); NULL })
  attr(res, "objs") <- objs
  res
}

# avisos de pressupostos (valem para o levantamento como um todo)
avisos_pressupostos <- function(L, R) {
  r <- L$registros; a <- list()
  add <- function(tema, nivel, texto) a[[length(a) + 1]] <<- list(tema = tema, nivel = nivel, texto = texto)
  npt <- if (!is.null(L$pontos)) nrow(L$pontos) else length(unique(r$ponto))
  ncamp <- length(unique(r$campanha))
  if (npt < 10) add("Número de unidades amostrais", "atenção", sprintf("Só %d pontos. Curvas de acumulação, estimadores de riqueza e similaridade ficam instáveis com menos de cerca de 10 unidades amostrais. O ideal é usar cada noite em cada ponto como uma unidade (ponto x noite) e somar campanhas.", npt))
  if (ncamp < 2) add("Sazonalidade", "atenção", paste("Uma única campanha.", if (any(r$grupo_pad == "Anuros")) "Anuros do Cerrado têm atividade muito sazonal (explosivos no início das chuvas, prolongados ao longo delas);" else "A maioria dos grupos de fauna muda ao longo do ano (reprodução, migração, chuva e seca);", "uma campanha só não representa a comunidade do ano. Para licenciamento, o comum é pedir ao menos uma campanha na chuva e outra na seca."))
  if (!is.null(L$pontos) && all(is.na(L$pontos$esforco))) add("Esforço amostral", "atenção", "Esforço por ponto não informado. Os índices supõem esforço igual em todos os pontos; se algum ponto teve mais tempo de busca ou mais pessoas, a comparação fica enviesada.")
  vo <- sum(r$vo, na.rm = TRUE); tot <- sum(r$total, na.rm = TRUE)
  grandes <- sum(r$total >= 20, na.rm = TRUE)
  if (grandes > 0) add("Contagens grandes", "atenção", sprintf("%d registros com 20 ou mais indivíduos (%s do total de indivíduos veio de vocalização). Contagem de coro, bando ou colônia é estimativa: os índices por abundância (Shannon, Simpson, Bray-Curtis, Chao 1) devem ser lidos com cautela. As análises por presença e ausência (Jaccard, Chao 2, curva por pontos) são as mais robustas aqui.", grandes, pct(vo / tot)))
  sf <- sum(r$sem_forma, na.rm = TRUE)
  if (sf > 0) add("Forma de registro", "informação", sprintf("%d indivíduos sem forma de registro anotada (%s do total).", sf, pct(sf / tot)))
  if (length(unique(r$grupo_pad)) > 1) add("Grupos misturados", "informação", "Grupos diferentes são detectados de formas muito diferentes (canto, encontro visual, captura, vestígio). As análises valem por grupo; o conjunto com todos os grupos é só descritivo.")
  rep <- R[["Répteis"]]
  if (!is.null(rep) && rep$n_individuos < 20) add("Répteis", "atenção", sprintf("Répteis: %d indivíduos de %d espécies. É pouco para curva, índices ou similaridade: no laudo, apresente só a lista e a abundância.", rep$n_individuos, rep$riqueza))
  add("Independência dos pontos", "informação", "As análises supõem que os pontos são independentes: distantes o bastante para não registrar o mesmo coro, bando ou os mesmos indivíduos. Confira as distâncias entre os pontos nas coordenadas.")
  a
}
