# Quadrante: análise descritiva (sem estatística) e análises complementares.
# Descritiva: famílias, espécies, riqueza e abundância por família e por ponto, forma de registro e atributos
# (endemismo, hábito, dependência, sensibilidade, SALVE, IUCN) da aba consolidada.
# Complementares: diversidade beta (Whittaker e partição de Baselga com Sørensen), teste t de Hutcheson entre pontos,
# cobertura de amostra por ponto e textos de cada estimador de riqueza.

ROTULOS_CAT <- list(
  endemismo = c(E = "Endêmica do Cerrado", AD = "Ampla distribuição"),
  habito = c(Tr = "Terrícola", "Arb." = "Arborícola", Arb = "Arborícola", "Aq." = "Aquática", Aq = "Aquática", Fos = "Fossorial", "Sa" = "Semiaquática"),
  dependencia = c(Baixa = "Baixa", "Méd" = "Média", "Média" = "Média", Alta = "Alta"),
  sensibilidade = c(Baixa = "Baixa", "Méd" = "Média", "Média" = "Média", Alta = "Alta"),
  salve = c(LC = "Menos preocupante (LC)", NT = "Quase ameaçada (NT)", VU = "Vulnerável (VU)", EN = "Em perigo (EN)", CR = "Criticamente em perigo (CR)", DD = "Dados insuficientes (DD)", NE = "Não avaliada (NE)"),
  iucn = c(LC = "Menos preocupante (LC)", NT = "Quase ameaçada (NT)", VU = "Vulnerável (VU)", EN = "Em perigo (EN)", CR = "Criticamente em perigo (CR)", DD = "Dados insuficientes (DD)", NE = "Não avaliada (NE)")
)
NOMES_CAT <- c(endemismo = "Distribuição (endemismo)", habito = "Hábito", dependencia = "Dependência de ambiente florestal",
               sensibilidade = "Sensibilidade a alterações", salve = "Status nacional (SALVE)", iucn = "Status global (IUCN)")

descritiva <- function(reg, M, consol) {
  esp <- colnames(M$abund); m <- M$abund
  r <- reg[reg$especie %in% esp, ]
  fam <- tapply(r$familia, r$especie, function(x) x[!is.na(x)][1])[esp]
  fam[is.na(fam) | !nzchar(trimws(fam))] <- "Família não informada"
  # por família
  tf <- do.call(rbind, lapply(sort(unique(fam)), function(f) {
    e <- esp[fam == f]; data.frame(familia = f, riqueza = length(e), abundancia = sum(m[, e, drop = FALSE]),
      especies = paste(e, collapse = "; "), stringsAsFactors = FALSE) }))
  tf$riqueza_pct <- round(tf$riqueza / sum(tf$riqueza), 4); tf$abundancia_pct <- round(tf$abundancia / sum(tf$abundancia), 4)
  tf <- tf[order(-tf$riqueza, -tf$abundancia), c("familia", "riqueza", "riqueza_pct", "abundancia", "abundancia_pct", "especies")]
  # por ponto
  tp <- data.frame(ponto = rownames(m), riqueza = rowSums(m > 0), abundancia = rowSums(m),
    familias = sapply(rownames(m), function(p) length(unique(fam[m[p, ] > 0]))),
    especies = sapply(rownames(m), function(p) paste(esp[m[p, ] > 0], collapse = "; ")), stringsAsFactors = FALSE)
  # lista de espécies (estilo tabela de laudo)
  le <- data.frame(familia = fam, especie = esp, stringsAsFactors = FALSE)
  le$especie_completa <- r$especie_completa[match(esp, r$especie)]
  for (p in rownames(m)) le[[p]] <- as.numeric(m[p, esp])
  le$total <- as.numeric(colSums(m)[esp]); le$n_pontos <- as.numeric(colSums(m > 0)[esp])
  vo <- as.numeric(tapply(r$vo, r$especie, sum, na.rm = TRUE)[esp]); av <- as.numeric(tapply(r$av, r$especie, sum, na.rm = TRUE)[esp])
  le$forma_registro_campo <- ifelse(vo > 0 & av > 0, "VO/AV", ifelse(vo > 0, "VO", ifelse(av > 0, "AV", "não anotada")))
  atr <- c("nome_comum", "endemismo", "habito", "dependencia", "sensibilidade", "forma_registro", "salve", "iucn")
  for (a in atr) le[[a]] <- if (!is.null(consol) && a %in% names(consol)) consol[[a]][match(esp, consol$especie)] else NA
  le <- le[order(le$familia, le$especie), ]
  # forma de registro (indivíduos)
  tr <- data.frame(forma = c("VO", "AV", "Sem forma anotada"), descricao = c("Vocalização", "Avistamento", "Não anotada no campo"),
                   individuos = c(sum(r$vo, na.rm = TRUE), sum(r$av, na.rm = TRUE), sum(r$sem_forma, na.rm = TRUE)))
  tr$pct <- round(tr$individuos / max(1, sum(tr$individuos)), 4); tr <- tr[tr$individuos > 0, ]
  # categorias de atributos
  cats <- list()
  for (a in names(NOMES_CAT)) {
    v <- le[[a]]; if (all(is.na(v))) next
    v[is.na(v)] <- "Sem informação"
    lab <- ROTULOS_CAT[[a]]; desc <- ifelse(v %in% names(lab), lab[v], v)
    t <- do.call(rbind, lapply(unique(desc), function(d) { e <- le$especie[desc == d]; data.frame(categoria = NOMES_CAT[[a]], classe = d, especies_n = length(e),
         especies_pct = round(length(e) / nrow(le), 4), abundancia = sum(le$total[desc == d]), especies = paste(e, collapse = "; "), stringsAsFactors = FALSE) }))
    cats[[a]] <- t[order(-t$especies_n), ]
  }
  top <- tf[1, ]
  endem <- if (!is.null(cats$endemismo)) cats$endemismo else NULL
  n_end <- if (!is.null(endem)) sum(endem$especies_n[grepl("Endêmica", endem$classe)]) else NA
  ameac <- if (!is.null(cats$salve)) sum(cats$salve$especies_n[grepl("VU|EN|CR", cats$salve$classe)]) else NA
  txt_res <- sprintf("Foram registradas %d espécies de %d famílias e %d indivíduos. %s foi a família mais rica (%d espécies, %s) e %s a mais abundante (%s dos indivíduos). O ponto com mais espécies foi o %s (%d) e o com mais indivíduos o %s (%d).%s%s",
    length(esp), nrow(tf), sum(m), top$familia, top$riqueza, pct(top$riqueza_pct), tf$familia[which.max(tf$abundancia)], pct(max(tf$abundancia_pct)),
    tp$ponto[which.max(tp$riqueza)], max(tp$riqueza), tp$ponto[which.max(tp$abundancia)], max(tp$abundancia),
    if (!is.na(n_end)) sprintf(" %d espécies são endêmicas do Cerrado segundo a planilha.", n_end) else "",
    if (!is.na(ameac)) sprintf(" %d espécie(s) em categoria de ameaça nacional.", ameac) else "")
  list(familias = tf, pontos = tp, especies = le, forma_registro = tr, categorias = cats,
       texto = txt("Descreve a comunidade sem testes estatísticos: quantas espécies e famílias, quantos indivíduos, como se distribuem entre famílias e pontos, como foram registrados e quais atributos de conservação têm.",
                   txt_res,
                   paste0("Famílias com muitas espécies indicam a composição típica da região", if (any(r$grupo_pad == "Anuros")) " (para anuros no Cerrado, em geral Hylidae e Leptodactylidae)" else "", ". Abundância concentrada em uma família quase sempre vem de poucas espécies muito numerosas (coros, bandos, colônias). Espécies endêmicas, sensíveis ou ameaçadas são as que mais pesam na avaliação de impacto."),
                   "Os atributos (endemismo, hábito, sensibilidade, status) vêm da planilha e devem ter fonte citada no laudo. Contagens grandes (coros, bandos) são estimativas, então a abundância por família também é."))
}

# ---- diversidade beta: Whittaker e partição de Baselga (Sørensen) ----
beta_baselga <- function(pr) {
  media_amostrada <- mean(rowSums(pr))
  pr <- pr[rowSums(pr) > 0, , drop = FALSE]; pr <- pr[, colSums(pr) > 0, drop = FALSE]
  S <- rowSums(pr); ST <- ncol(pr); n <- nrow(pr)
  a_tot <- sum(S) - ST; mins <- 0; maxs <- 0
  sim <- sor <- matrix(0, n, n, dimnames = list(rownames(pr), rownames(pr)))
  for (i in 1:n) for (j in 1:n) if (i != j) {
    a <- sum(pr[i, ] & pr[j, ]); b <- sum(pr[i, ] & !pr[j, ]); cc <- sum(!pr[i, ] & pr[j, ])
    sor[i, j] <- (b + cc) / (2 * a + b + cc); sim[i, j] <- min(b, cc) / (a + min(b, cc))
    if (i < j) { mins <- mins + min(b, cc); maxs <- maxs + max(b, cc) }
  }
  bsor <- (mins + maxs) / (2 * a_tot + mins + maxs); bsim <- mins / (a_tot + mins)
  list(whittaker = round(ST / media_amostrada - 1, 4), gama = ST, alfa_media = round(media_amostrada, 2),
       sorensen = round(bsor, 4), turnover = round(bsim, 4), aninhamento = round(bsor - bsim, 4),
       par_sorensen = round(sor, 3), par_turnover = round(sim, 3), par_aninhamento = round(sor - sim, 3))
}

# ---- teste t de Hutcheson (Shannon) entre pares de pontos ----
hutcheson <- function(m) {
  m <- m[rowSums(m) > 1, , drop = FALSE]
  if (nrow(m) < 2) return(NULL)
  st <- t(apply(m, 1, function(x) { x <- x[x > 0]; N <- sum(x); p <- x / N
    H <- -sum(p * log(p)); v <- (sum(p * log(p)^2) - sum(p * log(p))^2) / N + (length(x) - 1) / (2 * N^2); c(H = H, var = v, N = N) }))
  pares <- combn(rownames(m), 2); out <- list()
  for (k in seq_len(ncol(pares))) {
    a <- pares[1, k]; b <- pares[2, k]
    variancia <- st[a, "var"] + st[b, "var"]
    t <- gl <- NA_real_
    if (variancia > 0) {
      t <- (st[a, "H"] - st[b, "H"]) / sqrt(variancia)
      gl <- variancia^2 / (st[a, "var"]^2 / st[a, "N"] + st[b, "var"]^2 / st[b, "N"])
    }
    out[[k]] <- data.frame(ponto_a = a, ponto_b = b, H_a = round(st[a, "H"], 3), H_b = round(st[b, "H"], 3), t = round(t, 3), gl = round(gl, 1),
                           p = 2 * stats::pt(-abs(t), gl), stringsAsFactors = FALSE)
  }
  tt <- do.call(rbind, out); tt$p_holm <- stats::p.adjust(tt$p, "holm")
  tt$diferenca <- ifelse(is.na(tt$p_holm), "indeterminado", ifelse(tt$p_holm < 0.05, "diferente", "não diferente"))
  ic <- data.frame(ponto = rownames(st), H = round(st[, "H"], 3), li = round(st[, "H"] - 1.96 * sqrt(st[, "var"]), 3), ls = round(st[, "H"] + 1.96 * sqrt(st[, "var"]), 3), N = st[, "N"])
  list(pares = tt, ic = ic)
}

textos_estimadores <- function(G, pool) {
  est <- G$acumulacao$estimadores; ob <- est$valor[1]
  v <- function(n) est$valor[est$estimador == n]; ep <- function(n) est$ep[est$estimador == n]
  fim <- function(k) { z <- pool[pool$estimador == k, ]; if (!nrow(z)) return(""); sprintf(" Ao longo dos pontos foi de %s.", paste(sprintf("%s (%d pts)", fmt(z$media, 1), z$pontos), collapse = ", ")) }
  sobe <- function(k) { z <- pool[pool$estimador == k, ]; if (nrow(z) < 2) return(NA); tail(z$media, 1) - z$media[nrow(z) - 1] }
  frase_est <- function(k) { d <- sobe(k); if (is.na(d)) "" else if (abs(d) < 0.5) " A curva já está quase plana no último ponto." else if (d > 0) sprintf(" A curva ainda sobe %s espécies no último ponto: o estimador não estabilizou.", fmt(d, 1)) else sprintf(" A curva caiu %s no último ponto, aproximando-se da observada.", fmt(-d, 1)) }
  list(
    S = txt("A riqueza observada acumulada: quantas espécies diferentes aparecem quando se somam 1, 2, 3... pontos, na média de várias ordens sorteadas.",
            sprintf("Com todos os pontos, %d espécies.%s", round(ob), fim("S")),
            "Se a curva ainda sobe no fim, a lista está incompleta. É a base para comparar com os estimadores: a diferença entre eles e a observada é o número de espécies que provavelmente faltaram.",
            "Depende do esforço e da época. Não estima espécies não registradas."),
    chao = txt("Chao 2 usa as espécies vistas em um ponto (Q1) e em dois (Q2). Com m pontos e Q2 > 0, o vegan soma [(m − 1)/m] × Q1²/(2Q2) à riqueza observada. Se Q2 = 0, o termo é [(m − 1)/m] × Q1(Q1 − 1)/2.",
            sprintf("Chao 2 = %s espécies (erro padrão %s), %s acima da observada; únicas = %d, duplicadas = %d.%s%s", fmt(v("Chao 2"), 1), fmt(ep("Chao 2"), 2), fmt(v("Chao 2") - ob, 1), G$acumulacao$unicas, G$acumulacao$duplicadas, fim("chao"), frase_est("chao")),
            "É o estimador mais usado como limite inferior da riqueza real. Valor próximo da observada indica inventário quase completo para o método e a época.",
            "Com poucas unidades a incerteza pode ser grande, inclusive quando nenhuma espécie ocorre em exatamente duas unidades."),
    jack1 = txt("Jackknife de primeira ordem soma à riqueza observada as espécies únicas corrigidas pelo número de pontos: S = Sobs + Q1 (m − 1) / m, em que m é o número de pontos.",
            sprintf("Jackknife 1 = %s espécies (erro padrão %s), %s acima da observada.%s%s", fmt(v("Jackknife 1"), 1), fmt(ep("Jackknife 1"), 2), fmt(v("Jackknife 1") - ob, 1), fim("jack1"), frase_est("jack1")),
            "Costuma dar valores um pouco acima do Chao 2 com poucas amostras e é bastante usado em inventários de fauna no Brasil. Leia junto com o Chao 2: se os dois concordam, a estimativa é mais confiável.",
            "Cresce com o número de únicas; com 5 pontos, uma espécie rara a mais muda bastante o resultado."),
    jack2 = txt("Jackknife de segunda ordem usa as únicas e as duplicadas: S = Sobs + Q1 (2m − 3)/m − Q2 (m − 2)² / [m(m − 1)].",
            sprintf("Jackknife 2 = %s espécies, %s acima da observada.%s%s", fmt(v("Jackknife 2"), 1), fmt(v("Jackknife 2") - ob, 1), fim("jack2"), frase_est("jack2")),
             "Compare o valor com os demais estimadores. O Jackknife 2 não estabelece um limite superior para a riqueza.",
            "É o mais instável com poucas unidades amostrais e não tem erro padrão calculado pelo vegan."),
    boot = txt("Bootstrap reamostra os pontos com reposição e soma a contribuição de cada espécie conforme a fração de pontos em que ela falta: S = Sobs + Σ (1 − pk)^m.",
            sprintf("Bootstrap = %s espécies (erro padrão %s), %s acima da observada.%s%s", fmt(v("Bootstrap"), 1), fmt(ep("Bootstrap"), 2), fmt(v("Bootstrap") - ob, 1), fim("boot"), frase_est("boot")),
            "Uma estimativa acima da riqueza observada sugere a possibilidade de espécies ainda não detectadas.",
            "Subestima a riqueza quando há muitas espécies raras; use como limite inferior, junto com o Chao 2.")
  )
}
