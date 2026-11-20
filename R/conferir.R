# Quadrante: corretor da planilha. Confere os dados antes das análises e diz onde está cada problema.
# Níveis: "erro" (muda resultado), "atenção" (pode mudar; conferir), "informação" (registro do que foi feito).

# Lê uma coordenada da aba de pontos nos dois formatos usados ("-15.70° -47.80°" ou "22 K 668628.87 m E 8080967.81 m S").
# Devolve NULL se não der para ler; senão list(problema = NA ou o motivo de ser impossível, no_brasil = TRUE/FALSE).
ler_coordenada <- function(s) {
  s <- gsub(",", ".", trimws(s))
  u <- regmatches(s, regexec("^([0-9]{1,2})\\s*([A-Za-z])?\\s+([0-9.]+)\\s*m?\\s*E\\s+([0-9.]+)\\s*m?\\s*[NS]?$", s))[[1]]
  if (length(u) == 5) {
    z <- as.numeric(u[2]); e <- as.numeric(u[4]); n <- as.numeric(u[5])
    if (anyNA(c(z, e, n))) return(NULL)
    pr <- if (z < 1 || z > 60) "zona UTM fora de 1 a 60" else if (e < 100000 || e > 900000) "leste (E) fora de 100 000 a 900 000 m"
          else if (n < 0 || n > 10000000) "norte (N/S) fora de 0 a 10 000 000 m" else NA
    return(list(problema = pr, no_brasil = z >= 18 && z <= 25))
  }
  letras <- toupper(gsub("[^A-Za-z]", "", s))
  if (grepl("[^NSEWLO]", letras)) return(NULL)
  g <- as.numeric(regmatches(s, gregexpr("-?[0-9]+(\\.[0-9]+)?", s))[[1]])
  if (length(g) != 2) return(NULL)
  la <- g[1]; lo <- g[2]
  if (grepl("S", letras) && la > 0) la <- -la
  if (grepl("[WO]", letras) && lo > 0) lo <- -lo
  pr <- if (abs(la) > 90) "latitude fora de -90 a 90" else if (abs(lo) > 180) "longitude fora de -180 a 180" else NA
  list(problema = pr, no_brasil = la >= -34 && la <= 6 && lo >= -74 && lo <= -28)
}

conferir_planilha <- function(L) {
  r <- L$registros
  out <- list()
  # tipo/especie/ponto/dados: o painel usa para montar o formulário "Corrigir" de cada problema
  add <- function(nivel, aba, linha, campo, problema, sugestao, tipo = "", especie = "", ponto = "", dados = "")
    out[[length(out) + 1]] <<- data.frame(nivel = nivel, aba = aba, linha = as.character(linha),
      campo = campo, problema = problema, sugestao = sugestao, tipo = tipo, especie = especie, ponto = ponto, dados = as.character(dados), stringsAsFactors = FALSE)
  ab <- L$aba_registros

  # 1. contagem
  for (i in seq_len(nrow(r))) {
    x <- r[i, ]
    if (is.na(x$total)) {
      add("erro", ab, x$linha_planilha, "Total", sprintf("Contagem vazia ou não numérica (\"%s\") para %s.", x$total_txt %||% "", x$especie_completa),
          "Preencher o número de indivíduos.", tipo = "contagem", especie = x$especie, ponto = x$ponto)
    } else {
      if (x$total < 0 || x$total != round(x$total))
        add("erro", ab, x$linha_planilha, "Total", sprintf("Contagem %s não é um inteiro positivo.", x$total), "Corrigir o número.", tipo = "contagem", especie = x$especie, ponto = x$ponto)
      if (x$total == 0)
        add("atenção", ab, x$linha_planilha, "Total", sprintf("%s com total 0 no %s.", x$especie, x$ponto), "Apagar a linha ou corrigir a contagem.", tipo = "total_zero", especie = x$especie, ponto = x$ponto)
      partes <- c(x$vo, x$av, x$sem_forma)
      if (any(!is.na(partes)) && sum(partes, na.rm = TRUE) != x$total)
        add("erro", ab, x$linha_planilha, "Total", sprintf("Total %s diferente de VO + AV + sem forma (%s) para %s no %s.",
            x$total, sum(partes, na.rm = TRUE), x$especie, x$ponto), "Conferir qual número está certo.", tipo = "contagem", especie = x$especie, ponto = x$ponto)
      if (x$total >= 20 && (is.na(x$av) || x$av < x$total))
        add("atenção", ab, x$linha_planilha, "Total", sprintf("%s no %s: %s indivíduos%s. Número desse tamanho (coro, bando, colônia) costuma ser estimativa, não contagem.", x$especie, x$ponto, x$total,
            if (!is.na(x$vo) && x$vo > 0) " por canto" else " (sem forma anotada)"),
            "Anotar se foi contagem ou estimativa (ex.: classe 11 a 20, 21 a 50, mais de 50). O relatório trata esses casos com cautela.")
    }
  }
  sf <- !is.na(r$sem_forma) & r$sem_forma > 0
  if (any(sf))
    add("informação", ab, paste(r$linha_planilha[sf], collapse = ", "), "Forma de registro",
        sprintf("%d registros (%d indivíduos) sem forma de registro anotada, nos pontos %s.", sum(sf), sum(r$sem_forma[sf]), paste(sort(unique(r$ponto[sf])), collapse = ", ")),
        "Se souber, separar em VO (canto) e AV (visual). Não muda as análises, mas o laudo costuma pedir.")
  # 2. pontos
  if (any(is.na(r$ponto) | r$ponto == ""))
    for (i in which(is.na(r$ponto) | r$ponto == "")) add("erro", ab, r$linha_planilha[i], "Ponto", "Registro sem ponto.", "Preencher o ponto.", tipo = "ponto", especie = r$especie[i])
  if (!is.null(L$pontos)) {
    fora <- setdiff(unique(r$ponto), L$pontos$ponto)
    for (p in fora) add("erro", ab, paste(r$linha_planilha[r$ponto == p], collapse = ", "), "Ponto",
        sprintf("Ponto \"%s\" não existe na aba %s.", p, L$aba_pontos), "Corrigir o nome do ponto ou incluir o ponto na aba de pontos.", tipo = "ponto", ponto = p)
    sem <- setdiff(L$pontos$ponto, unique(r$ponto))
    for (p in sem) add("atenção", L$aba_pontos, "", "Ponto", sprintf("O %s não tem nenhum registro.", p),
        "Se o ponto foi amostrado e nada foi achado, ele entra com riqueza 0; se não foi amostrado, retirar da aba de pontos.")
    if (all(is.na(L$pontos$esforco)))
      add("atenção", L$aba_pontos, "", "Esforço", "Não há coluna de esforço (horas de busca, nº de pessoas) por ponto.",
          "Incluir uma coluna 'Esforço (h)' na aba de pontos. Sem ela, o programa supõe esforço igual em todos os pontos.")
    # 2b. coordenadas (19/11/2026, F05): impossível = erro; ilegível, vazia ou fora do Brasil = atenção.
    # Só confere quando a planilha tem alguma coordenada (sem a coluna, não há o que avisar ponto a ponto).
    co <- L$pontos$coordenadas
    if (any(!is.na(co) & trimws(co) != "")) for (i in seq_along(co)) {
      pt <- L$pontos$ponto[i]; s <- co[i]
      dica <- "Use graus decimais (ex.: -15.70 -47.80) ou UTM (ex.: 22 K 668628 m E 8080967 m S)."
      if (is.na(s) || trimws(s) == "") { add("atenção", L$aba_pontos, "", "Coordenadas", sprintf("O %s não tem coordenada.", pt),
          paste("Preencher a coordenada; sem ela as distâncias entre pontos não são calculadas.", dica), tipo = "coordenada", ponto = pt); next }
      k <- ler_coordenada(s)
      if (is.null(k)) add("atenção", L$aba_pontos, "", "Coordenadas", sprintf("Não consegui ler a coordenada do %s (\"%s\").", pt, s), dica, tipo = "coordenada", ponto = pt, dados = s)
      else if (!is.na(k$problema)) add("erro", L$aba_pontos, "", "Coordenadas", sprintf("Coordenada impossível no %s (\"%s\"): %s.", pt, s, k$problema),
          paste("Corrigir a coordenada.", dica), tipo = "coordenada", ponto = pt, dados = s)
      else if (!k$no_brasil) add("atenção", L$aba_pontos, "", "Coordenadas", sprintf("A coordenada do %s (\"%s\") fica fora do Brasil.", pt, s),
          "Conferir sinal (latitude sul e longitude oeste são negativas), a ordem latitude/longitude e a zona UTM.", tipo = "coordenada", ponto = pt, dados = s)
    }
  }
  # 3. nomes
  for (i in seq_len(nrow(r))) {
    x <- r[i, ]
    if (is.na(x$especie_completa) || x$especie_completa == "") {
      add("erro", ab, x$linha_planilha, "Espécie", "Registro sem nome de espécie.", "Preencher o nome."); next
    }
    w <- strsplit(x$especie, " ")[[1]]
    if (!grepl("^[A-Z][a-z]+$", w[1]))
      add("atenção", ab, x$linha_planilha, "Espécie", sprintf("Gênero \"%s\" fora do padrão (inicial maiúscula, resto minúsculo).", w[1]), "Corrigir a grafia.", tipo = "nome", especie = x$especie, dados = x$especie_completa)
    if (length(w) >= 2 && !(w[2] %in% c("sp.", "cf.", "aff.", "gr.")) && !grepl("^[a-z-]+$", w[2]))
      add("atenção", ab, x$linha_planilha, "Espécie", sprintf("Epíteto \"%s\" fora do padrão (tudo minúsculo).", w[2]), "Corrigir a grafia.", tipo = "nome", especie = x$especie, dados = x$especie_completa)
  }
  # mesmo binômio com autoria, família ou grupo diferentes
  for (e in unique(r$especie)) {
    s <- r[r$especie == e, ]
    if (length(unique(s$especie_completa)) > 1)
      add("atenção", ab, paste(s$linha_planilha, collapse = ", "), "Espécie", sprintf("%s escrita de %d jeitos: %s.", e, length(unique(s$especie_completa)),
          paste(unique(s$especie_completa), collapse = " | ")), "Padronizar o nome com a mesma autoria.", tipo = "nome", especie = e, dados = paste(unique(s$especie_completa), collapse = " | "))
    fam <- unique(na.omit(s$familia[s$familia != ""]))
    if (length(fam) > 1)
      add("erro", ab, paste(s$linha_planilha, collapse = ", "), "Família", sprintf("%s aparece em famílias diferentes: %s.", e, paste(fam, collapse = ", ")), "Deixar uma família só.", tipo = "familia", especie = e, dados = paste(fam, collapse = " | "))
    if (length(unique(s$grupo_pad)) > 1)
      add("erro", ab, paste(s$linha_planilha, collapse = ", "), "Grupo", sprintf("%s aparece em grupos diferentes.", e), "Corrigir o grupo.", tipo = "grupo", especie = e, dados = paste(unique(s$grupo), collapse = " | "))
    if (any(is.na(s$familia) | s$familia == ""))
      add("atenção", ab, paste(s$linha_planilha[is.na(s$familia) | s$familia == ""], collapse = ", "), "Família", sprintf("%s sem família.", e), "Preencher a família.", tipo = "familia", especie = e)
  }
  # mesmo ponto e espécie em mais de uma linha (soma, mas avisa)
  ch <- paste(r$ponto, r$especie, r$campanha)
  for (k in unique(ch[duplicated(ch)])) {
    s <- r[ch == k, ]
    add("informação", ab, paste(s$linha_planilha, collapse = ", "), "Ponto + espécie",
        sprintf("%s no %s aparece em %d linhas; as contagens válidas somam %s.", s$especie[1], s$ponto[1], nrow(s), somar_totais_de_campo(s$total)),
        "Confirmar que não é a mesma anotação repetida.")
  }
  # identificações incompletas
  for (e in unique(r$especie[grepl(" (sp\\.|cf\\.|aff\\.|gr\\.)", r$especie)])) {
    gen <- sub(" .*$", "", e)
    irmas <- setdiff(unique(r$especie[r$genero == gen]), e)
    irmas <- irmas[!grepl(" (sp\\.|cf\\.|aff\\.|gr\\.)", irmas)]
    if (length(irmas))
      add("atenção", ab, paste(r$linha_planilha[r$especie == e], collapse = ", "), "Espécie",
          sprintf("%s pode ser a mesma espécie já registrada (%s). Fica fora da riqueza e das análises de composição; entra só na abundância total.", e, paste(irmas, collapse = ", ")),
          "Se a gravação ou a foto permitir, identificar até espécie.", tipo = "nome", especie = e, dados = paste(irmas, collapse = " | "))
    else
      add("informação", ab, paste(r$linha_planilha[r$especie == e], collapse = ", "), "Espécie",
          sprintf("%s é o único representante do gênero e conta como uma espécie (morfoespécie).", e), "Nada a fazer.")
  }
  # 3b. atributos das espécies que faltam (só para os atributos que a planilha usa)
  if (!is.null(L$consolidada)) {
    at <- c(endemismo = "Endemismo", habito = "Hábito", dependencia = "Dependência", sensibilidade = "Sensibilidade", salve = "SALVE", iucn = "IUCN")
    k0 <- L$consolidada; ex <- L$atributos_extra
    valor <- function(e, a) { v <- if (!is.null(ex) && e %in% ex$especie && a %in% names(ex)) ex[[a]][match(e, ex$especie)] else NA
      if (is.na(v) && a %in% names(k0)) v <- k0[[a]][match(e, k0$especie)]; v }
    usados <- names(at)[vapply(names(at), function(a) a %in% names(k0) && any(!is.na(k0[[a]])), TRUE)]
    if (length(usados)) for (e in sort(unique(r$especie[!grepl(" (sp\\.|cf\\.|aff\\.|gr\\.)", r$especie)]))) {
      falta <- usados[vapply(usados, function(a) is.na(valor(e, a)), TRUE)]
      if (length(falta)) add("atenção", L$aba_consolidada, k0$linha_planilha[match(e, k0$especie)] %||% "", "Atributos",
        sprintf("%s sem: %s.", e, paste(at[falta], collapse = ", ")), "Preencher no quadro 'Informações que faltam' acima.", tipo = "atributos", especie = e, dados = paste(falta, collapse = "|"))
    }
  }
  # 4. comparação com a aba consolidada
  if (!is.null(L$consolidada)) {
    k <- L$consolidada; ac <- L$aba_consolidada
    pcols <- grep("^P\\d+$", names(k), value = TRUE)
    for (i in seq_len(nrow(k))) {
      soma <- sum(unlist(k[i, pcols]), na.rm = TRUE)
      if (!is.na(k$total_informado[i]) && k$total_informado[i] != soma)
        add("erro", ac, k$linha_planilha[i], "Total", sprintf("%s: total %s, mas a soma dos pontos dá %s.", k$especie[i], k$total_informado[i], soma), "Corrigir o total ou a fórmula.", tipo = "consolidada_total", especie = k$especie[i], dados = soma)
      for (p in pcols) {
        v_con <- k[[p]][i]; if (is.na(v_con)) v_con <- 0
        v_reg <- somar_totais_de_campo(r$total[r$especie == k$especie[i] & r$ponto == p])
        if (v_con != v_reg)
          add("erro", ac, k$linha_planilha[i], p, sprintf("%s no %s: %s na aba consolidada e %s nos registros de campo.", k$especie[i], p, v_con, v_reg),
              "Decidir qual está certo.", tipo = "consolidada_ponto", especie = k$especie[i], ponto = p, dados = paste(v_con, v_reg, sep = "|"))
      }
    }
    for (e in setdiff(unique(r$especie), k$especie))
      add("erro", ac, "", "Espécie", sprintf("%s está nos registros de campo mas não na aba consolidada.", e), "Incluir na consolidada.", tipo = "falta_na_consolidada", especie = e)
    for (e in setdiff(k$especie, unique(r$especie)))
      add("erro", ac, k$linha_planilha[k$especie == e], "Espécie", sprintf("%s está na aba consolidada mas não nos registros de campo.", e), "Responda nos pontos desta espécie (linhas acima) qual valor está certo.", tipo = "so_consolidada", especie = e)
  }
  if (!length(out)) return(data.frame(nivel = character(), aba = character(), linha = character(), campo = character(), problema = character(), sugestao = character(),
                                      tipo = character(), especie = character(), ponto = character(), dados = character()))
  o <- do.call(rbind, out)
  o[order(match(o$nivel, c("erro", "atenção", "informação"))), ]
}


# ---- conciliação: o que está na consolidada e falta nos registros de campo entra nas análises ----
# Para cada espécie e ponto: se a consolidada tem MAIS indivíduos que os registros, a diferença vira um registro
# "vindo da consolidada" (sem forma de registro anotada). Se os registros têm mais, valem os registros.
# A planilha original não muda; tudo o que foi completado aparece na conferência e na tabela 00.
completar_com_consolidada <- function(L) {
  k <- L$consolidada; r <- L$registros
  vazio <- data.frame(especie = character(), ponto = character(), na_consolidada = numeric(), nos_registros = numeric(), adicionados = numeric(), linha_consolidada = character())
  if (is.null(k) || !nrow(k)) { L$completados <- vazio; return(L) }
  if (length(unique(r$campanha)) > 1) { L$completados <- vazio; L$aviso_completar <- "Há mais de uma campanha nos registros e a consolidada não separa campanhas: nada foi completado automaticamente."; return(L) }
  pcols <- grep("^P\\d+$", names(k), value = TRUE); novos <- list(); feitos <- list()
  for (i in seq_len(nrow(k))) for (p in pcols) {
    v_con <- k[[p]][i]; if (is.na(v_con) || v_con <= 0) next
    v_reg <- somar_totais_de_campo(r$total[r$especie == k$especie[i] & r$ponto == p])
    if (v_con > v_reg) {
      dif <- v_con - v_reg
      novos[[length(novos) + 1]] <- data.frame(linha_planilha = paste("consolidada", k$linha_planilha[i]), ponto_original = p, ponto = p,
        especie_completa = k$especie_completa[i], familia = k$familia[i] %||% NA_character_, grupo = k$grupo[i] %||% NA_character_,
        vo = NA_real_, av = NA_real_, sem_forma = dif, total_txt = as.character(dif), total = dif, campanha = r$campanha[1],
        anotacao = "completado pela aba consolidada", observacao = sprintf("Veio da aba %s (linha %s): %s na consolidada, %s nos registros.", L$aba_consolidada, k$linha_planilha[i], v_con, v_reg),
        stringsAsFactors = FALSE)
      feitos[[length(feitos) + 1]] <- data.frame(especie = k$especie[i], ponto = p, na_consolidada = v_con, nos_registros = v_reg, adicionados = dif,
        linha_consolidada = as.character(k$linha_planilha[i]), stringsAsFactors = FALSE)
    }
  }
  if (!length(novos)) { L$completados <- vazio; return(L) }
  n <- do.call(rbind, novos)
  n$especie <- binomio(n$especie_completa); n$genero <- sub(" .*$", "", n$especie)
  n$grupo_pad <- grupo_padrao(n$grupo)   # mesmo padrão de grupo dos registros (Anuros, Répteis, Aves...)
  for (cc in setdiff(names(r), names(n))) n[[cc]] <- NA
  L$registros <- rbind(r, n[, names(r)])
  L$completados <- do.call(rbind, feitos)
  L
}

conferencia_completados <- function(L) {
  cp <- L$completados
  if (!is.null(L$aviso_completar)) return(data.frame(nivel = "atenção", aba = L$aba_consolidada %||% "", linha = "", campo = "Conciliação", tipo = "", especie = "", ponto = "", dados = "", problema = L$aviso_completar,
    sugestao = "Confira a consolidada com os registros de cada campanha.", stringsAsFactors = FALSE))
  if (is.null(cp) || !nrow(cp)) return(NULL)
  data.frame(nivel = "atenção", aba = L$aba_registros, linha = paste("consolidada", cp$linha_consolidada), campo = "Completado", tipo = "consolidada_ponto", especie = cp$especie, ponto = cp$ponto,
    dados = paste(cp$na_consolidada, cp$nos_registros, sep = "|"),
    problema = sprintf("%s no %s: %s na consolidada e %s nos registros. As análises usaram %s (+%s vindo da consolidada).", cp$especie, cp$ponto, cp$na_consolidada, cp$nos_registros, cp$na_consolidada, cp$adicionados),
    sugestao = "Se estiver certo, inclua o registro na aba de registros de campo (com a forma de registro); se não, corrija a consolidada.", stringsAsFactors = FALSE)
}
