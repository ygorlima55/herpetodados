# Herpetodados: corretor da planilha. Confere os dados antes das análises e diz onde está cada problema.
# Níveis: "erro" (muda resultado), "atenção" (pode mudar; conferir), "informação" (registro do que foi feito).

conferir_planilha <- function(L) {
  r <- L$registros
  out <- list()
  add <- function(nivel, aba, linha, campo, problema, sugestao)
    out[[length(out) + 1]] <<- data.frame(nivel = nivel, aba = aba, linha = as.character(linha),
      campo = campo, problema = problema, sugestao = sugestao, stringsAsFactors = FALSE)
  ab <- L$aba_registros

  # 1. contagem
  for (i in seq_len(nrow(r))) {
    x <- r[i, ]
    if (is.na(x$total)) {
      add("erro", ab, x$linha_planilha, "Total", sprintf("Contagem vazia ou não numérica (\"%s\") para %s.", x$total_txt %||% "", x$especie_completa),
          "Preencher o número de indivíduos.")
    } else {
      if (x$total < 0 || x$total != round(x$total))
        add("erro", ab, x$linha_planilha, "Total", sprintf("Contagem %s não é um inteiro positivo.", x$total), "Corrigir o número.")
      if (x$total == 0)
        add("atenção", ab, x$linha_planilha, "Total", sprintf("%s com total 0 no %s.", x$especie, x$ponto), "Apagar a linha ou corrigir a contagem.")
      partes <- c(x$vo, x$av, x$sem_forma)
      if (any(!is.na(partes)) && sum(partes, na.rm = TRUE) != x$total)
        add("erro", ab, x$linha_planilha, "Total", sprintf("Total %s diferente de VO + AV + sem forma (%s) para %s no %s.",
            x$total, sum(partes, na.rm = TRUE), x$especie, x$ponto), "Conferir qual número está certo.")
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
    for (i in which(is.na(r$ponto) | r$ponto == "")) add("erro", ab, r$linha_planilha[i], "Ponto", "Registro sem ponto.", "Preencher o ponto.")
  if (!is.null(L$pontos)) {
    fora <- setdiff(unique(r$ponto), L$pontos$ponto)
    for (p in fora) add("erro", ab, paste(r$linha_planilha[r$ponto == p], collapse = ", "), "Ponto",
        sprintf("Ponto \"%s\" não existe na aba %s.", p, L$aba_pontos), "Corrigir o nome do ponto ou incluir o ponto na aba de pontos.")
    sem <- setdiff(L$pontos$ponto, unique(r$ponto))
    for (p in sem) add("atenção", L$aba_pontos, "", "Ponto", sprintf("O %s não tem nenhum registro.", p),
        "Se o ponto foi amostrado e nada foi achado, ele entra com riqueza 0; se não foi amostrado, retirar da aba de pontos.")
    if (all(is.na(L$pontos$esforco)))
      add("atenção", L$aba_pontos, "", "Esforço", "Não há coluna de esforço (horas de busca, nº de pessoas) por ponto.",
          "Incluir uma coluna 'Esforço (h)' na aba de pontos. Sem ela, o programa supõe esforço igual em todos os pontos.")
  }
  # 3. nomes
  for (i in seq_len(nrow(r))) {
    x <- r[i, ]
    if (is.na(x$especie_completa) || x$especie_completa == "") {
      add("erro", ab, x$linha_planilha, "Espécie", "Registro sem nome de espécie.", "Preencher o nome."); next
    }
    w <- strsplit(x$especie, " ")[[1]]
    if (!grepl("^[A-Z][a-z]+$", w[1]))
      add("atenção", ab, x$linha_planilha, "Espécie", sprintf("Gênero \"%s\" fora do padrão (inicial maiúscula, resto minúsculo).", w[1]), "Corrigir a grafia.")
    if (length(w) >= 2 && !(w[2] %in% c("sp.", "cf.", "aff.", "gr.")) && !grepl("^[a-z-]+$", w[2]))
      add("atenção", ab, x$linha_planilha, "Espécie", sprintf("Epíteto \"%s\" fora do padrão (tudo minúsculo).", w[2]), "Corrigir a grafia.")
  }
  # mesmo binômio com autoria, família ou grupo diferentes
  for (e in unique(r$especie)) {
    s <- r[r$especie == e, ]
    if (length(unique(s$especie_completa)) > 1)
      add("atenção", ab, paste(s$linha_planilha, collapse = ", "), "Espécie", sprintf("%s escrita de %d jeitos: %s.", e, length(unique(s$especie_completa)),
          paste(unique(s$especie_completa), collapse = " | ")), "Padronizar o nome com a mesma autoria.")
    fam <- unique(na.omit(s$familia[s$familia != ""]))
    if (length(fam) > 1)
      add("erro", ab, paste(s$linha_planilha, collapse = ", "), "Família", sprintf("%s aparece em famílias diferentes: %s.", e, paste(fam, collapse = ", ")), "Deixar uma família só.")
    if (length(unique(s$grupo_pad)) > 1)
      add("erro", ab, paste(s$linha_planilha, collapse = ", "), "Grupo", sprintf("%s aparece em grupos diferentes.", e), "Corrigir o grupo.")
    if (any(is.na(s$familia) | s$familia == ""))
      add("atenção", ab, paste(s$linha_planilha[is.na(s$familia) | s$familia == ""], collapse = ", "), "Família", sprintf("%s sem família.", e), "Preencher a família.")
  }
  # mesmo ponto e espécie em mais de uma linha (soma, mas avisa)
  ch <- paste(r$ponto, r$especie, r$campanha)
  for (k in unique(ch[duplicated(ch)])) {
    s <- r[ch == k, ]
    add("informação", ab, paste(s$linha_planilha, collapse = ", "), "Ponto + espécie",
        sprintf("%s no %s aparece em %d linhas; as contagens foram somadas (%s).", s$especie[1], s$ponto[1], nrow(s), sum(s$total, na.rm = TRUE)),
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
          "Se a gravação ou a foto permitir, identificar até espécie.")
    else
      add("informação", ab, paste(r$linha_planilha[r$especie == e], collapse = ", "), "Espécie",
          sprintf("%s é o único representante do gênero e conta como uma espécie (morfoespécie).", e), "Nada a fazer.")
  }
  # 4. comparação com a aba consolidada
  if (!is.null(L$consolidada)) {
    k <- L$consolidada; ac <- L$aba_consolidada
    pcols <- grep("^P\\d+$", names(k), value = TRUE)
    for (i in seq_len(nrow(k))) {
      soma <- sum(unlist(k[i, pcols]), na.rm = TRUE)
      if (!is.na(k$total_informado[i]) && k$total_informado[i] != soma)
        add("erro", ac, k$linha_planilha[i], "Total", sprintf("%s: total %s, mas a soma dos pontos dá %s.", k$especie[i], k$total_informado[i], soma), "Corrigir o total ou a fórmula.")
      for (p in pcols) {
        v_con <- k[[p]][i]; if (is.na(v_con)) v_con <- 0
        v_reg <- sum(r$total[r$especie == k$especie[i] & r$ponto == p], na.rm = TRUE)
        if (v_con != v_reg)
          add("erro", ac, k$linha_planilha[i], p, sprintf("%s no %s: %s na aba consolidada e %s nos registros de campo.", k$especie[i], p, v_con, v_reg),
              "Decidir qual está certo. O Herpetodados usa os registros de campo.")
      }
    }
    for (e in setdiff(unique(r$especie), k$especie))
      add("erro", ac, "", "Espécie", sprintf("%s está nos registros de campo mas não na aba consolidada.", e), "Incluir na consolidada.")
    for (e in setdiff(k$especie, unique(r$especie)))
      add("erro", ac, k$linha_planilha[k$especie == e], "Espécie", sprintf("%s está na aba consolidada mas não nos registros de campo.", e), "Incluir o registro de campo ou retirar da consolidada.")
  }
  if (!length(out)) return(data.frame(nivel = character(), aba = character(), linha = character(), campo = character(), problema = character(), sugestao = character()))
  o <- do.call(rbind, out)
  o[order(match(o$nivel, c("erro", "atenção", "informação"))), ]
}
