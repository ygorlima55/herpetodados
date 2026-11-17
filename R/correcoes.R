# Herpetodados: correções feitas no painel (botão "Corrigir" da conferência).
# Ficam em correcoes.json na pasta do levantamento e são aplicadas sobre os dados lidos, antes da conferência e das análises.
# A planilha enviada não muda; a "planilha_corrigida.xlsx" mostra o resultado e a lista do que foi alterado.
#
# Tipos de correção (cada uma com id, data e motivo):
#   incluir_registro      ponto, especie_completa, familia, grupo, vo, av, sem_forma
#   editar_registro       linhas (vetor), e os campos a trocar: total, vo, av, sem_forma, ponto, especie_completa, familia, grupo
#   remover_registro      linhas
#   renomear              de (binômio), para (nome completo com autoria): troca em todas as linhas e na consolidada
#   definir_familia       especie (binômio), familia
#   definir_grupo         especie, grupo
#   editar_consolidada    especie, ponto (P1...) e valor; ou especie e total_informado
#   incluir_na_consolidada especie (usa os números dos registros)
#   definir_atributos     especie, valores = {habito, endemismo, dependencia, sensibilidade, salve, iucn, nome_comum}

ler_correcoes <- function(arq) {
  if (!file.exists(arq)) return(list())
  x <- tryCatch(jsonlite::fromJSON(arq, simplifyVector = FALSE), error = function(e) list())
  if (is.list(x)) x else list()
}

aplicar_correcoes <- function(L, corr) {
  if (!length(corr)) { L$correcoes_aplicadas <- NULL; return(L) }
  r <- L$registros; k <- L$consolidada
  r$alterado <- ""
  log <- list()
  nota <- function(c, txt, ok = TRUE) log[[length(log) + 1]] <<- data.frame(id = c$id %||% "", data = c$data %||% "", tipo = c$tipo %||% "",
    o_que = txt, motivo = c$motivo %||% "", aplicada = if (ok) "sim" else "não (dados mudaram)", stringsAsFactors = FALSE)
  num <- function(v) if (is.null(v) || identical(v, "")) NA_real_ else suppressWarnings(as.numeric(v))
  for (c in corr) {
    tp <- c$tipo %||% ""
    if (tp == "incluir_registro") {
      vo <- num(c$vo); av <- num(c$av); sf <- num(c$sem_forma); tot <- sum(c(vo, av, sf), na.rm = TRUE)
      nl <- r[1, ]; nl[] <- NA
      nl$linha_planilha <- paste("correção", c$id %||% ""); nl$ponto_original <- c$ponto; nl$ponto <- norm_ponto(c$ponto)
      nl$especie_completa <- c$especie_completa; nl$familia <- c$familia %||% NA; nl$grupo <- c$grupo %||% NA
      nl$vo <- vo; nl$av <- av; nl$sem_forma <- sf; nl$total <- tot; nl$total_txt <- as.character(tot)
      nl$campanha <- r$campanha[1]; nl$anotacao <- "incluído pela correção no painel"; nl$observacao <- c$motivo %||% ""; nl$alterado <- "incluída"
      r <- rbind(r, nl)
      nota(c, sprintf("Incluído: %s no %s, %s indivíduos (VO %s, AV %s, sem forma %s).", c$especie_completa, c$ponto, tot, vo %||% "-", av %||% "-", sf %||% "-"))
    } else if (tp %in% c("editar_registro", "remover_registro")) {
      ln <- as.character(unlist(c$linhas)); w <- which(as.character(r$linha_planilha) %in% ln)
      if (!length(w)) { nota(c, sprintf("Linhas %s não encontradas.", paste(ln, collapse = ", ")), FALSE); next }
      if (tp == "remover_registro") { nota(c, sprintf("Removidas as linhas %s (%s).", paste(ln, collapse = ", "), paste(unique(r$especie_completa[w]), collapse = "; "))); r <- r[-w, ]; next }
      mud <- character()
      for (campo in intersect(names(c), c("total", "vo", "av", "sem_forma", "ponto", "especie_completa", "familia", "grupo"))) {
        v <- c[[campo]]
        if (campo %in% c("total", "vo", "av", "sem_forma")) v <- num(v)
        if (campo == "ponto") { r$ponto_original[w] <- v; r$ponto[w] <- norm_ponto(v) } else r[[campo]][w] <- v
        if (campo == "total") r$total_txt[w] <- as.character(v)
        mud <- c(mud, sprintf("%s = %s", campo, if (is.na(v)) "vazio" else v))
      }
      r$alterado[w] <- paste(r$alterado[w], paste(mud, collapse = "; "))
      nota(c, sprintf("Linha %s: %s.", paste(ln, collapse = ", "), paste(mud, collapse = "; ")))
    } else if (tp == "renomear") {
      w <- which(r$especie == c$de)
      r$especie_completa[w] <- c$para; r$alterado[w] <- paste(r$alterado[w], "nome")
      if (!is.null(k)) { j <- which(k$especie == c$de); k$especie_completa[j] <- c$para; k$especie[j] <- binomio(c$para) }
      nota(c, sprintf("Nome %s -> %s (%d linhas).", c$de, c$para, length(w)))
    } else if (tp %in% c("definir_familia", "definir_grupo")) {
      campo <- if (tp == "definir_familia") "familia" else "grupo"; w <- which(r$especie == c$especie)
      r[[campo]][w] <- c[[campo]]; r$alterado[w] <- paste(r$alterado[w], campo)
      nota(c, sprintf("%s de %s = %s (%d linhas).", if (campo == "familia") "Família" else "Grupo", c$especie, c[[campo]], length(w)))
    } else if (tp == "editar_consolidada" && !is.null(k)) {
      j <- which(k$especie == c$especie)
      if (!length(j)) { nota(c, sprintf("%s não está na consolidada.", c$especie), FALSE); next }
      if (!is.null(c$ponto) && c$ponto %in% names(k)) { k[[c$ponto]][j] <- num(c$valor); nota(c, sprintf("Consolidada: %s no %s = %s.", c$especie, c$ponto, c$valor)) }
      if (!is.null(c$total_informado)) { k$total_informado[j] <- num(c$total_informado); nota(c, sprintf("Consolidada: total de %s = %s.", c$especie, c$total_informado)) }
    } else if (tp == "definir_atributos") {
      v <- c$valores; if (!length(v)) next
      j <- if (!is.null(k)) match(c$especie, k$especie) else NA
      if (!is.na(j)) { for (a in names(v)) if (a %in% names(k)) k[[a]][j] <- as.character(v[[a]]) }
      else { ex <- L$atributos_extra; nl <- data.frame(especie = c$especie, stringsAsFactors = FALSE); for (a in names(v)) nl[[a]] <- as.character(v[[a]])
        L$atributos_extra <- if (is.null(ex)) nl else { for (cc in setdiff(names(nl), names(ex))) ex[[cc]] <- NA; for (cc in setdiff(names(ex), names(nl))) nl[[cc]] <- NA; rbind(ex[ex$especie != c$especie, ], nl[, names(ex)]) } }
      nota(c, sprintf("Atributos de %s: %s.", c$especie, paste(sprintf("%s = %s", names(v), unlist(v)), collapse = "; ")))
    } else if (tp == "incluir_na_consolidada" && !is.null(k)) {
      s <- r[r$especie == c$especie, ]; if (!nrow(s)) next
      nl <- k[1, ]; nl[] <- NA; nl$linha_planilha <- NA; nl$especie_completa <- s$especie_completa[1]; nl$especie <- c$especie
      for (p in grep("^P\\d+$", names(k), value = TRUE)) nl[[p]] <- sum(s$total[s$ponto == p], na.rm = TRUE)
      nl$total_informado <- sum(s$total, na.rm = TRUE); nl$familia <- s$familia[1]; nl$grupo <- s$grupo_pad[1]
      k <- rbind(k, nl); nota(c, sprintf("Consolidada: incluída %s com os números dos registros.", c$especie))
    }
  }
  # recalcula o que vem do nome, do grupo e do ponto
  r$especie <- binomio(r$especie_completa); r$genero <- sub(" .*$", "", r$especie); r$grupo_pad <- grupo_padrao(r$grupo)
  r$alterado <- trimws(r$alterado)
  L$registros <- r; L$consolidada <- k
  L$correcoes_aplicadas <- if (length(log)) do.call(rbind, log) else NULL
  L
}

# planilha corrigida para o usuário baixar: registros como ficaram, o que entrou pela consolidada e a lista de correções
gravar_planilha_corrigida <- function(L, saida) {
  r <- L$registros
  out <- data.frame(Campanha = r$campanha, Ponto = r$ponto_original, `Espécie` = r$especie_completa, `Família` = r$familia, Grupo = r$grupo,
    VO = r$vo, AV = r$av, `Sem forma anotada` = r$sem_forma, Total = r$total, `Observação` = r$observacao,
    `Alterado no Herpetodados` = ifelse(is.na(r$alterado %||% NA) | (r$alterado %||% "") == "", ifelse(grepl("consolidada", r$anotacao %||% ""), "completado pela consolidada", ""), r$alterado),
    check.names = FALSE, stringsAsFactors = FALSE)
  folhas <- list(`Dados brutos (corrigido)` = out)
  if (!is.null(L$correcoes_aplicadas)) folhas[["Correções feitas"]] <- L$correcoes_aplicadas
  if (!is.null(L$completados) && nrow(L$completados)) folhas[["Completados pela consolidada"]] <- L$completados
  if (length(folhas) == 1) return(invisible(NULL))
  writexl::write_xlsx(folhas, file.path(saida, "planilha_corrigida.xlsx"))
  invisible(file.path(saida, "planilha_corrigida.xlsx"))
}


# consolidada com os atributos preenchidos no painel para espécies que não estão nela (só para a descritiva)
consol_atributos <- function(L) {
  k <- L$consolidada; ex <- L$atributos_extra
  if (is.null(ex) || !nrow(ex)) return(k)
  if (is.null(k)) { ex$especie_completa <- L$registros$especie_completa[match(ex$especie, L$registros$especie)]; return(ex) }
  ex <- ex[!(ex$especie %in% k$especie), , drop = FALSE]; if (!nrow(ex)) return(k)
  nl <- k[rep(1, nrow(ex)), ]; nl[] <- NA; nl$especie <- ex$especie
  nl$especie_completa <- L$registros$especie_completa[match(ex$especie, L$registros$especie)]
  for (a in intersect(names(ex), names(k))) nl[[a]] <- ex[[a]]
  rbind(k, nl)
}
