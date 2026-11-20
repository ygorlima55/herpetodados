# Quadrante: junta planilhas de campanhas diferentes numa planilha nova, com a coluna Campanha.
# Uso: Rscript juntar_campanhas.R <saida.xlsx> "<nome campanha 1>" <planilha1.xlsx> "<nome campanha 2>" <planilha2.xlsx> [...]
# - Planilha com aba de registros (brutos): os registros entram como estão.
# - Planilha só com a aba consolidada (espécie x P1..Pn): cada célula com número vira um registro (Total),
#   com família e grupo tirados das linhas de título da consolidada; VO e AV ficam vazios.
# - Pontos: a aba de pontos da ÚLTIMA planilha que tiver uma.
# - Consolidada nova: união das espécies, totais por ponto SOMADOS entre campanhas e atributos
#   (nome comum, End., Háb...) da campanha mais recente que os tiver. As originais não são alteradas.

# pacotes do R: pasta r-pacotes do programa (criada pelo instalar.cmd), ou HERP_RLIB, ou a biblioteca da máquina
local({ raiz <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
  libs <- c(Sys.getenv("HERP_RLIB"), file.path(raiz, "r-pacotes"), "C:/DadosBio/envs/R-herpetodados")
  .libPaths(c(libs[nzchar(libs) & dir.exists(libs)], .libPaths())) })
suppressPackageStartupMessages({ library(readxl); library(writexl) })
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(aqui, "ler.R"), encoding = "UTF-8")
a <- commandArgs(TRUE)
if (length(a) < 5 || (length(a) - 1) %% 2) stop("Uso: juntar_campanhas.R saida.xlsx \"Campanha 1\" planilha1.xlsx \"Campanha 2\" planilha2.xlsx")
saida <- a[1]; camps <- a[seq(2, length(a), 2)]; arqs <- a[seq(3, length(a), 2)]

grupo_de <- function(x) { s <- sem_acento(x); if (grepl("anur|amphib|anfib", s)) "Anuros" else if (grepl("rept|squam|serp", s)) "Reptilia" else NA }
registros <- list(); pontos <- NULL; consols <- list(); notas <- character()
for (i in seq_along(arqs)) {
  tmp <- copiar_planilha(normalizePath(arqs[i], mustWork = TRUE)); abas <- excel_sheets(tmp)
  L <- tryCatch(ler_planilha(tmp), error = function(e) NULL)
  a_pt <- abas[grepl("ponto", sem_acento(abas)) & !grepl("brut|analis", sem_acento(abas))][1]
  if (!is.na(a_pt)) pontos <- as.data.frame(read_excel(tmp, a_pt, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  a_con <- abas[grepl("consolid", sem_acento(abas))][1]
  k <- if (!is.na(a_con)) as.data.frame(read_excel(tmp, a_con, col_names = FALSE, col_types = "text", .name_repair = "minimal")) else NULL
  # família e grupo de cada linha da consolidada, pelas linhas de título acima dela
  fam_k <- grp_k <- rep(NA_character_, if (is.null(k)) 0 else nrow(k))
  if (!is.null(k)) { f <- NA; g <- NA
    for (j in seq_len(nrow(k))) { t <- norm_txt(k[j, 1]); if (is.na(t) || t == "") next
      if (!is.na(grupo_de(t)) && !grepl(" ", t)) { g <- grupo_de(t); next }
      if (grepl("idae\\b|idae \\(", t) && !grepl("[a-z]+ [a-z]+ \\(", t) && length(strsplit(t, " ")[[1]]) <= 3) { f <- t; next }
      if (sem_acento(t) != "total") { fam_k[j] <- f; grp_k[j] <- g } } }
  if (!is.null(L) && nrow(L$registros) && !grepl("consolid", sem_acento(L$aba_registros))) {
    r <- L$registros
    registros[[i]] <- data.frame(Campanha = camps[i], Ponto = r$ponto_original, `Espécie (nome válido)` = r$especie_completa, `Família` = r$familia,
      Grupo = r$grupo, VO = r$vo, AV = r$av, `Sem forma anotada` = r$sem_forma, Total = r$total, `Observação` = r$observacao, check.names = FALSE, stringsAsFactors = FALSE)
    notas <- c(notas, sprintf("%s: %d registros da aba '%s' de %s.", camps[i], nrow(r), L$aba_registros, basename(arqs[i])))
  } else if (!is.null(k)) {
    h <- which(apply(k, 1, function(x) any(grepl("^P ?\\d+$", norm_txt(x)))))[1]; cab <- norm_txt(unlist(k[h, ])); cps <- which(grepl("^P ?\\d+$", cab))
    out <- list()
    for (j in (h + 1):nrow(k)) { nome <- norm_txt(k[j, 1]); if (is.na(nome) || is.na(fam_k[j]) || sem_acento(nome) == "total") next
      for (c in cps) { v <- suppressWarnings(as.numeric(gsub(",", ".", k[j, c]))); if (!is.na(v) && v > 0)
        out[[length(out) + 1]] <- data.frame(Campanha = camps[i], Ponto = paste0("Ponto ", sprintf("%02d", as.integer(sub("\\D+", "", cab[c])))),
          `Espécie (nome válido)` = nome, `Família` = fam_k[j], Grupo = grp_k[j], VO = NA_real_, AV = NA_real_, `Sem forma anotada` = NA_real_, Total = v,
          `Observação` = sprintf("Vem da aba '%s' de %s (só total por ponto; sem VO/AV por registro).", a_con, basename(arqs[i])), check.names = FALSE, stringsAsFactors = FALSE) } }
    registros[[i]] <- do.call(rbind, out)
    notas <- c(notas, sprintf("%s: %d registros montados da aba consolidada '%s' de %s (sem aba de registros).", camps[i], length(out), a_con, basename(arqs[i])))
  }
  if (!is.na(a_con)) consols[[i]] <- ler_consolidada(tmp, a_con)
}
reg <- do.call(rbind, registros)
# grupo como texto padronizado
reg$Grupo <- ifelse(grepl("anur", sem_acento(reg$Grupo)), "Anuros", ifelse(grepl("rept", sem_acento(reg$Grupo)), "Reptilia", reg$Grupo))

# consolidada unida (totais somados; atributos da campanha mais recente que tiver)
pts <- sort(unique(norm_ponto(reg$Ponto))); pts <- pts[order(as.integer(sub("\\D+", "", pts)))]
reg$esp <- binomio(reg$`Espécie (nome válido)`)
ordem <- unique(reg[order(reg$Grupo, reg$`Família`, reg$esp), c("Grupo", "Família", "esp")])
atr <- c("nome_comum", "endemismo", "habito", "dependencia", "sensibilidade", "forma_registro", "salve", "iucn")
linhas <- list(c("Ordem/ Família/ Espécie", "Nome comum", "Pontos amostrais", rep("", length(pts) - 1), "", "End.", "Háb.", "Dep.", "Sens.", "Forma de Registro", "Status", ""),
               c("", "", pts, "Total", rep("", 5), "Salve", "IUCN"))
gant <- fant <- ""
for (j in seq_len(nrow(ordem))) {
  o <- ordem[j, ]; if (!identical(o$Grupo, gant)) { linhas[[length(linhas) + 1]] <- c(if (identical(o$Grupo, "Anuros")) "Anuros" else "Reptilia", rep("", length(pts) + 10)); gant <- o$Grupo; fant <- "" }
  if (!identical(o$`Família`, fant)) { linhas[[length(linhas) + 1]] <- c(o$`Família`, rep("", length(pts) + 10)); fant <- o$`Família` }
  rr <- reg[reg$esp == o$esp, ]; v <- vapply(pts, function(p) sum(rr$Total[norm_ponto(rr$Ponto) == p]), 0)
  at <- rep(NA_character_, length(atr)); nome <- rr$`Espécie (nome válido)`[1]
  for (q in rev(consols)) if (!is.null(q)) { w <- which(q$especie == o$esp)[1]; if (!is.na(w)) { at <- ifelse(is.na(at), unlist(q[w, atr]), at); nome <- q$especie_completa[w] %||% nome } }
  linhas[[length(linhas) + 1]] <- c(nome, at[1] %||% "", ifelse(v > 0, as.character(v), "-"), as.character(sum(v)), ifelse(is.na(at[2:8]), "", at[2:8]))
}
tot <- vapply(pts, function(p) sum(reg$Total[norm_ponto(reg$Ponto) == p]), 0)
linhas[[length(linhas) + 1]] <- c("Total", "", as.character(tot), as.character(sum(tot)), rep("", 7))
larg <- max(lengths(linhas)); cons <- as.data.frame(do.call(rbind, lapply(linhas, function(x) c(x, rep("", larg - length(x))))), stringsAsFactors = FALSE)

reg$esp <- NULL
notas <- c(paste("Planilha montada pelo Quadrante (juntar_campanhas.R) em", format(Sys.time(), "%d/%m/%Y %H:%M")), notas,
           "A aba 'Dados consolidados' desta planilha SOMA as campanhas por ponto; para os números de cada campanha, ver a coluna Campanha em 'Dados brutos (campo)'.",
           "As planilhas originais não foram alteradas.")
# primeira linha vira cabeçalho (nomes vazios repetidos viram espaços, para o Excel aceitar)
cabeca <- function(d, h = 1) { n <- ifelse(is.na(unlist(d[h, ])), "", unlist(d[h, ])); v <- which(n == ""); n[v] <- strrep(" ", seq_along(v))
  d <- d[-(1:h), , drop = FALSE]; names(d) <- n; rownames(d) <- NULL; d }
hp <- which(apply(pontos, 1, function(x) any(grepl("sitio|ponto", sem_acento(x)))))[1]
write_xlsx(list(`Pontos Amostrais` = cabeca(pontos, hp), `Dados consolidados` = cabeca(cons), `Dados brutos (campo)` = reg, `Origem` = data.frame(Nota = notas)), saida)
cat(paste(notas, collapse = "\n"), "\n", nrow(reg), "registros gravados em", saida, "\n")
