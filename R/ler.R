# Herpetodados: leitura da planilha de campo e conversão para tabela longa.
# Formato aceito: aba de registros (um registro por linha) com colunas Ponto, Espécie e
# contagem (Total ou Abundância). Família, Grupo/Classe, VO, AV e Campanha são opcionais.

copiar_planilha <- function(arquivo) {
  # Copia para a pasta temporária: o R não abre arquivo bloqueado pelo Excel nem
  # (no unzip) caminho com caracteres como "°". A cópia pelo Windows contorna os dois.
  tmp <- tempfile(fileext = ".xlsx")
  ok <- suppressWarnings(file.copy(arquivo, tmp))
  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) {
    Sys.setenv(HERP_SRC = arquivo, HERP_DST = tmp)
    system2("powershell", c("-NoProfile", "-Command",
      shQuote("Copy-Item -LiteralPath $env:HERP_SRC -Destination $env:HERP_DST -Force")))
  }
  if (!file.exists(tmp)) stop("Não consegui abrir a planilha: ", arquivo)
  tmp
}

norm_txt <- function(x) {
  x <- gsub(" ", " ", as.character(x))
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

sem_acento <- function(x) tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT"))

acha_coluna <- function(nomes, padroes) {
  n <- sem_acento(norm_txt(nomes))
  for (p in padroes) {
    i <- which(grepl(p, n))
    if (length(i)) return(i[1])
  }
  NA_integer_
}

# "Ponto 05", "P5", "5", "Ponto 5" -> "P5"
norm_ponto <- function(x) {
  x <- norm_txt(x)
  num <- suppressWarnings(as.integer(sub("^\\D*(\\d+).*$", "\\1", x)))
  ifelse(is.na(num), x, paste0("P", num))
}

# binômio sem autoria: "Rhinella diptycha (Cope, 1862)" -> "Rhinella diptycha"
binomio <- function(x) {
  x <- norm_txt(x)
  w <- strsplit(x, " ")
  vapply(w, function(v) {
    if (length(v) < 2) return(v[1] %||% "")
    if (v[2] %in% c("sp.", "sp", "spp.", "cf.", "aff.", "gr.")) {
      if (v[2] %in% c("cf.", "aff.", "gr.") && length(v) >= 3) return(paste(v[1:3], collapse = " "))
      return(paste(v[1], "sp."))
    }
    paste(v[1:2], collapse = " ")
  }, "")
}
# nome padronizado do grupo de fauna (aceita o que o campo costuma escrever)
grupo_padrao <- function(x) {
  g <- sem_acento(ifelse(is.na(x), "", x))
  out <- ifelse(grepl("anur|amphib|anfib|sapo|\\bras?\\b", g), "Anuros",
         ifelse(grepl("rept|squam|serp|lagart|testud|crocod|quelon|cobra", g), "Répteis",
         ifelse(grepl("^aves?$|ornit|passar|avifauna|\\bave", g), "Aves",
         ifelse(grepl("mamif|masto|mammal|quiropt|morceg|roedor|primat", g), "Mamíferos",
         ifelse(grepl("peixe|ictio|actinop", g), "Peixes",
         ifelse(grepl("inset|entomo|abelha|formiga|besouro|borbolet|mosquito|culicid|hymenop|coleop|lepidop|dipter", g), "Insetos",
         ifelse(grepl("aracn|aranha|escorpi", g), "Aracnídeos",
         ifelse(g == "", "Todas as espécies", tools::toTitleCase(norm_txt(x))))))))))
  out
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && !is.list(a) && is.na(a))) b else a

ler_planilha <- function(arquivo) {
  tmp <- copiar_planilha(arquivo)
  abas <- readxl::excel_sheets(tmp)
  a_bru <- abas[grepl("brut|registro", sem_acento(abas))][1]
  if (is.na(a_bru)) stop("Não achei a aba de registros (nome com 'brutos' ou 'registros'). Abas: ",
                          paste(abas, collapse = ", "))
  d <- as.data.frame(readxl::read_excel(tmp, sheet = a_bru, col_types = "text"))
  nm <- names(d)
  col <- list(
    ponto   = acha_coluna(nm, c("^ponto$", "ponto amostral", "^ponto", "sitio")),
    especie = acha_coluna(nm, c("nome valido", "^especie$", "especie")),
    familia = acha_coluna(nm, c("^familia")),
    grupo   = acha_coluna(nm, c("^grupo", "^classe")),
    vo      = acha_coluna(nm, c("^vo$", "vocaliza")),
    av      = acha_coluna(nm, c("^av$", "avista|visual")),
    semf    = acha_coluna(nm, c("sem forma")),
    total   = acha_coluna(nm, c("^total$", "abundancia", "^n$", "quantidade")),
    campanha= acha_coluna(nm, c("^campanha")),
    anot    = acha_coluna(nm, c("anotacao")),
    obs     = acha_coluna(nm, c("observ"))
  )
  if (is.na(col$ponto) || is.na(col$especie) || is.na(col$total))
    stop("A aba '", a_bru, "' precisa das colunas Ponto, Espécie e Total (ou Abundância).")
  pega <- function(k) if (is.na(col[[k]])) rep(NA_character_, nrow(d)) else norm_txt(d[[col[[k]]]])
  num  <- function(k) if (is.na(col[[k]])) rep(NA_real_, nrow(d)) else suppressWarnings(as.numeric(gsub(",", ".", d[[col[[k]]]])))
  reg <- data.frame(
    linha_planilha = seq_len(nrow(d)) + 1L,
    ponto_original = pega("ponto"),
    ponto    = norm_ponto(pega("ponto")),
    especie_completa = pega("especie"),
    familia  = pega("familia"),
    grupo    = pega("grupo"),
    vo = num("vo"), av = num("av"), sem_forma = num("semf"),
    total_txt = pega("total"),
    total = num("total"),
    campanha = pega("campanha"),
    anotacao = pega("anot"),
    observacao = pega("obs"),
    stringsAsFactors = FALSE
  )
  vazia <- (is.na(reg$especie_completa) | reg$especie_completa == "") &
           (is.na(reg$ponto_original) | reg$ponto_original == "")
  reg <- reg[!vazia, ]
  reg$especie <- binomio(reg$especie_completa)
  reg$genero  <- sub(" .*$", "", reg$especie)
  g <- sem_acento(ifelse(is.na(reg$grupo), "", reg$grupo))
  reg$grupo_pad <- grupo_padrao(reg$grupo)
  if (all(is.na(reg$campanha) | reg$campanha == "")) reg$campanha <- "Campanha única"

  # Aba de pontos (fitofisionomia, coordenadas, método)
  a_pt <- abas[grepl("ponto", sem_acento(abas)) & !grepl("brut", sem_acento(abas))][1]
  pontos <- NULL
  if (!is.na(a_pt)) {
    p <- as.data.frame(readxl::read_excel(tmp, sheet = a_pt, col_types = "text", .name_repair = "minimal"))
    # o modelo antigo tem uma linha de títulos acima do cabeçalho
    if (!any(grepl("sitio|ponto", sem_acento(names(p))))) {
      h <- which(apply(p, 1, function(r) any(grepl("sitio|ponto", sem_acento(r)))))[1]
      if (!is.na(h)) { names(p) <- norm_txt(unlist(p[h, ])); p <- p[-(1:h), , drop = FALSE] }
    }
    cp <- acha_coluna(names(p), c("sitio", "ponto"))
    cf <- acha_coluna(names(p), c("fitofision", "ambiente", "habitat"))
    cc <- acha_coluna(names(p), c("coorden"))
    cm <- acha_coluna(names(p), c("metodo"))
    cg <- acha_coluna(names(p), c("grupo amostral", "^grupo", "tratamento"))
    ce <- acha_coluna(names(p), c("esforco", "horas"))
    if (!is.na(cp)) {
      pontos <- data.frame(
        ponto = norm_ponto(p[[cp]]),
        nome = norm_txt(p[[cp]]),
        fitofisionomia = if (!is.na(cf)) norm_txt(p[[cf]]) else NA,
        coordenadas = if (!is.na(cc)) norm_txt(p[[cc]]) else NA,
        metodo = if (!is.na(cm)) norm_txt(p[[cm]]) else NA,
        grupo_amostral = if (!is.na(cg)) norm_txt(p[[cg]]) else NA,
        esforco = if (!is.na(ce)) suppressWarnings(as.numeric(gsub(",", ".", p[[ce]]))) else NA,
        stringsAsFactors = FALSE)
      pontos <- pontos[!is.na(pontos$nome) & pontos$nome != "" & grepl("\\d", pontos$nome), ]
    }
  }

  # Aba consolidada (para o corretor comparar com os registros)
  a_con <- abas[grepl("consolid", sem_acento(abas))][1]
  consol <- if (!is.na(a_con)) ler_consolidada(tmp, a_con) else NULL
  # família e grupo de cada espécie da consolidada (vêm dos registros)
  if (!is.null(consol)) {
    consol$familia <- reg$familia[match(consol$especie, reg$especie)]
    consol$grupo <- reg$grupo_pad[match(consol$especie, reg$especie)]
  }
  list(arquivo = arquivo, aba_registros = a_bru, aba_pontos = a_pt, aba_consolidada = a_con,
       registros = reg, pontos = pontos, consolidada = consol)
}

# lê a aba consolidada (espécie x P1..Pn, cabeçalho em duas linhas) com os atributos das espécies
ler_consolidada <- function(tmp, a_con) {
  consol <- NULL
    k <- as.data.frame(readxl::read_excel(tmp, sheet = a_con, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    h <- which(apply(k, 1, function(r) any(grepl("^P ?\\d+$", norm_txt(r)))))[1]
    if (!is.na(h)) {
      cab <- norm_txt(unlist(k[h, ]))
      cps <- which(grepl("^P ?\\d+$", cab))
      ctot <- which(sem_acento(cab) == "total")[1]
      linhas <- k[(h + 1):nrow(k), , drop = FALSE]
      nomes <- norm_txt(linhas[[1]])
      vals <- sapply(cps, function(j) suppressWarnings(as.numeric(gsub(",", ".", ifelse(norm_txt(linhas[[j]]) %in% c("-", "–", ""), "0", linhas[[j]])))))
      tem <- rowSums(!is.na(vals)) > 0 & !is.na(nomes) & !(sem_acento(nomes) %in% c("total", ""))
      if (any(tem)) {
        consol <- data.frame(linha_planilha = which(tem) + h, especie_completa = nomes[tem],
                             especie = binomio(nomes[tem]), stringsAsFactors = FALSE)
        m <- vals[tem, , drop = FALSE]; colnames(m) <- gsub(" ", "", cab[cps])
        consol <- cbind(consol, as.data.frame(m))
        consol$total_informado <- if (!is.na(ctot)) suppressWarnings(as.numeric(gsub(",", ".", linhas[[ctot]][tem]))) else NA
        # atributos das espécies (cabeçalho em duas linhas: usa a de baixo quando preenchida)
        cab1 <- if (h > 1) norm_txt(unlist(k[h - 1, ])) else rep("", length(cab))
        cab2 <- sem_acento(ifelse(is.na(cab) | cab == "", cab1, cab)); cab1s <- sem_acento(cab1)
        pega_at <- function(pad, onde = cab2) { j <- which(grepl(pad, onde))[1]; if (is.na(j)) rep(NA_character_, sum(tem)) else { v <- norm_txt(linhas[[j]][tem]); v[v %in% c("", "-", "–")] <- NA; v } }
        consol$nome_comum <- pega_at("nome comum", cab1s)
        consol$endemismo <- pega_at("^end")
        consol$habito <- pega_at("^hab")
        consol$dependencia <- pega_at("^dep")
        consol$sensibilidade <- pega_at("^sens")
        consol$forma_registro <- pega_at("forma")
        consol$salve <- pega_at("salve|icmbio|mma")
        consol$iucn <- pega_at("iucn")
      }
    }
  consol
}
