# Gera a planilha de EXEMPLO FICTÍCIO do Herpetodados (exemplo/planilha_exemplo_ficticia.xlsx).
# Os nomes das espécies são reais, mas pontos, coordenadas e contagens são inventados só para testar o programa.
# Uso: Rscript exemplo/gerar_exemplo.R
local({ raiz <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
  libs <- c(Sys.getenv("HERP_RLIB"), file.path(raiz, "r-pacotes"), "C:/DadosBio/envs/R-herpetodados")
  .libPaths(c(libs[nzchar(libs) & dir.exists(libs)], .libPaths())) })
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
set.seed(42)

esp <- data.frame(rbind(
  c("Anuros", "Bufonidae", "Rhinella diptycha (Cope, 1862)"),
  c("Anuros", "Hylidae", "Boana albopunctata (Spix, 1824)"),
  c("Anuros", "Hylidae", "Boana raniceps (Cope, 1862)"),
  c("Anuros", "Hylidae", "Dendropsophus minutus (Peters, 1872)"),
  c("Anuros", "Hylidae", "Dendropsophus nanus (Boulenger, 1889)"),
  c("Anuros", "Hylidae", "Scinax fuscovarius (Lutz, 1925)"),
  c("Anuros", "Hylidae", "Scinax fuscomarginatus (Lutz, 1925)"),
  c("Anuros", "Leptodactylidae", "Leptodactylus fuscus (Schneider, 1799)"),
  c("Anuros", "Leptodactylidae", "Leptodactylus labyrinthicus (Spix, 1824)"),
  c("Anuros", "Leptodactylidae", "Physalaemus cuvieri Fitzinger, 1826"),
  c("Anuros", "Leptodactylidae", "Physalaemus nattereri (Steindachner, 1863)"),
  c("Anuros", "Leptodactylidae", "Pseudopaludicola mystacalis (Cope, 1887)"),
  c("Anuros", "Microhylidae", "Elachistocleis cesarii (Miranda-Ribeiro, 1920)"),
  c("Répteis", "Tropiduridae", "Tropidurus torquatus (Wied-Neuwied, 1820)"),
  c("Répteis", "Teiidae", "Ameiva ameiva (Linnaeus, 1758)"),
  c("Répteis", "Viperidae", "Bothrops moojeni Hoge, 1966"),
  c("Répteis", "Dipsadidae", "Oxyrhopus guibei Hoge & Romano, 1978"),
  c("Aves", "Cariamidae", "Cariama cristata (Linnaeus, 1766)"),
  c("Aves", "Picidae", "Colaptes campestris (Vieillot, 1818)"),
  c("Aves", "Columbidae", "Columbina talpacoti (Temminck, 1810)"),
  c("Aves", "Tyrannidae", "Pitangus sulphuratus (Linnaeus, 1766)"),
  c("Aves", "Thraupidae", "Volatinia jacarina (Linnaeus, 1766)"),
  c("Aves", "Furnariidae", "Furnarius rufus (Gmelin, 1788)"),
  c("Aves", "Psittacidae", "Eupsittula aurea (Gmelin, 1788)")), stringsAsFactors = FALSE)
names(esp) <- c("Grupo", "Família", "Espécie")

pontos <- data.frame(
  `Sítios amostrais` = sprintf("Ponto %02d", 1:6),
  Fitofisionomia = c("MG", "MG/VR", "MG", "CE", "CE", "CE/AA"),
  `Grupo amostral` = c("Mata", "Mata", "Mata", "Aberto", "Aberto", "Aberto"),
  Coordenadas = sprintf("%.6f° %.6f°", -15.70 - c(0, .012, .025, .041, .055, .068), -47.80 - c(0, .015, .006, .031, .044, .052)),
  `Esforço (h)` = 2, Métodos = "Busca ativa e ponto de escuta", check.names = FALSE)

# probabilidade de ocorrer depende do ambiente (mata x aberto) para parecer um levantamento de verdade
pref <- c(rep(c(.7, .5), length.out = 13), .3, .6, .5, .4, rep(.5, 7))
reg <- list()
for (i in seq_len(nrow(pontos))) for (j in seq_len(nrow(esp))) {
  mata <- pontos$`Grupo amostral`[i] == "Mata"
  p <- if (mata) pref[j] else 1 - pref[j] * .8
  if (runif(1) < p * .75) {
    n <- if (esp$Grupo[j] == "Anuros") rpois(1, sample(c(2, 6, 15), 1)) + 1 else rpois(1, 2) + 1
    vo <- if (esp$Grupo[j] %in% c("Anuros", "Aves")) round(n * runif(1, .4, 1)) else 0
    reg[[length(reg) + 1]] <- data.frame(Campanha = "Campanha 1 (exemplo)", Ponto = pontos$`Sítios amostrais`[i], `Espécie` = esp$Espécie[j],
      `Família` = esp$Família[j], Grupo = esp$Grupo[j], VO = vo, AV = n - vo, Total = n, check.names = FALSE)
  }
}
reg <- do.call(rbind, reg)
leia <- data.frame(`EXEMPLO FICTÍCIO` = c(
  "Esta planilha é só para testar o Herpetodados.",
  "Os nomes das espécies são reais, mas pontos, coordenadas, fitofisionomias e contagens são INVENTADOS.",
  "Não use estes números em nenhum relatório.",
  "Siglas das fitofisionomias: MG = Mata de Galeria; VR = Vereda; CE = Cerrado sentido restrito; AA = Área antropizada.",
  "Formato: aba 'Dados brutos' com um registro por linha (Ponto, Espécie, Família, Grupo, VO, AV, Total) e aba 'Pontos Amostrais'."), check.names = FALSE)
arq <- file.path(aqui, "planilha_exemplo_ficticia.xlsx")
writexl::write_xlsx(list(`Leia-me` = leia, `Dados brutos (exemplo)` = reg, `Pontos Amostrais` = pontos), arq)
cat(nrow(reg), "registros,", length(unique(reg$Espécie)), "espécies em", arq, "\n")
