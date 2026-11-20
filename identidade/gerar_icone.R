# Gera identidade/quadrante.ico (ícone do atalho) a partir da marca Quadrante (20/11/2026), em 256, 64, 48, 32 e 16 px.
# Uso: Rscript identidade/gerar_icone.R
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
desenha <- function(arq, px) {
  grDevices::png(arq, width = px, height = px, bg = "transparent", type = "cairo")
  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit({ graphics::par(op); grDevices::dev.off() })
  graphics::plot.new(); graphics::plot.window(c(0, 44), c(44, 0), xaxs = "i", yaxs = "i", asp = 1)
  # marca Quadrante (20/11/2026): parcela em quadrantes, um quadrante terracota, registro e curva de nível
  graphics::rect(0, 0, 44, 44, col = "#2a2620", border = NA)
  pe <- if (px <= 32) 6 else 7; lado <- 44 - 2 * pe; meio <- pe + lado / 2
  graphics::rect(pe, pe, meio, meio, col = "#b5552c", border = NA)
  lw <- max(1, px / 44 * (if (px <= 32) 2.2 else 1.4) * 72 / 96)
  graphics::rect(pe, pe, 44 - pe, 44 - pe, border = "#e9dfc8", lwd = lw)
  if (px > 32) {
    graphics::segments(c(meio, pe), c(pe, meio), c(meio, 44 - pe), c(44 - pe, meio), col = "#e9dfc8", lwd = lw)
    s <- seq(0, 1, length.out = 40)
    graphics::lines(23.5 + 12 * s, 34.5 - 10.3 * (1 - (1 - s)^2), col = "#e9dfc8", lwd = lw * .7)
    a <- seq(0, 2 * pi, length.out = 30); graphics::polygon(29.5 + 1.7 * cos(a), 29 + 1.7 * sin(a), col = "#e9dfc8", border = NA)
  }
}
tams <- c(256, 64, 48, 32, 16); pngs <- character()
for (s in tams) { f <- tempfile(fileext = ".png"); desenha(f, s); pngs <- c(pngs, f) }
# ICO com PNG dentro (aceito pelo Windows desde o Vista)
dados <- lapply(pngs, function(f) readBin(f, "raw", file.info(f)$size))
con <- file(file.path(aqui, "quadrante.ico"), "wb")
writeBin(as.integer(c(0, 1, length(tams))), con, size = 2, endian = "little")
off <- 6 + 16 * length(tams)
for (i in seq_along(tams)) {
  s <- tams[i]; writeBin(as.raw(c(if (s >= 256) 0 else s, if (s >= 256) 0 else s, 0, 0)), con)
  writeBin(as.integer(c(1, 32)), con, size = 2, endian = "little")
  writeBin(as.integer(c(length(dados[[i]]), off)), con, size = 4, endian = "little"); off <- off + length(dados[[i]])
}
for (d in dados) writeBin(d, con)
close(con)
cat("ok:", file.path(aqui, "quadrante.ico"), "\n")
