# Gera identidade/herpetodados.ico (ícone do atalho) a partir da logo Sálvia, em 256, 64, 48, 32 e 16 px.
# Uso: Rscript identidade/gerar_icone.R
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
desenha <- function(arq, px) {
  grDevices::png(arq, width = px, height = px, bg = "transparent", type = "cairo")
  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit({ graphics::par(op); grDevices::dev.off() })
  graphics::plot.new(); graphics::plot.window(c(0, 44), c(44, 0), xaxs = "i", yaxs = "i", asp = 1)
  # quadrado preto com cantos arredondados
  r <- 13; t <- seq(0, pi / 2, length.out = 20)
  cx <- c(44 - r + r * cos(t), r - r * sin(t), r - r * cos(t), 44 - r + r * sin(t))
  cy <- c(r - r * sin(t), r - r * cos(t), 44 - r + r * sin(t), 44 - r + r * cos(t))
  graphics::polygon(cx, cy, col = "#161716", border = NA)
  # curva de acumulação que estabiliza
  b <- function(s) { x <- (1 - s)^3 * 8 + 3 * (1 - s)^2 * s * 12 + 3 * (1 - s) * s^2 * 18 + s^3 * 36
                     y <- (1 - s)^3 * 34 + 3 * (1 - s)^2 * s * 19 + 3 * (1 - s) * s^2 * 14 + s^3 * 12.5; cbind(x, y) }
  p <- b(seq(0, 1, length.out = 80)); lw <- px / 44 * 2.6 * 72 / 96
  graphics::lines(p[, 1], p[, 2], col = "#b9d7a8", lwd = lw, lend = 1)
  # olho de anuro com pupila horizontal
  a <- seq(0, 2 * pi, length.out = 60)
  graphics::polygon(28.5 + 4.6 * cos(a), 13.6 + 4.6 * sin(a), col = "#161716", border = "#b9d7a8", lwd = px / 44 * 2 * 72 / 96)
  graphics::rect(26, 12.7, 31, 14.6, col = "#b9d7a8", border = NA)
}
tams <- c(256, 64, 48, 32, 16); pngs <- character()
for (s in tams) { f <- tempfile(fileext = ".png"); desenha(f, s); pngs <- c(pngs, f) }
# ICO com PNG dentro (aceito pelo Windows desde o Vista)
dados <- lapply(pngs, function(f) readBin(f, "raw", file.info(f)$size))
con <- file(file.path(aqui, "herpetodados.ico"), "wb")
writeBin(as.integer(c(0, 1, length(tams))), con, size = 2, endian = "little")
off <- 6 + 16 * length(tams)
for (i in seq_along(tams)) {
  s <- tams[i]; writeBin(as.raw(c(if (s >= 256) 0 else s, if (s >= 256) 0 else s, 0, 0)), con)
  writeBin(as.integer(c(1, 32)), con, size = 2, endian = "little")
  writeBin(as.integer(c(length(dados[[i]]), off)), con, size = 4, endian = "little"); off <- off + length(dados[[i]])
}
for (d in dados) writeBin(d, con)
close(con)
cat("ok:", file.path(aqui, "herpetodados.ico"), "\n")
