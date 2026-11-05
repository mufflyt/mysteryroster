# Regenerates the README's no-gold-standard figures in man/figures/ from the
# vignette chunks that draw them, so README and vignettes show the same plots.
# Run from the package root after installing the package (R CMD INSTALL .).
for (v in c("attribute-agreement", "lead-lag-staleness")) {
  knitr::opts_chunk$set(fig.path = file.path("man", "figures", paste0(v, "-")),
                        dev = "png", dpi = 96)
  out <- tempfile(fileext = ".md")
  knitr::knit(file.path("vignettes", paste0(v, ".Rmd")), output = out,
              envir = new.env(), quiet = TRUE)
  unlink(out)
}
