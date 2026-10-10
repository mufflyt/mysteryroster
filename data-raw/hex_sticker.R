# Hex sticker for mysteryroster: dark hexagon on cream, flat cream icon, no text
# (same family style as mysterycall's logo). Re-run to regenerate man/figures/logo.png.
library(ggplot2)
navy  <- "#1F2A33"; cream <- "#F6F3E7"; shade <- "#D9D5C6"; mark <- "#F6F3E7"
hex <- function(cx, cy, r) { a <- seq(30, 390, by = 60) * pi / 180; data.frame(x = cx + r * cos(a), y = cy + r * sin(a)) }
rrect <- function(x0, y0, x1, y1, r = 0.06, n = 12) {           # rounded rectangle outline points
  a <- seq(0, pi / 2, length.out = n)
  rbind(data.frame(x = x1 - r + r * cos(a),        y = y1 - r + r * sin(a)),
        data.frame(x = x0 + r + r * cos(a + pi/2), y = y1 - r + r * sin(a + pi/2)),
        data.frame(x = x0 + r + r * cos(a + pi),   y = y0 + r + r * sin(a + pi)),
        data.frame(x = x1 - r + r * cos(a + 3*pi/2), y = y0 + r + r * sin(a + 3*pi/2)))
}
circ <- function(cx, cy, r, n = 60) { a <- seq(0, 2 * pi, length.out = n); data.frame(x = cx + r * cos(a), y = cy + r * sin(a)) }
check <- function(cx, cy, s = 1) data.frame(x = cx + s * c(-0.09, -0.02, 0.11), y = cy + s * c(0.00, -0.08, 0.09))
cross <- function(cx, cy, s = 1) rbind(data.frame(x = cx + s * c(-0.08, 0.08), y = cy + s * c(-0.08, 0.08), g = 1),
                                       data.frame(x = cx + s * c(-0.08, 0.08), y = cy + s * c(0.08, -0.08), g = 2))
rows <- data.frame(y = c(0.26, 0.00, -0.26))
p <- ggplot() +
  geom_polygon(data = hex(0.05, -0.05, 1.0), aes(x, y), fill = shade) +          # soft shadow
  geom_polygon(data = hex(0, 0, 1.0), aes(x, y), fill = navy) +
  # clipboard body + clip
  geom_polygon(data = rrect(-0.52, -0.56, 0.42, 0.62, r = 0.08), aes(x, y), fill = cream) +
  geom_polygon(data = rrect(-0.16, 0.54, 0.06, 0.70, r = 0.04), aes(x, y), fill = cream) +
  geom_polygon(data = rrect(-0.12, 0.57, 0.02, 0.66, r = 0.02), aes(x, y), fill = navy) +
  # three roster rows: avatar circle + name line + mark
  geom_polygon(data = do.call(rbind, lapply(seq_len(3), function(i) transform(circ(-0.36, rows$y[i], 0.07), g = i))), aes(x, y, group = g), fill = navy) +
  geom_segment(data = rows, aes(x = -0.22, xend = 0.06, y = y, yend = y), colour = navy, linewidth = 4, lineend = "round") +
  geom_path(data = check(0.24, rows$y[1]), aes(x, y), colour = navy, linewidth = 3.2, lineend = "round", linejoin = "round") +
  geom_path(data = check(0.24, rows$y[2]), aes(x, y), colour = navy, linewidth = 3.2, lineend = "round", linejoin = "round") +
  geom_path(data = cross(0.24, rows$y[3]), aes(x, y, group = g), colour = navy, linewidth = 3.2, lineend = "round") +
  # magnifying glass over the lower-right corner
  geom_polygon(data = circ(0.46, -0.44, 0.27), aes(x, y), fill = navy) +
  geom_polygon(data = circ(0.46, -0.44, 0.19), aes(x, y), fill = cream) +
  geom_polygon(data = circ(0.46, -0.44, 0.13), aes(x, y), fill = navy) +
  geom_segment(aes(x = 0.63, xend = 0.84, y = -0.61, yend = -0.82), colour = navy, linewidth = 7, lineend = "round") +
  geom_segment(aes(x = 0.63, xend = 0.84, y = -0.61, yend = -0.82), colour = cream, linewidth = 3.2, lineend = "round") +
  coord_equal(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15), expand = FALSE) +
  theme_void() + theme(plot.background = element_rect(fill = cream, colour = NA))
ragg::agg_png("man/figures/logo.png", width = 1024, height = 1024, res = 300, background = cream); print(p); dev.off()
cat("wrote man/figures/logo.png\n")
