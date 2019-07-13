library(tsviz)
library(ggplot2)

sysfonts::font_add_google("Roboto", "roboto")
showtext::showtext_auto()

p <- ggplot(aes(x = Date, y = BTC), data = crypto_prices) +
  geom_line(color = "white") +
  theme_void()

hexSticker::sticker(p,
  package = "tsviz",
  p_family = "roboto",
  p_size = 20,
  p_x = 1,
  p_y = 1.5,
  s_x = 1,
  s_y = 0.8,
  s_width = 1.2,
  s_height = 0.8,
  h_fill = "#001e51",
  h_color = "#11b52b",
  filename = "man/figures/tsviz_large.png"
)

shinyhelper <- magick::image_read("man/figures/tsviz_large.png")
magick::image_scale(shinyhelper, "130") %>%
  magick::image_write(image = ., path = "man/figures/tsviz.png", format = "png")
