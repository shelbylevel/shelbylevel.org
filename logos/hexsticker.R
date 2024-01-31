library(hexSticker)
library(showtext)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Nunito Sans", "Montserrat")
## Automatically use showtext to render text for future devices
showtext_auto()

imgurl <- system.file("star-icon.svg", package="Shelby Level")
sticker("logos/black-sl-logo-crop.png", package= NULL, p_size=20, s_x=1.01, s_y=1, s_width=.8, s_height=.9, h_fill="#7fa6ad", h_color="#45767a", p_family = 'Montserrat', filename="logos/sl_sticker.png")
