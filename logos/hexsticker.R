library(hexSticker)

imgurl <- system.file("logos/sl_logo_transparent.png", package="Shelby Level")

sticker(imgurl, package="Shelby Level", p_size=20, s_x=1, s_y=.75, s_width=.6,
        filename= paste0(imgurl, ".png"))

magick::image_read("logos/sl_logo_transparent.png")

plot(sticker)

imgurl <- system.file("logos/sl_logo_transparent.png", package="Shelby Level")
sticker(imgurl, package="Shelby Level", p_size=20, s_x=1, s_y=.75, s_width=.6,
        filename="logos/imgfile.png")

library(hexSticker)
library(lattice)

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
bwplot <- bwplot(counts ~ outcome | treatment, xlab=NULL, ylab=NULL, cex=.5,
                 scales=list(cex=.5), par.strip.text=list(cex=.5))

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Nunito Sans", "Montserrat")
## Automatically use showtext to render text for future devices
showtext_auto()

imgurl <- system.file("star-icon.svg", package="Shelby Level")
sticker("logos/sl_logo_transparent.png", package= NULL, p_size=20, s_x=1, s_y=.85, s_width=.9, s_height=.9, h_fill="#7fa6ad", h_color="#45767a", p_family = 'Montserrat', filename="logos/sl_sticker.png")
