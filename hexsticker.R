library(hexSticker)

imgurl <- system.file("logos/sl_logo_transparent.png", package="Shelby Level")

sticker(imgurl, package="Shelby Level", p_size=20, s_x=1, s_y=.75, s_width=.6,
        filename= paste0(imgurl, ".png"))

magick::image_read("logos/sl_logo_transparent.png")

plot(sticker)
