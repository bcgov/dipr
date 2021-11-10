library(hexSticker)
library(magick)

filename <- "inst/sticker/JJA_AMDI.jpg"

download.file('https://upload.wikimedia.org/wikipedia/commons/1/10/Robert_Havell_after_John_James_Audubon%2C_American_Water_Ouzel%2C_1837%2C_NGA_32511.jpg',
              destfile = filename)

img <- image_read(filename)
img_data <- image_data(img)
ratio <- dim(img_data)[2] / dim(img_data)[3]

height <- 1.2
width <- height * ratio

# Path for Mac - need path for Windows
sysfonts::font_add("Academy Engraved LET", "/System/Library/Fonts/Supplemental/Academy Engraved LET Fonts.ttf")

sticker(filename, package="dipr",
        s_x=1, s_y=0.75, s_height = height, s_width = width,
        p_size=24, p_color = "#60574E", p_family = "Academy Engraved LET",
        h_color = "#9A7F64", h_size = 1.3,
        white_around_sticker = TRUE,
        filename="inst/sticker/sticker.png")
