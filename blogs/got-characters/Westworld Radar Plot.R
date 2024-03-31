library(tidyverse)
library(ggtext)
library(ggimage)
library(sysfonts)
library(showtext)

#import fonts for plot
sysfonts::font_add_google("montserrat")
sysfonts::font_add_google("cinzel decorative")
showtext::showtext_auto()

##coord radar hack---- use to remove last gridline, custom with ggpronto object
#https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co
coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"

  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }

  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }

  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)

            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)

            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))

            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")

            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),

              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}



#Data ----

readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv'
  ) %>%
  dplyr::filter(
    uni_name == "Game of Thrones"
    ) -> got_profiles

characters_stark <- c("Arya Stark","Brandon Stark","Catelyn Stark",
               "Eddard Stark","Jon Snow","Robb Stark",
               "Sansa Stark")

# psycho_df %>% filter(grepl('Stark|Snow', char_name)) %>% group_by(personality) %>% count() %>% arrange(-n) -> top_traits

# top 8 house traits
traits <- c("diligent","persistent","devoted","egalitarian","loyal","serious","dramatic","self-disciplined")


data <- ww_profiles %>%
  mutate(personality = case_when(personality=="dunce" ~ "genius",
                            personality=="deranged" ~"reasonable",TRUE ~ personality),
         rating = case_when(trait_max==personality ~ avg_rating, TRUE ~ 100-avg_rating))|>
  filter(character %in% characters & personality %in% traits)|>
  arrange(character, personality)|>
  #add image paths, images taken from Google Search and saved locally
  #add custom labels for x axis, use these later with geom_richtext
  mutate(image = paste0("../images/",tolower(str_replace_all(character," ","_")),".png"),
         label=paste0("<span style='color:white;font-size:10pt;'>",toupper(personality),"</span><br><span style='color:#A6ECFF;size:'10pt;'>[", round(rating),"]</span>")
  )

#create factor for characters to order them in our facet plot based on *subjective* importance (otherwise defaults to alphabetical order)
data$character<-factor(data$character, levels=c("Dolores Abernathy","Maeve Millay", "Bernard Lowe", "Charlotte Hale",
                                                "Man in Black","Teddy Flood","Ashley Stubbs", "Clementine Pennyfeather"))


#grid lines for x axis, use geom_line to add back in
line<-data.frame(x=rep(traits,2),y=c(rep(0, length(traits)),rep(100, length(traits))))

#create custom title with html/css using ggtext::element_textbox_simple
title="<span style='color:white;font-size:24pt;font-family:cinzel+decorative'>**HOUSE STARK ATTRIBUTE MATRIX**</span><br><br>
       <span style='font-size:11pt;color:#C8F3FF'>Inspired by the the HBO TV series <span style='color:white;'>**GAME OF THRONES**</span>. Personality traits scaled from 0 to 100. Data from the Open-Source Psychometrics Project.</span><br>"


#PLOT
ggplot(data, aes(y=rating, x=personality, group=character))+
  geom_polygon(fill="#44C7EA", color="#44C7EA", alpha=0.35)+
  geom_point(color="#44C7EA", size=3)+
  geom_line(data=line, mapping=aes(x=x,y=y, group=x), color="white",alpha=0.5)+
  geom_point(inherit.aes=FALSE, data=data.frame(x=traits, y=rep(100,length(traits))),
             mapping=aes(x=x, y=y),
             shape=21, fill="#1D3540", color="white", size=3)+
 geom_image(aes(x=1, y=-40,image=image), size=0.18)+
  geom_richtext(aes(label=label, y=130),label.color = NA, fill=NA, family="teko")+
  facet_wrap(~character, ncol=4)+
  scale_y_continuous(limits=c(-40,130), breaks=c(0,20,40,60,80,100))+
  coord_radar()+
  labs(x="",y="", title=title, caption="Graphic @shelbylevel")+
  theme_minimal()+
  theme(plot.background=element_rect(fill="#1D3540",color=NA),
        panel.grid.major.x=element_blank(),
        plot.title=element_textbox_simple(margin=margin(b=20)),
        strip.text=element_text(color="white", face="bold", size=11, family="montserrat"),
        axis.text=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid = element_line(color="#1794B6"),
        plot.margin=margin(t=30,b=30,r=30,l=30),
        plot.caption=element_text(color="#91CDDD",size=10),
        text= element_text(color="#44C7EA"),
  )



