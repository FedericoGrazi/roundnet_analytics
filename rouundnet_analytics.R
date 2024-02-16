library(tidyverse)
library(readxl)

game <- read_excel("C:/Users/Feder/Downloads/SPD Vs BB.xlsx")

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dat <- circleFun(c(1,-1),2.3,npoints = 130)


set.seed(1234)
p <- game %>% 
  mutate(coords1 = 
           case_when(
             Set == 1 ~ -.25,
             Set == 2 ~ -.25,
             Set == 3 ~ -.75,
             Set == 4 ~ -1.25,
             Set == 5 ~ -1.75,
             Set == 6 ~ -1.75,
             Set == 7 ~ -1.25,
             Set == 8 ~ -.75,
           ),
         coords2 = 
           case_when(
             Set == 1 ~ .75,
             Set == 2 ~ 1.25,
             Set == 3 ~ 1.75,
             Set == 4 ~ 1.75,
             Set == 5 ~ 1.25,
             Set == 6 ~ .75,
             Set == 7 ~ .25,
             Set == 8 ~ .25
           )) %>% 
  group_by(Plays) %>% 
  mutate(
    coords1 = coords1 + rnorm(1,0,.15),
    coords2 = coords2 + rnorm(1,0,.15),
  ) %>% 
  ggplot(aes(coords2,coords1)) +
  # geom_point(col = "black")
  stat_density_2d(aes(fill = ..density..), geom = "raster",contour = F,show.legend = F)+
  geom_path(aes(dat$x,dat$y),col = "black", size = 2)+
  scale_fill_distiller(palette = 7, direction = 1,limits = c(0.08,
                                                             max(apply(MARGIN = 1,
                                                                      X = as.data.frame(MASS::kde2d(fame$coords1,fame$coords2)$z),
                                                                      FUN = max))
                                                             ),
                       na.value = "white") +
  theme_classic()+
  annotate("text", x = 1, y = .4,size = 9, label = "Server", col = "black")+
  annotate("text", x = 1, y = .4,size = 9, label = "Server", col = "black")+
  xlab("")+ylab("")+
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "white")
    )

ggsave("D:/Personal Statistics/set_dens.png", p, width = 10, height = 11)  


## SET DENS for SDP ###

set.seed(1)
game_spd <- game %>% 
  filter(NameSet %in% c("P1","P2")) %>% 
  mutate(coords1 = 
           case_when(
             Set == 1 ~ -.25,
             Set == 2 ~ -.25,
             Set == 3 ~ -.75,
             Set == 4 ~ -1.25,
             Set == 5 ~ -1.75,
             Set == 6 ~ -1.75,
             Set == 7 ~ -1.25,
             Set == 8 ~ -.75,
           ),
         coords2 = 
           case_when(
             Set == 1 ~ .75,
             Set == 2 ~ 1.25,
             Set == 3 ~ 1.75,
             Set == 4 ~ 1.75,
             Set == 5 ~ 1.25,
             Set == 6 ~ .75,
             Set == 7 ~ .25,
             Set == 8 ~ .25
           )) %>% 
  group_by(Plays) %>% 
  mutate(
    coords1 = coords1 + rnorm(1,0,.15),
    coords2 = coords2 + rnorm(1,0,.15),
  )

dat_spd <- circleFun(c(1,-1),2.3,npoints = nrow(game_spd))

spd <- ggplot(game_spd,aes(coords2,coords1)) +
  # geom_point(col = "black")
  stat_density_2d(aes(fill = ..density..), geom = "raster",contour = F,show.legend = F)+
  geom_path(aes(dat_spd$x,dat_spd$y),col = "black", size = 2)+
  scale_fill_distiller(palette = 7, direction = 1,limits = c(0.08,
                                                             max(apply(MARGIN = 1,
                                                                       X = as.data.frame(MASS::kde2d(game_spd$coords1,game_spd$coords2)$z),
                                                                       FUN = max)+.01)
  ),
  na.value = "white") +
  theme_classic()+
  annotate("text", x = 1, y = .4,size = 9, label = "Server", col = "black")+
  annotate("text", x = 1, y = .4,size = 9, label = "Server", col = "black")+
  xlab("")+ylab("")+
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "white")
  )+
  ggtitle("Setting for Swedish Pole Dancer")

ggsave("D:/Personal Statistics/set_dens_SPD.png", spd, width = 10, height = 11)  
