# @author fcifuentes
# @crdate 2020-12-01
library(ggplot2)
library(dplyr)
library(grid)
library(RColorBrewer)


make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}
colores <- c("#FFBBBB","#FFFFFF","#BBFFBB")



votosFavor <- 0:100
votosFavor %>% View

votosContra <- 100:0
votosContra %>% View

votosGrafico <- data.frame(favor=votosFavor,contra= votosContra)
votosGrafico %>% View

votosGrafico <- votosGrafico %>% mutate(numer = favor*favor + contra*contra) %>% 
  mutate(denom = (favor+contra)*(favor+contra)) %>% 
  mutate(aparty = round(numer/denom, 2)) %>% 
  mutate(rice_party = abs(favor-contra)/(favor+contra))   %>% 
  mutate(nuevo = round((cos(pi*(1-rice_party))/2)+0.5,2))  


votosGrafico %>% 
  View

votosGrafico<- votosGrafico %>% 
  mutate(total = favor+contra) %>% 
  mutate(cfav = favor/total, ccon=contra/total) 

votosGrafico<- votosGrafico %>% 
  mutate(stdev = apply(votosGrafico[9:10],1,sd)) %>% 
  #mutate(stdev = sd(c(cfav,ccon))) %>% 
  mutate(polarizacion = round(1-stdev*sqrt(2), 2)) %>% 
  mutate(polarizacion2 = 1-rice_party) 

g <- make_gradient(
  deg = 180, n = 500, cols = colores
)
#--- grafico polarizacion
votosGrafico %>%  
  ggplot() +
  annotation_custom(
    grob = g#, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) + 
  
  geom_line( aes(x=contra, y=polarizacion2, color="Polarization"),linetype = "solid")+
  
  
  
  scale_x_continuous(breaks = scales::extended_breaks(5),labels=c("100% / 0%", "25% / 75%",  "50% / 50%","25% / 75%","0% / 100%" ))+
  # scale_color_discrete(name = "Vote discipline metrics", labels = c( 'Cos-Rice-index',"Rice-index", "Alignment")) +
  scale_y_continuous(labels = scales::percent) +
  
  labs(y = "Polarization", x="% of Yes / No") + 
  geom_vline(xintercept = 50 ) +
  #theme(axis.title.y =  element_text(margin = margin(t = 100)))
  theme(axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10),
        axis.title.y =  element_text(margin = margin(t = 100)), 
        legend.title = element_blank(), legend.position="bottom", 
        legend.margin=margin(), legend.box="vertical",
        legend.text =  element_text(size = 10), strip.text=element_text(size=10))