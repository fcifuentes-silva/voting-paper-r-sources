library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

votos <- read_xlsx("votingDataAll.xlsx") 
#votos <- read_xlsx("5146.xlsx") 
votos %>% 
  View()
votaciones <- votos %>% 
  distinct(voting)


datos <- votos %>% 
  
  filter(voteValue == "http://datos.bcn.cl/ontologies/bcn-resources#aFavor" |
         voteValue == "http://datos.bcn.cl/ontologies/bcn-resources#enContra") %>% 
  select(voteValue, voting) %>% 
  group_by(voting, voteValue) %>% 
  count() %>% 
  spread(voteValue, n) 


datos<- datos %>% rename(favor= `http://datos.bcn.cl/ontologies/bcn-resources#aFavor`, 
                 contra=`http://datos.bcn.cl/ontologies/bcn-resources#enContra`) 
datos<- datos %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

datos %>% View()
#--- calculo de polarizacion
polarizacion <- datos %>% 
  mutate(total = favor+contra) %>% 
  mutate(cfav = favor/total, ccon=contra/total) %>% 
  mutate(stdev = sd(c(cfav,ccon))) %>% 
  mutate(polarizacion = round(1-stdev*sqrt(2), 2)) 

polarizacion %>% View
  


##-- calculo de alineamiento

votos %>% View

alineamiento <- votos %>% 
  
  filter(voteValue == "http://datos.bcn.cl/ontologies/bcn-resources#aFavor" |
           voteValue == "http://datos.bcn.cl/ontologies/bcn-resources#enContra") %>% 
  select(party, voteValue, voting) %>% 
  group_by( voting,party, voteValue) %>% 
  count() %>% 
  spread(voteValue, n) 

alineamiento<- alineamiento %>% rename(favor= `http://datos.bcn.cl/ontologies/bcn-resources#aFavor`, 
                         contra=`http://datos.bcn.cl/ontologies/bcn-resources#enContra`) 
alineamiento<- alineamiento %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

alineamiento %>% View

alineamiento <- alineamiento %>% 
  mutate(numer = favor*favor + contra*contra) %>% 
  mutate(denom = (favor+contra)*(favor+contra)) %>% 
  mutate(aparty = round(numer/denom, 2)) %>% 
  mutate(rice_party = abs(favor-contra)/(favor+contra)) 

alineamiento %>% View

alineamiento<- alineamiento %>% 
  
  select(voting, aparty) %>% 
  group_by(voting) %>%
  summarise_at(vars(aparty), funs(mean(., na.rm=TRUE))) %>% 
  rename(alineamiento = aparty)

resumen<- alineamiento %>% 
  inner_join(polarizacion, by = "voting") %>% 
  select(voting, favor, contra, alineamiento, polarizacion)

polarizacion %>% View
alineamiento %>% View
resumen %>% View

#--- agregar la camara al join
camaras <- votos %>% 
  filter(voteValue == "http://datos.bcn.cl/ontologies/bcn-resources#aFavor" |
           voteValue == "http://datos.bcn.cl/ontologies/bcn-resources#enContra") %>% 
  group_by(voting, camera) %>% 
  select(camera, voting) %>% 
  count() 



resumen <- resumen %>% 
  inner_join(camaras, by = "voting") 

resumen %>% 
  View
  
library(ggplot2)
resumen %>% 
ggplot(aes(x=alineamiento, y=polarizacion)) 
+ geom_point()

ggplot(resumen, aes(x=alineamiento, y=polarizacion, , shape=camera, color=camera)) +
  geom_point(size=1, shape=2)

resumen %>% 
  mutate(rice_index = abs(favor-contra)/(favor+contra)) %>% 
  View



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


votosGrafico %>%  
  ggplot() +
  geom_line(aes(x=contra, y=aparty, color="red"))+
  
  geom_line( aes(x=contra, y=rice_party, color="green"),linetype = "dashed")+
  
  
  geom_line( aes(x=contra, y=nuevo, color="blue"),linetype = "dotdash",size=0.7)+
  scale_x_continuous(breaks = scales::extended_breaks(5),labels=c("0% - 100%", "25% - 75%", "50% - 50%", "25% - 75%", "100% - 0%"))+
  scale_color_discrete(name = "Vote discipline metrics", labels = c( 'Cos-Rice-index',"Rice-index", "Alignment")) +
  scale_y_continuous(labels = scales::percent) +
  #scale_color_manual(values = c( 'grey',    'black',     'blue')) +
  labs(y = "Vote discipline", x="% of Yes - No") +

  geom_vline(xintercept = 50) 
  



votosGrafico %>% mutate(numer = favor*favor + contra*contra) %>% 
  mutate(denom = (favor+contra)*(favor+contra)) %>% 
  mutate(aparty = round(numer/denom, 2)) %>% 
  mutate(rice_party = abs(favor-contra)/(favor+contra))  %>% 
  mutate(nuevo = (cos(pi*(1-rice_party))/2)+0.5)  %>% 
  View



#-*---

library(ggplot2) 
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
#g <- make_gradient(
#  deg = 180, n = 500, cols = brewer.pal(9, "RdBu")
#)

g <- make_gradient(
  deg = 180, n = 500, cols = colores
)

ggplot(mtcars, aes(factor(cyl))) +
  annotation_custom(
    grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) + 
  geom_bar()

#--- gráfico de alineamiento
votosGrafico %>%  
  ggplot() +
  annotation_custom(
    grob = g#, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) + 
  geom_line(aes(x=contra, y=aparty, color="red"))+
  
  geom_line( aes(x=contra, y=rice_party, color="green"),linetype = "dashed")+
  
  
  geom_line( aes(x=contra, y=nuevo, color="blue"),linetype = "dotdash",size=0.7)+
  
  scale_x_continuous(breaks = scales::extended_breaks(5),labels=c("100% / 0%", "25% / 75%",  "50% / 50%","25% / 75%","0% / 100%" ))+
  scale_color_discrete(name = "Vote discipline metrics", labels = c( 'Cos-Rice-index',"Rice-index", "Alignment")) +
  scale_y_continuous(labels = scales::percent) +

  labs(y = "Vote discipline", x="% of Yes / No") + 
  geom_vline(xintercept = 50 ) +
  #theme(axis.title.y =  element_text(margin = margin(t = 100)))
theme(axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10),
      axis.title.y =  element_text(margin = margin(t = 100)), 
      legend.title = element_blank(), legend.position="bottom", 
      legend.margin=margin(), legend.box="vertical",
      legend.text =  element_text(size = 10), strip.text=element_text(size=10))






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