library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

scaleFUN <- function(x) sprintf("%.f", x)

leyes <- read_xlsx("estadisticas_legislativas.xlsx",sheet = "leyes") 
leyes %>% 
  ggplot()  + 
  geom_line(mapping=aes(y = mociones, x=año, color ="Parliamentarian")) + 
  geom_line(mapping=aes(y = mensajes, x=año, color="President")) +
  labs(y = "Total laws by bill type", x="") +
  scale_x_continuous(labels=scaleFUN,breaks = scales::extended_breaks(10)) +
  scale_color_manual(values = c(
    'Parliamentarian' = 'black',
    'President' = '#999999')) +
  labs(color = '') +
  theme(axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10),
        legend.title = element_blank(), legend.position="bottom", 
        legend.margin=margin(), legend.box="vertical",
        legend.text =  element_text(size = 10), strip.text=element_text(size=10)) +
  
  geom_vline(size=0.2, xintercept = 2010) +
  geom_vline(size=0.2, xintercept = 2014) +
  geom_vline(size=0.2, xintercept = 2018) +
# Add the annotations
  annotate("text", x = (2009 ), y = 65, label = "Bachelet", size=8) +
  annotate("text", x = (2012), y = 65, label = "Piñera", size=8) +
  annotate("text", x = (2016), y = 65, label = "Bachelet", size=8)+
  annotate("text", x = (2019), y = 65, label = "Piñera", size=8)

##--------- grafico proyectos

proyectos <- read_xlsx("estadisticas_legislativas.xlsx", sheet = "proyectos") 
proyectos %>% 
  ggplot( aes(x=año, y = total, fill=tipo)) + 
  
  geom_bar(position="stack", stat="identity") + 
  
 
  scale_fill_manual(values = c("#999999", "#555555")) +
  labs(y = "Total bills", x="") +
  scale_x_continuous(labels=scaleFUN,breaks = scales::extended_breaks(10)) +
  theme(axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10),
        legend.title = element_blank(), legend.position="bottom", 
        legend.margin=margin(), legend.box="vertical",
        legend.text =  element_text(size = 10), strip.text=element_text(size=10)) +
  
  geom_vline(size=0.2, xintercept = 2010) +
  geom_vline(size=0.2, xintercept = 2014) +
  geom_vline(size=0.2, xintercept = 2018) +
  # Add the annotations
  annotate("text", x = (2009 ), y = 850, label = "Bachelet") +
  annotate("text", x = (2012), y = 850, label = "Piñera") +
  annotate("text", x = (2016), y = 850, label = "Bachelet")+
  annotate("text", x = (2019), y = 850, label = "Piñera")


#### ----- aprobados

aprobados <- read_xlsx("estadisticas_legislativas.xlsx",sheet = "aprobados") 
aprobados %>% 
  ggplot()  + 
  geom_line(mapping=aes(y = mociones, x=año, color ="Parliamentarian")) + 
  geom_line(mapping=aes(y = mensajes, x=año, color="President")) +
  labs(y = "Total approved laws by bill type", x="") +
  scale_x_continuous(labels=scaleFUN,breaks = scales::extended_breaks(10)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c(
    'Parliamentarian' = 'black',
    'President' = '#999999')) +
  labs(color = '') +
  theme(axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10),
        legend.title = element_blank(), legend.position="bottom", 
        legend.margin=margin(), legend.box="vertical",
        legend.text =  element_text(size = 10), strip.text=element_text(size=10)) +
  geom_vline(size=0.2, xintercept = 2010) +
  geom_vline(size=0.2, xintercept = 2014) +
  geom_vline(size=0.2, xintercept = 2018) +
  # Add the annotations
  annotate("text", x = (2009 ), y = 1, label = "Bachelet") +
  annotate("text", x = (2012), y = 1, label = "Piñera") +
  annotate("text", x = (2016), y = 1, label = "Bachelet")+
  annotate("text", x = (2019), y = 1, label = "Piñera")

