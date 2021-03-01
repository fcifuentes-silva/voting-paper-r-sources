library(dplyr)
library(ggplot2)

path <- "datos.csv"
data <- read.csv(path, encoding='UTF-8')

data <- data %>% 
  mutate(camara = ifelse(camara == "Senado", "Senate", "Chamber of deputies")) 

summary(data)

data %>% 
  ggplot(aes( x = alineamiento_promedio, group = camara, fill = camara )) +
  geom_density(alpha=0.5, adjust=1) +
  labs(x="Alignment score", y="Total bills",
       title="" 
  )+
  
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title   = element_text(size = 10))