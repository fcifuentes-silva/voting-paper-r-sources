library(SPARQL)
library(dplyr)
library(stringr)






endpoint<-"http://datos.bcn.cl/sparql"

q <- "select ?s str(?o) as ?fecha ?i where {?s a bcnres:ProyectoDeLey; dc:date ?o; bcnres:tieneIniciativa ?i} "


prefix <- c('bcnres','<http://datos.bcn.cl/ontologies/bcn-resources#>',
            'bcnnorms','<http://datos.bcn.cl/ontologies/bcn-norms#>',
            'rdfs','<http://datos.bcn.cl/ontologies/bcn-resources#>',
            "dc","http://www.w3.org/2000/01/rdf-schema#")

d <- SPARQL(url=endpoint,
            query=q,
            ns=prefix)
is.data.frame(d$results)
View(d$results)

datos <- d$results

d2 <- datos %>% 
  mutate(anio=as.numeric(substring(fecha,1,4))) 

d2 %>% View

d2 %>% 
  filter(anio>1925) %>%
  select(anio, i) %>% 
  
  View


library (ggplot2)
d3 <-d2 %>% 
  filter(anio>1925) %>%
  select(anio, i) %>% 
  rename (x=anio, g=i)




View(d3)  
ggplot( d3, aes(x, colour = g)) + stat_ecdf()


d4 <- d3 %>% 
  count(x,g) %>% 
  rename(y=n) %>% 
  mutate(g=case_when(str_detect(g, "Mensaje") ~ "Executive",
                        str_detect(g, "Mocion") ~ "MCs",
                        str_detect(g, "Oficio") ~ "Other"))
                        
View(d4)



d4 %>% 
ggplot( aes(fill=g, y=y, x=x)) + 
  geom_bar(position="stack", stat="identity") +
  labs(y = "Total bills by type", x="Year") +
  theme(axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10),
        legend.title = element_blank(), legend.position="bottom", 
        legend.margin=margin(), legend.box="vertical",
        legend.text =  element_text(size = 10), strip.text=element_text(size=10)) +
  geom_vline(size=0.2, xintercept = 1990) +
  geom_vline(size=0.2, xintercept = 1994) +
  geom_vline(size=0.2, xintercept = 2000) +
  geom_vline(size=0.2, xintercept = 2006) +
  geom_vline(size=0.2, xintercept = 2010) +
  geom_vline(size=0.2, xintercept = 2014) +
  geom_vline(size=0.2, xintercept = 2018) +
  # Add the annotations
  annotate("text", x = (1985 ), y = 65, label = "Pinochet", size=4) +
  annotate("text", x = (1992 ), y = 65, label = "Aylwin", size=4) +
  annotate("text", x = (1997 ), y = 65, label = "Frei", size=4) +
  annotate("text", x = (2003 ), y = 65, label = "Lagos", size=4) +
  annotate("text", x = (2008 ), y = 65, label = "Bachelet", size=4) +
  annotate("text", x = (2012), y = 65, label = "Piñera", size=4) +
  annotate("text", x = (2016), y = 65, label = "Bachelet", size=4)+
  annotate("text", x = (2019.5), y = 65, label = "Piñera", size=4)
  

