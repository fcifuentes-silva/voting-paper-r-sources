library(SPARQL)
library(dplyr)
library(stringr)






endpoint<-"http://datos.bcn.cl/sparql"

q <- "SELECT ?chamber ?type count(?vote) as ?total WHERE {
?pl bcnres:tieneVotacion ?v .
?v bcnnorms:createdBy ?chamber;
bcnres:tipoVotacion ?typeVoting .
?typeVoting rdfs:label ?type .
?v bcnres:tieneVoto ?vote .
} 
group by ?pl ?chamber ?type"


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
  mutate(tipo=case_when(str_detect(type, "UNICA") ~ "GENERAL",
                        str_detect(type, "Discusión única") ~ "GENERAL",
                        str_detect(type, "GENERAL") ~ "GENERAL",
                        str_detect(type, "Discusión general") ~ "GENERAL",
                        str_detect(type, "PARTICULAR") ~ "PARTICULAR",
                        str_detect(type, "Discusión informe de Comisión Mixta") ~ "PARTICULAR",
                        
                        
                        str_detect(type, "Discusión particular") ~ "PARTICULAR")) %>% 
  mutate(camara = case_when(str_detect(chamber, "senado") ~ "Senate",
                            str_detect(chamber, "camara") ~ "Chamber of Deputies")) %>% 
  select(total, camara, tipo) 

d2 %>% View
d2 %>% filter(camara == "Senate" , tipo == "GENERAL") %>%  summary
d2gs <- d2 %>% 
  filter(camara == "Senate" , tipo == "GENERAL")

sum(as.numeric(d2gs$total), na.rm = TRUE) %>% View
  
d2 %>% filter(camara == "Senate" , tipo == "PARTICULAR") %>%  summary

d2ps <- d2 %>% 
  filter(camara == "Senate" , tipo == "PARTICULAR")

sum(as.numeric(d2ps$total), na.rm = TRUE) %>% View

#---- Chamber of deputies
d2 %>% filter(camara == "Chamber of Deputies") %>%  summary

d2gd <- d2 %>% 
  filter(camara == "Chamber of Deputies" , tipo == "GENERAL")

sum(as.numeric(d2gd$total), na.rm = TRUE) %>% View

d2 %>% filter(camara == "Chamber of Deputies" , tipo == "PARTICULAR") %>%  summary

d2pd <- d2 %>% 
  filter(camara == "Chamber of Deputies" , tipo == "PARTICULAR")

sum(as.numeric(d2pd$total), na.rm = TRUE) %>% View




