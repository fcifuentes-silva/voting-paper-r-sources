#install.packages("SPARQL")
library(SPARQL)
library(dplyr)
library(purrr)
endpoint<-"http://datos.bcn.cl/sparql"

q <- "SELECT count(*) as ?total str(?fecha) as ?fecha WHERE {
?pl bcnres:tieneVotacion ?v; dc:date ?fecha .
?v bcnres:tieneVoto ?s .
?s a bcnres:VotoProyectoDeLey} group by ?fecha order by ?fecha"

prefix <- c('bcnres','<http://datos.bcn.cl/ontologies/bcn-resources#>',
            "dc","http://purl.org/dc/elements/1.1/")

d <- SPARQL(url=endpoint,
            query=q,
            ns=prefix)
is.data.frame(d$results)

View(d$results)

datos <- d$results

datos2 <- datos %>% 
  mutate(anio = stringr::str_split(fecha, "-") %>% map_chr(., 1)) %>% 
  mutate(mes = stringr::str_split(fecha, "-") %>% map_chr(., 2)) %>% 
  mutate(dia = stringr::str_split(fecha, "-") %>% map_chr(., 3)) %>% 
  mutate(nomdia = weekdays(as.Date(fecha)))

datos2 %>% 
  View



datos2 %>% 
  group_by(anio) %>% 
  summarise(total = sum(total)) %>% 
  View