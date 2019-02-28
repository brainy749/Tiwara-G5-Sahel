## DATA Pipelines
## DONE : INFORM ACLED DHS ILO

## TODO MicroWorld UNHCR


## UNHCR
## OK
download.file("http://popstats.unhcr.org/en/persons_of_concern.csv","datas/UNHCR_persons_of_concern.csv")


## Micro World -> Souci car inscription pr la maj.

download.file("http://www.acleddata.com/download/2909","datas/AcledData.xlsx")
library(readxl)
aa<-read_excel("datas/AcledData.xlsx") %>% 
  rename_all(funs(tolower(.))) %>% 
  filter(country %in% c("Mali","Burkina Faso", "Mauritania","Chad","Niger")) %>% 
  filter(year>=2010) %>% 
  mutate(event_date = dmy(event_date)) %>% 
  mutate_if(is.character, factor, ordered = FALSE)


aaa<-read_csv("datas/1900-01-01-2018-11-05-Burkina_Faso-Chad-Mali-Mauritania-Niger.csv")

download.file("http://microdata.worldbank.org/index.php/catalog/3324/download/44596","datas/testab.csv")
