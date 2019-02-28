
# Imports paquets ---------------------------------------------------------
# library(devtools)
#devtools::install_github("RinteRface/bs4Dash")
library(shiny)
library(fontawesome)
library(shinyWidgets)
library(bs4Dash)
library(plotly)
library(r2d3)
library(r2d3maps)
library(rnaturalearth)
library(magick)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(DT)
library(recipes)
library(highcharter)
library(tidyverse)
library(billboarder)
library(echarts4r)
library(countrycode) 
library(shinydashboard)
library(flexdashboard)
library(sf)
library(shinythemes)
library(htmlwidgets)
library(janitor)
library(configr)
library(httr)
library(jsonlite)
library(shinyWidgets)
library(corrplot)
library(tsne)
library(NbClust)
library(readxl)
library(readr)
library(geosphere)
library(qualpalr)
library(circlize)
library(rdbnomics)
library(esquisse)


conf <- read.config(file="./cfg_shinyapps.ini")

##Import chemin
data_path<-conf$CHEMIN$SAVE_DIR
processed_folder<-conf$CHEMIN$PROCESSED_FOLDER
##Import nom fichiers
acled_file<-conf$NOM_FICHIERS$ACLED
wdi_file<-conf$NOM_FICHIERS$WDI
dhs_file<-conf$NOM_FICHIERS$DHS
inform_file<-conf$NOM_FICHIERS$INFORM
sahel_shapefile<-conf$SHP$SAHEL_SHAPEFILE
micro_world_file<-conf$NOM_FICHIERS$MICRO_WORLD
unhcr_file<-conf$NOM_FICHIERS$UNHCR
countries_file=conf$NOM_FICHIERS$COUNTRIES

## Nom_fichiers_processed
dhs_processed<-conf$NOM_FICHIERS$PROCESSED_DHS
dhs_sf_processed<-conf$NOM_FICHIERS$PROCESSED_DHS_SF
dhs_recent_processed<-conf$NOM_FICHIERS$PROCESSED_DHS_RECENT
wdi_processed<-conf$NOM_FICHIERS$PROCESSED_WDI
acled_processed<-conf$NOM_FICHIERS$PROCESSED_ACLED
inform_processed<-conf$NOM_FICHIERS$PROCESSED_INFORM
inform_data_processed<-conf$NOM_FICHIERS$PROCESSED_INFORM_DATA
cluster_processed<-conf$NOM_FICHIERS$PROCESSED_CLUSTER
afd_processed<-conf$NOM_FICHIERS$PROCESSED_AFD
micro_world_processed<-conf$NOM_FICHIERS$PROCESSED_MICRO_WORLD
sahel_tiwara_geo_processed<-conf$NOM_FICHIERS$PROCESSED_SAHEL_TIWARA_GEO
unhcr_processed<-conf$NOM_FICHIERS$PROCESSED_UNHCR
work_data_processed<-conf$NOM_FICHIERS$PROCESSED_WORK_DATA
work_data_ts_prep_processed<-conf$NOM_FICHIERS$PROCESSED_WORK_DATA_TS_PREP
esquisse_processed<-conf$NOM_FICHIERS$PROCESSED_ESQUISSE
boxplot_processed<-conf$NOM_FICHIERS$PROCESSED_BOXPLOT
############################### FILE READING
variables<-strsplit(conf$VARIABLES_DHS$var_dhs,",")[[1]]

databases_work<-strsplit(conf$DBNOMICS_EMPLOYMENT$databases,",")[[1]]
countries_work<-strsplit(conf$DBNOMICS_EMPLOYMENT$countries,",")[[1]]
masques_work<-conf$DBNOMICS_EMPLOYMENT$mask
provider_work<-conf$DBNOMICS_EMPLOYMENT$provider
databases_work_ts<-conf$DBNOMICS_EMPLOYMENT$database_ts
iso3_cn<-strsplit(conf$COUNTRY_LISTS$ISO3_CN,",")[[1]]
iso3_clust<-strsplit(conf$COUNTRY_LISTS$ISO3_CLUST,",")[[1]]
country_names<-strsplit(conf$COUNTRY_LISTS$COUNTRY_NAME,",")[[1]]
country_names_fr<-strsplit(conf$COUNTRY_LISTS$COUNTRY_NAME_FR,",")[[1]]

# DHS IMPORT----------------------
df_dhs <- tbl_df(read_csv(paste0(data_path,dhs_file)))%>%
  mutate(
    CharacteristicLabel = stringr::str_replace_all( # remove leading points (ex: "..Kanem") in labels
      string = CharacteristicLabel,
      pattern = "^\\.+", replacement = ""
    )
  )%>%
  mutate(location=paste0(DHS_CountryCode,sep='-',CharacteristicLabel))%>%
  mutate_if(is.character, factor, ordered = FALSE)%>%
  mutate('iso3c' = countrycode(CountryName, 'country.name', 'iso3c'))

saveRDS(df_dhs,paste0(data_path,processed_folder,dhs_processed))

# sdg4 indicators IMPORT------------------------
sdg4indics<-tbl_df(read_csv(paste0(data_path,wdi_file)))%>%mutate_if(is.character, factor, ordered = FALSE)  

saveRDS(sdg4indics,paste0(data_path,processed_folder,wdi_processed))


# ACLED IMPORT---------------------------
acled_tiwara <- read_excel(paste0(data_path,acled_file),col_types = c("numeric",
                                                                      "text",
                                                                      "numeric",
                                                                      "date",
                                                                      "numeric",
                                                                      "numeric",
                                                                      "guess",
                                                                      "text",
                                                                      "text",
                                                                      "guess",
                                                                      "text",
                                                                      "text",
                                                                      "guess",
                                                                      "numeric",
                                                                      "guess",
                                                                      "guess",
                                                                      "text",
                                                                      "text",
                                                                      "text",
                                                                      "text",
                                                                      "numeric",
                                                                      "numeric",
                                                                      "guess",
                                                                      "text",
                                                                      "guess",
                                                                      "text",
                                                                      "numeric",
                                                                      "numeric")) %>% 
  rename_all(funs(tolower(.))) %>% 
  filter(country %in% c("Mali","Burkina Faso", "Mauritania","Chad","Niger")) %>% 
  filter(year>=2010) %>% 
  mutate(event_date = dmy(event_date)) %>% 
  mutate_if(is.character, factor, ordered = FALSE)

saveRDS(acled_tiwara,paste0(data_path,processed_folder,acled_processed))


# data for map --------------------------------------------------------------------
df_dhs_sf <- df_dhs %>%
  filter(!is.na(Coordinates)) %>% # remove missing values
  select(IndicatorId, CountryName, CharacteristicLabel, Value, Coordinates) %>%
  distinct(IndicatorId, CharacteristicLabel,CountryName, .keep_all = TRUE) %>% # <<<---- deduplicated (you need one row by country at the end)
  tidyr::spread(IndicatorId, Value) %>% # <<<---- here transpose data
  mutate(
    Coordinates = st_as_sfc(Coordinates) # convert to sf geometries
  )%>%
  st_as_sf()

saveRDS(df_dhs_sf,paste0(data_path,processed_folder,dhs_sf_processed))



#### Donnee INform Risk  IMPORT#####@-----------------------------------
# Inform Indicators
INFORM_SAHEL<- read_excel(paste0(data_path,inform_file),
                          sheet = "INFORM SAHEL Sep 2018 (a-z)", 
                          na = c("(0-10)","x","(0-50)","(0-100%)","()","(1-191)")) %>% 
  slice(-1L)%>%janitor::clean_names() #remove 1st row, which is simple the scale

saveRDS(INFORM_SAHEL,paste0(data_path,processed_folder,inform_processed))



sahel_tiwara <- country_names
inform_sahel_tiwara <- INFORM_SAHEL %>%filter(iso3 %in% iso3_cn)

## On récupère la fraicheur des datas pour chaque dataset
fraicheur_acled<-max(acled_tiwara$event_date)
fraicheur_inform<-str_match(pattern = " (\\d+)_",inform_file)[2]
fraicheur_sdg4<-max(sdg4indics$period)
fraicheur_dhs<-max(df_dhs$SurveyYear)
fraicheur_dhs_pays<-df_dhs %>% 
  group_by(CountryName) %>% 
  summarise(max(SurveyYear))

chaine_char_dhs<-"<ul>"
for (i in 1:nrow(fraicheur_dhs_pays)){
  chaine<-paste0("<li>",as.character(fraicheur_dhs_pays[i,1] %>% pull(CountryName))," : ",fraicheur_dhs_pays[i,2],"</li>")
  chaine_char_dhs<-paste0(chaine_char_dhs,chaine)
}
chaine_char_dhs<- paste0(chaine_char_dhs,"</ul>")


# Code clusters --------------------------------------------------------------------
## Import_Data

df_dhs_recents<-df_dhs %>% 
  slice(-starts_with("Population",T,as.character(Indicator))) %>% 
  select(Indicator,Value,SurveyYear,DHS_CountryCode,CountryName,CharacteristicLabel) %>% 
  group_by(Indicator,DHS_CountryCode,CharacteristicLabel,CountryName) %>% 
  arrange(desc(SurveyYear)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-SurveyYear) %>% 
  spread(Indicator,Value)


inform_data<-read_excel(paste0(data_path,inform_file),
                        sheet = "INFORM SAHEL Sep 2018 (a-z)", 
                        na = c("(0-10)","x","(0-50)","(0-100%)","()","(1-191)")) %>% 
  slice(-1L)%>%janitor::clean_names() #remove 1st row, which is simple the scale
inform_sahel_tiwara <- inform_data %>%filter(iso3 %in% iso3_clust) %>% 
  mutate(admin1bis=case_when(admin1=="Boucle du Mouhoun" ~ "Boucle de Mouhoun",
                             admin1=="Centre Est" ~ "Centre-Est",
                             admin1=="Centre Nord" ~ "Centre-Nord",
                             admin1=="Centre Ouest" ~ "Centre-Ouest",
                             admin1=="Centre Sud" ~ "Centre-Sud",
                             admin1=="Centre" ~ "Centre (excluding Ouagadougou)",
                             admin1=="Segou" ~ "Ségou",
                             admin1=="Timbuktu" ~ "Tombouctou",
                             admin1=="Tillabery" ~ "Tillabéri",
                             admin1=="Barh el Ghazel" ~ "Barh El Gazal",
                             admin1=="Chari-Baguirmi" ~ "Chari Baguirmi",
                             admin1=="Guera"~"Guéra",
                             admin1=="Mayo-Kebbi Est" ~ "Mayo Kebbi Est",
                             admin1=="Mayo-Kebbi Ouest" ~ "Mayo Kebbi Ouest",
                             admin1=="Moyen-Chari" ~ "Moyen Chari",
                             admin1=="Ouaddai" ~ "Ouaddaï",
                             admin1=="Tandjile"~"Tandjilé",
                             T ~ admin1))


## Jointure DHS Inform


## Jointure sur les sous régions
df_etude<-df_dhs_recents %>% 
  left_join(inform_sahel_tiwara,by=c("CountryName"="country","CharacteristicLabel"="admin1bis")) %>% 
  filter(!is.na(admin1)) %>% 
  mutate(rname=paste(DHS_CountryCode,CharacteristicLabel))
## Sélection de variables -> fait à partir d'une matrice de corrélations, on supprimes des variables corrélées
df_etude<-df_etude %>% 
  mutate(children_u5=ifelse(is.na(children_u5),mean(df_etude$children_u5,na.rm=T),children_u5)) %>% 
  column_to_rownames("rname") %>% 
  select(-DHS_CountryCode,-CharacteristicLabel,-admin1,-iso3,-iso_admin1,-CountryName) %>% 
  select(-drr,-institutional,-lack_of_coping_capacity,-infrastructure,-`Men who are literate`,-vulnerable_groups,-natural,-human,-`Women who are literate`,-risk,-governance,
         -`Gross secondary school attendance rate: Total`,-vulnerability,-communication,-recent_shocks,-political_violence,-development_deprivation,-`Gross primary school attendance rate: Total`)


## Normalisation
scaled_df<-scale(df_etude,center = T,scale=T)
## NbClust
result_nbclus<-NbClust(data = scaled_df,method="average") 
## Kmeans k=4, basé sur le résultat de nbClust
kms<-kmeans(scaled_df,centers=4,iter.max = 50)
##  On prend le rés de NbClust
df_etude<-df_etude %>% mutate(clust=result_nbclus$Best.partition)
## Polygones
poly_dhs<-df_dhs %>% 
  select(CharacteristicLabel,Coordinates) %>%
  mutate(Coordinates=as.character(Coordinates)) %>% 
  distinct()
## Jointure avec les data inform puis mise en forme pour les cartes 
df_clustered<-df_dhs_recents %>% 
  select(CountryName,CharacteristicLabel) %>% 
  left_join(inform_sahel_tiwara %>% select(country,admin1bis,admin1),by=c("CountryName"="country","CharacteristicLabel"="admin1bis")) %>% 
  filter(!is.na(admin1)) %>% 
  bind_cols(df_etude) %>% 
  mutate(clust=as.factor(clust)) %>% 
  left_join(poly_dhs,by=c("CharacteristicLabel")) %>% # <<<---- here transpose data
  mutate(
    Coordinates = st_as_sfc(Coordinates) # convert to sf geometries
  )%>%
  st_as_sf() 
  
saveRDS(df_clustered,paste0(data_path,processed_folder,cluster_processed))




numeric_car_clust<-names(df_clustered)[sapply(df_clustered,class)=="numeric"]

### DATA Carte AFD projets


# Données de l'aide au développement de l'AFD -----------------------------
url <- "https://opendata.afd.fr/api/records/1.0/search/?dataset=donnees-aide-au-developpement-afd&rows=5000&facet=etat_du_projet&facet=pays_de_realisation&facet=region&facet=libelle_cicid"

connecAfd <- fromJSON(url)
afddata<-tbl_df(connecAfd$records$fields)
afddata_sahel<-afddata %>% 
  filter(pays_de_realisation %in% country_names_fr) 

saveRDS(afddata_sahel,paste0(data_path,processed_folder,afd_processed))


## Icone
Icon <- makeIcon(
  iconUrl = "https://i.vimeocdn.com/portrait/17890243_640x640",
  iconWidth = 35*215/230, iconHeight = 35,
  iconAnchorX = 35*215/230/2, iconAnchorY = 35) # retrieve AFD logo to add on the map at each Beneficiaire location 

## Dataframe coordonnées
AFDCoords_sahel <- data.frame(
  lat = afddata_sahel$latitude,
  lng = afddata_sahel$longitude)  # prepare a dataframe with GPS coordinates

## Libellés, bénéficiares
BeneficiaireName <- paste("<a href='", "'>", afddata_sahel$libelle_beneficiaire_primaire,"</a>" ,sep = "")
AFD_Desc<-paste(afddata_sahel$libelle_cicid,afddata_sahel$description_du_projet,sep=" :<br/>")

## Data Financial Index ---------------------------------------
classic_labels<-c("yes","no","don't know", "refuse")
unclassic_labels<-c("no","yes")

## Data financial index
data_index<-read_csv(paste0(data_path,micro_world_file)) %>% 
  filter(economycode %in% iso3_cn) %>% 
  mutate(classe_age=cut(age,breaks=c(0,25,40,60,100),labels = c("0-25","25-40","40-60","60-100"))) %>% 
  rename("has_db_card"="fin2","has_cd_card"="fin7","save_for_business"="fin15","save_for_old_age"="fin16","has_national_id"="fin48","gender"="female","work"="emp_in") %>%
  mutate(gender=factor(gender,labels=c("Male","Female")),
         education=factor(educ,labels=c("primary-","secondary","tertiary+","don't know","ref")),
         income_quantile=factor(inc_q,labels=c("poorest 20%","second 20%","middle 20%","fourth 20%","richest 20%")),
         work=factor(work,labels=c("out of workforce","in the workforce")),
         has_db_card=factor(has_db_card,labels=classic_labels),
         has_cd_card=factor(has_cd_card,labels=classic_labels),
         save_for_business=factor(save_for_business,labels=classic_labels),
         save_for_old_age=factor(save_for_old_age,labels=classic_labels),
         mobileowner=factor(mobileowner,labels=classic_labels),
         has_national_id=factor(has_national_id,labels=classic_labels),
         account=factor(account,labels=unclassic_labels),
         saved=factor(saved,labels=unclassic_labels)
  )  

saveRDS(data_index,paste0(data_path,processed_folder,micro_world_processed))





## Variables catégorielles
data_index_cate<-c("gender","education","work","classe_age","income_quantile")

## Variables numériques
data_index_rep<-c("has_db_card","has_cd_card","save_for_business","save_for_old_age","mobileowner","has_national_id","account","saved")

##-------get geodata on Sahel states 
#sahel <- ne_states(country=sahel_tiwara, returnclass = "sf") # filter on states in target countries
#st_write(st_as_sf(sahel), './inputs/sahel.shp')
sahel <- st_read(paste0(data_path,sahel_shapefile))
sahel_tiwara_geo_mr <- left_join(
  y = sahel %>%select(name, geometry) %>% mutate(name=as.character(name)) %>% rename(CharacteristicLabel=name,Coordinates=geometry),
  x = inform_sahel_tiwara %>% filter(country == "Mauritania" | admin1=="Sila"),
  by = c("admin1" = "CharacteristicLabel")
) %>% 
  st_as_sf()

sahel_tiwara_geo_else <- left_join(
  y = poly_dhs %>% mutate(CharacteristicLabel=as.character(CharacteristicLabel)),
  x = inform_sahel_tiwara %>% 
    mutate(admin1bis=case_when(admin1=="Boucle du Mouhoun" ~ "Boucle de Mouhoun",
                               admin1=="Centre Est" ~ "Centre-Est",
                               admin1=="Centre Nord" ~ "Centre-Nord",
                               admin1=="Centre Ouest" ~ "Centre-Ouest",
                               admin1=="Centre Sud" ~ "Centre-Sud",
                               admin1=="Centre" ~ "Centre (excluding Ouagadougou)",
                               admin1=="Segou" ~ "Ségou",
                               admin1=="Timbuktu" ~ "Tombouctou",
                               admin1=="Tillabery" ~ "Tillabéri",
                               admin1=="Barh el Ghazel" ~ "Barh El Gazal",
                               admin1=="Chari-Baguirmi" ~ "Chari Baguirmi",
                               admin1=="Guera"~"Guéra",
                               admin1=="Mayo-Kebbi Est" ~ "Mayo Kebbi Est",
                               admin1=="Mayo-Kebbi Ouest" ~ "Mayo Kebbi Ouest",
                               admin1=="Moyen-Chari" ~ "Moyen Chari",
                               admin1=="Ouaddai" ~ "Ouaddaï",
                               admin1=="Tandjile"~"Tandjilé",
                               admin1=="Borkou" ~ "Borkou/Tibesti",
                               admin1=="Ennedi Est" ~ "Ennedi Est/Ennedi Ouest",
                               T ~ admin1)),
  by = c("admin1bis" = "CharacteristicLabel")
) %>%
  filter(! is.na(Coordinates)) %>% 
  mutate(Coordinates=st_as_sfc(Coordinates)) %>% 
  st_as_sf()

sahel_tiwara_geo<-bind_rows(sahel_tiwara_geo_else,sahel_tiwara_geo_mr)


saveRDS(sahel_tiwara_geo,paste0(data_path,processed_folder,sahel_tiwara_geo_processed))



### HDX Segment ----------
locale("fr")
hdx<-read_csv(paste0(data_path,unhcr_file),skip = 3) %>% 
  rename(arrivee=`Country / territory of asylum/residence`) %>% 
  mutate(Origin2=gsub("\\s?\\(.+\\)","",Origin),arrivee2=gsub("\\s?\\(.+\\)","",arrivee))%>% 
  mutate(Year=as.numeric(Year)) %>% 
  mutate(Origin=iconv(Origin,from="latin1",to="utf8"),arrivee=iconv(arrivee,from="latin1",to="utf8"))



countries_coord<-read_delim(paste0(data_path,countries_file),delim = "\t")
hdx_geo<-hdx %>% 
  left_join(countries_coord,by=c("Origin2" = "name")) %>% 
  rename(lat_debt=latitude,long_debt=longitude) %>% 
  left_join(countries_coord,by=c("arrivee2" = "name")) %>% 
  rename(lat_fin=latitude,long_fin=longitude) %>% 
  select(-country.y,-country.x,-arrivee2,-Origin2)

hdx_geo_2<-hdx_geo %>% 
  filter(! (is.na(lat_debt) | is.na(lat_fin) | is.na(long_debt) | is.na(long_fin)))

saveRDS(hdx_geo_2,paste0(data_path,processed_folder,unhcr_processed))


## ILO - Youth employment data ----------------------------
get_indic_rdb_one_db<-function(provider,dataset,countries,mask_end){
  sahel_masks<-paste(countries,mask_end,sep=".")
  data_rdb<-bind_rows(lapply(sahel_masks,function(x) {tryCatch({res<-data.frame()
  res<-rdb(provider, dataset, mask = x)},
  error=function(cond){print("DBNOMICS - ILO : Error in data recup")},
  finally = {return(res)}
  )})) %>% 
    select(dataset_name,original_period,ref_area,series_name,value)
}


work_data<-bind_rows(lapply(databases_work, function(x) {get_indic_rdb_one_db(provider = provider_work,dataset = x,countries = countries_work,mask_end = masques_work)})) %>% 
  group_by(ref_area,dataset_name) %>% 
  arrange(original_period)

saveRDS(work_data,paste0(data_path,processed_folder,work_data_processed))



work_data_ts_prep<-bind_rows(lapply(databases_work_ts, function(x) {get_indic_rdb_one_db(provider = provider_work,dataset = x,countries = countries_work,mask_end = masques_work)})) %>% 
  group_by(ref_area,dataset_name) %>% 
  arrange(original_period)%>% 
  ungroup()
saveRDS(work_data_ts_prep,paste0(data_path,processed_folder,work_data_ts_prep_processed))

work_data_ts_name<-work_data_ts_prep$dataset_name[1]
work_data_ts<-work_data_ts_prep %>% 
  select(-series_name,-dataset_name) %>% 
  spread(ref_area,value) %>% 
  arrange(original_period)

## Esquisse Country Dataset ================


### Country based information based on most recent data



## ACLED Pour celui ci on a tout
ACLEDTOMERGEC<-acled_tiwara %>% 
  select(country,year,event_type,fatalities) %>% 
  group_by(country,year,event_type) %>% 
  summarise(fatalities=sum(fatalities)) %>% 
  ungroup() %>% group_by(country,event_type) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  select(-year) %>%
  ungroup() %>% 
  mutate(event_type=paste0("ACLED:Fatalities - ",event_type,"- Most recent year")) %>% 
  rename(value=fatalities,var=event_type)

## AFD Pour celui ci on a pas vraiment de précision géographique mieux que Country
AFDDATATOMERGEC<-afddata_sahel %>% 
  mutate(event_date=as.Date(date_de_1er_versement_projet),year=year(event_date)) %>% 
  select(pays_de_realisation,year,libelle_secteur_economique_cad_5,versements_euro) %>%
  mutate(pays_de_realisation=case_when(pays_de_realisation=="BURKINA FASO" ~ "Burkina Faso",
                                       pays_de_realisation=="MAURITANIE" ~ "Mauritania",
                                       pays_de_realisation=="MALI" ~ "Mali",
                                       pays_de_realisation=="NIGER" ~ "Niger",
                                       pays_de_realisation=="TCHAD" ~ "Chad")) %>% 
  rename(country=pays_de_realisation,event_type=libelle_secteur_economique_cad_5) %>% 
  group_by(country,year,event_type) %>% 
  summarise(versements_euro=sum(versements_euro,na.rm=T)) %>% 
  ungroup() %>% group_by(country,event_type) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  select(-year) %>%
  ungroup() %>% 
  mutate(event_type=paste0("AFD: Versements euros - ",event_type,"- Most recent year")) %>% 
  rename(value=versements_euro,var=event_type)




## Data Index : On a year, country

GLOBALFINDEXTOMERGEC<-read_excel("datas/Global Findex Database.xlsx",sheet = "Data") %>% 
  rename(year=X__1,country=X__3) %>% 
  select(-X__2,-X__4,-X__5) %>% 
  filter(country %in% country_names) %>% 
  reshape2::melt(id=c("country","year")) %>% 
  group_by(country,variable) %>% 
  top_n(1,year) %>% 
  ungroup() %>% 
  select(-year) %>% 
  rename(var=variable) %>% 
  mutate(var=paste0("GLOBAL FINDEX : ",var))

## DHS Year/country/Admin1 

DHSTOMERGEC<-df_dhs %>% 
  rename(admin1=CharacteristicLabel,country=CountryName,year=SurveyYear) %>% 
  mutate(admin1=as.character(admin1),country=as.character(country)) %>% 
  select(country,year,admin1,IndicatorId,Value) %>% 
  filter(!is.na(Value)) %>% 
  group_by(country,admin1,IndicatorId) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>%
  ungroup() %>% 
  select(-year,-admin1) %>% 
  group_by(country,IndicatorId) %>% 
  summarise(Value=sum(Value)) %>% 
  rename(var=IndicatorId,value=Value) %>% 
  mutate(var=paste0("DHS SURVEY : ",var))



## INFORM Country/admin1

INFORMTOMERGEC<- INFORM_SAHEL %>% 
  select(-iso3,-iso_admin1,-admin1) %>% 
  reshape2::melt(id="country") %>% 
  rename(var=variable) %>% 
  filter(country %in% country_names) %>% 
  group_by(country,var) %>% 
  summarise(value=mean(value,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(var=paste0("INFORM RISK : ",var))

## SDG4Indics Country/Year

SDG4TOMERGEC<- sdg4indics %>% 
  filter(!is.na(value)) %>% 
  mutate(year=year(period)) %>% 
  select(country,year,indicator_label,value) %>% 
  group_by(country,indicator_label) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  select(-year) %>%
  ungroup() %>% 
  mutate(indicator_label=paste0("SDG4 - ",indicator_label,"- Most recent year")) %>% 
  rename(var=indicator_label) %>% 
  mutate(country=case_when(country=="BF" ~ "Burkina Faso",
                           country=="MR" ~ "Mauritania",
                           country=="ML" ~ "Mali",
                           country=="NR" ~ "Niger",
                           country=="TD" ~ "Chad"))


## ILO Data CountryISO3/YEAR

ILOTOMERGEC<-work_data %>% 
  rename(country=ref_area) %>% 
  mutate(year=original_period) %>% 
  select(country,year,dataset_name,value) %>% 
  group_by(country,dataset_name) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  select(-year) %>% 
  rename(var=dataset_name) %>% 
  ungroup() %>% 
  mutate(country=case_when(country=="BFA" ~ "Burkina Faso",
                           country=="MRT" ~ "Mauritania",
                           country=="MLI" ~ "Mali",
                           country=="NER" ~ "Niger",
                           country=="TCD" ~ "Chad")) %>% 
  mutate(var=paste0("ILO : ",gsub("Key Indicators / Youth / ","",var)))


esquisse_country<-bind_rows(ACLEDTOMERGEC,AFDDATATOMERGEC,GLOBALFINDEXTOMERGEC,DHSTOMERGEC,INFORMTOMERGEC,SDG4TOMERGEC,ILOTOMERGEC) %>%
  filter(!(is.na(country)|is.na(value))) %>% 
  spread(key=c("var"),value = value) 

saveRDS(esquisse_country,paste0(data_path,processed_folder,esquisse_processed))


## BoxPlot Clusters 
boxplot_cluster<-df_dhs_recents %>% 
  select(CountryName,CharacteristicLabel) %>% 
  left_join(inform_sahel_tiwara %>% select(country,admin1bis,admin1),by=c("CountryName"="country","CharacteristicLabel"="admin1bis")) %>% 
  filter(!is.na(admin1)) %>% 
  bind_cols(df_etude)

saveRDS(boxplot_cluster,paste0(data_path,processed_folder,boxplot_processed))
