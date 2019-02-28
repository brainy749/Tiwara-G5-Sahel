##################################
#
#         Tiwara Get Data
#         Sydney Becu 
#         30/10/2018
#
###################################

### Ce script a pour objectif de récupérer les différentes données utilisées dans le projet Tiwara
### Les types de données : 


### DHS : données issues de sondages sur différents indicateurs (santé, pauvreté ...) dans plus de 90 pays. 
#https://www.dhsprogram.com/


### WDI :  World Development Indicators. Indicateurs écnomiques fournis par dbnomics (data source : World Bank)
#https://next.nomics.world/WB/WDI


### ACLED : Données de conflits et de crises dans le monde.
#https://www.acleddata.com/


# Import paquets ----------------------------------------------------------
library(configr)
library(rdhs)
library(rdbnomics)
library(jsonlite)
library(dplyr)
library(RSelenium)
library(readr)

# Import conf -------------------------------------------------------------
conf <- read.config(file="./datas/cfg.ini")

# Récupération des données ------------------------------------------------


##Get dhs 
##En attente de validation du compte
get_dhs<-function(config){
  set_rdhs_config(email=config$INFOS$dhs_login,
                  project=config$INFOS$dhs_project,
                  global=FALSE)
  country_arg<-strsplit(config$PAYS$countries_dhs,",")[[1]]
  var_arg<-strsplit(config$VARIABLES_DHS$var_dhs,",")[[1]]
  start_year_arg<-as.numeric(config$VARIABLES_DHS$start_year_dhs)
  end_year_arg<-as.numeric(config$VARIABLES_DHS$end_year_dhs)
  
  print(country_arg)
  print(var_arg)
  
  if(!is.null(start_year_arg) & !is.null(end_year_arg)){
    data_recup <-
      data.frame(dhs_data(
        countryIds = country_arg,
        indicatorIds = var_arg,
        surveyYearStart = start_year_arg,
        surveyYearEnd = end_year_arg,
        breakdown = "subnational",
        returnGeometry = TRUE
      ))
  }
  else if (!is.null(start_year_arg) & is.null(end_year_arg)) {
    data_recup <-
      data.frame(dhs_data(
        countryIds = country_arg,
        indicatorIds = var_arg,
        surveyYearStart = start_year_arg,
        breakdown = "subnational",
        returnGeometry = TRUE
      ))
  }
  else if (is.null(start_year_arg) & !is.null(end_year_arg)) {
    data_recup <-
      data.frame(dhs_data(
        countryIds = country_arg,
        indicatorIds = var_arg,
        surveyYearEnd = end_year_arg,
        breakdown = "subnational",
        returnGeometry = TRUE
      ))
  }
  else{
    data_recup <-
      data.frame(dhs_data(
        countryIds = country_arg,
        indicatorIds = var_arg,
        breakdown = "subnational",
        returnGeometry = TRUE
      ))
  }
  
  return(data_recup)
}

## Get WDI
## Si trop de variables d'un coup, httperror, on les gère donc en petits groupes
## Fonctionne a partir du fichier de conf.
get_wdi<-function(config){
  datas_wdi<-data.frame()
  nb_vars<-length(unlist(strsplit(config$VARIABLES_WDI$var_wdi,",")))
  nb_runs<-nb_vars%/%5
  if(nb_vars>5){
    for(i in 1:nb_runs){
      liste_JSON<-list()
      liste_JSON$country<-unlist(strsplit(config$PAYS$countries_wdi,","))
      liste_JSON$indicator<-unlist(strsplit(config$VARIABLES_WDI$var_wdi,","))[(5*(i-1)+1):(5*i)]
      requete=toJSON(liste_JSON)
      print(requete)
      datas_wdi<-bind_rows(datas_wdi,data.frame(rdb("WB","WDI",dimensions=requete)))
    }
  }
  liste_JSON<-list()
  liste_JSON$country<-unlist(strsplit(config$PAYS$countries_wdi,","))
  liste_JSON$indicator<-unlist(strsplit(config$VARIABLES_WDI$var_wdi,","))[(5*nb_runs+1):nb_vars]
  # liste_JSON<-list()
  # liste_JSON$country<-unlist(strsplit(config$PAYS$countries,","))
  # liste_JSON$indicator<-unlist(strsplit(config$VARIABLES_WDI$var_wdi,","))
  requete<-toJSON(liste_JSON)
  print(requete)
  datas_wdi<-bind_rows(datas_wdi,data.frame(rdb("WB","WDI",dimensions=requete)))
  return(datas_wdi)
}



# Get_ACLED ---------------------------------------------------------------
# 
# get_acled<-function(config){
#   ##
#   chemin_telechargement<-config$CHEMIN$SAVE_DIR
#   
#   ## Création du profil firefox
#   fprof <- makeFirefoxProfile(list(browser.download.dir = chemin_telechargement
#                                    ,  browser.download.folderList = 2L
#                                    , browser.download.manager.showWhenStarting = FALSE
#                                    , browser.helperApps.neverAsk.saveToDisk = "text/csv"))
#   
#   ## Pays pour lesquels on récupère des données
#   liste_pays<-strsplit(config$PAYS$countries_acled,",")[[1]]
#   ## Dates
#   date_start<-config$VARIABLES_ACLED$date_start
#   date_end<-config$VARIABLES_ACLED$date_end
#   
#   ## On lance le selenium server et le browser
#   driver<-rsDriver(browser="firefox",extraCapabilities=fprof)
#   remDr<-driver$client
#   
#   ## Navigation sur la page Data de l'Acled
#   remDr$navigate("https://www.acleddata.com/data/")
#   
#   ## Scroll down pour afficher les champs, voir si ça fonctionne bien sur serveur
#   webElem <- remDr$findElement("css", "body")
#   for(i in 1:7){webElem$sendKeysToElement(list(key = "down_arrow"))}
#   
#   ## Récupération des différents éléments qui nous intéressent
#   date_from <- remDr$findElement(using = 'name', value = "event_date_from")
#   Sys.sleep(5)
#   date_to <- remDr$findElement(using = 'name', value = "event_date_to")
#   Sys.sleep(5)
#   selecteur_pays<-remDr$findElement(using = 'name', value="country_typing")
#   Sys.sleep(5)
#   
#   ## Selection des pays
#   selecteur_pays$clickElement()
#   Sys.sleep(2)
#   for(pays in liste_pays){
#     selecteur<-paste0("input[class='aaad-country-check'][value='",pays,"']")
#     select_pays<-remDr$findElement(using = 'css',selecteur)
#     Sys.sleep(2)
#     select_pays$sendKeysToElement(list(key="space"))
#   }
#   selecteur_pays$sendKeysToElement(list(key="escape"))
#   
#   ## Sélection des dates
#   date_from$sendKeysToElement(list(date_start))
#   date_from$sendKeysToElement(list(key="escape"))
#   Sys.sleep(2)
#   date_to$sendKeysToElement(list(date_end))
#   date_to$sendKeysToElement(list(key="escape"))
#   
#   ## Téléchargement 
#   bouton_export<-remDr$findElement(using = 'css',"input[type=submit][value=Export]")
#   bouton_export$clickElement()
#   
#   Sys.sleep(60)
#   
#   ## On ferme le navigateur et le serveur
#   remDr$closeall()
#   driver$server$stop()
#   
# }

#mesdatadhs<-get_dhs(config = conf)
# mes_data_df<-get_wdi(config=conf)
# 
# mesdatas2<-data.frame(mes_data[1:10])


write_data<-function(config){
  data_dhs<-get_dhs(config = config)
  data_wdi<-get_wdi(config = config)
  write_csv(x = data_dhs,path = paste0(config$CHEMIN$SAVE_DIR,config$OUTPUT_FILES$output_dhs,Sys.Date(),".csv"))
  write_csv(x = data_wdi,path = paste0(config$CHEMIN$SAVE_DIR,config$OUTPUT_FILES$output_wdi,Sys.Date(),".csv"))
  #get_acled(conf)
  
}

write_data(config = conf)

# 
# school_enrol_funnel2 <- rdb("WB","WDI",dimensions='{"country": ["TD","NE","ML","BF","MR"],"indicator": ["SE.PRM.ENRR","SE.SEC.ENRR","SE.TER.ENRR"]}') #school_enrollment_funnel
# enrol_by_income2 <- rdb('WB','WDI',dimensions='{"country": ["TD","NE","ML","BF","MR"],"indicator": ["SE.PRM.ENRR","SE.PRM.NENR","SE.PRM.TENR"]}')
# 
# 
# 
# toJSON(c(conf$PAYS,conf$VARIABLES_WDI))
# maliste<-list()
# maliste$countries<-unlist(strsplit(conf$PAYS$countries,","))
# maliste$indicator<-unlist(strsplit(conf$VARIABLES_WDI$var_wdi,","))
# gsub("var_wdi","indicator",toJSON(c(countries,conf$VARIABLES_WDI)))
# toJSON(maliste)
# 
# 

dat <- dhs_data(surveyType="MICS",all_results=FALSE)

