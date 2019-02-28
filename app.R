
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

## Permet de télécharger les cartes leaflet
jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 
# Declaration variables et imports fichiers -------------------------------
##Import Config
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
wdi_processed<-conf$NOM_FICHIERS$PROCESSED_WDI
acled_processed<-conf$NOM_FICHIERS$PROCESSED_ACLED
inform_processed<-conf$NOM_FICHIERS$PROCESSED_INFORM
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
df_dhs <- read_rds(paste0(data_path,processed_folder,dhs_processed))

# sdg4 indicators IMPORT------------------------
sdg4indics<-read_rds(paste0(data_path,processed_folder,wdi_processed))

# ACLED IMPORT---------------------------
acled_tiwara <- read_rds(paste0(data_path,processed_folder,acled_processed))

# data for map --------------------------------------------------------------------
df_dhs_sf <- read_rds(paste0(data_path,processed_folder,dhs_sf_processed))


#### Donnee INform Risk  IMPORT#####@-----------------------------------
# Inform Indicators
INFORM_SAHEL<- read_rds(paste0(data_path,processed_folder,inform_processed))
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
df_clustered<-read_rds(paste0(data_path,processed_folder,cluster_processed))
numeric_car_clust<-names(df_clustered)[sapply(df_clustered,class)=="numeric"]
boxplot_df_processed<-read_rds(paste0(data_path,processed_folder,boxplot_processed))
### DATA Carte AFD projets


# Données de l'aide au développement de l'AFD -----------------------------

afddata_sahel<-read_rds(paste0(data_path,processed_folder,afd_processed))
  
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
data_index<-read_rds(paste0(data_path,processed_folder,micro_world_processed))

## Variables catégorielles
data_index_cate<-c("gender","education","work","classe_age","income_quantile")

## Variables numériques
data_index_rep<-c("has_db_card","has_cd_card","save_for_business","save_for_old_age","mobileowner","has_national_id","account","saved")

##-------get geodata on Sahel states 
#sahel <- ne_states(country=sahel_tiwara, returnclass = "sf") # filter on states in target countries
#st_write(st_as_sf(sahel), './inputs/sahel.shp')

sahel_tiwara_geo <- read_rds(paste0(data_path,processed_folder,sahel_tiwara_geo_processed))

### HDX Segment ----------

hdx_geo_2<-read_rds(paste0(data_path,processed_folder,unhcr_processed))


## ILO - Youth employment data ----------------------------

work_data<-read_rds(paste0(data_path,processed_folder,work_data_processed))


work_data_ts_prep<-read_rds(paste0(data_path,processed_folder,work_data_ts_prep_processed))
work_data_ts_name<-work_data_ts_prep$dataset_name[1]
work_data_ts<-work_data_ts_prep %>% 
  select(-series_name,-dataset_name) %>% 
  spread(ref_area,value) %>% 
  arrange(original_period)

## Esquisse Country Dataset ================

esquisse_country<-read_rds(paste0(data_path,processed_folder,esquisse_processed))

## Polygones
poly_dhs<-df_dhs %>% 
  select(CharacteristicLabel,Coordinates) %>%
  mutate(Coordinates=as.character(Coordinates)) %>% 
  distinct()

#Shiny UI ----------------------------------------------
shiny::shinyApp(
  
  ui = bs4DashPage(

    # old_school = TRUE,
    # Navbar ===================================
    navbar = bs4DashNavbar(
      status = "white",
      "TIWARA - L'initiative Française Portée par L'AFD",
      rightUi = bs4DropdownMenu(
        show = FALSE,
        labelText = "!",
        status = "danger",
        src = "http://www.google.fr",
        bs4DropdownMenuItem(
          text = "Survey Year",
          date = "today"
        ),
        bs4DropdownMenuItem(
          text = "Last Data Update",
          date = "yesterday"
        )
      )
    ),
    
    # Sidebar ===================================
    
    
    sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      title = "Tiwara Dash",
      brandColor = "primary",
      url = "http://www.afd.fr",
      src = "https://upload.wikimedia.org/wikipedia/fr/5/58/Logo-AFD.jpg",
      #src = "https://pbs.twimg.com/profile_images/819938214853148673/5X2_8VLs_400x400.jpg", #brainy pic
      elevation = 4,
      opacity = 0.9,
      bs4SidebarMenu(
        id="hello",
        selectInput('CountryName', "Pays",
                    c("Sahel G5",levels(df_dhs$CountryName)),selected = "Sahel G5"),
        bs4SidebarHeader("Socio economic indicators"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "General Outlook",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "general",
          icon = "laptop"  #"desktop"
        ),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Education",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "indicatorMap",
          icon = "atlas" #"sliders"
        ),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Carto Bar",
              bs4Badge(
                "beta!",
                position = "right",
                status = "warning"
              )
            )
          ),
          tabName = "carto",
          icon = "map"
        ),
        bs4SidebarHeader("Crisis and Conflict Trends"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "INFORM Risks",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "inform_risk_data",
          icon = "map"
        ),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "ACLED Reports",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "crisis",
          icon = "desktop"
        ),
        bs4SidebarHeader("Demographics"),
        bs4SidebarMenuItem(
          "Social inclusion",
          tabName = "démograph",
          icon = "object-ungroup"
        ),
        #   bs4SidebarHeader("Developpement local"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Migration",
              bs4Badge(
                "bêta",
                position = "right",
                status = "warning"
              )
            )
          ),
          tabName = "migration",
          icon = "globe"
        ),
        bs4SidebarHeader("Data exploration"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Exploration",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "esquisseSandbox",
          icon = "search"
        ),
        
        bs4SidebarHeader("About Project"),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "Data Sources",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "dbsource",
          icon = "database"
        ),
        bs4SidebarMenuItem(
          HTML(
            paste(
              "About Tiwara",
              bs4Badge(
                "new",
                position = "right",
                status = "success"
              )
            )
          ),
          tabName = "description",
          icon = "info-circle"
        )
        
      )
    ),
    
    
    # Body --------------------------------------------------------------------
    
    
    body = bs4DashBody(
      tags$head(tags$style(
                   "
                           . {  overflow-y: scroll; }
                           
                         ")),
      tags$head(tags$script(src = jsfile)),
      
      bs4TabItems(
        ## GENERAL OUTLOOK =====================
        bs4TabItem(
          tabName = "general",
          # LES KPI
          bs4Card(
            title=paste0("Education KPI"),
            closable = FALSE,
            solidHeader = TRUE,
            collapsible=T,
            elevation=4,
            status = "primary",
            fluidRow(
              align = "bottom",
              column(3,billboarderOutput("gplt_w_literacy", height = "auto")), #column(6,echarts4rOutput("picto_rate")),
              column(3,billboarderOutput("gplt_m_literacy", height = "auto")),
              column(3,billboarderOutput("gplt_seco", height = "auto")),
              column(3,billboarderOutput("gplt_prim", height = "auto"))
            ),width = 12),
          br(),
          
          # LA CARTE
          fluidRow(
            bs4Card(
              title = "Education Indicator Map",
              elevation = 2,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              selectInput('varia', "Select Education Indicator",choices = list(
                "Women who are literate"="ED_LITR_W_LIT",
                "Men who are literate"="ED_LITR_M_LIT",
                "Number of years of education"="ED_EDAT_B_MYR",
                "Primary school attendance rate"="ED_GARP_B_BTH",
                "Secondary school attendance rate"="ED_GARS_B_BTH",
                "Parity index for primary school attendance"="ED_GARP_B_GPI",
                "Parity index for secondary school attendance"="ED_GARS_B_GPI",
                "Population in the lowest wealth quintile"="HC_WIXQ_P_LOW",
                "Population in the second wealth quintile"="HC_WIXQ_P_2ND",
                "Population in the middle wealth quintile"="HC_WIXQ_P_MID",
                "Population in the fourth wealth quintile"="HC_WIXQ_P_4TH",
                "Population in the highest wealth quintile"="HC_WIXQ_P_HGH"), selected = "ED_LITR_W_LIT"),  
              d3Output('mymap')#, # , width = "1000px" #leafletOutput('indicatormap'),
            )# end bs4Card
          ), 
          ## UN DIAGRAMME EN BARRE
          fluidRow(
            bs4Card(
              title = "Vulnerable States by Indicator",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              selectInput('Indicator', NULL,
                          choices = c(
                            "Men who are literate",
                            "Women who are literate",
                            "Number of years of education" = "Median number of years of education: Both sexes",
                            "Primary school attendance rate" = "Gross primary school attendance rate: Total",
                            "Secondary school attendance rate" = "Gross secondary school attendance rate: Total",
                            "Parity index for primary school attendance" = "Gross parity index for gross primary school attendance",
                            "Parity index for secondary school attendance" = "Gross parity index for gross secondary school attendance"
                          ),
                          #levels(df_dhs$Indicator),
                          selected = "Women who are literate"),
              billboarderOutput("barplot_lol"),
              footer = column(
                width = 12,
                align = "center",
                sliderInput("vuls", "Select number of Vulnerable States:",
                            min = 0, max = 15, value = 10
                )
              )# end footer
            )
          )
        ),
        ## EDUCATION ================
        bs4TabItem(
          tabName = "indicatorMap",
          ## LES INDICATEURS, A REVOIR la taille des chiffres?
          fluidRow(
            bs4Card(
              title="Indicators",width=12,
              
              fluidRow(
                bs4ValueBoxOutput('primary_pupil_teacher_ratio_reat_vbox'),
                bs4ValueBoxOutput('lower_sec_pupil_teacher_ratio_reat_vbox'),
                bs4ValueBoxOutput('upper_sec_pupil_teacher_ratio_reat_vbox')
              )
              
            )
          ),
          ## Diagrammes en barres
          fluidRow(
            bs4Card(
              title = "Gender Parity Index and School Enrolment (Primary, Secondary & Tertiary)",
              closable = FALSE,
              width = 12,
              #status = "warning",
              solidHeader = TRUE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              fluidRow(
                column(6,billboarderOutput("gend_parity_plt")), #column(6,echarts4rOutput("picto_rate")),
                column(6,billboarderOutput("enrol_funnel_plt")) #column(6,echarts4rOutput("picto_rate_mont"))
              ),
              footer = column(
                width = 12,
                align = "center",
                sliderInput("period", "Period in years", min = 2010, max = year(today()), value = c(min,max),step = 1,sep="")
              )# end footer
            ), # end bs4Card
            bs4Card(
              title = "School Attendance with Literacy Rate",
              elevation = 4,
              closable = FALSE,
              width = 12,
              status = "primary", # "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("bubble"),
              footer = column(
                width = 12,
                align = "center",
                radioButtons(
                  "literacy", label = NULL,
                  choices=c("Women Literacy Rate" = 'ED_LITR_W_LIT',
                            "Men Literacy Rate" = 'ED_LITR_M_LIT'
                  )
                )
              )# end footer
            ),
            bs4Card(
              title = "Primary Completion Rate by Sex",
              closable = FALSE,
              elevation = 4,
              width = 6,
              #status = "warning",
              solidHeader = TRUE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              billboarderOutput("prim_completion_by_gend_income_plt")
            ), # end bs4Card
            bs4Card(
              title = "Lower Secondary Completion Rate by Sex",
              elevation = 4,
              closable = FALSE,
              width = 6,
              #status = "warning",
              solidHeader = TRUE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              billboarderOutput("seco_completion_by_gend_income_plt")
            )
          ),
          fluidRow(
            bs4Card(
              title = "Out-of-School Children and Youth",
              elevation = 4,
              closable = FALSE,
              width = 12,
              #status = "warning",
              solidHeader = TRUE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              billboarderOutput("out_of_school_primary_plt")
            )
          )
        ),
        
        # Kepler.gl ---------------------------------------------------------------
        
        
        bs4TabItem(
          tabName = "carto",
          fluidRow(
            tags$iframe(seamless="seamless",src="https://kepler.gl/#/demo",height=("1200"), width="100%")
          )
        ),
        
        # About Tiwara ------------------------------------------------------------
        
        
        
        bs4TabItem(
          tabName = "description",
          fluidRow(
            
            column(6,
                   fluidRow(
                     bs4UserCard(
                       width = 12,
                       type = 2,
                       src = "https://www.afd.fr/sites/afd/files/styles/1120_x_750/public/2017-07/Remy-Rioux-par-Alain-Buu.jpg?itok=9vbKvJam",
                       status = "info",
                       title = "Rémy Rioux",
                       subtitle = "Directeur général de l'AFD",
                       "Nous allons intervenir plus et mieux au Sahel, avec nos partenaires africains, européens et internationaux. L’AFD contribuera à ce plan d’action d’envergure centré sur l’éducation, l’emploi des jeunes, l’agriculture, les énergies vertes et la gouvernance."
                     )
                   ),
                   
                   fluidRow(
                     bs4Card(
                       width = 12,
                       title = "Les 3 Axes Prioritaires de L'Initiative",
                       closable = FALSE,
                       status = "primary",
                       footer = tagList(
                         # h4("L’AFD souhaite traiter les vulnérabilités principales qui sous-tendent les crises au Sahel, à travers trois axes d’intervention"),
                         bs4Accordion(id="AccordionAbout",
                           bs4AccordionItem(
                             id = "item1",
                             title = "Insertion socio économique des jeunes",
                             status = "primary",
                             "L’initiative Tiwara au Sahel vise à lutter contre les fragilités profondes qui
                             sous-tendent les crises pour accroître la résilience des régions du Sahel.
                             Ces interventions cibleront les populations dans les zones fragilisées à travers un
                             effort financier additionnel, financé par une part du produit de la taxe sur les
                             transactions financières. L'insertion sociale et économique de la jeunesse, afin d'offrir des perspectives
                             d'avenir aux jeunes femmes et hommes à la recherche d’un emploi décent et d'une meilleure place dans la société"
                           ),
                           bs4AccordionItem(
                             id = "item2",
                             title = "Enjeux démographiques",
                             status = "warning",
                             "La réponse aux enjeux démographiques, afin de limiter les pressions déjà à
                             l'oeuvre (sociales, économiques, sanitaires, ressources naturelles)"
                           ),
                           bs4AccordionItem(
                             id = "item3",
                             title = "Développement local",
                             status = "warning",
                             "L’appui au développement local, afin de réduire les inégalités territoriales et renforcer la cohésion sociale."
                           )
                           )
                       )
                     ))
                   
            ),
            column(6,
                   bs4Card(
                     title = "TIWARA - L'initiative Française Portée par L'AFD",
                     width = 12,
                     bs4Carousel(
                       id = "mycarousel",
                       width = 12,
                       
                       bs4CarouselItem(
                         active = TRUE,
                         src = "https://upload.wikimedia.org/wikipedia/fr/5/58/Logo-AFD.jpg"
                         
                       ),
                       bs4CarouselItem(
                         active = FALSE,
                         src = "https://www.afd.fr/sites/afd/files/styles/1120_x_750/public/2018-02-11-18-08/mali-mosquee-dicko.jpg?itok=7EVOrzOi"
                         #src = "https://raw.githubusercontent.com/brainy749/Tiwara/master/g5_sahel.jpg"
                       ),
                       bs4CarouselItem(
                         active = FALSE,
                         src = "https://www.afd.fr/sites/afd/files/2017-12/2945206494_925047b344_o.jpg"
                         #src = "https://raw.githubusercontent.com/brainy749/Tiwara/master/g5_sahel_en_3_cle8243c4.jpg"
                       )
                     )
                   )
            )
          ),
          fluidRow(width=12,bs4Card(title="Carte des action AFD", leafletOutput("leaflet_action_afd")))
        ),
        
        # DataSources -------------------------------------------------------------
        
        
        bs4TabItem(
          tabName = "dbsource",
          fluidRow(
            bs4Card(
              title = "Data Sources Providers",
              width = 12,
              closable = FALSE,
              footer = tagList(
                bs4Accordion(id = "AccordionDS",
                  bs4AccordionItem(
                    id = "wb",
                    title = "World Bank",
                    status = "info",
                    tags$p("We make use of world bank development indicators and microdata. More Info: https://data.worldbank.org") ,
                    tags$p(paste("Data freshness :", fraicheur_sdg4))
                  ),
                  bs4AccordionItem(
                    id = "dhs",
                    title = "USAID - DHS Program",
                    status = "info",
                    tags$p("The Demographic and Health Surveys (DHS) Program has collected, analyzed, and disseminated accurate and representative data on population,
                           health, HIV, and nutrition through more than 300 surveys in over 90 countries.
                           We make use of some education indicators. More info: https://www.dhsprogram.com/"),
                    # tags$p("Data freshness :"),
                    tags$div("Data freshness :",
                             HTML(chaine_char_dhs))


                    ),
                  bs4AccordionItem(
                    id = "unesco",
                    title = "UNESCO",
                    status = "info",
                    tags$p("UNESCO has a rich source of data for sustainable development goals.
                           We make use of some indicators on education and literacy. More Info: http://uis.unesco.org"),
                    tags$p(paste("Data freshness :", fraicheur_sdg4))
                    ),
                  bs4AccordionItem(
                    id = "acled",
                    title = "ACLED",
                    status = "info",
                    tags$p("The Armed Conflict Location & Event Data Project (ACLED) is a disaggregated conflict analysis and crisis mapping project. ACLED is the highest quality, most widely used, realtime data and analysis source on political violence and protest in the developing world. Practitioners, researchers and governments depend on ACLED for the latest reliable information on current conflict and disorder patterns.
                           More Info: https://www.acleddata.com"),
                    tags$p("Data freshness :", fraicheur_acled)

                    ),
                  bs4AccordionItem(
                    id = "ilo",
                    title = "ILO",
                    status = "info",
                    tags$p("The only tripartite U.N. agency, since 1919 the ILO brings together governments, employers and workers of 187 member States , to set labour standards, develop policies and devise programmes promoting decent work for all women and men.
                           More Info: https://www.ilo.org"),
                    tags$p("Data freshness :", year(Sys.Date()))
                    ),
                  bs4AccordionItem(
                    id = "unhcr",
                    title = "UNHCR",
                    status = "info",
                    tags$p("The only tripartite U.N. agency, since 1919 the ILO brings together governments, employers and workers of 187 member States , to set labour standards, develop policies and devise programmes promoting decent work for all women and men.
                           More Info: https://www.unhcr.org/"),
                    tags$p("Data freshness :", year(Sys.Date()))
                    ),
                  bs4AccordionItem(
                    id = "inform",
                    title = "INFORM Risk",
                    status = "info",
                    tags$p("INFORM is a global, open-source risk assessment for humanitarian crises and disasters.
                           It can support decisions about prevention, preparedness and response.
                           More Info: http://www.inform-index.org") ,
                    tags$p("Data freshness :", fraicheur_inform)

                    )

                  )
                )),
            bs4Card(
              title = "Data API Clients",
              width = 12,
              closable = FALSE,
              footer = tagList(
                bs4Accordion(id="AccordionAPI",
                  bs4AccordionItem(
                    id = "dbnomics",
                    title = "DBnomics",
                    status = "info",
                    "We used rdbnomics, which is the R Client for DBNomics. More Info: https://db.nomics.world"
                  ),
                  bs4AccordionItem(
                    id = "rdhs",
                    title = "R Client DHS",
                    status = "info",
                    "We used rdhs, which is a package for management and analysis of Demographic and Health Survey (DHS) data. More Info on Source: https://ojwatson.github.io/rdhs/articles/client.html"
                  )
                )
              )
            )
            )
              ),
        
     
        # Démographie -------------------------------------------------------------
        
        
        bs4TabItem(
          tabName = "démograph",
          bs4Card(
            title = "Social Inclusion",
            closable = FALSE,
            solidHeader = TRUE,
            collapsible = T,
            elevation = 6,
            width = 12,
            status = "primary",
            fluidRow(
              selectInput(
                inputId = "Input_cate_index",
                label = "Variable of analysis",
                choices = gsub("_"," ",data_index_cate)
              ),
              selectInput(
                inputId = "Input_rep_index",
                label = "Response",
                choices = gsub("_"," ",data_index_rep)
              )),
            fluidRow(column(6, plotlyOutput(
              "demo_barplot_solo"
            )),
            column(6, plotlyOutput("demo_barplot")))
            
            
          ),
          bs4Card(title = "Youth employment",
                  closable = FALSE,
                  solidHeader = TRUE,
                  collapsible = T,
                  elevation = 6,
                  width = 12,
                  status = "primary",
                  column(2,selectInput(
                    inputId = "youthEmployment",
                    label = "Selection de la variable à représenter",
                    choices = unique(work_data$dataset_name)
                  )),
                  column(12,plotlyOutput("youthEmploymentplot"))
          )
          ,
          bs4Card(title = "Youth unemployment estimations",
                  closable = FALSE,
                  solidHeader = TRUE,
                  collapsible = T,
                  elevation = 6,
                  width = 12,
                  status = "primary",
                  billboarderOutput("estimationTsYouthUnemployment")
          )
          # , fluidRow(
          #   imageOutput("construction")
          # )
        )
        ,
        
        # migration -----------------------------------------------------------------
        
        bs4TabItem(
          tabName = "migration",
          fluidRow(bs4ValueBoxOutput("from_migration_vb"),bs4ValueBoxOutput("to_migration_vb"),bs4ValueBoxOutput("inner_migration_vb")),
          fluidRow(
            bs4Card(
              title = "Controllers",
              closable = FALSE,
              solidHeader = TRUE,
              collapsible = T,
              elevation = 6,
              width = 6,
              status = "primary",
              selectInput("migration_measure","Metric",choices = names(hdx_geo_2)[4:11],selected = "Total Population"),
              pickerInput(
                "from_migration",
                "From:",
                c(
                  "Burkina Faso",
                  "Chad",
                  "Mali",
                  "Mauritania",
                  "Niger"
                ),selected = "Mali",multiple=T,
                options = list(`actions-box` = TRUE)
                
              ),
              pickerInput(
                "to_migration",
                "To:",
                c(
                  "Burkina Faso",
                  "Chad",
                  "Mali",
                  "Mauritania",
                  "Niger"
                ),selected = "All",multiple=T,
                options = list(`actions-box` = TRUE)
                
              ),
              sliderInput(width="100%","migrationYear", "Year", min = max(c(min(hdx_geo_2$Year,na.rm = TRUE),2000)), max = max(hdx_geo_2$Year,na.rm = TRUE), value = c(min,max),step =   1,sep=""),
              sliderInput(width="100%",inputId = "flux_sup_migration","Migration flow superior to", min=0,max=100000,value=10000)
            ),
            
            bs4Card(
              title = "Migration map",
              closable = FALSE,
              solidHeader = TRUE,
              collapsible = T,
              elevation = 6,
              width = 6,
              status = "primary",
              footer="Data from UNHCR",
              fluidRow(leafletOutput("migration_flow_map",height = "500px"))
            ))
          ,
          
          fluidRow(
            # height="1800px",
                  column(width=6,bs4Card(title="Migration flows",plotOutput("migrationChord",height = "600px"),width = 12,footer = "Data from UNHCR")),
                  column(width=6, bs4Card(title="Migration Data Portal", "More info on migration",a("here",href="https://migrationdataportal.org",target="_blank"),
                                    fluidRow(img(src="Migration_data_portal.PNG")),width=12))
                   )
          #   bs4Card(
          #     title = "Chord Diagram",
          #     closable = FALSE,
          #     solidHeader = TRUE,
          #     collapsible = T,
          #     elevation = 6,
          #     width = 12,
          #     height="800px",
          #     status = "primary",
          #     #echarts4rOutput("sunburstMigration1")
          # ))
          ###h4("Under construction")
        ),
        ### Esquisse Tab =======================
        # bs4TabItem(
        #   tabName = "esquisseSandbox",
        #    
        #    column(12,height="400px;", # needs to be in fixed height container
        #     esquisserUI(
        #       id = "esquisse",
        #       # header = FALSE, # dont display gadget title
        #       # choose_data = FALSE # dont display button to change data
        #       # 
        # 
        #     
        #   )
        #   ))
        # ,
        
        # Crisis ACLED Reports ------------------------------------------------------------------
        
        
        bs4TabItem(
          tabName = "crisis",
          bs4Card(
            title="Global Information",
            closable = FALSE,
            solidHeader = TRUE,
            collapsible=T,
            elevation=6,
            width=12,
            status = "primary",
            fluidRow(
              bs4InfoBoxOutput("ibox1"),
              bs4InfoBoxOutput("ibox2"),
              bs4InfoBoxOutput("ibox3")
            )
          ),
          fluidRow(
            bs4Card(
              title = "Crisis and Conflict trends",
              closable = FALSE,
              width = 12,
              #status = "warning",
              solidHeader = FALSE,
              status = "primary",
              #gradientColor = "primary", #"success",
              collapsible = TRUE,
              fluidRow(
                column(6,billboarderOutput("olachart_arc_trends")), #column(6,echarts4rOutput("picto_rate")),
                column(6,fluidRow(billboarderOutput("chart_arc_donut")),
                       fluidRow(sliderInput(width="100%","year_donut", "Year", min = min(acled_tiwara$year,na.rm = TRUE), max = max(acled_tiwara$year,na.rm = TRUE), value = c(min,max),step = 1,sep="")))
                #column(3,))#column(6,echarts4rOutput("picto_rate_mont"))
              )
              # ,
              # footer = column(
              #   width = 12,
              #   align = "center",
              #   radioButtons('country', NULL,
              #                c("Sahel G5",levels(acled_tiwara$country)),selected = "Sahel G5",inline = TRUE) 
              # )# end footer
            )
            
          ),
          fluidRow(
            column(width=6,
                   bs4Card(width=12,
                           height = "300px",
                           title="ACLED Controllers",
                           elevation = 4,
                           closable = FALSE,
                           solidHeader = TRUE,
                           status = "primary",
                           collapsible = TRUE,
                           fluidRow(column(4,(selectInput('event_type', 'Reported Event Type',
                                                          c('All',levels(acled_tiwara$event_type))))),
                                    column(8,fluidRow(sliderInput('fatalities', label = 'Number of Fatalities', min = min(acled_tiwara$fatalities,na.rm = TRUE), max = max(acled_tiwara$fatalities,na.rm = TRUE), value = c(min,max),
                                                                  step = 1,width = "100%")),
                                           fluidRow(sliderInput("year", "Year", min = min(acled_tiwara$year,na.rm = TRUE), max = max(acled_tiwara$year,na.rm = TRUE), value = c(min,max),step = 1,sep="",width="100%")))
                           )),
                   fluidRow(width=12,bs4Card(
                     title = "Fragile Location in terms of fatalities",
                     elevation = 4,
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "danger",
                     collapsible = TRUE,
                     DTOutput("responses")
                   ))),
            column(width=6,bs4Card(
              title = "Crisis & Conflict Map",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "warning",
              collapsible = TRUE,
              leafletOutput('map2')
              # ,footer = column(
              #   width = 12,
              #   align = "center",
              #   DTOutput("responses")
              # )
            ))
            
            
          )
        ),
        
        # Inform risks ------------------------------------------------------------
        
        bs4TabItem(
          tabName = "inform_risk_data",
          fluidRow(
            column(
              width = 12, offset = 1,
              selectInput('inform_indicator', "Select Risk Indicator",choices = list(
                "Global Risk"="risk",
                "Vulnerability"="vulnerability",
                "Socio Economic Vulnerability"="socio_economic_vulnerability",
                "Inequality"="inequality",
                "Health conditions"="health_conditions",
                "Lack of coping capacity"="lack_of_coping_capacity",
                "Food insecurity probability"="food_insecurity_probability",
                "Physical exposure to flood"="physical_exposure_to_flood",
                "Land degradation"="land_degradation",
                "Droughts impact"="droughts_probability_and_historical_impact",
                "Political violence"="political_violence",
                "Access to health care"="access_to_health_care",
                "Governance"="governance",
                "Infrastructure"="infrastructure",
                "Malnutrition"="malnutrition",
                "Health of children under 5"="children_u5"), selected = "risk")
              
            ),
            bs4Card(
              title = "Risk Indicators for G5 Sahel",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = FALSE,
              d3Output('informmap')
            ),
            bs4Card(
              title = "Sahel Clustering",
              elevation = 4,
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = FALSE,
              d3Output('clustermap')
            ),
            bs4Card( title = "Cluster correspondance",
                     elevation = 4,
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     collapsible = FALSE,
                     DT::dataTableOutput("clustDataTable"))
            ,
            bs4Card( title = "Exploration Clustering",
                     elevation = 4,
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     collapsible = FALSE,
                     selectInput("Select_var_Clust","Variable of interest",choices = numeric_car_clust),
                     plotlyOutput("clustly")
            )
          )
        )
    )
          ),
    
    # Control bar Crisis and fatality action, mais a quoi sert elle? ----------
    # controlbar = bs4DashControlbar(
    #   skin = "light",
    #   title = "Crisis and Fragility Action",
    #   setSliderColor(sliderId = 1, "black"),
    #   
    # ),
    # tags$head(tags$style("
    #   .myrow{height:800px;}
    #   .card-body{max-height:1000px;}")),
    
    
    # Footer ------------------------------------------------------------------
    footer = bs4DashFooter(
      copyrights = "@AFD Data Team",
      right_text = "2018"
    ),
    title = "AFD Tiwara App"
        ),
  
  
  # ShinyServer -------------------------------------------------------------
  server = function(input, output, session) {
    
    
    
    ### load datasets in reactive mode :)
    # Datasets reactifs -------------------------------------------------------
    
    get_country<-reactive({
      input$CountryName
    })
    output$text<-renderUI({
      HTML(paste0("<h5><b>",get_country(),"</b></h5>"))})
    
    dhs_data <- reactive({
      select_country <- input$CountryName
      dhs_data_filtered <- df_dhs
      
      if (input$CountryName != 'Sahel G5') {
        dhs_data_filtered <-
          dhs_data_filtered %>% filter(CountryName == select_country)
      }
      dhs_data_filtered
    })
    
    clustering_var_sel<-reactive({
      input$Select_var_Clust
    })
    
    
    dhs_data2 <- reactive({
      select_country <- input$CountryName
      select_indicator <- input$Indicator
      
      dhs_data_filtered <-
        df_dhs %>% filter(IndicatorId == select_indicator)
      
      if (input$CountryName != 'Sahel G5') {
        ## On peut le faire en un filter, puis on a déjà filtré sur select indicator, donc pas besoin de refaire?
        dhs_data_filtered <-
          dhs_data_filtered %>% filter(CountryName == select_country) %>% filter(Indicator ==
                                                                                   select_indicator)
      }
      dhs_data_filtered
    })
    
    mapdata_react<-reactive({
      dhs_data_sf()%>%select(input$varia, CharacteristicLabel)
    })
    
    arc_analysis_data <- reactive({
      
      minyear <- input$year[1]
      maxyear <- input$year[2]
      min_fatalities <- input$fatalities[1]
      max_fatalities <- input$fatalities[2]
      
      arc_data_filtered <- acled_tiwara %>% filter(
        year >= minyear,
        year <= maxyear,
        fatalities >= min_fatalities,
        fatalities <= max_fatalities
      )
      
      if (input$event_type != 'All') {
        arc_data_filtered <- filter(arc_data_filtered, event_type == input$event_type)
      }
      arc_data_filtered
    })
    
    
    arc_analysis_data2 <- reactive({
      select_country <- get_country()
      arc_data_filtered <- acled_tiwara 
      if (get_country() != 'Sahel G5') {
        arc_data_filtered <- filter(arc_data_filtered, country == get_country())
      }
      arc_data_filtered
    })
    
    ###♦ indicatormap
    dhs_data_sf <- reactive({
      select_country <- input$CountryName
      dhs_data_filtered <- df_dhs_sf
      if (input$CountryName != 'Sahel G5') {
        dhs_data_filtered <- dhs_data_filtered%>% filter(CountryName == select_country)
      }
      dhs_data_filtered
    })
    
    ## ODD
    ## Pas mal de graphes liés à ce dataset qui originalement était divisé en plusieur sources
    sdg4indics_reat<-reactive({
      select_country <- input$CountryName
      res_sdg4<-sdg4indics
      if (select_country != 'Sahel G5') {
        res_sdg4 <- sdg4indics%>% 
          filter(country_label == select_country)
      }
      res_sdg4
    })
    
    ###Préparation des indics SDG4 pour certains graphes
    sdg4indics_prp_reat<-reactive({
      sdg4indics_reat() %>% 
        select(country_label,period,indicator_label,value)%>%
        spread(indicator_label,value) %>% 
        select(-country_label)%>%
        mutate(year=year(period))%>%
        filter(between(year,input$period[1],input$period[2]))%>%
        select(-period, -year)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        gather(Indicator)
    })
    
    ### Pour une carto
    sahel_tiwara_geo_reat <- reactive({
      
      select_indicator <- input$inform_indicator
      
      #select_country <- input$CountryName
      sahel_tiwara_geo_filtered <- sahel_tiwara_geo%>% 
        mutate(Coordinates=st_as_sfc(Coordinates)) %>% 
        select(select_indicator,admin1,Coordinates) %>% 
        st_as_sf()
      # if (input$CountryName != 'Sahel G5') {
      #   sahel_tiwara_geo_filtered <- sahel_tiwara_geo_filtered%>% filter(admin == select_country)
      # }
      sahel_tiwara_geo_filtered
    })
    
    ## Reactive demographie
    
    demo_cate_selec<-reactive({
      # print(input$Input_cate_index)
      gsub(" ","_",input$Input_cate_index)
    })
    
    demo_rep_selec<-reactive({
      gsub(" ","_",input$Input_rep_index)
      
    })
    
    demo_dataset_barplot<-reactive({
      if (input$CountryName != 'Sahel G5') {
        res<-data_index %>% 
          filter(get_country()==economy) %>% 
          select_at(.vars=c(demo_cate_selec(),'wgt',demo_rep_selec())) %>% 
          filter_at(.vars = demo_rep_selec(),any_vars(!is.na(.))) %>%
          group_by_at(.vars=c(demo_cate_selec(),demo_rep_selec())) %>% 
          summarise(total_weight=sum(wgt)) %>% 
          mutate(percentage=total_weight/sum(total_weight)) %>% 
          rename_at(.vars=c(demo_cate_selec(),demo_rep_selec()),~ c("categorie","reponse")) %>% 
          ungroup() 
      }
      
      else{
        res<-data_index %>% 
          select_at(.vars=c(demo_cate_selec(),'wgt',demo_rep_selec())) %>% 
          filter_at(.vars = demo_rep_selec(),any_vars(!is.na(.))) %>%
          group_by_at(.vars=c(demo_cate_selec(),demo_rep_selec())) %>% 
          summarise(total_weight=sum(wgt)) %>% 
          mutate(percentage=total_weight/sum(total_weight)) %>% 
          rename_at(.vars=c(demo_cate_selec(),demo_rep_selec()),~ c("categorie","reponse")) %>% 
          ungroup() 
      }
      
      return(res)
    })
    demo_dataset_barplot_solo<-reactive({
      if (input$CountryName != 'Sahel G5') {
        res<-data_index %>% 
          filter(get_country()==economy) %>% 
          select_at(.vars=c(demo_cate_selec(),'wgt',demo_rep_selec())) %>% 
          filter_at(.vars = demo_rep_selec(),any_vars(!is.na(.))) %>%
          group_by_at(.vars=c(demo_cate_selec())) %>% 
          summarise(total_weight=sum(wgt)) %>% 
          mutate(percentage=total_weight/sum(total_weight)) %>% 
          rename_at(.vars=c(demo_cate_selec()),~ c("categorie"))
      }
      else{
        res<-data_index %>% 
          select_at(.vars=c(demo_cate_selec(),'wgt',demo_rep_selec())) %>% 
          filter_at(.vars = demo_rep_selec(),any_vars(!is.na(.))) %>%
          group_by_at(.vars=c(demo_cate_selec())) %>% 
          summarise(total_weight=sum(wgt)) %>% 
          mutate(percentage=total_weight/sum(total_weight)) %>% 
          rename_at(.vars=c(demo_cate_selec()),~ c("categorie"))
      }
      return(res)
    })
    
    # Generation des outputs --------------------------------------------------
    
    output$olachart_arc_trends <- renderBillboarder({
      # print("olachart_arc_trends")
      acled <- arc_analysis_data2()
      acled_event_line <- acled %>% 
        dplyr::select(event_type,year,fatalities) %>%
        group_by(year, event_type)%>%
        summarise_all(funs(sum(., na.rm = TRUE)))%>% 
        ungroup()%>%
        spread(event_type, fatalities)%>% 
        arrange(year)
      # print(acled_event_line)
      
      billboarder() %>% 
        bb_linechart(data = acled_event_line, type = "spline") %>% 
        bb_data(x = "year") %>% 
        bb_data(color = htmlwidgets::JS("function(color, d) {return d3.rgb(color).brighter().toString();}")) %>% 
        bb_y_grid(show = TRUE) %>% 
        bb_x_grid(show = TRUE) %>% 
        bb_x_axis(tick = list(fit = FALSE)) %>% 
        # bb_legend(position = "inset") %>% # , inset = list(anchor = "top-right")
        bb_labs(title = "Trends", 
                y = "Official Number of Fatalities", 
                caption = "Data source: ACLED")
    })
    
    
    output$chart_arc_donut <- renderBillboarder({
      acled <- arc_analysis_data2() %>% 
        filter(between(year,input$year_donut[1],input$year_donut[2]))
      acled_event_donut <- acled %>% 
        dplyr::select(event_type,fatalities) %>%
        group_by(event_type)%>%
        summarise_all(funs(sum(., na.rm = TRUE)))%>% 
        arrange(desc(fatalities))%>%
        ungroup()
      
      billboarder() %>% 
        bb_donutchart(data = acled_event_donut) %>% 
        bb_donut(title = "Fatality Distribution") %>%
        bb_labs(title = "Fatality distribution", 
                caption = "Data source: ACLED")
    })
    
    
    ### Map
    # Carte leaflet ACLED conflits ===================================
    output$map2 <- renderLeaflet({
      if(length(dataset_acled_select_row())>0){
        acled <- arc_analysis_data() %>%
          arrange(desc(fatalities)) %>% slice(dataset_acled_select_row())}
      else{acled <-arc_analysis_data()}
      Icon <- makeIcon(
        iconUrl = "https://s1.qwant.com/thumbr/0x380/3/a/5b39451840de4de17f61ceee11abfab1a6d53470bd4f7ca56490b77402783e/616500.png?u=https%3A%2F%2Fimage.flaticon.com%2Ficons%2Fpng%2F512%2F616%2F616500.png&q=0&b=1&p=0&a=1",#"http://12zc4845uhr73vbfjp3ubgkz.wpengine.netdna-cdn.com/wp-content/themes/MTIRedesign/assets/img/crisisIcon.png",
        iconWidth = 35*215/230, iconHeight = 35,
        iconAnchorX = 35*215/230/2, iconAnchorY = 35) # retrieve AFD logo to add on the map at each Beneficiaire location
      AFDCoords <- data.frame(
        lat = acled$latitude,
        lng = acled$longitude)  # prepare a dataframe with GPS coordinates
      popup <-
        paste0(
          "<br><strong>Fatalities: </strong>", acled$fatalities,
          "<br><strong>Source Notes: </strong>",acled$notes,
          "<br><strong>Location: </strong>",acled$location,
          "<br><strong>Country: </strong>",acled$country,
          "<br><strong>Region: </strong>",acled$region,
          "<br><strong>Source: </strong>",paste("<a href='", "'>", acled$source,"</a>" ,sep = ""),
          "<br><strong>Admin1: </strong>", acled$admin1,
          "<br><strong>Admin2: </strong>", acled$admin2,
          "<br><strong>Admin3: </strong>", acled$admin3
        )
      InteractiveMap <- AFDCoords %>% leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions, icon=Icon, popup = popup)%>%
        onRender(
          "function(el, x) {
          L.easyPrint({
          sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
          filename: 'mymap',
          exportOnly: true,
          hideControlContainer: true
          }).addTo(this);
    }"
        )
      InteractiveMap
      })
    
    ## Datatable ACLED =====
    output$responses <- renderDT({
      acled <- arc_analysis_data() %>% 
        select(country,location,fatalities)%>%
        arrange(desc(fatalities)) %>% # %>% top_n(10)
        DT::datatable()
      #DT::datatable(acled_tbl, options = list(bPaginate = TRUE), style = 'bootstrap')
    })
    
    dataset_acled_select_row<-reactive({
      lignes<-input$responses_rows_selected
    })  
    
    
    ### gplt4a , A quoi ca sert actuellement?? ======
    output$gplt4a <- renderValueBox({
      dhs_sub <- dhs_data()
      dhs_sub %>%
        select(IndicatorId,Value)%>%
        group_by(IndicatorId)%>%
        summarise(average_Value = max(Value[Value != 0], na.rm = TRUE))%>%
        ungroup()#%>% filter(IndicatorId=='ED_EDAT_B_MYR')
    })
    
    
    
    ### Barplot output non utilisé? ================
    output$barplot <- renderBillboarder({
      dhs_sub <- dhs_data()
      dhs_sub  %>% select(location,Indicator,IndicatorId,Value)%>%
        filter(IndicatorId %in% variables) %>% 
        group_by(location,Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE))) %>%
        ungroup()%>%filter(Indicator==input$Indicator) %>% 
        select(-Indicator,-IndicatorId) %>% 
        arrange(desc(Value)) %>% 
        top_n(-10) -> hehe  # bottom 10
      
      billboarder() %>%
        bb_barchart(data = hehe) %>%
        bb_data(labels = FALSE) %>%
        bb_y_grid(show = TRUE) %>%
        bb_legend(position = "right") %>%
        bb_labs(title = "Indicator against subnational (bottom 10)",
                y = "Value",
                caption = "Data source: DHS")
    })
    
    ### Billboarder barplot lol ======================
    # Utilisé dans général outlook
    output$barplot_lol <- renderBillboarder({
      dhs_sub <- dhs_data()
      ## Rendre ca réactif?
      dhs_sub  %>% select(location,Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(location,Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE))) %>%
        ungroup()%>%filter(Indicator==input$Indicator) %>%
        select(-Indicator,-IndicatorId) %>%
        arrange(desc(Value)) %>%
        top_n(-input$vuls) -> hehe  # bottom 10
      
      billboarder() %>%
        bb_lollipop(data = arrange(hehe, -desc(Value)), rotated = FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_x_axis(tick = list(centered = TRUE)) %>%
        bb_title(get_country()) %>% 
        bb_labs(
          caption = "Data source: DHS program"
        )
    })
    
    
    ### Radarchart non utilisé ? ===========
    output$radarchart <- renderBillboarder({
      dhs_sub <- dhs_data()
      
      df_dhs %>% select(CountryName,Indicator,IndicatorId,Value) %>% filter(IndicatorId %in% variables) %>% group_by(CountryName,Indicator,IndicatorId)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE))) ->hehehe
      
      billboarder() %>%
        bb_radarchart(
          data = hehehe,
          mapping = bbaes(x = Indicator, y = Value, group = CountryName)
        )%>%
        bb_labs(#title = "Indicator by location",
          y = "Value")
    })
    
    
    ### KPIS  En entête de Général outlook ya moyen de faire des trucs réactifs ici *4--------------------------------
    

    output$gplt_w_literacy <- renderBillboarder({
      
      dhs_sub <- dhs_data()
      ## Rendre réactif
      df_prim <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))
      
      df_prim%>%filter(IndicatorId=="ED_LITR_W_LIT") ->prim
      
      billboarder() %>%
        bb_gaugechart(
          value = round(prim$Value,2),
          name = "Average Literacy Women",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044")) #rev() #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Women Literacy Rate", width = 30,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        ) 
      
    })
    
    
    output$gplt_m_literacy <- renderBillboarder({
      
      dhs_sub <- dhs_data()
      
      df_seco <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))
      
      df_seco%>%filter(IndicatorId=="ED_LITR_M_LIT") ->seco
      
      billboarder() %>%
        bb_gaugechart(
          value = round(seco$Value,2),
          name = "Average Literacy Men",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044"))  #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Men Literacy Rate", width = 30,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        )
    })
    
    output$gplt_seco <- renderBillboarder({
      
      dhs_sub <- dhs_data()
      
      df_seco <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))
      
      df_seco%>%filter(IndicatorId=="ED_GARS_B_BTH") ->seco
      
      billboarder() %>%
        bb_gaugechart(
          value = round(seco$Value,2),
          name = "Secondary school attendance rate",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044"))  #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Secondary school", width = 30,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        )
    })
    
    
    output$gplt_prim <- renderBillboarder({
      
      dhs_sub <- dhs_data()
      
      df_seco <- dhs_sub  %>%
        select(Indicator,IndicatorId,Value) %>%
        filter(IndicatorId %in% variables) %>%
        group_by(Indicator,IndicatorId)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))
      
      df_seco%>%filter(IndicatorId=="ED_GARP_B_BTH") ->seco
      
      billboarder() %>%
        bb_gaugechart(
          value = round(seco$Value,2),
          name = "Primary school attendance rate",
          steps_color = (c("#FF0000", "#F97600", "#F6C600", "#50B044"))  #see color-hex.com
        ) %>% bb_gauge(
          label = list(format = suffix("%")),units = "Primary school", width = 30,
          min = 0, max = 100, label = list(format = htmlwidgets::JS("function(value) {return value;}"))
        )
    })
    ###
    
    # Plotly Indicator view ---------------------------------------------------
    
    output$bubble <- renderPlotly({
      
      dhs_sub <- dhs_data()
      bubble <- dhs_sub %>%
        select(IndicatorId,CountryName, location,Value)%>%
        group_by(IndicatorId,CountryName, location)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        ungroup()%>%spread(IndicatorId, Value)
      
      literacyRate <- switch(input$literacy,
                             'ED_LITR_W_LIT' = bubble$ED_LITR_W_LIT,
                             'ED_LITR_M_LIT' = bubble$ED_LITR_M_LIT,
                             'ED_LITR_W_LIT' = bubble$ED_LITR_W_LIT)
      
      if (sum(!is.na(bubble$ED_GARP_B_GPI | bubble$ED_GARS_B_GPI | bubble$ED_LITR_W_LIT | bubble$ED_LITR_M_LIT ))==0)
        plotly_empty() else 
          plot_ly(bubble, x = ~ED_GARP_B_GPI, y = ~ED_GARS_B_GPI,
                  text = ~paste('Location:', location, '<br>Country Name:', CountryName,
                                '<br>Women who are literate:', literacyRate),
                  mode = "markers", color = ~CountryName, size = ~literacyRate,
                  marker = list(symbol = 'circle', sizemode = 'diameter',
                                line = list(width = 2, color = '#FFFFFF'))) %>%
        layout(#title = 'Gross Parity Index: Primary v. Secondary school attendance',
          xaxis = list(title = 'Primary school attendance'),
          yaxis = list(title = 'Secondary school attendance'),
          plot_bgcolor = 'rgb(243, 243, 243)')  # parity index Primary vs secondary. size is women literate, color is country
    })
    
    
    # ODD, Tout commenté ------------------------------------------------------
    
    
    ### ODD
    
    # dhs_data <- reactive({
    #
    #   select_country <- input$CountryName
    #
    #   dhs_data_filtered <- df_dhs
    #
    #   if (input$CountryName != 'Sahel G5') {
    #     dhs_data_filtered <- dhs_data_filtered%>% filter(CountryName == select_country)
    #   }
    #
    #   dhs_data_filtered
    #
    #
    # })
    
    # output$enrol_funnel <- renderBillboarder({
    #
    #
    #   school_enrol_funnel%>%filter(country==input$CountryName)%>%
    #     select(country_label,period,indicator_label,value)%>%
    #     spread(indicator_label,value) %>% select(-country_label)%>%
    #     mutate(year=year(period))%>% filter(between(year,2010,2017))%>%
    #     select(-period, -year)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
    #     gather(Indicator)->hehehe
    #
    #
    #
    #   billboarder() %>%
    #     bb_lollipop(hehehe, rotated = TRUE)%>%
    #     bb_y_grid(show = TRUE) %>%
    #     bb_x_axis(tick = list(centered = TRUE)) %>%
    #     bb_labs(
    #       #title = "Indicator against subnational",
    #       caption = "Data source: UNESCO Institute for Statistics"
    #     )
    #
    # })
    
    ### BubbleLol non mappé ==========
    output$bubble_lol <- renderBillboarder({
      dhs_sub <- dhs_data()
      bubble <- dhs_sub %>%
        select(IndicatorId,CountryName, location,Value)%>%
        group_by(IndicatorId,CountryName, location)%>%
        summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%
        ungroup()%>%spread(IndicatorId, Value)
      
      billboarder() %>%
        bb_scatterplot(
          data = bubble,
          mapping = bbaes(ED_GARP_B_GPI, ED_GARS_B_GPI, group = CountryName)
        ) %>%
        bb_x_axis(tick = list(fit = FALSE))
    })
    
    ## Picto rates commentés ===============
    # output$picto_rate <- renderEcharts4r({
    # 
    #   hehe <- dhs_data()%>%filter(IndicatorId %in%c('ED_LITR_W_LIT','ED_LITR_M_LIT'))
    # 
    #   path <- "path://M0,10 L10,10 C5.5,10 5.5,5 5,0 C4.5,5 4.5,10 0,10 z"
    # 
    #   style <- list(
    #     normal = list(opacity = 0.5), # normal
    #     emphasis = list(opacity = 1) # on hover
    #   )
    # 
    #   hehe %>%
    #     e_charts(Indicator) %>%
    #     e_pictorial(Value, symbol = path,name = "Literacy Rate",
    #                 barCategoryGap = "-10%",
    #                 itemStyle = style) %>%
    #     e_legend(FALSE)%>%
    #     e_tooltip()%>%
    #     e_title("")
    # 
    # })
    # 
    # output$picto_rate_mont <- renderEcharts4r({
    # 
    #   hehe <- dhs_data()%>%filter(IndicatorId %in%c('ED_LITR_W_LIT','ED_LITR_M_LIT'))
    # 
    #   qomo <- paste0(
    #     "https://ecomfe.github.io/echarts-examples/public/",
    #     "data/asset/img/hill-Qomolangma.png"
    #   )
    # 
    #   kili <- paste0(
    #     "https://ecomfe.github.io/echarts-examples/public/",
    #     "data/asset/img/hill-Kilimanjaro.png"
    #   )
    # 
    #   hehe%>%mutate(images= case_when(IndicatorId=='ED_LITR_W_LIT' ~ paste0("image://", qomo), IndicatorId=='ED_LITR_M_LIT' ~ paste0("image://", kili))) -> hehehe
    # 
    #   style <- list(
    #     normal = list(opacity = 0.5), # normal
    #     emphasis = list(opacity = 1) # on hover
    #   )
    # 
    #   hehehe %>%
    #     e_charts(Indicator) %>%
    #     e_pictorial(Value, images, name = "Literacy Rate") %>%
    #     e_legend(FALSE) %>%
    #     e_tooltip()%>%
    #     #e_theme("westeros")%>%
    #     e_title("")
    # 
    # })
    
    
    
    ## Indicator view, Billboard out of school ====================
    output$out_of_school_primary_plt <- renderBillboarder({
      
      sdg4indics_reat()%>%
        filter(indicator %in% c('SE.PRM.UNER.MA.ZS','SE.PRM.UNER.FE.ZS'))%>%
        select(country_label,period,indicator_label,value)%>% 
        mutate(value=round(value,2))%>%
        spread(indicator_label,value)%>% 
        select(-country_label) %>%
        rename("Female (% of female primary school age)"="Children out of school, female (% of female primary school age)",
               "Male (% of male primary school age)"="Children out of school, male (% of male primary school age)")->hehe
      
      hehe_out <- hehe
      if (input$CountryName == 'Sahel G5') {
        hehe_out <- hehe%>% group_by(period)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%ungroup()
      }
      
      billboarder() %>% 
        bb_linechart(
          data = hehe_out, 
          type = "spline"
        ) %>%   
        bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))%>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_labs(
          #title = "Indicator against subnational",
          caption = "Data source: UNESCO Institute for Statistics"
        )%>% 
        bb_title(get_country())
    })
    
    ## Completion by gender plots, Indicator view ====================
    output$prim_completion_by_gend_income_plt <- renderBillboarder({
      
      sdg4indics_reat()%>%
        filter(indicator %in% c('SE.PRM.CMPT.FE.ZS','SE.PRM.CMPT.MA.ZS'))%>%
        select(country_label,period,indicator_label,value)%>% 
        mutate(value=round(value,2))%>%
        spread(indicator_label,value) %>% 
        select(-country_label)%>%
        rename("Female (% of relevant age group)"="Primary completion rate, female (% of relevant age group)",
               "Male (% of relevant age group)"="Primary completion rate, male (% of relevant age group)")->hehe
      
      hehe_out <- hehe
      if (input$CountryName == 'Sahel G5') {
        hehe_out <- hehe%>% group_by(period)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%ungroup()
      }
      
      billboarder() %>% 
        bb_linechart(
          data = hehe_out, 
          type = "spline"
        ) %>%   
        bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))%>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_data(names =list("value"="Primary completion rate (% of relevant age group)"))%>%
        bb_labs(
          #title = "Indicator against subnational",
          caption = "Data source: UNESCO Institute for Statistics"
        ) %>% 
        bb_title(get_country())
    })
    
    output$seco_completion_by_gend_income_plt <- renderBillboarder({
      
      sdg4indics_reat()%>%
        filter(indicator %in% c('SE.SEC.CMPT.LO.FE.ZS','SE.SEC.CMPT.LO.MA.ZS'))%>%
        select(country_label,period,indicator_label,value)%>% 
        mutate(value=round(value,2))%>%
        spread(indicator_label,value) %>% 
        select(-country_label)%>%
        rename("Female (% of relevant age group)"="Lower secondary completion rate, female (% of relevant age group)",
               "Male (% of relevant age group)"="Lower secondary completion rate, male (% of relevant age group)")->hehe
      
      hehe_out <- hehe
      if (input$CountryName == 'Sahel G5') {
        hehe_out <- hehe%>% group_by(period)%>%summarise_all(funs(mean(.[. != 0], na.rm = TRUE)))%>%ungroup()
      }
      
      # print(hehe_out)
      
      billboarder() %>% 
        bb_linechart(
          data = hehe_out, 
          type = "spline"
        ) %>%   
        bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))%>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_data(names =list("value"="Lower secondary completion rate (% of relevant age group)"))%>%
        bb_labs(
          #title = "Indicator against subnational",
          #y = "(% of relevant age group)",
          caption = "Data source: UNESCO Institute for Statistics"
        )%>% 
        bb_title(get_country())
    })
    
    ## Render parity barplots, indicator view ==================
    output$gend_parity_plt <- renderBillboarder({
      gend_parity_prep<-sdg4indics_prp_reat()%>%
        filter(Indicator %in% c("School enrollment, primary (gross), gender parity index (GPI)",
                                "School enrollment, secondary (gross), gender parity index (GPI)",
                                "School enrollment, tertiary (gross), gender parity index (GPI)")) %>% 
        mutate(Indicator = case_when(Indicator=='School enrollment, primary (gross), gender parity index (GPI)' ~ 'Primary', 
                                     Indicator=='School enrollment, secondary (gross), gender parity index (GPI)' ~ 'Secondary', 
                                     Indicator =='School enrollment, tertiary (gross), gender parity index (GPI)' ~'Tertiary'))%>%
        mutate(value=round(value,2))
      
      billboarder() %>%
        bb_barchart(gend_parity_prep, rotated = TRUE)%>%
        bb_data(names =list("value"="Gender Parity Index (GPI) in school enrollment (gross)"))%>%
        bb_y_grid(show = TRUE) %>%
        bb_x_axis(tick = list(centered = TRUE)) %>%
        bb_labs(
          title = paste("Gender Parity Index -",get_country()),
          caption = " "
        )
    })
    
    output$enrol_funnel_plt <- renderBillboarder({
      
      school_enrol_prep<-sdg4indics_prp_reat()%>%
        filter(Indicator %in% c("School enrollment, primary (% gross)",
                                "School enrollment, secondary (% gross)",
                                "School enrollment, tertiary (% gross)")) %>% 
        mutate(Indicator = case_when(Indicator=='School enrollment, primary (% gross)' ~ 'Primary', 
                                     Indicator=='School enrollment, secondary (% gross)' ~ 'Secondary', 
                                     Indicator =='School enrollment, tertiary (% gross)' ~'Tertiary'))%>%
        mutate(value=round(value,2)) 
      
      billboarder() %>%
        bb_barchart(school_enrol_prep, rotated = TRUE)%>%
        #bb_lollipop(hehehe, rotated = TRUE)%>%
        bb_data(names =list("value"="School enrollment (% gross)"))%>%
        # bb_legend(show=FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_x_axis(tick = list(centered = TRUE)) %>%
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_labs(
          title = paste("School enrollment -",get_country()),
          y = "% gross",
          caption = "Data source: UNESCO Institute for Statistics"
        )
      
    })
    
    ## TreeMap non utilisée ===============
    output$treemap <- renderEcharts4r({
      
      dhs_sub <- dhs_data()
      
      dhs_sub  %>%
        filter(Indicator==input$Indicator)%>%
        e_charts() %>%
        e_treemap(CountryName, location, Value)
      
    })
    
    # [Reactive??]Map principale - General Outlook ==================
    
    
    output$mymap <- renderD3({
      # # # -----working-------
      #  d3_map(shape = mapdata) %>%
      #    add_continuous_breaks(var = input$varia,palette = input$palette) %>%
      #    add_tooltip(value = sprintf("<b>{CharacteristicLabel}</b>: {round(%s, 1)}%%", input$varia))%>%
      #    add_legend(title = "Value of Indicator", suffix = "")
      # #  # ------------
      # print(input$varia)
      mesure<-case_when(input$varia == "ED_LITR_W_LIT" ~ "%",
                        input$varia == "ED_LITR_M_LIT" ~ "%",
                        input$varia == "ED_EDAT_B_MYR" ~ "Years",
                        input$varia == "ED_GARP_B_BTH" ~ "%",
                        input$varia == "ED_GARS_B_BTH" ~ "%",
                        input$varia == "ED_GARP_B_GPI" ~ "",
                        input$varia == "ED_GARS_B_GPI" ~ "",
                        input$varia == "HC_WIXQ_P_LOW" ~ "%",
                        input$varia == "HC_WIXQ_P_2ND" ~ "%",
                        input$varia == "HC_WIXQ_P_MID" ~ "%",
                        input$varia == "HC_WIXQ_P_4TH" ~ "%",
                        input$varia == "HC_WIXQ_P_HGH" ~ "%"
      )
      
      if (sum(!is.na(mapdata_react()[[input$varia]])) == 0) {
        he <- d3_map(shape = mapdata_react())%>%
          add_tooltip(value = sprintf("<b>{CharacteristicLabel}</b>: {round(%s, 1)}", input$varia)) %>% 
          add_labs(title = paste0("Education Map in ",get_country())) %>% 
          add_labs(caption = "Data from DHS Program")
      }
      else {
        he <- d3_map(shape = mapdata_react()) %>%
          add_continuous_breaks(var = input$varia,palette = "Blues",direction = -1) %>%
          add_labs(title = get_country()) %>% 
          add_tooltip(value = sprintf("<b>{CharacteristicLabel}</b>: {round(%s, 1)}", input$varia))%>%
          add_legend(title = paste("Value of Indicator :",mesure), suffix = "")
        observeEvent(list(input$var, input$palette), {
          d3_map_proxy(shinyId = "mymap", data = mapdata_react()) %>%
            update_continuous_breaks(var = input$varia, palette = "Blues",direction = -1) 
        })
        
        
      }
      
      
      
      he
      
      
      
    })
    ## Map Risk Indicator ===================
    output$informmap <- renderD3({
      mapdata <- sahel_tiwara_geo_reat() 
      d3_map(shape = mapdata) %>%
        add_continuous_breaks(var = input$inform_indicator, palette = "Reds",direction = -1) %>%
        add_tooltip(value = sprintf("<b>{admin1}</b>: {round(%s, 1)}", input$inform_indicator))%>%
        add_legend(title = "Range", suffix = "")%>% 
        add_labs(caption = "Data from Inform Risk")
      
    })
    
    ## Map des CLusters =====================
    
    output$clustermap <- renderD3({
      mapdata <- df_clustered
      d3_map(shape = mapdata) %>%
        add_discrete_scale(var="clust",palette = "Set1") %>% 
        #add_continuous_breaks(var = input$inform_indicator, palette = "Blues",direction = -1) %>%
        add_tooltip(value = sprintf("<b>{CharacteristicLabel}</b>: {clust}",1))%>%
        add_legend(title = "Cluster", suffix = "") %>% 
        add_labs(caption = "Data from DHS Program and INFORM Database")
      
    })
    
    ## Les boxplots des clusters
    output$clustly<-renderPlotly({
      plot_table<- boxplot_df_processed %>% 
        mutate(clust=as.factor(clust)) %>% 
        #  mutate(clust=df_etude$clust) %>% 
        left_join(poly_dhs,by=c("CharacteristicLabel"))%>% 
        rename_at(clustering_var_sel(), ~"varsel") %>% 
        select(varsel,clust)
      
      plot_ly(plot_table,y= ~varsel,color=~clust, colors=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00"),type="box") %>% 
        layout(yaxis=list(title=clustering_var_sel(),titlefont=list(
          family = "Arial",
          size = 12,
          color = "#7f7f7f")))
    })
    
    output$clustDataTable<-renderDT({
      DT::datatable(data.frame(df_clustered %>% select(CharacteristicLabel,CountryName,clust)) %>% select(-Coordinates))
    })
    
    # image <- image_read("./images/construction2.gif")
    
    # output$construction <- renderImage({
    #   # Numeric operators
    #   tmpfile <- image%>%
    #     image_write(tempfile(fileext='gif'), format = 'gif')
    #   # Return a list
    #   list(src = tmpfile, contentType = "image/gif")
    # })
    
    # General Section, info boxes ===========
    
    
    output$ibox1 <- renderbs4InfoBox({
      if(get_country()=="Sahel G5"){
        no_events <- acled_tiwara %>% distinct(data_id) %>% tally()
      }
      else{
        no_events <- acled_tiwara %>%filter(country==get_country()) %>% distinct(data_id) %>% tally()
      }
      bs4InfoBox(
        title = tags$h3("Number of ACLED Events ",style="color:white"),
        width = 3,
        value = tags$h2(tags$b(no_events,style="color:white")),
        status = "info",
        icon = "newspaper"
      )
    })
    
    output$ibox2 <- renderbs4InfoBox({
      if(get_country()=="Sahel G5"){
        repo_fatalities <-sum(acled_tiwara$fatalities,na.rm = TRUE)}
      else{
        repo_fatalities <- acled_tiwara %>%filter(country==get_country()) %>% pull(fatalities) %>% sum(na.rm = T)
      }
      bs4InfoBox(
        title = tags$h3("Reported Fatalities",style="color:white"),
        status = "info",
        width = 3,
        value = tags$h2(tags$b(repo_fatalities,style="color:white")),
        gradientColor = ifelse(repo_fatalities > 10, "danger", "warning"),
        icon = "bullhorn"
      )
    })
    
    output$ibox3 <- renderbs4InfoBox({
      if(get_country()=="Sahel G5"){
        no_violence <- acled_tiwara %>% filter(event_type == 'Violence against civilians')%>%tally()
      }
      else{
        no_violence <- acled_tiwara %>%filter(country==get_country() & event_type == 'Violence against civilians') %>% tally()
      }
      bs4InfoBox(
        title = tags$h3("Number of Violence against civilians",style="color:white"),
        gradientColor = "warning",
        width = 3,
        value = tags$h2(tags$b(no_violence,style="color:white")),
        icon = "ambulance" #icon = "sliders"
      )
    })
    
    ### Actions AFD -------------------
    output$leaflet_action_afd<-renderLeaflet({
      # print(AFDCoords_sahel)
      AFDCoords_sahel %>%
        leaflet() %>%
        addBootstrapDependency()%>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions, icon=Icon, popup = paste(BeneficiaireName,AFD_Desc,sep="<br/>"))%>%
        #addProviderTiles(providers$CartoDB.Positron)%>%
        addProviderTiles(providers$OpenStreetMap) %>%
        onRender(
          "function(el, x) {
          L.easyPrint({
          sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
          filename: 'mymap',
          exportOnly: true,
          hideControlContainer: true
          }).addTo(this);
    }"
        )
      # %>%
      #   #  addMiniMap()%>%
      #   addSearchOSM()
      })
    
    
    ### Demographie -----------------------
    youth_employment_value<-reactive({
      input$youthEmployment
    })
    
    youth_employment_dataset<-reactive({
      work_data %>%
        arrange(original_period) %>% 
        slice(1) %>% 
        ungroup() %>% 
        filter(dataset_name==youth_employment_value())
    })
    
    output$demo_barplot<-renderPlotly({
      # print(demo_dataset_barplot())
      demo_dataset_barplot() %>% 
        plot_ly(x= ~reponse,y= ~percentage,color= ~categorie) 
      
    })
    
    
    output$demo_barplot_solo<-renderPlotly({
      # print(demo_dataset_barplot_solo())
      demo_dataset_barplot_solo() %>% 
        plot_ly(x= ~categorie,y= ~percentage) 
      
    })
    
    output$youthEmploymentplot<-renderPlotly({
      xlab <- list(
        title = "Country")
      ylab <- list(
        title = "Value")
      youth_employment_dataset() %>% 
        plot_ly( x = ~ref_area, y = ~value, type = 'bar', name = 'Participation rate') %>% 
        layout(title=youth_employment_value(),
               xaxis = xlab, yaxis = ylab,
               annotations=list(text = 'Source : ILO',
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = 1,
                                yref = 'paper', y = -0.07))
      
      
    })
    ## TS d'estimations ! 
    output$estimationTsYouthUnemployment<-renderBillboarder({
      billboarder() %>% 
        bb_linechart(data=work_data_ts,
                     type = "spline") %>% 
        bb_y_axis(tick = list(format = suffix("%"))) %>%
        bb_labs(
          caption = "Data source: ILO"
        )%>% 
        bb_title(work_data_ts_name)
    })
    ## Indicator view : Infos en haut ================

    output$primary_pupil_teacher_ratio_reat_vbox <- renderbs4ValueBox({
      pupil_teacher_ratio_prep<-sdg4indics_prp_reat()%>%
        filter(Indicator == 'Pupil-teacher ratio, primary')%>%
        pull(value) ->primary_pt_ratio
      
      ## Les infos box
      if(!is.nan(pupil_teacher_ratio_prep)){
        bs4ValueBox(
          value = tags$h1(tags$b(round(pupil_teacher_ratio_prep,digits = 0)),style="color:white"), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
          subtitle = tags$b(paste0("Primary pupil-teacher ratio in ",get_country()),style="color:white"),
          status = ifelse(round(pupil_teacher_ratio_prep,2) > 40, "danger", ifelse(round(pupil_teacher_ratio_prep,2)> 20, "warning", "success")),
          icon = "chalkboard-teacher",
          href = "#",  #http://uis.unesco.org/en/glossary
          elevation = 4
          
        )
      }
      else{
        bs4ValueBox(
          value = tags$h1(tags$b("No Data"),style="color:white"), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
          subtitle = "",
          status = "primary",
          icon = icon("comment-slash"),
          href = "#",  #http://uis.unesco.org/en/glossary
          elevation = 4
        )
      }
      
    })
    
    output$lower_sec_pupil_teacher_ratio_reat_vbox <- renderbs4ValueBox({
      pupil_teacher_ratio_prep<-sdg4indics_prp_reat()%>%
        filter(Indicator == 'Pupil-teacher ratio, lower secondary')%>%
        pull(value) 
      
      if(!is.nan(pupil_teacher_ratio_prep)){
        bs4ValueBox(
          value = tags$h1(tags$b(round(pupil_teacher_ratio_prep,digits = 0)),style="color:white"), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
          subtitle = tags$b(paste0("Lower Secondary pupil-teacher ratio in ",get_country()),style="color:white"),
          status = ifelse(round(pupil_teacher_ratio_prep,2) > 40, "danger", ifelse(round(pupil_teacher_ratio_prep,2)> 20, "warning", "success")),
          icon = "users",
          href = "#",
          width = 2,
          elevation = 4
        )
      }
      else{  
        bs4ValueBox(
          value = tags$h1(tags$b("No Data"),style="color:white"), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
          subtitle = "",
          status = "primary",
          icon = "comment-slash",
          href = "#",  #http://uis.unesco.org/en/glossary
          elevation = 4
        )
      }
      
    })
    
    output$upper_sec_pupil_teacher_ratio_reat_vbox <- renderbs4ValueBox({
      pupil_teacher_ratio_prep<-sdg4indics_prp_reat()%>%
        filter(Indicator == 'Pupil-teacher ratio, upper secondary')%>%
        pull(value) 
      
      if(!is.nan(pupil_teacher_ratio_prep)){
        bs4ValueBox(
          value = tags$h1(tags$b(round(pupil_teacher_ratio_prep,digits = 0)),style="color:white"), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
          subtitle = tags$b(paste0("Upper Secondary pupil-teacher ratio in ",get_country()),style="color:white"),
          status = ifelse(round(pupil_teacher_ratio_prep,2) > 40, "danger", ifelse(round(pupil_teacher_ratio_prep,2)> 20, "warning", "success")),
          icon = "users-cog",
          elevation = 4
          
        )
      }
      else{  
        bs4ValueBox(
          value = tags$h1(tags$b("No Data"),style="color:white"), #(gplt4a %>%filter(IndicatorId=='ED_EDAT_B_MYR'))$average_Value,
          subtitle = "",
          status = "primary",
          icon = "comment-slash",
          href = "#",  #http://uis.unesco.org/en/glossary
          elevation = 4
        )
      }
    })
    
    
    #### Tab Migration ---------------
    
    # reactive_migration_measure<-reactive({
    #   input$migration_measure
    # })
    
    ## On filtre les données sur les années sélectionnées, puis les flux from/to sélectionnés (from OU to) et enfin on ne garde que les flux supérieurs à la limite fixée par l'utilisateur
    reactive_hdx<-reactive({
      hdx_geo_2%>%
        mutate(lat_debt=as.numeric(lat_debt),
               lat_fin=as.numeric(lat_fin),
               long_debt=as.numeric(long_debt),
               long_fin=as.numeric(long_fin)) %>%
        filter(Year>=input$migrationYear[1] & Year<=input$migrationYear[2]) %>%
        filter(arrivee %in% input$to_migration | Origin %in% input$from_migration)%>%
        rename_at(.vars=input$migration_measure,~"mesure") %>%
        group_by(Origin,arrivee,lat_debt,lat_fin,long_debt,long_fin) %>%
        summarise(mesure=sum(mesure)) %>%
        filter(mesure > input$flux_sup_migration) 
      
    })
    
    ## On génère une palette de couleur s'il y a plusieurs couleurs. Cela sera utilisé pour le leaflet
    reactive_hdx_palette<-reactive({
      if(length(unique(reactive_hdx()$Origin))>1){
        pal <- qualpal(length(unique(reactive_hdx()$Origin)), colorspace="pretty")#list(h=c(0,360), s=c(0.3,1), l=c(0.2,0.8)))
        to_join<-data.frame(Origin=unique(reactive_hdx()$Origin),pal=pal$hex)
        res<-reactive_hdx() %>%
          select(Origin) %>%
          inner_join(to_join,by="Origin")
      }
      else{
        list(pal="black")
      }
    })
    
    
    
    ## Les value box
    ## Celle-ci concerne les volumes from
    output$from_migration_vb<-renderbs4ValueBox({
      from<-reactive_hdx() %>%
        filter(Origin!=arrivee) %>%
        filter(Origin %in% input$from_migration) %>%
        pull(mesure) %>%
        sum()
      pays_texte<-paste(input$from_migration,collapse = ", ")
      bs4ValueBox(
        value = tags$h1(tags$b(format(from,big.mark = " ",small.mark = " ")),style="color:white"),
        subtitle = tags$b(paste0("Migration from : ",pays_texte," according to selected parameters "),style="color:white"),
        status = "primary",
        icon = "users-cog",
        elevation = 4
        
      )
    })
    
    ## Celle-ci concerne les volumes to
    output$to_migration_vb<-renderbs4ValueBox({
      to<-reactive_hdx() %>%
        filter(Origin!=arrivee) %>%
        filter(arrivee %in% input$to_migration) %>%
        pull(mesure) %>%
        sum()
      pays_texte<-paste(input$to_migration,collapse = ", ")
      bs4ValueBox(
        value = tags$h1(tags$b(format(to,big.mark = " ",small.mark = " ")),style="color:white"),
        subtitle = tags$b(paste0("Migration to : ",pays_texte," according to selected parameters "),style="color:white"),
        status = "primary",
        icon = "users-cog",
        elevation = 4
        
      )
    })
    
    ## Celle-ci concerne les volumes internes
    output$inner_migration_vb<-renderbs4ValueBox({
      inner<-reactive_hdx() %>%
        filter(Origin==arrivee) %>%
        filter(arrivee %in% input$from_migration) %>%
        pull(mesure) %>%
        sum()
      pays_texte<-paste(input$from_migration,collapse = ", ")
      bs4ValueBox(
        value = tags$h1(tags$b(format(inner,big.mark = " ",small.mark = " ")),style="color:white"),
        subtitle = tags$b(paste0("Internal migration in : ",pays_texte," according to selected parameters "),style="color:white"),
        status = "primary",
        icon = "users-cog",
        elevation = 4
        
      )
    })
    
    ## Le Leaflet, on utilise gcIntermediate pour créer les lignes et addPolylines pour les tracer
    output$migration_flow_map<-renderLeaflet({
      if(nrow(reactive_hdx())>0){
        gcIntermediate(reactive_hdx()[,c("long_debt","lat_debt")]+1,reactive_hdx()[,c("long_fin","lat_fin")],addStartEnd = T,sp = T,n=100) %>%
          leaflet() %>%
          addTiles() %>%
          addPolylines(color = reactive_hdx_palette()$pal ,
                       opacity = 1,
                       weight =  scale(log(reactive_hdx()$mesure))+2,
                       popup = paste(reactive_hdx()$Origin,"->",reactive_hdx()$arrivee ,":",reactive_hdx()$mesure)) %>%
          addProviderTiles("OpenStreetMap.HOT")%>%
          onRender(
            "function(el, x) {
            L.easyPrint({
            sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
            filename: 'mymap',
            exportOnly: true,
            hideControlContainer: true
            }).addTo(this);
      }"
        )
        }
      
      })
    
    
    
    
    
    ## Le sunburst, mais ils fait planter d'autres pages
    output$sunburstMigration1<-renderEcharts4r({
      if(nrow(reactive_hdx())>0){
        reactive_hdx() %>%
          e_charts() %>%
          e_sunburst(Origin,arrivee,mesure) %>%
          e_title("Flux migratoires")
      }
    })
    
    ## Chord Diagram
    
    output$migrationChord<-renderPlot({
      # print(reactiv))
      hdx_chord<-reactive_hdx() %>% ungroup() %>% select(Origin,arrivee,mesure)
      # print(hdx_chord)
      liste_pays_hdx<-unique(c(hdx_chord$Origin,hdx_chord$arrivee))
      circos.clear()
      circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
      monChord<-chordDiagram(x = hdx_chord , transparency = 0.25,
                             order=liste_pays_hdx,
                             directional = 1,
                             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                             annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
                             link.arr.type = "big.arrow", link.arr.length=0.1,link.sort = TRUE, link.largest.ontop = TRUE)
      
      # print(liste_pays_hdx)
      # print(monChord)
      circos.trackPlotRegion(
        track.index = 1,
        bg.border = NA,
        panel.fun = function(x, y) {
          xlim = get.cell.meta.data("xlim")
          sector.index = get.cell.meta.data("sector.index")
          reg1 = liste_pays_hdx[liste_pays_hdx == sector.index]
          circos.text(x = mean(xlim), y =3.5,
                      labels = reg1 , facing = "reverse.clockwise", cex = 1)
          # circos.axis(h = "top",
          #             major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)),
          #             minor.ticks = 1, major.tick.percentage = 0.5,
          #             labels.niceFacing = FALSE)
        }
      )
      # plot(monChord)
    })
    
    ## Esquisse  =====================
    
    data_esquisse_country<-reactiveValues(data=esquisse_country,name="esquisse_country")
    callModule(module = esquisserServer, id = "esquisse", data = data_esquisse_country,sizeDataModule="l")

    })
