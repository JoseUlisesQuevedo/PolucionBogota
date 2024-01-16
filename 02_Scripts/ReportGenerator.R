
library(rmarkdown)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(leaflet)
library(sf)
library(scales)
library(leaflet)
library(RColorBrewer)


###Sourcing file with helper functions
source("02_Scripts/helper_functions.R")

#### Reads dataset, shapefiles, etc.
source("02_Scripts/DataReading.R")

#### Cleaning, wrangling and filtering

  ###Make date column dates
  datos$date <- ymd(datos$date)
  ###Find last date in date and set 30 day reference date
  last_date <- max(datos$date,na.rm=T)
  cutoff_date <- last_date - days(30)
  ###Filter for 30 day cutoff
  datos <- get_last_30_days(datos,last_date)

#### Generate shared plots, tables and variables
  
  ###Choropleth Map
  #Table with average PM2.5 per locallity 
  # (we get daily average per locality for all sensors, then monthly for all days)
  promedio_mensual_localidad <- datos %>% 
    group_by(localidad, date) %>% 
    summarise(prom_dia = mean(PM25,na.rm = T)) %>%
    group_by(localidad) %>% 
    summarise(prom_30_dias = mean(prom_dia,na.rm=T)) %>% 
    mutate(localidad = toupper(localidad))
  
  #We paste previous table with map & convert it to sf table
  promedio_mensual_localidad <- merge(promedio_mensual_localidad,shapefile,by.x="localidad",by.y="LocNombre")
  map_df <- st_as_sf(promedio_mensual_localidad)
  #Set up palet for map colors
  pal <- colorBin("RdYlGn", domain = map_df$prom_30_dias, bins = 5,reverse = T)
  mapa1 <- leaflet(map_df,elementId = "MapaBogota")  %>% 
            addPolygons(fillOpacity = 0.7,color="black",
                        fillColor = ~pal(map_df$prom_30_dias),
                        weight=1,
                        highlight = highlightOptions(
                                      weight = 4,
                                      color = "#666",
                                      fillOpacity = 0.7),
                        label = ~paste(localidad, "-" ,round(prom_30_dias,2)),
                        labelOptions = labelOptions(
                                      textsize="20px" )) %>% 
            addControl("Polución Promedio - 30 Días", position = "bottomleft")

  
    
    ###Table for report: we keep only locality and average data
    tabla1 <- promedio_mensual_localidad %>% 
      select(localidad,prom_30_dias) %>% 
      mutate(prom_30_dias=round(prom_30_dias,2))%>%
      rename("Localidad"=localidad,"PM2.5"=prom_30_dias)
  
    ### Days above recommended threshold plot
    plot1 <- datos %>% 
    #Wrangling - we find average daily pollution and determine if a day was
      # above threshold
    group_by(localidad,date) %>% 
    summarise(polucion_promedio_dia = mean(PM25,na.rm=T)) %>%
    mutate(dia=as.Date(date,format="%Y-%m-%d")) %>% 
    arrange(dia) %>% 
    mutate(arriba=ifelse(polucion_promedio_dia>WHO_THRESHOLD,1,0)) %>%
    group_by(localidad) %>%
    mutate(dias_totales=sum(arriba,na.rm=T)) %>%
    mutate(localidad = paste0(localidad," (",dias_totales,")")) %>% 
    arrange(date)%>%
    ungroup() %>% 
    #Plot
    ggplot() + 
    geom_line(aes(x=dia,y=polucion_promedio_dia,group=localidad),alpha=0.8) +
    geom_hline(yintercept=DAILY_THRESHOLD,color="blue",linetype="dashed")+
    geom_point(aes(x=dia,y=polucion_promedio_dia,color=factor(arriba)),size=1,alpha=0.8)+
    geom_hline(yintercept=WHO_THRESHOLD,color="red",linetype="dashed")+
    labs(x="Día",y="Polución",title="Polución diaria promedio - PM2.5")+
    scale_color_manual(values=c("0"="#5ea45b","1"="red"))+
    theme_classic()+
    theme(legend.position = "none")+
    scale_x_date(labels = date_format("%m-%d"))+
    facet_wrap(~localidad,ncol=3)
    
    
#### END OF SHARED PLOTS AND TABLES    
###############################################################################
#### TABLES AND PLOTS PER LOCALLITY
  
LOCALIDADES <- datos$localidad %>% unique()

#For each locality, we create the tables and figures and then
#render the report
for(LOCALIDAD in LOCALIDADES){
  
  #We keep only rows from this locality
  datos.localidad <- datos %>% filter(localidad == LOCALIDAD)
  
  ### We find averages and days above threshold for all localities
  ### and rank them accordingly
  stats.localidad <- datos %>% 
    group_by(localidad, date) %>% 
    summarise(prom_dia = mean(PM25,na.rm=T), arriba=ifelse(prom_dia>WHO_THRESHOLD,1,0))%>%
    group_by(localidad) %>% 
    summarise(prom_30_dias = mean(prom_dia,na.rm=T),dias_totales=sum(arriba,na.rm = T)) %>%
    mutate(localidad = toupper(localidad)) %>%
    mutate(rank_prom = rank(-prom_30_dias,ties.method = "min"),rank_dia = rank(-dias_totales,ties.method = "min"))
  
  ### We get the relevant stats for THIS locality only (average pollution, days above and ranks)
  polucion.mes.localidad <- round(stats.localidad[stats.localidad["localidad"] == toupper(LOCALIDAD),]$prom_30_dias,2)
  dias.arriba.localidad <- stats.localidad[stats.localidad["localidad"] == toupper(LOCALIDAD),]$dias_totales
  rank_dias <- stats.localidad[stats.localidad["localidad"] == toupper(LOCALIDAD),]$rank_dia
  ### Labelling for text
  rank_dias <- ifelse(rank_dias == 8 | rank_dias > 10, paste0(rank_dias,"va"),paste0(rank_dias,"a"))
  rank_polucion <- stats.localidad[stats.localidad["localidad"] == toupper(LOCALIDAD),]$rank_prom
  ### Labelling for text
  rank_polucion <- ifelse(rank_polucion == 8 | rank_polucion > 10, paste0(rank_polucion,"va"),paste0(rank_polucion,"a"))
  
  ### For this locallity, we generate the "days above threshold" plot 
  plot.arriba.localidad <- datos.localidad %>% 
    #Wrangling
    group_by(date) %>% 
    summarise(polucion_promedio_dia = mean(PM25,na.rm=T)) %>%
    mutate(dia=as.Date(date,format="%Y-%m-%d")) %>% 
    arrange(dia) %>% 
    mutate(arriba=ifelse(polucion_promedio_dia>WHO_THRESHOLD,1,0)) %>%
    ggplot() + 
    geom_line(aes(x=dia,y=polucion_promedio_dia,group=1)) +
    geom_hline(yintercept=DAILY_THRESHOLD,color="blue",linetype="dashed")+
    geom_point(aes(x=dia,y=polucion_promedio_dia,color=factor(arriba)),size=2)+
    geom_hline(yintercept=WHO_THRESHOLD,color="red",linetype="dashed")+
    labs(x="Día",y="Polución",title="Polución diaria promedio - PM2.5")+
    scale_color_manual(values=c("0"="#5ea45b","1"="red"))+
    theme_classic()+
    scale_x_date(labels = date_format("%m-%d"))+
    ylim(0,NA)+
    theme(legend.position = "none")
  
  
  ### We get the number of schools from this locality
      ##TODO::This number should come from ground truth, not data file
  NUMESCUELAS <- datos.localidad %>% summarise(n_distinct(cdigosede)) %>% pull()
  
  ### "Days above plot" for each school
  plot.escuelas.localidad <- datos.localidad %>% 
    group_by(nombresede,date) %>%
    summarise(polucion_promedio_dia = mean(PM25,na.rm=T)) %>%
    mutate(dia=as.Date(date,format="%Y-%m-%d")) %>% 
    arrange(dia) %>% 
    mutate(arriba=ifelse(polucion_promedio_dia>WHO_THRESHOLD,1,0))%>%
    group_by(nombresede) %>% 
    mutate(dias_totales=sum(arriba,na.rm=T)) %>%
    mutate(nombresede = paste0(nombresede," (",dias_totales,")")) %>% 
    ungroup() %>% 
    #Plot
    ggplot() + 
    geom_line(aes(x=dia,y=polucion_promedio_dia,group=nombresede)) +
    geom_hline(yintercept=DAILY_THRESHOLD,color="blue",linetype="dashed")+
    geom_point(aes(x=dia,y=polucion_promedio_dia,color=factor(arriba)),size=1)+
    geom_hline(yintercept=WHO_THRESHOLD,color="red",linetype="dashed")+
    labs(x="Día",y="Polución promedio")+
    scale_color_manual(values=c("0"="#5ea45b","1"="red"))+
    theme_classic()+
    theme(legend.position = "none")+
    scale_x_date(labels = date_format("%m-%d"))+
    facet_wrap(~nombresede,ncol = 2)
  
  
  ### Map with schools as points with avg pollution
  #We filter map dataframe to only keep the locality polygon
  map_df.localidad <- as.data.frame(map_df) %>% filter(localidad==toupper(LOCALIDAD)) %>% st_as_sf()
  #We keep only schools in this locality
    ##TODO::This school list should come from gt file
  gt.escuelas <- gt %>% filter(tolower(localidad)==tolower(LOCALIDAD))
  escuelas.localidad <- coordenadas_escuela %>% filter(cdigosede %in% unique(gt.escuelas$cdigosede))
  
  ### We find number avg daily pollution and days above recommended per school
  escuela.dias.arriba <- datos.localidad %>% 
    group_by(date,nombresede) %>% 
    summarise(prom_dia = mean(PM25,na.rm=T)) %>% 
    mutate(arriba=ifelse(prom_dia>WHO_THRESHOLD,1,0))%>%
    group_by(nombresede) %>% 
    summarise(dias_totales=sum(arriba,na.rm=T))
  
  ### We join data with coordinate and polygon data
  escuelas.localidad <- datos.localidad %>% 
    group_by(nombresede,cdigosede) %>% s
  ummarise(prom_em = mean(PM25,na.rm=T)) %>% 
    merge(.,escuelas.localidad,by = "cdigosede") 
  
  ### Map for schools in locallity
  pal.escuelas <- colorBin("RdYlGn", domain = escuelas.localidad$prom_em, bins = 5,reverse = T)
  mapa.localidad <-  leaflet(map_df.localidad,elementId = "MapaLocalidad")  %>% 
    addPolygons(fillOpacity = 0.7,color="black",fill = T,fillColor = "white") %>% 
    addCircleMarkers(lng=escuelas.localidad$lon_c,
                     lat=escuelas.localidad$lat_c,
                     radius=8,
                     color=~pal.escuelas(escuelas.localidad$prom_em),stroke=F,fillOpacity = 0.6,
                     label = ~paste(escuelas.localidad$nombresede, "-" ,round(escuelas.localidad$prom_em,2)),
                     labelOptions = labelOptions(
                       textsize="20px"
                     ))
  
  ### We get table for map (avg. daily pollution and days above threshold)
  tabla.polucion.escuelas <- escuelas.localidad %>% 
    merge(escuela.dias.arriba,by="nombresede") %>% 
    select(nombresede,prom_em,dias_totales)%>% 
    mutate(prom_em=round(prom_em,2)) %>% 
    rename("Escuela"=nombresede,"PM2.5"=prom_em,"Dias arriba del límite"=dias_totales)
  
  ### Pollution inside vs outside of each school
  afuera.adentro <- datos.localidad %>% 
    group_by(nombresede,date,exterior,treatment) %>% 
    mutate(exterior=factor(exterior))%>% 
    summarise(polucion_promedio = mean(PM25,na.rm=T))
  
  ### We split between schools that have filters and those that don't
  afuera.adentro.filtro <- afuera.adentro %>% filter(treatment==3)
  afuera.adentro.control <- afuera.adentro %>% filter(treatment==2)
  
  ###SOME LOCALITIES DON'T HAVE SCHOOLS IN TREATMENT
  ### THESE IF STATEMENTS PREVENT THE CODE FROM CRASHING BY CHECKING
  ### THAT THERE IS SOMETHING TO PLOT/CALCULATE
  
  afuera.adentro.escuelas.filtro <- NULL
  
  if(nrow(afuera.adentro.filtro) !=0){
    ### Inside vs outside pollution graph for schools with filters
    afuera.adentro.escuelas.filtro <- ggplot(afuera.adentro.filtro) + 
      geom_line(aes(x=date,y=polucion_promedio,group=exterior,color=exterior)) +
      geom_point(aes(x=date,y=polucion_promedio,color=exterior))+
      geom_hline(yintercept=WHO_THRESHOLD,color="red",linetype="dashed")+
      geom_hline(yintercept=DAILY_THRESHOLD,color="blue",linetype="dashed")+
      labs(x="Día",y="Polución promedio")+
      theme_classic()+
      ylim(0,30)+
      labs(color="Monitor")+
      scale_color_discrete(labels=c("Adentro","Afuera"),type=c("1"="#5ea45b","0"="black"))+
      facet_wrap(~nombresede,ncol = 2) 
  }
  afuera.adentro.escuelas.control<- NULL
  if(nrow(afuera.adentro.control)!=0){
    ### Inside vs outside pollution graph for schools without filters
    afuera.adentro.escuelas.control <- ggplot(afuera.adentro.control) + geom_line(aes(x=date,y=polucion_promedio,group=exterior,color=exterior)) +
      geom_point(aes(x=date,y=polucion_promedio,color=exterior))+
      geom_hline(yintercept=15,color="red",linetype="dashed")+
      geom_hline(yintercept=5,color="blue",linetype="dashed")+
      labs(x="Día",y="Polución promedio")+
      theme_classic()+
      ylim(0,30)+
      labs(color="Monitor")+
      scale_color_discrete(labels=c("Adentro","Afuera"),type=c("1"="#5ea45b","0"="black"))+
      facet_wrap(~nombresede,ncol = 2) 
  }
  
  
  ### Bar graph comparing avg inside vs average outside pollution, by treatment status
  comparacion.filtro <- afuera.adentro %>% 
    group_by(exterior,treatment) %>% 
    summarise(PM25 = mean(polucion_promedio,na.rm=T)) %>%
    ggplot() + 
    geom_col(aes(x=exterior,y=PM25,fill=factor(treatment)),position="dodge2")+
    scale_x_discrete(labels=c("Adentro","Afuera"))+
    scale_fill_discrete(labels=c("Sin filtro","Con Filtro"),type=c("2"="black","3"="blue"))+
    labs(x="Monitor",fill="Filtro de aire",
         title="Polución promedio adentro y afuera de los salones",
         subtitle = "Comparación de escuelas con filtro y sin filtro")+
    theme_classic()
  
  
  ### Use of pollution sensors statistics
  ### We get the number of days in the dataset
  n_dias <- datos.localidad$date %>% unique() %>% length()
  ### We calculate the number of readings a healthy sensor should have:
    ### 24 readings a day (1/hour) times the number of days
    ### TODO:: Check if 24 readings is the standard
  n_lecturas <- 24*n_dias 
  
  ### We get the sensors ids and their location
  tabla.sensores <- datos.localidad %>% 
    select(sensor,nombresede,cdigosede,exterior) %>% 
    distinct() 
  
  #Pct of total readings: we calculate total number of readings in period
  ### and then divide by n_lecturas
  tabla.sensores <- datos.localidad %>% 
    group_by(sensor,date) %>% 
    ### Assumes faulty reading has NA instead of missing row
    summarise(medidas = sum(!is.na(PM25))) %>% 
    group_by(sensor) %>%  
    summarise(medidas=sum(medidas)) %>% 
    mutate(pct_lecturas=medidas/n_lecturas * 100) %>% 
    mutate(pct_lecturas = round(pct_lecturas,2))%>%
    merge(tabla.sensores,by="sensor")
  
  #Days with 0 readings
  ### We calculate the number of readings per day for each sensor
  ### Then count the number of days with 0 readings
  tabla.sensores <- datos.localidad %>% 
    group_by(date,sensor) %>% 
    summarise(medidas = sum(!is.na(PM25))) %>% 
    #We keep only faulty readers
    filter(medidas!=0) %>%
    group_by(sensor) %>% 
    summarise(dias.con.medidas = n()) %>%
    mutate(dias.sin.medidas = n_dias - dias.con.medidas) %>% 
    select(sensor, dias.sin.medidas) %>%
    merge(tabla.sensores,by="sensor",all.y=TRUE)
  
  #Status: 
  #Criterio
  #Amarillo: Lecturas en 50%-85% o 5-10 días sin lecturas
  #Rojo: Menos de 50% de lecturas o 10+ días sin lecturas
  
  tabla.sensores <- tabla.sensores %>% 
    #Checks which of the above conditions is met and assigns relevant emoji
    mutate(status = ifelse(dias.sin.medidas < 5 & pct_lecturas >85,emo::ji("white heavy check mark"),
                           ifelse(dias.sin.medidas < 10 & pct_lecturas > 50,emo::ji("large orange diamond"),
                            emo::ji("red_circle")) ))
  
  ##Pastes everything together and cleans up table for report
  tabla.sensores <- tabla.sensores %>% 
    select(nombresede,sensor,exterior,pct_lecturas,dias.sin.medidas,status) %>% 
    arrange(pct_lecturas,-dias.sin.medidas)%>%
    ### Cleans up sensor name with regex
    mutate(sensor=sub("^(?:[^_]+_){3}(.+)_.+$","\\1",sensor))%>%
    mutate(exterior=ifelse(exterior,"Sí","No"))%>%
    rename("Sensor"=sensor,"Sede"=nombresede,"Está en el exterior"=exterior,"Porcentaje de lecturas" = pct_lecturas,
           "Dias sin registro"=dias.sin.medidas,"Status"=status)
  
  
  
  ####Uso de filtros
  ### Finds average difference between inside and outside sensor for both
  ### treatment and control schools
  baselines <- datos.localidad %>% 
    group_by(nombresede,exterior,treatment,date) %>% 
    summarise(pol=mean(PM25,na.rm = T)) %>% 
    pivot_wider(id_cols =c(nombresede,treatment,date),names_from = exterior,values_from = pol)%>%
    rename("Adentro"=`0`,"Afuera"=`1`) %>% 
    mutate(dif = Afuera-Adentro) %>% 
    drop_na() %>% group_by(treatment)%>% 
    summarise(dif=mean(dif,na.rm=T))
  
  ### Gets the average control value. With this we'll decide if a filter
  ### school is using the air filter
  control <- baselines%>% filter(treatment==2) %>% pull(dif)
  
  ###Some pretty suspicious calculations
  ### For each treatment schools, calculates the average inside-outside difference
  ### Compares this measurement with control (using distance from) and assigns distance as weight
  ### The larger the difference between control and these (if difference is greater), 
  ### the higher the probability of sensor use
  ### Normalizes measurments to present as probability (not a true probabilty)
  tabla.probabilidad <- datos.localidad.filtros %>% 
    group_by(nombresede,exterior,treatment,date) %>% 
    summarise(pol=mean(PM25,na.rm=T)) %>% 
    pivot_wider(id_cols =c(nombresede,treatment,date),names_from = exterior,values_from = pol)%>%
    ##Finds average inside-outside difference for treatment schools
    rename("Adentro"=`0`,"Afuera"=`1`) %>% 
    mutate(dif = Afuera-Adentro) %>% drop_na() %>% 
    filter(treatment==3) %>% group_by(nombresede) %>% summarise(dif=mean(dif,na.rm=T))%>%
    ### Compares to control
    mutate(distance = ifelse(control > dif,1,abs(control-dif))) %>% 
    ### "Normalizes" from likeliest to least likely
    mutate(using=ifelse(n()!= 0,round(distance/max(distance),2),0)) %>% 
    ### Asigns status marker (emoji) based on this "probability"
    mutate(riesgo=ifelse(using<0.5,emo::ji("red_circle"),ifelse(using<0.7,emo::ji("large orange diamond"),emo::ji("white heavy check mark")))) %>% arrange(using) %>% select(nombresede,using,riesgo) %>%
    rename("Escuela"=nombresede,"Probabilidad de uso"=using,"Riesgo"=riesgo)
  
  ##Cleans locality name for report
  LOCALIDAD_LIMPIA <- tolower(gsub("ñ","n",gsub(" ","_",LOCALIDAD)))
  
  ## Generates report into Reports folder
  render("03_Reports/Reporte.Rmd",output_file=paste0(LOCALIDAD_LIMPIA,".html"))
  
}

  
