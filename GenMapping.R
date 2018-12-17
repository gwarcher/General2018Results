library(tidyverse)
library(leaflet)
library(scales)
library(htmltools)
library(RColorBrewer)
library(rgdal)
library(rmapshaper)


shp <- readOGR("shape/az_vtd_2018_new_pima.shp", layer="az_vtd_2018_new_pima")

roads <- readOGR("shape/tl_2013_04_prisecroads.shp", layer="tl_2013_04_prisecroads")

#shp <- ms_simplify(shp)
#names(shp) = NULL
#shp <- ms_simplify(shp)


prop127 <- AZ_Precinct_Results %>%
  filter(contestLongName == "Proposition 127") %>%
  spread(choiceName, precinctVotes)

write.csv(prop127, "prop127.csv", row.names = FALSE)



############## SOS   #################


sos <- AZ_Precinct_Results %>%
  filter(contestLongName == "Secretary of State") %>%
  select(contestLongName, choiceName, Pctcd, precinctVotes)%>%
  spread(choiceName, precinctVotes)

write.csv(sos, "Sos.csv", row.names = FALSE)


#############################




#################Governor############################


Governor <- AZ_Precinct_Results %>%
  filter(contestLongName == "Governor") %>%
  select(contestLongName, choiceName, Pctcd, precinctVotes)%>%
  spread(choiceName, precinctVotes) %>%
  mutate(total = rowSums(.[3:4])) %>%
  mutate(color = case_when(
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= .6 ~ "#660000",
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= .5 & (`Ducey, Doug` / total) - (`Garcia, David` /total) < .6 ~ "#881111",
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= .4 & (`Ducey, Doug` / total) - (`Garcia, David` /total) < .5 ~ "#aa2222",
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= .3 & (`Ducey, Doug` / total) - (`Garcia, David` /total) < .4 ~ "#cc3333",
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= .2 & (`Ducey, Doug` / total) - (`Garcia, David` /total) < .3 ~ "#eb4747",
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= .1 & (`Ducey, Doug` / total) - (`Garcia, David` /total) < .2 ~ "#f58181",
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= .05 & (`Ducey, Doug` / total) - (`Garcia, David` /total) < .1 ~ "#ffaaae",
    (`Ducey, Doug` / total) - (`Garcia, David` /total) >= 0 & (`Ducey, Doug` / total) - (`Garcia, David` /total) < .05 ~ "#ffd5d9",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= .6 ~ "#000066",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= .5 & (`Garcia, David` /total) - (`Ducey, Doug` / total)  < .6 ~ "#111188",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= .4 & (`Garcia, David` /total) - (`Ducey, Doug` / total)  < .5 ~ "#2222aa",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= .3 & (`Garcia, David` /total) - (`Ducey, Doug` / total)  < .4 ~ "#3333cc",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= .2 & (`Garcia, David` /total) - (`Ducey, Doug` / total)  < .3 ~ "#4747eb",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= .1 & (`Garcia, David` /total) - (`Ducey, Doug` / total)  < .2 ~ "#8181f5",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= .05 & (`Garcia, David` /total) - (`Ducey, Doug` / total)  < .1 ~ "#aaaeff",
    (`Garcia, David` /total) - (`Ducey, Doug` / total)  >= 0 & (`Garcia, David` /total) - (`Ducey, Doug` / total)  < .05 ~ "#d5d9ff"
  ))

write.csv(Governor, "governor.csv", row.names = FALSE)





pct <- shp
pct@data <- left_join(pct@data, Governor, by=c("pctnum" = "Pctcd"))


popupdata=pct

popupdata$label <-   popupdata$label <- paste0('<h4>', popupdata$precinctna, '</h4>',
                                               '<h5>', popupdata$pctnum, '</h5>',
                                               "County: ", popupdata$COUNTY, '</br>',
                                               "Congressional: ", popupdata$DistCD, '</br>',
                                               "Legislative: ", popupdata$DistLD, '</br>',
                                               "Total Votes: ", popupdata$total, '</br>',
                                               "Ducey: ", popupdata$`Ducey, Doug`, " (", percent(popupdata$`Ducey, Doug` / popupdata$total), ")", '</br>',
                                               "Garcia: ", popupdata$`Garcia, David`, " (" ,percent(popupdata$`Garcia, David` / popupdata$total), ")",'</br>', ")", '</br>') %>%
  lapply(HTML)

g <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(stroke = TRUE, weight = 2, color = popupdata$color, fillOpacity = 0.7, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
  addLegend("bottomright",
            colors = c("#660000", "#881111", "#aa2222", "#cc3333", "#eb4747", "#f58181", "#ffaaae", "#ffd5d9",
                       "#000066", "#111188", "#2222aa", "#3333cc", "#4747eb", "#8181f5", "#aaaeff", "#d5d9ff",
                       "DFDFDF"),
            labels = c("Ducey +60%", "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "Garcia + 60%",  "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "No Returns"),
            title = "Ducey v Garcia",
            opacity = 0.8)





####SENATE  ############




Senate <- AZ_Precinct_Results %>%
  filter(contestLongName == "U.S. Senator") %>%
  select(contestLongName, choiceName, Pctcd, precinctVotes)%>%
  spread(choiceName, precinctVotes) %>%
  mutate(total = rowSums(.[3:5])) %>%
  mutate(color = case_when(
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= .6 ~ "#660000",
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= .5 & (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) < .6 ~ "#881111",
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= .4 & (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) < .5 ~ "#aa2222",
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= .3 & (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) < .4 ~ "#cc3333",
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= .2 & (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) < .3 ~ "#eb4747",
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= .1 & (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) < .2 ~ "#f58181",
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= .05 & (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) < .1 ~ "#ffaaae",
    (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) >= 0 & (`McSally, Martha` / total) - (`Sinema, Kyrsten` /total) < .05 ~ "#ffd5d9",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= .6 ~ "#000066",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= .5 & (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  < .6 ~ "#111188",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= .4 & (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  < .5 ~ "#2222aa",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= .3 & (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  < .4 ~ "#3333cc",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= .2 & (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  < .3 ~ "#4747eb",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= .1 & (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  < .2 ~ "#8181f5",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= .05 & (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  < .1 ~ "#aaaeff",
     (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  >= 0 & (`Sinema, Kyrsten` /total) - (`McSally, Martha` / total)  < .05 ~ "#d5d9ff"
  ))

write.csv(Senate, "senate.csv", row.names = FALSE)





pct <- shp
pct@data <- left_join(pct@data, Senate, by=c("pctnum" = "Pctcd"))


popupdata=pct

popupdata$label <-   popupdata$label <- paste0('<h4>', popupdata$precinctna, '</h4>',
                                               '<h5>', popupdata$pctnum, '</h5>',
                                               "County: ", popupdata$COUNTY, '</br>',
                                               "Congressional: ", popupdata$DistCD, '</br>',
                                               "Legislative: ", popupdata$DistLD, '</br>',
                                               "Total Votes: ", popupdata$total, '</br>',
                                               "McSalley: ", popupdata$`McSally, Martha`, " (", percent(popupdata$`McSally, Martha` / popupdata$total), ")", '</br>',
                                               "Sinema: ", popupdata$`Sinema, Kyrsten`, " (" ,percent(popupdata$`Sinema, Kyrsten` / popupdata$total), ")",'</br>', ")", '</br>') %>%
  lapply(HTML)

s <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(stroke = TRUE, weight = 2, color = popupdata$color, fillOpacity = 0.7, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
  addLegend("bottomright",
            colors = c("#660000", "#881111", "#aa2222", "#cc3333", "#eb4747", "#f58181", "#ffaaae", "#ffd5d9",
                       "#000066", "#111188", "#2222aa", "#3333cc", "#4747eb", "#8181f5", "#aaaeff", "#d5d9ff",
                       "DFDFDF"),
            labels = c("McSally +60%", "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "Sinema + 60%",  "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "No Returns"),
            title = "Sinema v McSally",
            opacity = 0.8)




#### CORP COMM ####

CorpComm <- AZ_Precinct_Results %>%
  filter(contestLongName == "Corporation Commissioner") %>%
  select(contestLongName, choiceName, Pctcd, precinctVotes)%>%
  spread(choiceName, precinctVotes) %>%
  mutate(total = rowSums(.[3:6])) %>%
  mutate(color = case_when(
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= .6 ~ "#660000",
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= .5 & (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) < .6 ~ "#881111",
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= .4 & (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) < .5 ~ "#aa2222",
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= .3 & (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) < .4 ~ "#cc3333",
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= .2 & (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) < .3 ~ "#eb4747",
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= .1 & (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) < .2 ~ "#f58181",
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= .05 & (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) < .1 ~ "#ffaaae",
    (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) >= 0 & (`Glassman, Rodney` / total) - (`Kennedy, Sandra` / total) < .05 ~ "#ffd5d9",
    (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= .6 ~ "#000066",
   (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= .5 &(`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) < .6 ~ "#111188",
   (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= .4 &(`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) < .5 ~ "#2222aa",
   (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= .3 &(`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) < .4 ~ "#3333cc",
   (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= .2 &(`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) < .3 ~ "#4747eb",
   (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= .1 &(`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) < .2 ~ "#8181f5",
   (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= .05 &(`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) < .1 ~ "#aaaeff",
   (`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) >= 0 &(`Kennedy, Sandra` / total) - (`Glassman, Rodney` / total) < .05 ~ "#d5d9ff"
  ))


write.csv(CorpComm, "corpcomm.csv", row.names = FALSE)

pct <- shp
pct@data <- inner_join(pct@data, CorpComm, by=c("pctnum" = "Pctcd"))


cc <- leaflet(pct) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(stroke = TRUE, weight = 2, color = pct@data$color, fillOpacity = 0.5, smoothFactor = 0.5,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          )))





#######Ducey/Sinema#############
Ducey_Sinema <- AZ_Precinct_Results %>%
  filter(choiceName == "Ducey, Doug" | choiceName == "Sinema, Kyrsten") %>%
  select( choiceName, Pctcd, precinctVotes)%>%
  spread(choiceName, precinctVotes) %>%
  mutate(total = rowSums(.[2:3])) %>%
  left_join(., az_precincts, by="Pctcd") %>%
  mutate(color = case_when(
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= .6 ~ "#660000",
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= .5 & (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) < .6 ~ "#881111",
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= .4 & (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) < .5 ~ "#aa2222",
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= .3 & (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) < .4 ~ "#cc3333",
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= .2 & (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) < .3 ~ "#eb4747",
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= .1 & (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) < .2 ~ "#f58181",
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= .05 & (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) < .1 ~ "#ffaaae",
    (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) >= 0 & (`Ducey, Doug` / total) - (`Sinema, Kyrsten` /total) < .05 ~ "#ffd5d9",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= .6 ~ "#000066",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= .5 & (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  < .6 ~ "#111188",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= .4 & (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  < .5 ~ "#2222aa",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= .3 & (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  < .4 ~ "#3333cc",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= .2 & (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  < .3 ~ "#4747eb",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= .1 & (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  < .2 ~ "#8181f5",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= .05 & (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  < .1 ~ "#aaaeff",
    (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  >= 0 & (`Sinema, Kyrsten` /total) - (`Ducey, Doug` / total)  < .05 ~ "#d5d9ff"
  ))


pct <- shp
pct@data <- left_join(pct@data, Ducey_Sinema, by=c("pctnum" = "Pctcd"))


popupdata=pct

popupdata$label <-   popupdata$label <- paste0('<h4>', popupdata$precinctna, '</h4>',
                                               '<h5>', popupdata$pctnum, '</h5>',
                                               "County: ", popupdata$COUNTY, '</br>',
                                               "Congressional: ", popupdata$DistCD, '</br>',
                                               "Legislative: ", popupdata$DistLD, '</br>',
                                               "Total Votes: ", popupdata$total, '</br>',
                                               "Ducey: ", popupdata$`Ducey, Doug`, " (", percent(popupdata$`Ducey, Doug` / popupdata$total), ")", '</br>',
                                               "Sinema: ", popupdata$`Sinema, Kyrsten`, " (" ,percent(popupdata$`Sinema, Kyrsten` / popupdata$total), ")",'</br>') %>%
  lapply(HTML)

ds <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  
  addPolygons(stroke = TRUE, weight = 2, color = popupdata$color, fillOpacity = 1, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
  addPolylines(data=roads, weight = 3, color = "black") %>%
  addLegend("bottomright",
            colors = c("#660000", "#881111", "#aa2222", "#cc3333", "#eb4747", "#f58181", "#ffaaae", "#ffd5d9",
                       "#000066", "#111188", "#2222aa", "#3333cc", "#4747eb", "#8181f5", "#aaaeff", "#d5d9ff",
                       "DFDFDF"),
            labels = c("Ducey +60%", "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "Sinema + 60%",  "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "No Returns"),
            title = "Ducey v Sinema",
            opacity = 0.8)

write.csv(Ducey_Sinema, "ducey_sinema.csv", row.names = FALSE)





####################Olson Glassman ####################################

Olson_Glassman <- AZ_Precinct_Results %>%
  filter(choiceName == "Olson, Justin" | choiceName == "Glassman, Rodney") %>%
  select( choiceName, Pctcd, precinctVotes)%>%
  spread(choiceName, precinctVotes) %>%
  mutate(total = rowSums(.[2:3])) %>%
  left_join(., az_precincts, by="Pctcd") %>%
  mutate(color = case_when(
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= .6 ~ "#660000",
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= .5 & (`Glassman, Rodney` / total) - (`Olson, Justin` /total) < .6 ~ "#881111",
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= .4 & (`Glassman, Rodney` / total) - (`Olson, Justin` /total) < .5 ~ "#aa2222",
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= .3 & (`Glassman, Rodney` / total) - (`Olson, Justin` /total) < .4 ~ "#cc3333",
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= .2 & (`Glassman, Rodney` / total) - (`Olson, Justin` /total) < .3 ~ "#eb4747",
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= .1 & (`Glassman, Rodney` / total) - (`Olson, Justin` /total) < .2 ~ "#f58181",
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= .05 & (`Glassman, Rodney` / total) - (`Olson, Justin` /total) < .1 ~ "#ffaaae",
    (`Glassman, Rodney` / total) - (`Olson, Justin` /total) >= 0 & (`Glassman, Rodney` / total) - (`Olson, Justin` /total) < .05 ~ "#ffd5d9",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= .6 ~ "#000066",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= .5 & (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  < .6 ~ "#111188",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= .4 & (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  < .5 ~ "#2222aa",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= .3 & (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  < .4 ~ "#3333cc",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= .2 & (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  < .3 ~ "#4747eb",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= .1 & (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  < .2 ~ "#8181f5",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= .05 & (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  < .1 ~ "#aaaeff",
    (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  >= 0 & (`Olson, Justin` /total) - (`Glassman, Rodney` / total)  < .05 ~ "#d5d9ff"
  ))


pct <- shp
pct@data <- left_join(pct@data, Olson_Glassman, by=c("pctnum" = "Pctcd"))


popupdata=pct

popupdata$label <-   popupdata$label <- paste0('<h4>', popupdata$precinctna, '</h4>',
                                               '<h5>', popupdata$pctnum, '</h5>',
                                               "County: ", popupdata$COUNTY, '</br>',
                                               "Congressional: ", popupdata$DistCD, '</br>',
                                               "Legislative: ", popupdata$DistLD, '</br>',
                                               "Total Votes: ", popupdata$total, '</br>',
                                               "Glassman: ", popupdata$`Glassman, Rodney`, " (", percent(popupdata$`Glassman, Rodney` / popupdata$total), ")", '</br>',
                                               "Olson: ", popupdata$`Olson, Justin`, " (" ,percent(popupdata$`Olson, Justin` / popupdata$total), ")",'</br>') %>%
  lapply(HTML)

og <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  
  addPolygons(stroke = TRUE, weight = 2, color = "black", fillColor = popupdata$color, fillOpacity = 1, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
  addPolylines(data=roads, weight = 3, color = "green") %>%
  addLegend("bottomright",
            colors = c("#660000", "#881111", "#aa2222", "#cc3333", "#eb4747", "#f58181", "#ffaaae", "#ffd5d9",
                       "#000066", "#111188", "#2222aa", "#3333cc", "#4747eb", "#8181f5", "#aaaeff", "#d5d9ff",
                       "DFDFDF"),
            labels = c("Glassman +60%", "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "Olson + 60%",  "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "No Returns"),
            title = "Glassman v Olson",
            opacity = 0.8)

write.csv(Ducey_Sinema, "ducey_sinema.csv", row.names = FALSE)




#################################Statistical Maps ##########################################


#############DEM Standard Dev #####################################

dem_sd <- AZ_Precinct_Results %>%
  filter(districtName != 'Arizona Supreme Court' & districtName != 'Court of Appeals Division II (Pima)'
         & districtName != 'Court of Appeals Division I (Maricopa)' & districtName != 'Ballot Measure Statewide') %>%
  select(Pctcd, contestLongName, partyShortName, precinctVotes) %>%
  mutate(contestLongName = ifelse(substr(contestLongName,0, 20) == "State Representative", "State Rep", contestLongName),
         contestLongName = ifelse(substr(contestLongName,0, 13 ) == "State Senator", "State Senator", contestLongName),
         contestLongName = ifelse(substr(contestLongName, 0, 31) == "U.S. Representative in Congress", "Congress", contestLongName)) %>%
  filter(contestLongName != 'State Rep' & contestLongName != 'Corporation Commissioner') %>%
  unite(.,contestLongName, partyShortName, col="contestParty", sep = "_") %>%
  spread(.,contestParty, precinctVotes) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(dem_gov = Governor_DEM / rowSums(.[7:9]),
         dem_sen = `U.S. Senator_DEM` / rowSums(.[23:25]),
         dem_ag = `Attorney General_DEM` / rowSums(.[2:3]),
         dem_cong = Congress_DEM / rowSums(.[4:6]),
         dem_sos = `Secretary of State_DEM` / rowSums(.[10:11]),
         dem_sm = `State Mine Inspector_DEM` / rowSums(.[12:13]),
         dem_st_sen = `State Senator_DEM` / rowSums(.[14:18]),
         dem_suped = `Superintendent of Public Instruction_DEM` / rowSums(.[21:22]),
         dem_treas = `State Treasurer_DEM` / rowSums(.[19:20])) %>%
  mutate(dem_st_sen = ifelse(dem_st_sen >= 1, NA, dem_st_sen),
         dem_st_sen = ifelse(dem_st_sen <= 0, NA, dem_st_sen)) %>%
  #mutate(dem_stdev = rowSds(., rows = c(26:34), na.rm = TRUE))
  mutate(sd_dem = apply(.[26:34], 1, sd, na.rm = TRUE)) %>%
  mutate(sd_mean = apply(.[26:34], 1, mean, na.rm = TRUE)) %>%
  mutate(gov_diff = dem_gov - sd_mean,
         ussen_diff = dem_sen - sd_mean) %>%
  left_join(., az_precincts, by="Pctcd") %>%
  select(Pctcd, County, DistCD, DistLD, dem_gov, dem_sen, dem_ag, dem_cong, dem_sos, dem_sm, dem_st_sen, dem_suped, dem_treas, sd_dem, sd_mean, gov_diff, ussen_diff)




pct <- shp
pct@data <- left_join(pct@data, dem_sd, by=c("pctnum" = "Pctcd"))


popupdata=pct

popupdata$label <-   popupdata$label <- paste0('<h4>', popupdata$precinctna, '</h4>',
                                               '<h5>', popupdata$pctnum, '</h5>',
                                               "County: ", popupdata$County, '</br>',
                                               "Congressional: ", popupdata$DistCD, '</br>',
                                               "Legislative: ", popupdata$DistLD, '</br>',
                                               "Mean: ", popupdata$sd_mean, '</br>',
                                               "Dem Gov: ", popupdata$gov_diff, '</br>',
                                               "Dem Senate: ", popupdata$ussen_diff,'</br>') %>%
  lapply(HTML)

bins <- c(-.5, -.1, -.05, -.025, 0, .025, .05, .1, .5)

#pal <- colorBin("blue2red", domain = popupdata$gov_diff, bins = bins)
pal <- colorBin("RdBu", domain=popupdata$gov_diff, bins = bins)
#binpal <- colorBin("RdBu", popupdata$gov_diff, 9)

dem_gov_mean <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(stroke = TRUE, weight = 2, color = "black",  fillColor = ~pal(gov_diff), fillOpacity = .8, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
 # addPolylines(data=roads, weight = 3, color = "green") %>%
  addLegend("bottomright",
            pal = pal,
            values = ~gov_diff,
            title = "Diff from Mean Dem - Gov",
            opacity = 0.8)


##############################################################
bins <- c(-.5, -.1, -.05, -.025, 0, .025, .05, .1, .5)

#pal <- colorBin("blue2red", domain = popupdata$gov_diff, bins = bins)
pal <- colorBin("RdBu", domain=popupdata$ussen_diff, bins = bins)
#binpal <- colorBin("RdBu", popupdata$gov_diff, 9)

dem_ussen_mean <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(stroke = TRUE, weight = 2, color = "black", fillColor = ~pal(ussen_diff), fillOpacity = .8, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
  #addPolylines(data=roads, weight = 3, color = "green") %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ussen_diff,
            title = "Diff from Mean Dem - Sen",
            opacity = 0.8)



write.csv(Ducey_Sinema, "ducey_sinema.csv", row.names = FALSE)






#####################prop 127######################################

prop127 <- AZ_Precinct_Results %>%
  filter(contestLongName == "Proposition 127") %>%
  select( choiceName, Pctcd, precinctVotes)%>%
  spread(choiceName, precinctVotes) %>%
  mutate(total = rowSums(.[2:3])) %>%
  left_join(., az_precincts, by="Pctcd") %>%
  mutate(color = case_when(
    (No / total) - (Yes /total) >= .6 ~ "#660000",
    (No / total) - (Yes /total) >= .5 & (No / total) - (Yes /total) < .6 ~ "#881111",
    (No / total) - (Yes /total) >= .4 & (No / total) - (Yes /total) < .5 ~ "#aa2222",
    (No / total) - (Yes /total) >= .3 & (No / total) - (Yes /total) < .4 ~ "#cc3333",
    (No / total) - (Yes /total) >= .2 & (No / total) - (Yes /total) < .3 ~ "#eb4747",
    (No / total) - (Yes /total) >= .1 & (No / total) - (Yes /total) < .2 ~ "#f58181",
    (No / total) - (Yes /total) >= .05 & (No / total) - (Yes /total) < .1 ~ "#ffaaae",
    (No / total) - (Yes /total) >= 0 & (No / total) - (Yes /total) < .05 ~ "#ffd5d9",
    (Yes /total) - (No / total)  >= .6 ~ "#000066",
    (Yes /total) - (No / total)  >= .5 & (Yes /total) - (No / total)  < .6 ~ "#111188",
    (Yes /total) - (No / total)  >= .4 & (Yes /total) - (No / total)  < .5 ~ "#2222aa",
    (Yes /total) - (No / total)  >= .3 & (Yes /total) - (No / total)  < .4 ~ "#3333cc",
    (Yes /total) - (No / total)  >= .2 & (Yes /total) - (No / total)  < .3 ~ "#4747eb",
    (Yes /total) - (No / total)  >= .1 & (Yes /total) - (No / total)  < .2 ~ "#8181f5",
    (Yes /total) - (No / total)  >= .05 & (Yes /total) - (No / total)  < .1 ~ "#aaaeff",
    (Yes /total) - (No / total)  >= 0 & (Yes /total) - (No / total)  < .05 ~ "#d5d9ff"
  ))


pct <- shp
pct@data <- left_join(pct@data, prop127, by=c("pctnum" = "Pctcd"))


popupdata=pct

popupdata$label <-   popupdata$label <- paste0('<h4>', popupdata$precinctna, '</h4>',
                                               '<h5>', popupdata$pctnum, '</h5>',
                                               "County: ", popupdata$COUNTY, '</br>',
                                               "Congressional: ", popupdata$DistCD, '</br>',
                                               "Legislative: ", popupdata$DistLD, '</br>',
                                               "Total Votes: ", popupdata$total, '</br>',
                                               "No on 127: ", popupdata$No, " (", percent(popupdata$No / popupdata$total), ")", '</br>',
                                               "Yes on 127: ", popupdata$Yes, " (" ,percent(popupdata$Yes / popupdata$total), ")",'</br>') %>%
  lapply(HTML)

p127 <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  
  addPolygons(stroke = TRUE, weight = 2, color = popupdata$color, fillOpacity = 1, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
  addPolylines(data=roads, weight = 3, color = "black") %>%
  addLegend("bottomright",
            colors = c("#660000", "#881111", "#aa2222", "#cc3333", "#eb4747", "#f58181", "#ffaaae", "#ffd5d9",
                       "#000066", "#111188", "#2222aa", "#3333cc", "#4747eb", "#8181f5", "#aaaeff", "#d5d9ff",
                       "DFDFDF"),
            labels = c("No +60%", "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "Yes + 60%",  "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "No Votes"),
            title = "Proposition 127",
            opacity = 0.8)

  




#######Precincts Sinema and DOnald won   ####################################

Senate_Potus <- Senate %>%
  left_join(., POTUS_2018, by = "Pctcd") %>%
  filter(`Sinema, Kyrsten` >= `McSally, Martha` & Trump >= Clweighton) %>%
  mutate(Sinema_total = `Sinema, Kyrsten` / total,
         Trump_total = Trump / (Trump + Clweighton + Other),
         Diff = Sinema_total - Trump_total) 



pct <- shp
pct@data <- left_join(pct@data, Senate_Potus, by=c("pctnum" = "Pctcd"))


popupdata=pct

popupdata$label <-   popupdata$label <- paste0('<h4>', popupdata$precinctna, '</h4>',
                                               '<h5>', popupdata$pctnum, '</h5>',
                                               "County: ", popupdata$COUNTY, '</br>',
                                               "Congressional: ", popupdata$DistCD, '</br>',
                                               "Legislative: ", popupdata$DistLD, '</br>',
                                               "Total Votes: ", popupdata$total, '</br>',
                                               "McSally: ", popupdata$`McSally, Martha`, " (", percent(popupdata$`McSally, Martha` / popupdata$total), ")", '</br>',
                                               "Sinema: ", popupdata$`Sinema, Kyrsten`, " (" ,percent(popupdata$`Sinema, Kyrsten` / popupdata$total), ")",'</br>', ")", '</br>') %>%
  lapply(HTML)

s <- leaflet(popupdata) %>%
  setView(lng= -111.093735, lat = 34.048927, zoom = 7) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(stroke = TRUE, weight = 2, color = popupdata$color, fillOpacity = 0.7, smoothFactor = 0.5, label=popupdata$label,  
              labelOptions = labelOptions(opacity = 0.85,
                                          style = list(
                                            "font-size" = "12px"
                                          ))) %>%
  addLegend("bottomright",
            colors = c("#660000", "#881111", "#aa2222", "#cc3333", "#eb4747", "#f58181", "#ffaaae", "#ffd5d9",
                       "#000066", "#111188", "#2222aa", "#3333cc", "#4747eb", "#8181f5", "#aaaeff", "#d5d9ff",
                       "DFDFDF"),
            labels = c("McSally +60%", "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "Sinema + 60%",  "+50%", "+40%", "+30%", "+20%", "+10%", "+5%", "+0%",
                       "No Returns"),
            title = "Sinema v McSally",
            opacity = 0.8)