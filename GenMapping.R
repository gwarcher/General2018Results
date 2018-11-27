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
  