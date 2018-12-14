library(readr)
library(tidyverse)





Results2016_Normalized <- read_csv("C:/Users/garcher/Documents/R/Projects/ElectionResults_Normalization/Results2016_Normalized.csv")


census_weight <- read_csv("C:/Users/garcher/Documents/R/Projects/CensusPrecinctMapping/Data/census_weight.csv")

pct_16_18_weight <- read_csv("C:/Users/garcher/Documents/R/Projects/CensusPrecinctMapping/Data/pct_16_18_weight_new.csv")

pct_16_18_weight <- pct_16_18_weight %>%
  filter(weight >= .001)



eight), 0),
            Clweighton = round(sum(POTUS_DEM_1 * weight), 0 ),
            Other =#Census Tables###############################

cb_education <- read_csv("C:/Users/garcher/Documents/R/Projects/CensusLoading/census tables/education.csv")

cb_hh_income <- read_csv("C:/Users/garcher/Documents/R/Projects/CensusLoading/census tables/hh_income.csv")

cb_home_value <- read_csv("C:/Users/garcher/Documents/R/Projects/CensusLoading/census tables/home_value.csv")

cb_population_hispanic <- read_csv("C:/Users/garcher/Documents/R/Projects/CensusLoading/census tables/population_hispanic.csv")

cb_pop_race <- read_csv("C:/Users/garcher/Documents/R/Projects/CensusLoading/census tables/pop_race.csv")



POTUS_2018 <- az_precincts %>%
  left_join(., pct_16_18_weight, by = c("Pctcd" = "pctnum_18") ) %>%
  left_join(., Results2016_Normalized, by=c("pctnum_16" = "PrecinctID")) %>%
  group_by(Pctcd) %>%
  summarize(Trump = round(sum(POTUS_REP_1 * w round(sum(POTUS_OTH * weight), 0))







pct_pop <- census_weight %>%
  inner_join(.,cb_pop_race, by = "GEOID") %>%
  group_by(pctnum) %>%
  summarise(Total = round(sum(Total* weight),0))




pct_education <- census_weight %>%
  inner_join(., cb_education, by = "GEOID") %>%
  group_by(pctnum) %>%
  summarise(Total = round(sum(Total * weight),0),
            No_Schooling = round(sum(No_Schooling * weight),0),
            Some_HS = round(sum(Some_HS * weight),0),
            High_School = round(sum(High_School* weight),0),
            Some_College = round(sum(Some_College * weight),0),
            College_Grad = round(sum(College_Grad * weight),0),
            PostGrad = round(sum(PostGrad * weight),0)) %>%
  mutate(pct_No_Schooling = No_Schooling / Total,
         pct_Some_HS = Some_HS / Total,
         pct_High_School = High_School / Total,
         pct_Some_College = Some_College / Total,
         pct_College_Grad = College_Grad / Total,
         pct_PostGrad = PostGrad / Total)


pct_hispanic <- census_weight %>%
  inner_join(., cb_population_hispanic, by = "GEOID") %>%
  group_by(pctnum) %>%
  summarise(Total_pop = round(sum(Total * weight),0),
    Hispanic = round(sum(Hispanic * weight),0)) %>%
  mutate(pct_hispanic = Hispanic / Total_pop)

pct_hh_income <- census_weight %>%
  inner_join(., cb_hh_income, by = "GEOID") %>%
  group_by(pctnum) %>%
  summarise(Total_Working = round(sum(Total_Working * weight), 0),
            Under_35k =  round(sum(Under_35k * weight), 0),
            `35k_50k` = round(sum(`35k_50k` * weight),0), 
            `50k_99k` = round(sum(`50k_99k` * weight), 0),
            `100k_200k` = round(sum(`100k_200k` * weight), 0),
            Over_200k = round(sum(Over_200k * weight), 0)) %>%
  mutate(pct_under_35k = Under_35k / Total_Working,
         pct_35_50k = `35k_50k`/ Total_Working,
         pct_50_99k = `50k_99k` / Total_Working,
         pct_100_200k = `100k_200k` / Total_Working,
         pct_Over_200k = Over_200k / Total_Working)


pct_pop_race <- census_weight %>%
  inner_join(., cb_pop_race, by = "GEOID") %>%
  group_by(pctnum) %>%
  summarise(Total = round(sum(Total * weight), 0),
            White =  round(sum(White * weight), 0),
            Black = round(sum(Black * weight),0), 
            Native = round(sum(Native_Am * weight), 0),
            Other_and_2_or_More = round(sum(Other * weight), 0) + round(sum(Asian * weight), 0) + round(sum(Polynesian * weight), 0) + round(sum(Two_Or_More * weight), 0)) %>%
  mutate(pct_White = White / Total,
         pct_Black = Black / Total,
         pct_Native = Native / Total,
         pct_Other = Other_and_2_or_More / Total)
  






Census_Results_Aggregate <- Senate %>%
  left_join(., POTUS_2018, by = "Pctcd") %>%
  left_join(., pct_hispanic, by = c("Pctcd" = "pctnum")) %>%
  left_join(., pct_education, by = c("Pctcd" = "pctnum")) %>%
  left_join(., pct_hh_income, by = c("Pctcd" = "pctnum")) %>%
  left_join(., pct_pop_race, by = c("Pctcd" = "pctnum")) %>%
  mutate(pct_CollegeGrad = pct_College_Grad + pct_PostGrad,
         pct_Less_College = pct_No_Schooling + pct_Some_HS + pct_High_School + pct_Some_College,
         pct_hh_under_50k = pct_under_35k + pct_35_50k,
         pct_hh_Over_100k = pct_100_200k + pct_Over_200k,
         pct_Clinton = Clinton / (Trump + Clinton + Other),
         pct_Trump = Trump / (Clinton + Trump + Other),
         pct_Sinema = `Sinema, Kyrsten`/ (`Sinema, Kyrsten` + `McSally, Martha` + `Green, Angela`), 
         pct_McSally =  `McSally, Martha`/ (`Sinema, Kyrsten` + `McSally, Martha` + `Green, Angela`)) %>%
  #mutate_all(funs(replace., is.na(.), 0)) %>%
  column_to_rownames(., var = "Pctcd") %>%

  select( pct_Trump,pct_Clinton, pct_Sinema, pct_McSally,   pct_CollegeGrad, pct_Less_College, pct_hh_under_50k, pct_50_99k, pct_hh_Over_100k, pct_White, pct_Native, pct_Other, pct_hispanic)

write.csv(Census_Results_Aggregate, "data/census_agg.csv")

test <- Senate_Potus %>%
  left_join(., pct_hispanic, by = c("Pctcd" = "pctnum")) %>%
  left_join(., pct_education, by = c("Pctcd" = "pctnum")) %>%
  left_join(., pct_hh_income, by = c("Pctcd" = "pctnum")) %>%
  left_join(., pct_pop_race, by = c("Pctcd" = "pctnum")) %>%
  mutate(pct_CollegeGrad = pct_College_Grad + pct_PostGrad,
         pct_Less_College = pct_No_Schooling + pct_Some_HS + pct_High_School + pct_Some_College,
         pct_hh_under_50k = pct_under_35k + pct_35_50k,
         pct_hh_Over_100k = pct_100_200k + pct_Over_200k,
         pct_Clinton = Clinton / (Trump + Clinton + Other),
         pct_Trump = Trump / (Clinton + Trump + Other),
         pct_Sinema = `Sinema, Kyrsten`/ (`Sinema, Kyrsten` + `McSally, Martha` + `Green, Angela`), 
         pct_McSally =  `McSally, Martha`/ (`Sinema, Kyrsten` + `McSally, Martha` + `Green, Angela`)) %>%
  #mutate_all(funs(replace., is.na(.), 0)) %>%
  column_to_rownames(., var = "Pctcd") %>%
  select( pct_Trump,pct_Clinton, pct_Sinema, pct_McSally, pct_CollegeGrad, pct_Less_College, pct_hh_under_50k, pct_50_99k, pct_hh_Over_100k, pct_White, pct_Native, pct_Other, pct_hispanic)



az <- cor(Census_Results_Aggregate, use = "complete.obs")


t <- cor(test, use = "complete.obs")
round(t, 2)


###ScatterPlots
###white and hispanic
ggplot(Census_Results_Aggregate, aes(Census_Results_Aggregate$pct_White, Census_Results_Aggregate$pct_hispanic)) + geom_point() + geom_smooth(se=FALSE, method=lm)

ggplot(test, aes(test$pct_White, test$pct_hispanic)) + geom_point() + geom_smooth(se=FALSE, method=lm)


###hispanic and college grad
ggplot(Census_Results_Aggregate, aes(Census_Results_Aggregate$pct_hispanic, Census_Results_Aggregate$pct_CollegeGrad)) + geom_point() + geom_smooth(se=FALSE, method=lm)

ggplot(test, aes(test$pct_hispanic, test$pct_CollegeGrad)) + geom_point() + geom_smooth(se=FALSE, method=lm)


####white and college grad #############
ggplot(Census_Results_Aggregate, aes(Census_Results_Aggregate$pct_White, Census_Results_Aggregate$pct_CollegeGrad)) + geom_point() + geom_smooth(se=FALSE, method=lm)

ggplot(test, aes(test$pct_White, test$pct_CollegeGrad)) + geom_point() + geom_smooth(se=FALSE, method=lm)





##hispanic and 100k
ggplot(Census_Results_Aggregate, aes(Census_Results_Aggregate$pct_hispanic, Census_Results_Aggregate$pct_hh_Over_100k)) + geom_point() + geom_smooth(se=FALSE, method=lm)

ggplot(test, aes(test$pct_hispanic, test$pct_hh_Over_100k)) + geom_point() + geom_smooth(se=FALSE, method=lm)


###Other and College Grad########
ggplot(Census_Results_Aggregate, aes(Census_Results_Aggregate$pct_Other, Census_Results_Aggregate$pct_CollegeGrad)) + geom_point() + geom_smooth(se=FALSE, method=lm)

ggplot(test, aes(test$pct_Other, test$pct_CollegeGrad)) + geom_point() + geom_smooth(se=FALSE, method=lm)






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