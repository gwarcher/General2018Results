library(DBI)
library(odbc)

library(tidyverse)


#Maricopa drop time and file




con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};server=sql02;\ndatabase=ElectionManagement;\nTrusted_Connection=True;")



results_1119 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6987, @IsState = 1")



results_1117 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6984, @IsState = 1")


results_1116 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6980, @IsState = 1")

results_1115 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6978, @IsState = 1")

results_1114 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6956, @IsState = 1")

results_1113 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6950, @IsState = 1")

results_1112 <-  dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6935, @IsState = 1")

results_1111 <-  dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6930, @IsState = 1")

results_1110 <-  dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6922, @IsState = 1")

results_1109 <-  dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6908, @IsState = 1")

results_1108 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6891, @IsState = 1")

results_1106 <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6877, @IsState = 1")

results_1106_eb <- dbGetQuery(con, "Exec enr.Export_GetSummaryExport_GA @UploadFileLogId = 6797, @IsState = 1")

##################MARICOPA#########################


Maricopa_1107 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail741.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)

Maricopa_1109 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1109.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)



Maricopa_1111 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1111.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Maricopa_1112 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1112.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Maricopa_1113 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1113.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Maricopa_1114 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1114_SOS.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Maricopa_1115 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1115_SOS.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Maricopa_1116 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1116_SOS.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)


Maricopa_1117 <- read_delim("//ad.local/system/Users/SOS/garcher/Desktop/Maricopa Detail/PrecinctDetail1117_SOS.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)


precinctNames <- read_csv("C:/Users/garcher/Documents/R/Projects/ENRCleaning/precinctNames_.csv")

pct_id <- read_csv("C:/Users/garcher/Documents/R/Projects/ENRCleaning/pct_id.csv")

####################### FULL PRECINCT BY PRECINCT ###############################################

library(readxl)
 Results_Precincts_AZ <- read_excel("~/PrecinctByPrecincts.xlsx", 
                                          sheet = "Sheet1")

 Results_Contests <- read_excel("~/PrecinctByPrecincts.xlsx", 
                                sheet = "Sheet2")
 
 
 
 
 AZ_Precinct_Results <- Results_Contests %>%
   filter(totalVotes != 0) %>%
   select(choiceKey, contestLongName, districtName, choiceName, partyShortName ) %>%
   inner_join(., Results_Precincts_AZ, by="choiceKey") %>%
   select(contestLongName, districtName, choiceName, partyShortName, precinctId, precinctName, precinctVotes, choicePollingPlaceVotes, choiceEarlyVotes, choiceProvisionalVotes, choiceMaricopaVotes) %>%
   inner_join(pct_id, by="precinctId") %>%
   distinct(.)

test<- AZ_Precinct_Results %>%
  select(Pctcd, contestLongName, choiceName,  precinctVotes) %>%
  filter(contestLongName == "U.S. Senator") %>%
  spread(choiceName, precinctVotes)


# 
# write.csv(results_1106_eb, "PRR/results_1106_8pm.csv", row.names = FALSE)
# 
# write.csv(results_1106, "PRR/results_1106_lastEnight.csv", row.names = FALSE)
# 
# write.csv(results_1108, "PRR/results_1108.csv", row.names = FALSE)
# 
# write.csv(results_1109, "PRR/results_1109.csv", row.names = FALSE)
# 
# write.csv(results_1110, "PRR/results_1110.csv", row.names = FALSE)
# 
# write.csv(results_1111, "PRR/results_1111.csv", row.names = FALSE)
# 
# write.csv(results_1112, "PRR/results_1112.csv", row.names = FALSE)
# 
# write.csv(results_1113, "PRR/results_1113.csv", row.names = FALSE)
# 
# write.csv(results_1114, "PRR/results_1114.csv", row.names = FALSE)
# 


r1106=results_1106[,21]
r1108=results_1108[,21]
r1109=results_1109[,21]
r1110=results_1110[,21]
r1111=results_1111[,21]
r1112=results_1112[,21]
r1113=results_1113[,21]
r1114=results_1114[,21]
r1115=results_1115[,21]
r1116=results_1116[,21]
r1117=results_1117[,21]
r1119=results_1119[,21]

  
results_cume <- results_1106_eb %>%
  select(contestKey, contestLongName, districtName, jurisdictionName, choiceKey, choiceName, partyShortName, r1106_eb = totalVotes) %>%
  cbind(.,r1106) %>%
  cbind(.,r1108) %>%
  cbind(.,r1109) %>%
  cbind(.,r1110) %>%
  cbind(.,r1111) %>%
  cbind(.,r1112) %>%
  cbind(.,r1113) %>%
  cbind(.,r1114) %>%
  cbind(.,r1115) %>%
  cbind(.,r1116) %>%
  cbind(.,r1117) %>%
  cbind(.,r1119) %>%
  filter(r1106_eb != 0 & r1106 != 0 & r1108 != 0 & r1109 != 0 & r1110 != 0 & r1111 != 0 & r1112 != 0 & r1113 != 0 & r1114 != 0 & r1115 != 0 & r1116 != 0 & r1117 != 0)


write.csv(results_cume, "results_cume.csv", row.names = FALSE)


results_daily <- results_cume %>%
  mutate(r1106_d = r1106 - r1106_eb) %>%
  mutate(r1108_d = r1108 - r1106) %>%
  mutate(r1109_d = r1109 - r1108) %>%
  mutate(r1110_d = r1110 - r1109) %>%
  mutate(r1111_d = r1111 - r1110) %>%
  mutate(r1112_d = r1112 - r1111) %>%
  mutate(r1113_d = r1113 - r1112) %>%
  mutate(r1114_d = r1114 - r1113) %>%
  mutate(r1115_d = r1115 - r1114) %>%
  mutate(r1116_d = r1116 - r1115) %>%
  mutate(r1117_d = r1117 - r1116) %>%
  mutate(r1119_d = r1119 - r1117) %>%
  select(contestKey, contestLongName, districtName, jurisdictionName, choiceKey, choiceName, partyShortName, r1106_eb,
         r1106 = r1106_d, r1108 = r1108_d, r1109 = r1109_d, r1110 = r1110_d, r1111 = r1111_d, r1112 = r1112_d,
         r1113 = r1113_d, r1114 = r1114_d, r1115 = r1115_d, r1116 = r1116_d, r1117 = r1117_d, r1119 = r1119_d)



write.csv(results_daily, "results_daily.csv", row.names = FALSE)
  

  

# 
# 
# test <- results_1112 %>%
#   filter(contestLongName == "Governor" | contestLongName == "U.S. Senator") %>%
#   filter(totalVotes != 0) %>%
#   select( jurisdictionName, choiceName, totalVotes) %>%
#   spread(choiceName, totalVotes) %>%
#   mutate(R_Sen = `McSally, Martha` / (`McSally, Martha` + `Sinema, Kyrsten` + `Green, Angela`)) %>%
#   mutate(R_Gov = `Ducey, Doug` / (`Ducey, Doug` + `Garcia, David` + `Torres, Angel`))
# 
# g <- ggplot(test, aes(x=test$R_Gov, y=test$R_Sen), color = test$jurisdictionName) + geom_point() +
#   geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
# 
# 
# 
# 
# 
# 
# 
# ########sos##########################
# sos_eb <- Results_BeginningELectionNight %>%
#   filter(contestLongName == "Secretary of State") %>%
#   select(jurisdictionName, choiceName, totalVotes)
# 
# 
# sos_election <- Results_ElectionNight %>%
#   filter(contestLongName == "Secretary of State") %>%
#   select(jurisdictionName, choiceName, totalVotes)
# 
# 
# 
# sos_election_1111 <- Results_1111 %>%
#   filter(contestLongName == "Secretary of State") %>%
#   filter(jurisdictionName == "State") %>%
#   select(jurisdictionName, choiceName, totalVotes)
# 
# 
# 
# 
# write.csv(Results_BeginningELectionNight, "results_eb.csv", row.names = FALSE)
# 
# write.csv(Results_ElectionNight, "results_election.csv", row.names = FALSE)




#################MARICOPA########################

mc_1109=Maricopa_1109[,10]
mc_1111=Maricopa_1111[,10]
mc_1112=Maricopa_1112[,10]
mc_1113=Maricopa_1113[,10]
mc_1114=Maricopa_1114[,10]
mc_1115=Maricopa_1115[,10]
mc_1114_contestTotal=Maricopa_1114[,16]


Maricopa_Detail <- Maricopa_1107 %>%
  select(PRECINCT_NAME, PRECINCT_ID, CONTEST_FULL_NAME, CANDIDATE_FULL_NAME, candidate_party_id, TOTAL) %>%
  cbind(.,mc_1109) %>%
  cbind(.,mc_1111) %>%
  cbind(.,mc_1112) %>%
  cbind(.,mc_1113) %>%
  cbind(.,mc_1114) %>%
  cbind(.,mc_1115) 
  
  








mc_1108_senate <- mc_pct_1108 %>%
  filter(CONTEST_FULL_NAME == 'US Senate' | CONTEST_FULL_NAME == "Early Voting Edge Turnout
         ") %>%
  select(PRECINCT_NAME, CANDIDATE_FULL_NAME, TOTAL, CONTEST_TOTAL) %>%
  spread(CANDIDATE_FULL_NAME, TOTAL) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter( CONTEST_TOTAL != 0 )

mc_1109_senate <- mc_pct_1109 %>%
  filter(CONTEST_FULL_NAME == 'US Senate' | CONTEST_FULL_NAME == "Early Voting Edge Turnout
         ") %>%
  select(PRECINCT_NAME, CANDIDATE_FULL_NAME, TOTAL, CONTEST_TOTAL) %>%
  spread(CANDIDATE_FULL_NAME, TOTAL) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter( CONTEST_TOTAL != 0 ) 

maricopa <- mc_1108_senate %>%
  left_join(mc_1109_senate, by = "PRECINCT_NAME") %>%
  mutate(CONTEST_TOTAL_1109 = `CONTEST_TOTAL.y` - `CONTEST_TOTAL.x`) %>%
  mutate(MCSALLY_1109 = `REP - MCSALLY, MARTHA.y` - `REP - MCSALLY, MARTHA.x`) %>%
  mutate(SINEMA_1109 = `DEM - SINEMA, KYRSTEN.y` - `DEM - SINEMA, KYRSTEN.x`) %>%
  mutate(GREEN_1109 = `GRN - GREEN, ANGELA - WITHDRAWN.y` - `GRN - GREEN, ANGELA - WITHDRAWN.x`) %>%
  select(PRECINCT_NAME, MCSALLY_1109, SINEMA_1109, GREEN_1109, CONTEST_TOTAL_1109)