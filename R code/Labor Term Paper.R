# Load pkgs
library(fixest)
library(data.table)
library(lubridate)
library(tidyverse)
library(jtools)
library(broom.mixed)
library(RColorBrewer)

# Get data
case_grid <- fread("Traffic/R files/1230/casewithgrid.csv")
sw_info <- fread("Traffic/R files/1230/Sidewalk201912.csv")
grid_info_5 <- fread("Traffic/R files/1230/grid_info_5.csv")
indiviual_info <- fread("Traffic/CarAccidents/AccidentIndividual.csv",skip = 1) # You can't have it since data policy
village <- fread("Traffic/Various data/鄉鎮市區代碼代碼表.csv")
village_code <- fread("Traffic/Various data/village.csv")
# land
land = fread("/home/b08302146/Traffic/Various data/land_taiwan.csv")
land = land[!duplicated(land$id),]
land = subset(land, land$COUNTYCODE!=c(9007,9020))
# get county & village
grid_info_5[land, on = "id", "County" := i.COUNTYCODE]
sw_info[village, "Village" := i.鄉鎮代碼, on = c(COUNTY_NA="縣市名稱", VILL_NAME="鄉鎮名稱")]
village_code = village_code[!duplicated(village_code$id),]
grid_info_5[village_code, on = "id", "Village" := i.Town_ID]
# combine grid_info + sw_info + county code
grid_info_5[sw_info, on = "id", "PC" := i.PC]
grid_info_5[sw_info, on = "id", "RC" := i.RC]
grid_info_5[sw_info, on = "id", "Drain" := i.Drain]
grid_info_5[,usefulSidewalk := PC + RC + Drain >= 1]
grid_info_5[exist==1 & usefulSidewalk==F, Others:=1]
grid_info_5[exist==1 & usefulSidewalk==T, Others:=0]
# make sub case table
sub_case_5 = case_grid[(year(Infodate) %in% c(2018:2019))|
                         (year(Infodate)==2017&month(Infodate)%in%c(7:12))]
#####
sub_case_5 = sub_case_5[year(Infodate)==2017 & month(Infodate) %in% c(7:12), year:=1]
sub_case_5 = sub_case_5[year(Infodate)==2018 & month(Infodate) %in% c(1:6), year:=2]
sub_case_5 = sub_case_5[year(Infodate)==2018 & month(Infodate) %in% c(7:12), year:=3]
sub_case_5 = sub_case_5[year(Infodate)==2019 & month(Infodate) %in% c(1:6), year:=4]
sub_case_5 = sub_case_5[year(Infodate)==2019 & month(Infodate) %in% c(7:12), year:=5]
# get dead & injured info
for (i in 1:5){
  sub_case_5[year == i, dead := sum(Death), by = "id"]
  sub_case_5[year == i, injured := sum(Injured), by = "id"]
}
grid_info_5[sub_case_5, Death := i.dead, on = c(id="id", year="year")]
grid_info_5[sub_case_5, Injured := i.injured, on = c(id="id", year="year")]
grid_info_5[is.na(Death), Death := 0]
grid_info_5[is.na(Injured), Injured := 0]
# merge case_grid & individual
indiviual_info_5 = indiviual_info[(year(InfoDate) %in% c(2018:2019))|
                                    (year(InfoDate)==2017&month(InfoDate)%in%c(7:12))]
case_prsn_5 = merge(sub_case_5, indiviual_info_5[,c(2,10,14)], by = "AccKeyID", all.x = T)
for (i in 1:5){
  case_prsn_5[year == i, Footman := sum(DistiguishCode==2713), by = "id"]
}
for (i in 1:5){
  case_prsn_5[year==i&DistiguishCode==2713, Footman := .N, by = "id"]
}
# Injured footman
for (i in 1:5){
  case_prsn_5[year == i, I_Footman := sum(InjuredLevelCode==258&DistiguishCode==2713), by = "id"]
}
# Dead footman
for (i in 1:5){
  case_prsn_5[year == i, D_Footman := sum(InjuredLevelCode==257&DistiguishCode==2713), by = "id"]
}
grid_info_5[case_prsn_5, Footman := i.Footman, on = c(id="id", year="year")]
grid_info_5[case_prsn_5, I_Footman := i.I_Footman, on = c(id="id", year="year")]
grid_info_5[case_prsn_5, D_Footman := i.D_Footman, on = c(id="id", year="year")]
grid_info_5[is.na(Footman), Footman:=0]
grid_info_5[is.na(I_Footman), I_Footman:=0]
grid_info_5[is.na(D_Footman), D_Footman:=0]
head(grid_info_5)
rm(case_grid, case_prsn_5, i, indiviual_info, indiviual_info_5, land, sub_case_5, 
   sw_info, village, village_code)
gc()

# Summary Statistics
mean(grid_info_5[year_treated != 1000]$count)
mean(grid_info_5[year_treated != 1000]$Footman)

# Analysis
fit2 <- feols(count ~ sunab(year_treated,year)|id + year, new_grid_info_5[year_treated!= 1000]) # for accidents number
fit2_F <- feols(Footman ~ sunab(year_treated,year)|id + year, grid_info_5[year_treated!= 1000]) # for pedestrians
iplot(fit2)
iplot(fit2_F)
etable(list(fit2, fit2_F), tex = T)