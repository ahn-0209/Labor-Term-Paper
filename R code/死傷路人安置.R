gc()
# get package
library(data.table)
library(fixest)
library(ggplot2)
library(lubridate)
# get data
ori = fread("Traffic/Car accidents/AccidentCase.csv")
case_grid <- fread("R/1230/casewithgrid.csv")
sw_info <- fread("R/1230/Sidewalk201912.csv")
grid_info <- fread("R/1230/grid_info.csv")
indiviual_info <- fread("Traffic/Car accidents/AccidentIndividual.csv",skip = 1)
# combine grid_info + sw_info
grid_info[sw_info, on = "id", "PC" := i.PC]
grid_info[sw_info, on = "id", "RC" := i.RC]
grid_info[sw_info, on = "id", "Drain" := i.Drain]
grid_info[,usefulSidewalk := PC + RC + Drain >= 1]
grid_info[exist==1 & usefulSidewalk==F, Others:=1]
grid_info[exist==1 & usefulSidewalk==T, Others:=0]
# make sub case table
sub_case = case_grid[year(Infodate) %in% c(2017:2019)]
sub_case = sub_case[!is.na(id)]
#####
sub_case = sub_case[year(Infodate)==2017 & month(Infodate) %in% c(1:9), year:=1]
sub_case = sub_case[year(Infodate)==2017 & month(Infodate) %in% c(10:12), year:=2]
sub_case = sub_case[year(Infodate)==2018 & month(Infodate) %in% c(1:6), year:=3]
sub_case = sub_case[year(Infodate)==2018 & month(Infodate) %in% c(7:12), year:=4]
sub_case = sub_case[year(Infodate)==2019 & month(Infodate) %in% c(1:6), year:=5]
sub_case = sub_case[year(Infodate)==2019 & month(Infodate) %in% c(7:12), year:=6]
# get dead & injured info
for (i in 1:6){
  sub_case[year == i, dead := sum(Death), by = "id"]
  sub_case[year == i, injured := sum(Injured), by = "id"]
}
grid_info[sub_case, Death := i.dead, on = c(id="id", year="year")]
grid_info[sub_case, Injured := i.injured, on = c(id="id", year="year")]
grid_info[is.na(Death), Death := 0]
grid_info[is.na(Injured), Injured := 0]
# case + individual
indiviual_info = indiviual_info[(year(InfoDate) %in% c(2018:2019))|
                   (year(InfoDate)==2017&month(InfoDate)%in%c(3:12))]
case_prsn = merge(sub_case, indiviual_info, by = "AccKeyID", all.x = T)
# merge data
#####
panel = fread("R/1230/grid_info.csv")
panel = rbind(panel, sub_case, fill=T, bynames="id")
panel = panel[order(id)]
panel[,c("Lat", "Lon", "x",  
         "CaseTime", "CaseSite", 
         "CountyID" ,"CityCode", 
         "TownKeyID", "VillageKeyID", 
         "VillageCode", "UpdateTime",
         "InfoTime", "Infodate"):=NULL]
#####