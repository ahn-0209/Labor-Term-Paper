library(data.table)
library(lubridate)
gc()
######
# making count
ori_case = fread("Traffic/CarAccidents/AccidentCase.csv",skip = 1)
indiviual_info <- fread("Traffic/CarAccidents/AccidentIndividual.csv",skip = 1)
all_lld = fread("Traffic/Getgeoinfo/all_lld_grid.csv")
all_lld[ori_case, on="AccKeyID", "Infodate":=i.Infodate]
all_lld_1 = all_lld[(year(Infodate)==2017)&(month(Infodate)%in%c(7:12))]
all_lld_2 = all_lld[(year(Infodate)==2018)&(month(Infodate)%in%c(1:6))]
all_lld_3 = all_lld[(year(Infodate)==2018)&(month(Infodate)%in%c(7:12))]
all_lld_4 = all_lld[(year(Infodate)==2019)&(month(Infodate)%in%c(1:6))]
all_lld_5 = all_lld[(year(Infodate)==2019)&(month(Infodate)%in%c(7:12))]
# by year
all_lld_count_1 = setDT(data.frame(table(all_lld_1$id)))
all_lld_count_1[,year:=1]
all_lld_count_2 = setDT(data.frame(table(all_lld_2$id)))
all_lld_count_2[,year:=2]
all_lld_count_3 = setDT(data.frame(table(all_lld_3$id)))
all_lld_count_3[,year:=3]
all_lld_count_4 = setDT(data.frame(table(all_lld_4$id)))
all_lld_count_4[,year:=4]
all_lld_count_5 = setDT(data.frame(table(all_lld_5$id)))
all_lld_count_5[,year:=5]
# rename col
colnames(all_lld_count_1)[1] <- "id"
colnames(all_lld_count_2)[1] <- "id"
colnames(all_lld_count_3)[1] <- "id"
colnames(all_lld_count_4)[1] <- "id"
colnames(all_lld_count_5)[1] <- "id"
colnames(all_lld_count_1)[2] <- "count"
colnames(all_lld_count_2)[2] <- "count"
colnames(all_lld_count_3)[2] <- "count"
colnames(all_lld_count_4)[2] <- "count"
colnames(all_lld_count_5)[2] <- "count"
lld_count = rbind(all_lld_count_1,all_lld_count_2)
lld_count = rbind(lld_count,all_lld_count_3)
lld_count = rbind(lld_count,all_lld_count_4)
lld_count = rbind(lld_count,all_lld_count_5)
#
grid_info_5$id = as.character(grid_info_5$id)
grid_info_5[lld_count, on=c(id="id",year="year"), "plus":=i.count]
grid_info_5[is.na(plus), plus := 0]
grid_info_5[,count:=count+plus]
grid_info_5[id==750097]
#####
# making death, injured, footman
lld_case = ori_case[AccKeyID %in% all_lld$AccKeyID]
lld_case[all_lld, on="AccKeyID", id := i.id]
lld_case_5 = lld_case[(year(Infodate) %in% c(2018:2019))|
                         (year(Infodate)==2017&month(Infodate)%in%c(7:12))]
lld_case_5 = lld_case_5[(year(Infodate)==2017)&(month(Infodate)%in%c(7:12)), year:=1]
lld_case_5 = lld_case_5[(year(Infodate)==2018)&(month(Infodate)%in%c(1:6)), year:=2]
lld_case_5 = lld_case_5[(year(Infodate)==2018)&(month(Infodate)%in%c(7:12)), year:=3]
lld_case_5 = lld_case_5[(year(Infodate)==2019)&(month(Infodate)%in%c(1:6)), year:=4]
lld_case_5 = lld_case_5[(year(Infodate)==2019)&(month(Infodate)%in%c(7:12)), year:=5]
#
for (i in 1:5){
  lld_case_5[year == i, p_dead := sum(Death), by = "id"]
  lld_case_5[year == i, p_injured := sum(Injured), by = "id"]
}
#
lld_case_5$id = as.character(lld_case_5$id)
grid_info_5[lld_case_5, p_Death := i.p_dead, on = c(id="id", year="year")]
grid_info_5[lld_case_5, p_Injured := i.p_injured, on = c(id="id", year="year")]
grid_info_5[is.na(p_Death), p_Death := 0]
grid_info_5[is.na(p_Injured), p_Injured := 0]
grid_info_5[,Death:=Death+p_Death]
grid_info_5[,Injured:=Injured+p_Injured]
#
lld_indiviual_info_5 = indiviual_info[(year(InfoDate) %in% c(2018:2019))|
                                    (year(InfoDate)==2017&month(InfoDate)%in%c(7:12))]
lld_case_prsn_5 = merge(lld_case_5, lld_indiviual_info_5[,c(2,10,14)], by = "AccKeyID", all.x = T)
for (i in 1:5){
  lld_case_prsn_5[year == i, p_Footman := sum(DistiguishCode==2713), by = "id"]
}
for (i in 1:5){
  lld_case_prsn_5[year==i&DistiguishCode==2713, p_Footman := .N, by = "id"]
}
# Injured footman
for (i in 1:5){
  lld_case_prsn_5[year == i, p_I_Footman := sum(InjuredLevelCode==258&DistiguishCode==2713), by = "id"]
}
# Dead footman
for (i in 1:5){
  lld_case_prsn_5[year == i, p_D_Footman := sum(InjuredLevelCode==257&DistiguishCode==2713), by = "id"]
}
#
grid_info_5[lld_case_prsn_5, p_Footman := i.p_Footman, on = c(id="id", year="year")]
grid_info_5[lld_case_prsn_5, p_I_Footman := i.p_I_Footman, on = c(id="id", year="year")]
grid_info_5[lld_case_prsn_5, p_D_Footman := i.p_D_Footman, on = c(id="id", year="year")]
grid_info_5[is.na(p_Footman), p_Footman:=0]
grid_info_5[is.na(p_I_Footman), p_I_Footman:=0]
grid_info_5[is.na(p_D_Footman), p_D_Footman:=0]
#
grid_info_5[,Footman:=Footman+p_Footman]
grid_info_5[,I_Footman:=I_Footman+p_I_Footman]
grid_info_5[,D_Footman:=D_Footman+p_D_Footman]
