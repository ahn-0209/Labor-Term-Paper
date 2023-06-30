# import packages
library(ggmap)
library(data.table)
library(lubridate)
# warning setuup
options()$nwarnings  # check current state
options(nwarnings=60000)  # set, store old options
options()$nwarnings  # check
# Using account: Geocoding1
# Remaing usage: 59000
# set missing lontitude data
ori_case = fread("Traffic/CarAccidents/AccidentCase.csv",skip = 1)
lld = ori_case[Lon==0]
lld = lld[!(CountyID %in% c(09020, 10016, 09007))]
rm("ori_case")
table(year(lld$Infodate))
gc()


w2019 = lld[year(Infodate)==2019]
w2019_130728 = w2019[c(120001:130728),c(1,14)]
register_google(key = "AIzaSyCf-1s3kzcsiwwy5KdRpg3c3N1624MLj6Y")
# run the geocode function from ggmap package
w2019_130728_ggmap <- geocode(location = w2019_130728$CaseSite, output = "more", source = "google")
w2019_130728_ggmap <- cbind(w2019_130728, w2019_130728_ggmap)
write.csv(w2019_130728_ggmap,file="/home/b08302146/Traffic/Get geoInfo/w2019_130728_ggmap.csv",row.names = FALSE)
# start capture
sink("/home/b08302146/Traffic/Get geoInfo/warning/warning_w2019_130728_ggmap.txt", append = TRUE)
# produce warning
warnings()
# finishing capture
sink()

library(readr)
w2019_130728_ggmap <- fread("Traffic/Getgeoinfo/w2019_130728_ggmap.csv")
View(w2019_130728_ggmap)
w2018[26537,]
w2018_21687[24850,]
Error in `map()`:
  ℹ In index: 24850.26538 BAD GUY
花蓮縣花蓮市林森路168號
w2019_20000[CaseSite=="花蓮縣花蓮市林森路168號"]