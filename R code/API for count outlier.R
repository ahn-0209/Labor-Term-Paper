id3356309 = case_grid[id==3356309&year(Infodate)==2017]
id3356309 = id3356309[,c(1,15)]
id4887760 = case_grid[id==4887760&year(Infodate)==2017]
id4887760 = id4887760[,c(1,15)]
id5154876 = case_grid[id==5154876&year(Infodate)==2017]
id5154876 = id5154876[,c(1,15)]

register_google(key = "AIzaSyAAC7072MHVCbrVeMVqDcnnqv3-kH2QzRQ")
# run the geocode function from ggmap package
w5154876_ggmap <- geocode(location = id5154876$CaseSite, output = "more", source = "google")
w5154876_ggmap <- cbind(id5154876, w5154876_ggmap)
write.csv(w5154876_ggmap,file="/home/b08302146/Traffic/Get geoInfo/w5154876_ggmap.csv",row.names = FALSE)
# start capture
sink("/home/b08302146/Traffic/Get geoInfo/warning/warning_w5154876_ggmap.txt", append = TRUE)
# produce warning
warnings()
# finishing capture
sink()

library(readr)
w2017_3_ggmap <- fread("Traffic/Get geoInfo/w2017_3_ggmap.csv")
View(w2017_3_ggmap)
test = w2017_3_ggmap[is.na(w2017_3_ggmap$lon)]
