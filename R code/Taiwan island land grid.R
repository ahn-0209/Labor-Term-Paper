library(data.table)
library(lubridate)
# import data
land_ocean = fread("/home/b08302146/Traffic/land_ocean_taiwan.csv")
land_ocean = land_ocean[!duplicated(land_ocean$id),]
land = fread("/home/b08302146/Traffic/land_taiwan.csv")
land = land[!duplicated(land$id),]
land = land[!(COUNTYCODE %in% c(09020, 10016, 09007))]
land[land_ocean, on="id", "left":=i.left]
land[land_ocean, on="id", "top":=i.top]
land[land_ocean, on="id", "right":=i.right]
land[land_ocean, on="id", "bottom":=i.bottom]
land = land[,c(2:6)]
d = grid[,c(1,6:23)]
o = grid[,c()]
count_09to16 = grid$count_2016 + grid$`2015_count` + grid$`2014_count` + 
  grid$`2013_count` + grid$`2012_count` + 
  grid$`2011_count` + grid$`2010_count` + grid$`2009_count`
d = cbind(d, count_09to16)
land = merge(land, d, by = "id", all.x = T)
land[is.na(land)] <- 0
# remove outlier
setDT(land)
land[count_2017>=500]
land = subset(land, land$`2017_count`<1000)
dat = land[,c(1,21,15,16,17,18,19,20,14)]

# 放人行道
e = intersection_201709[,c(2,16,12)]
dat = merge(dat, e, by = "id", all.x = T)
colnames(dat)[10] = "1709_exist"
colnames(dat)[11] = "1709_WTH"
e = intersection_201712[,c(2,16,12)]
dat = merge(dat, e, by = "id", all.x = T)
colnames(dat)[12] = "1712_exist"
colnames(dat)[13] = "1712_WTH"
e = intersection_201806[,c(2,15,12)]
dat = merge(dat, e, by = "id", all.x = T)
colnames(dat)[14] = "1806_exist"
colnames(dat)[15] = "1806_WTH"
e = intersection_201812[,c(2,15,12)]
dat = merge(dat, e, by = "id", all.x = T)
colnames(dat)[16] = "1812_exist"
colnames(dat)[17] = "1812_WTH"
e = intersection_201906[,c(2,15,12)]
dat = merge(dat, e, by = "id", all.x = T)
colnames(dat)[18] = "1906_exist"
colnames(dat)[19] = "1906_WTH"
e = intersection_201912[,c(2,55,20)]
dat = merge(dat, e, by = "id", all.x = T)
colnames(dat)[20] = "1912_exist"
colnames(dat)[21] = "1912_WTH"
e = intersection_201912[,c(2,45)]
dat = merge(dat, e, by = "id", all.x = T)
# na to 0
a = dat
table(is.na(a$COUNTY_NA))
a[is.na(a)] <- 0
a = a[-1,]
#
colnames(a)[3] = 'count_1709'
colnames(a)[4] = 'count_1712'
colnames(a)[5] = 'count_1806'
colnames(a)[6] = 'count_1812'
colnames(a)[7] = 'count_1906'
colnames(a)[8] = 'count_1912'
