library(data.table)
library(lubridate)
library(stringr)
intersection_201912 = fread("/home/b08302146/Traffic/sidewalks_0005/201912_intersection.csv")
# delect 
intersection_201912 = intersection_201912[!duplicated(id)]
intersection_201912$SW_PAVE = toupper(intersection_201912$SW_PAVE)
# replace
dict = c("AP", "AC", "專", "磚", "枰", "坪", "押花", "壓花", "PC高壓", "RC", "組合高壓磚", "組合磚",
         "角型高壓磚", "角型磚", "水泥版", "水泥磚", "版磚", "板磚", "其它", "其他",
         "瓷磚", "磁磚", "木紋磚", "磁磚")
for (i in seq(1,24,2)){
  k = i+1
  intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, dict[i], dict[k])
}
# set dictionary
pave_type = c("AC", "AC|瀝青|柏油", 
              "PC", "PC|地坪|混凝土|壓花|水泥|混泥土", 
              "RC", "RC|高壓|水泥磚|板磚|仿石|仿木",
              "Drain", "連鎖|植草|組合|角型|紅磚|金錢|麵包|透水",
              "Tiles", "磁|磁磚|印花地磚",
              "Stone", "石子|砂|石板",
              "Marble", "大理石|岡|崗",
              "Wood",  "木板",
              "Blind", "導盲磚",
              "Polyethylene", "聚乙烯",
              "Bone", "骨|卵|石塊",
              "Others", "其他|止滑磚|植栽帶|楔型石磚|標線型|自強路|陶磚")
# set new col
for (i in seq(1,24,2)){
  eval(parse(text = paste0("intersection_201912$", pave_type[i],"=0")))
}
# set value to each situation
for (i in seq(2,24,2)){
  k = i-1
  intersection_201912[grepl(pave_type[i],SW_PAVE), eval(parse(text=paste0(pave_type[k],":=1")))]
}
write.csv(intersection_201912,file="/home/b08302146/R/1230/Sidewalk201912.csv",row.names = FALSE)
write.csv(big_panel,file="/home/b08302146/R/1230/grid_count.csv",row.names = FALSE)
write.csv(case_grid, file="/home/b08302146/R/1230/casewithgrid.csv",row.names = FALSE)
gc()
#####
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "AP", "AC")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "專", "磚")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "枰", "坪")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "押花", "壓花")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "PC高壓", "RC")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "組合高壓磚", "組合磚")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "角型高壓磚", "角型磚")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "水泥版", "水泥磚")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "版磚", "板磚")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "其它", "其他")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "瓷磚", "磁磚")
intersection_201912$SW_PAVE = str_replace_all(intersection_201912$SW_PAVE, "木紋磚", "磁磚")
#####
intersection_201912[, ":="("AC" = 0)]
intersection_201912[, ":="("PC" = 0)]
intersection_201912[, ":="("RC" = 0)]
intersection_201912[, ":="("Drain" = 0)]
intersection_201912[, ":="("Tiles" = 0)]
intersection_201912[, ":="("Stone" = 0)]
intersection_201912[, ":="("Marble" = 0)]
intersection_201912[, ":="("Wood" = 0)]
intersection_201912[, ":="("Blind" = 0)]
intersection_201912[, ":="("Polyethylene" = 0)]
intersection_201912[, ":="("Bone" = 0)]
intersection_201912[, ":="("Others" = 0)]
#####
pave_type = c("AC", "AC|瀝青|柏油", 
              "PC", "PC|地坪|混凝土|壓花|水泥|混泥土", 
              "RC", "RC|高壓|水泥磚|板磚|仿石|仿木",
              "Drain", "連鎖|植草|組合|角型|紅磚|金錢|麵包|透水",
              "Tiles", "磁|磁磚|印花地磚",
              "Stone", "石子|砂|石板",
              "Marble", "大理石|岡|崗",
              "Wood",  "木板",
              "Blind", "導盲磚",
              "Polyethylene", "聚乙烯",
              "Bone", "骨|卵|石塊",
              "Others", "其他|止滑磚|植栽帶|楔型石磚|標線型|自強路|陶磚")
#####
intersection_201912[grepl("AC|瀝青|柏油",SW_PAVE), AC:=1]
intersection_201912[grepl("PC|地坪|混凝土|壓花|水泥|混泥土",SW_PAVE), PC:=1]
intersection_201912[grepl("RC|高壓|水泥磚|板磚|仿石|仿木",SW_PAVE), RC:=1]
intersection_201912[grepl("連鎖|植草|組合|角型|紅磚|金錢|麵包|透水",SW_PAVE), Drain:=1]
intersection_201912[grepl("磁|磁磚|印花地磚|陶磚",SW_PAVE), Tiles:=1]
intersection_201912[grepl("石子|砂|石板",SW_PAVE), Stone:=1]
intersection_201912[grepl("大理石|岡|崗",SW_PAVE), Marble:=1]
intersection_201912[grepl("木板",SW_PAVE), Wood:=1]
intersection_201912[grepl("導盲磚",SW_PAVE), Blind:=1]
intersection_201912[grepl("聚乙烯",SW_PAVE), Polyethylene:=1]
intersection_201912[grepl("骨|卵|石塊",SW_PAVE), Bone:=1]
intersection_201912[grepl("其他|止滑磚|植栽帶|楔型石磚|標線型|自強路|地磚|磚塊",SW_PAVE), Others:=1]