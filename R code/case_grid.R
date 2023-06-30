accident_case = fread("/home/b08302146/Traffic/Car accidents/AccidentCase.csv",skip=1)
accident_case = accident_case[Lat!=0]
accident_case$Infodate = ymd(accident_case$Infodate)
# set the list of dot_grid
dot_grid_data <- list()
# get the file path
dot_grid_file_name <- list.files("/home/b08302146/Traffic/dot_grid_dt_0005", 
                                 pattern = ".csv", 
                                 full.names = TRUE)
# read files
for (i in 1:length(dot_grid_file_name)){
  dot_grid_data[[i]] = fread(dot_grid_file_name[i])
}
# clean useless info
for (i in 1:11){
  if (length(dot_grid_data[[i]]) ==9){
    dot_grid_data[[i]] = dot_grid_data[[i]][,-c(2,3,4,6:9)]
    colnames(dot_grid_data[[i]])[1] = "AccKeyID"
  }else{
    dot_grid_data[[i]] = dot_grid_data[[i]][,-c(2,3,4)]
  }
}
# conbine list
all = rbind(dot_grid_data[[1]], dot_grid_data[[2]], dot_grid_data[[3]],
            dot_grid_data[[4]], dot_grid_data[[5]], dot_grid_data[[6]],
            dot_grid_data[[7]], dot_grid_data[[8]], dot_grid_data[[9]],
            dot_grid_data[[10]], dot_grid_data[[11]])
# make accident with grid
case_grid = merge(all, accident_case, by = "AccKeyID")