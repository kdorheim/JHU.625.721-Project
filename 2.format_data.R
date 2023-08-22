# Format results from all the different netcdf files into a single csv file. 

library(dplyr)

files <- list.files("data", full.names = TRUE, pattern = "csv")
lapply(files, function(f){
  df <- read.csv(f)
  df %>%  
    select(experiment, ensemble, model, year, TXx = value) %>% 
    mutate(TXx = (TXx - 273.15) * 9/5 + 32) -> 
    out 
  return(out)
}) %>% 
  do.call(what = "rbind") %>% 
  filter(grepl(x = ensemble, "p1")) -> 
  tasmax_df

tasmax_df %>% 
  filter(experiment %in% c("historical", "ssp245", "ssp585", "ssp119")) %>% 
  write.csv("tasmax.csv", row.names = FALSE)
