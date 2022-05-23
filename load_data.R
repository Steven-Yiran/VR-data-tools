# load_data.R
# Author: Bo Liu, Jiayi Chen, Steven Shi
# Merge all google_drive data sheets as one csv file. 

# import library
library(googlesheets4)
library(dplyr)

# make sure google drive token is registered 
if (gs4_has_token()) {
  # find all files named "Batch **" 
  sheet_ref <- gs4_find(pattern = "^Batch\\s\\d{2}.*") %>% 
    tidyr::hoist(drive_resource, created_on = "createdTime") %>% 
    dplyr::mutate(created_on = as.Date(created_on))
  
  data_merged <- read_sheet(sheet_ref$id[1]) %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(batch_file = sheet_ref$name[1]) 
  
  # download all sheets using their references
  for (i in 2:nrow(sheet_ref)) { 
    sheet <- read_sheet(sheet_ref$id[i]) %>% 
      mutate(across(everything(), as.character)) %>% # format all column as character
      mutate(batch_file = sheet_ref$name[i]) # create a filename column 
    data_merged <- bind_rows(data_merged, sheet) # merge all data into data_merged
  }
}

# output combined data as csv
write.csv(data_merged, file = "all_batch.csv")


