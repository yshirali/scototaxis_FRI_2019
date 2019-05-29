library(tidyverse)

data_path <-"~/data_comgar_2019/scototaxis_FRI/data"

file_list <- dir(data_path,
                 pattern = "*.csv",
                 recursive = TRUE)
head(file_list)


glue::glue("There are {length(file_list)} csv files in {data_path}.")


extractExperiment <- function(filename) {
  str_split(basename(filename), "_")[[1]][3]
}

extractFishID <- function(filename) {
  str_split(basename(filename), "_")[[1]][4]
}

extractScorerName <- function(filename) {
  with_csv <- str_split(basename(filename), "_")[[1]][11]
  str_to_title(str_split(with_csv, "\\.")[[1]][1])  #removes .csv from title 
}

numberTransits <- function(dataframe) {
  nrow(dataframe) - 1
}

getTimeSpentInEachRegion <- function(dataframe) {
  
  # get the time the first starts
  startTime <- dataframe %>% pull(time) %>% first
  
  # calculate the time the trial ends
  endTime <- startTime + 600
  
  dataframe %>% 
    mutate(time = replace(time, time > endTime, endTime)) %>% # make all the values of 'time' greater than endTime equal to endTime
    mutate(timeSpent = lead(time, default = 0) - time) %>% # take the differences. Any scoring after the end of the trial will get a 0 for timeSpent
    filter(timeSpent > 0) %>% # get rid of any zones where timeSpent = 0
    group_by(code) %>%
    summarise(totalTime = sum(timeSpent)) # use summarise to distill information (take a sum or average)
  }

getProportionOfTimeInEachRegion <- function(dataframe) {
  dataframe %>%
    getTimeSpentInEachRegion %>%
    group_by(code) %>%
    summarise(totalTime = sum(timeSpent)) %>% # use summarise to distill information (take a sum or average)
    mutate(proportionTime = totalTime / sum(totalTime)) # use mutate to add columns
}


# next
clean_codename <- function(data){
  # read in the csv
  # create a column for filename
  # create new variables
  # get the transits and proportions  'Mutate = adds or overwrites columns, map_dbl returns a floating decimal..map replaces a for loop
  data %>%
    mutate(code=case_when(
      str_detect(code,"^[Oo].*$") ~ "white",
      str_detect(code,"^[Qq].*$") ~ "black_thigmo",
      str_detect(code,"^[Pp].*$") ~ "white_thigmo",
      str_detect(code,"^[Ww].*$") ~ "black",
      TRUE ~ "not recognized"
    ))
}


# next
messy_dataframe <- tibble(filename = file_list) %>%
  mutate(file_contents = map(filename, ~ read_csv(file.path(data_path, .)))) %>%
  mutate(file_contents = map(file_contents, clean_codename)) %>%
  mutate(number_transits = map_dbl(file_contents, numberTransits),
         timeSpent = map(file_contents, getTimeSpentInEachRegion)) %>%
  unnest(timeSpent) %>%
  # complete(filename, code, fill = list(totalTime = 0, proportionTime = 0)) %>% # I'm getting rid of this line because it cause more harm than good. We will fill these values with 0 later on in the process
  rowwise() %>%
  mutate(experiment = extractExperiment(filename),
         fishID = extractFishID(filename),
         scorer = extractScorerName(filename)
  ) %>%
  ungroup # undos rowwise(), which we don't need anymore

head(messy_dataframe)

write_csv(messy_dataframe, 'messyDF_scototaxis_FRI_may28_2019.csv')

# other version
full_df <- messy_dataframe %>%
  select(filename, code, totalTime, number_transits, scorer, experiment,fishID) %>%
  spread(code, totalTime) %>% # create columns for each zone
  replace(., is.na(.), 0) # replace NAs (where the fish didn't enter a zone) with 0

head(full_df)


#write csv
write_csv(full_df, 'fullDF_scototaxis_FRI_may28_2019.csv')
