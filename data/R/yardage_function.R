library(tidyverse)
library(rvest)
library(httr2)
library(xml2)


add_yardage_misc_variables <- function(pbp, week) {
  
  pbp_with_yards <- pbp |> 
    filter(!str_detect(value,"1st & "),
           !str_detect(value,"2nd & "),
           !str_detect(value,"3rd & "),
           !str_detect(value,"4th & "),
           # !str_detect(value,"[0-9]+ plays"),
           !str_detect(value,"1st and"),
           !str_detect(value,"2nd and"),
           !str_detect(value,"3rd and"),
           !str_detect(value,"4th and"),
           !str_detect(value, "[A-Z][A-Z] ball on"),
           !str_detect(value,"will receive;"),
           !str_detect(value,"will kickoff;"),
           !str_detect(value,regex("end of game", ignore_case = TRUE))) |> 
    mutate(completion_yds = if_else(completion == 1,str_extract(value, "(?= for ).*"),NA_character_),
      completion_yds = if_else(completion == 1, parse_number(completion_yds),NA_integer_),
           completion_yds = if_else(completion == 1 & str_detect(value, "loss"),
                                    completion_yds * (-1),completion_yds), 
      completion_yds = if_else(completion == 1 & str_detect(value,regex("no gain",ignore_case = TRUE)),0,completion_yds),
           reception_yds = completion_yds,
      rushing_yds = if_else(rush_attempt == 1,str_extract(value, "(?= for ).*"),NA_character_),
  rushing_yds = if_else(rush == 1, as.numeric(str_extract(rushing_yds,"[0-9]+")),NA_integer_),
  rushing_yds = if_else(str_detect(value, ".*\\((\\d+)\\).*") & rush_attempt == 1,
                                              as.numeric(sub(".*\\((\\d+)\\).*", "\\1", value)),
                                              rushing_yds),
  rushing_yds = if_else(rush_attempt == 1 & str_detect(value, "loss"),
                           rushing_yds * (-1),rushing_yds),
  rushing_yds = if_else(rush_attempt == 1 & str_detect(value,regex("no gain",ignore_case = TRUE)),0,rushing_yds),
  field_goal_yds = if_else(field_goal == 1,
                        str_remove(value, "^\\S* "),
                        NA_character_),
  field_goal_yds = if_else(!is.na(field_goal_yds),
                           as.numeric(str_extract(field_goal_yds,"[0-9]+")),
                           NA_integer_),
  punt_yds = if_else(punt == 1,
                           str_remove(value, "^\\S* "),
                           NA_character_),
  punt_yds = if_else(punt == 1,as.numeric(str_extract(punt_yds,"[0-9]+")),NA_integer_),
  punt_returner_yds = if_else(punt == 1 & !is.na(punt_returner_player_name),
                            as.numeric(gsub("^.*return.*?([0-9]+).*","\\1",value)),
                            NA_integer_),
  punt_returner_yds = if_else(punt == 1 & str_detect(value, "loss"),
                        punt_returner_yds * (-1),punt_returner_yds),
  kickoff_yds = if_else(kickoff_play == 1,
                        str_remove(value, "^\\S* "),
                        NA_character_),
  kickoff_yds = if_else(kickoff_play == 1,
                        parse_number(kickoff_yds,trim_ws = TRUE),
                        NA_integer_),
  kickoff_returner_yds = if_else(kickoff_play == 1 & !is.na(kickoff_returner_player_name),
                               as.numeric(gsub("^.*return.*?([0-9]+).*","\\1",value)),
                               NA_integer_),
  kickoff_returner_yds = if_else(kickoff_play == 1 & str_detect(value, "loss"),
                        kickoff_returner_yds * (-1),kickoff_returner_yds),
  sack_yds = if_else(sack == 1, as.numeric(str_extract(value,"[0-9]+")),NA_integer_),
  penalty_yds = if_else(penalty == 1,as.numeric(str_extract(penalty_play_text,"[0-9]+")),NA_integer_),
  fumble_return_yds = if_else(fumble_return == 1,
                              as.numeric(gsub("^.*return.*?([0-9]+).*","\\1",value)),
                              NA_integer_),
  rush_yards_gained = if_else(rush_attempt == 1,rushing_yds,NA_integer_),
  pass_yards_gained = if_else(pass_attempt == 1,completion_yds,NA_integer_),
  drive_start_time = if_else(str_detect(value,"[0-9] plays"),value,NA_character_),
  drive_start_time = if_else(str_detect(value,"[0-9] plays"),str_extract(value,".+?(?=,)"),NA_character_),
  drive_start_time = if_else(kickoff_play == 1,lag(drive_start_time, n = 1),lag(drive_start_time, n = 2))) |> 
 # drive_start_time = if_else(str_detect(value, "drive start"),str_extract(value,"(?<= at).*"),drive_start_time),
#  drive_start_time = if_else(str_detect(value, "drive start"),lag(drive_start_time, n = 2),drive_start_time),
#  drive_length = if_else(str_detect(value,"[0-9] plays"),value,NA_character_),
 # drive_length = if_else(!is.na(drive_length),str_extract(value,"(?=,).*"),NA_character_),
  #drive_length = if_else(!is.na(drive_length),str_extract(drive_length,"[0-9]+:[0-9]+"),NA_character_),
  #drive_number_plays = if_else(str_detect(value,"[0-9] plays"),str_extract(value,"[0-9]+ plays"),
       #                        NA_character_),
  #drive_number_plays = str_remove(drive_number_plays,"plays"),
  #drive_yds = if_else(str_detect(value,"[0-9] plays"),str_extract(value,"[0-9]+ yards"),
        #              NA_character_),
  #drive_yds = str_remove(drive_yds,"yards"),
  #drive_yds = as.numeric(drive_yds),
  #drive_yds = if_else(!is.na(drive_yds) & str_detect(value, "-"),
  #                    drive_yds * -1,drive_yds),
  #drive_length = lag(drive_length, n = 4),
  #drive_number_plays = lag(drive_number_plays, n = 4),
  #drive_yds = lag(drive_yds, n = 4),
    mutate(
  qtr = if_else(str_detect(value, regex("start of 2nd quarter", ignore_case = TRUE)),2,NA_integer_),
  qtr = if_else(str_detect(value, regex("start of 3rd quarter", ignore_case = TRUE)),3,qtr),
  qtr = if_else(str_detect(value, regex("start of 4th quarter", ignore_case = TRUE)),4,qtr),
  posteam = case_when(str_detect(value, regex("drive start")) ~ 
                        str_extract(value, ".*drive start")),
  posteam = str_remove_all(posteam, "drive start"),
  posteam = lag(posteam)) |> 
    fill(qtr,.direction = "down") |> 
    fill(posteam, .direction = "downup")|> 
    mutate( 
      qtr = if_else(is.na(qtr),1,qtr)) |> 
    mutate(game_half = case_when(qtr <= 2 ~ "1st",
                                 qtr >= 3 ~ "2nd")) |>  
    rename(play_desc = value)  |> 
    filter(!is.na(down), 
           !str_detect(play_desc,"drive start")) |> 
    mutate(play_id = 1:n(),
           week = week) |> 
           fill(drive_start_time, .direction = "downup") |> 
         #  fill(drive_length, .direction = "down") |> 
          # fill(drive_yds, .direction = "down") |> 
           #fill(drive_number_plays, .direction = "down") |> 
    mutate(posteam = if_else(kickoff_play == 1,NA_character_,posteam))
    
  
  
}


pbp_final <- add_yardage_misc_variables(test_names, week = 1) |> 
  select(play_desc,drive_start_time,posteam,timeout)
view(pbp_final)
  
rushing_check <- pbp_final |> 
  filter(rush == 1) |> 
  group_by(rusher_player_name) |> 
  summarise(
  rush_yds = sum(rush_yards_gained, na.rm = TRUE),
  rush_att = sum(rush_attempt),
  .groups = "drop"
  )

view(rushing_check)

passing_check <- pbp_final |> 
  filter(pass == 1) |> 
  group_by(passer_player_name) |> 
  summarise(
    passing_yds = sum(pass_yards_gained, na.rm = TRUE),
    pass_att = sum(pass_attempt),
    pass_comp = sum(completion,na.rm = TRUE),
    comp_pct = round(pass_comp/pass_att, digits = 2),
    .groups = "drop"
  )
  
  view(passing_check)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  