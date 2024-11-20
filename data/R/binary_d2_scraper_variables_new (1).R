library(rvest)
library(httr2)
library(xml2)
library(tidyverse)




binary_d2_scraper_variables <- function(url_id) {
  d2_pbp <- glue::glue("https://stats.ncaa.org/contests/{url_id}/play_by_play")
  d2_pbp_page <- read_html(d2_pbp)
  ##### function to get all of the binary columns from NCAA STATS Org play by play data
  d2_tbl <- d2_pbp_page |> 
    html_elements("div") |> 
    html_elements("span") |> 
    html_text2() |> 
    as_tibble() |> 
    mutate(
      #value = if_else(str_detect(value, regex("Play overturned",ignore_case = TRUE)),
       #                    str_extract(value,"The previous play.*"),
        #                   value),
           value = if_else(str_detect(value,regex("play overturned.",ignore_case = TRUE)),
                           str_remove(value,regex("play overturned.*",ignore_case = TRUE)),
                           value),
      down = case_when(str_detect(value,"1st &") ~ str_extract(value, "[0-9]+"),
                            str_detect(value,"2nd &") ~ str_extract(value, "[0-9]+"),
                            str_detect(value,"3rd &") ~ str_extract(value, "[0-9]+"),
                            str_detect(value,"4th &") ~ str_extract(value, "[0-9]+"))) |> 
    mutate(yds_to_go = case_when(str_detect(value,"1st &") ~ str_extract(value, "[0-9]+ "),
                                 str_detect(value,"2nd &") ~ str_extract(value, "[0-9]+ "),
                                 str_detect(value,"3rd &") ~ str_extract(value, "[0-9]+ "),
                                 str_detect(value,"4th &") ~ str_extract(value, "[0-9]+ "))) |> 
    mutate(field_side = case_when(str_detect(value,"1st &") ~ str_extract(value, "[A-Z]+"),
                                  str_detect(value,"2nd &") ~ str_extract(value, "[A-Z]+"),
                                  str_detect(value,"3rd &") ~ str_extract(value, "[A-Z]+"),
                                  str_detect(value,"4th &") ~ str_extract(value, "[A-Z]+"))) |> 
    mutate(yard_line = case_when(str_detect(value,"1st &") ~ str_extract(value, "[0-9]+$"),
                                 str_detect(value,"2nd &") ~ str_extract(value, "[0-9]+$"),
                                 str_detect(value,"3rd &") ~ str_extract(value, "[0-9]+$"),
                                 str_detect(value,"4th &") ~ str_extract(value, "[0-9]+$"))) |> 
    mutate(down = lag(down),
           yds_to_go = lag(as.numeric(yds_to_go)),
           field_side = lag(field_side),
           yard_line = lag(yard_line)) |> 
    mutate(pass = if_else(str_detect(value, regex("pass complete|pass incomplete|intercepted|sacked",ignore_case = TRUE)),1,0),
          rush = if_else(str_detect(value, "rush"),1,0),
          punt = if_else(str_detect(value, "punt"),1,0),
          punt_return = if_else(punt == 1 & str_detect(value, regex("return",ignore_case = TRUE)),1,0),
          punt_muffed = if_else(punt == 1 & str_detect(value, regex("muff",ignore_case = TRUE)),1,0),
          kickoff_play = if_else(str_detect(value, "kickoff"),1,0),
          incompletion = if_else(pass == 1 & str_detect(value,regex("incomplete",ignore_case = TRUE)),1,0),
          completion = if_else(incompletion == 0 & str_detect(value,regex("complete",ignore_case = TRUE))
                            & !str_detect(value,regex("no play",ignore_case = TRUE)),1,0),
          touchdown_play = if_else(str_detect(value, regex("touchdown", ignore_case = TRUE)) &
                                                !str_detect(value, regex("no play", ignore_case = TRUE)),1,0),
          passing_touchdown = if_else(pass == 1 & touchdown_play == 1 & !str_detect(value,regex("fumble",ignore_case = TRUE))
                                      &!str_detect(value, regex("intercepted",ignore_case = TRUE)), 1,0),
          rushing_touchdown = if_else(rush == 1 & touchdown_play == 1 & !str_detect(value,regex("fumble|fumbled|touchdown nullified",ignore_case = TRUE)), 1,0),
          receiving_touchdown = passing_touchdown,
          field_goal = if_else(str_detect(value, regex("field goal", ignore_case = TRUE)),1,0),
          field_goal_made = if_else(field_goal == 1 & str_detect(value, regex("good", ignore_case = TRUE))
                                                                 & !str_detect(value,regex("no good",ignore_case = TRUE)),
                                    1,0),
          field_goal_missed = if_else(field_goal == 1 & str_detect(value, regex("no good", ignore_case = TRUE)),
                                    1,0),
          fumble = if_else(str_detect(value, regex("fumble", ignore_case = TRUE)),1,0),
          fumble_forced = if_else(fumble == 1 & str_detect(value, regex("fumble forced", ignore_case = TRUE)) | 
                                   fumble == 1 & str_detect(value, regex("forced", ignore_case = TRUE)) ,1,0),
          punt_return_td = if_else(punt == 1 &
                                     str_detect(value, regex("touchdown", ignore_case = TRUE)),
                                   1,0),
          kickoff_return_td = if_else(kickoff_play == 1 &
                                        str_detect(value, regex("touchdown", ignore_case = TRUE)),
                                      1,0),
          kickoff_touchback = if_else(kickoff_play == 1 & str_detect(value,regex("touchback", ignore_case = TRUE)),1,0),
          punt_downed = if_else(punt == 1 & 
                                  str_detect(value, regex("downed", ignore_case = TRUE)),1,0),
          penalty = if_else(str_detect(value, regex("penalty", ignore_case = TRUE)),1,0),
          penalty_declined = if_else(penalty == 1 & 
                                       str_detect(value, regex("declined", ignore_case = TRUE)),1,0),
          penalty_accepted = if_else(penalty == 1 & penalty_declined == 0,1,0),
          no_play = if_else( 
                              str_detect(value, regex("no play|offsides|false start|delay of game|illegal participation|illegal forward pass", 
                                                      ignore_case = TRUE)),1,0),
          extra_point_attempt = if_else(str_detect(value, regex("kick attempt", ignore_case = TRUE)),1,0),
          extra_point_made = if_else(str_detect(value, regex("kick attempt good", ignore_case = TRUE)),1,0),
          extra_point_missed = if_else(str_detect(value, regex("kick attempt no good", ignore_case = TRUE)),1,0),
          first_down = if_else(str_detect(value,regex("1st down",ignore_case = TRUE)),1,0),
          first_down_pass = if_else(pass == 1 & first_down == 1,1,0),
          first_down_rush = if_else(rush == 1 & first_down == 1,1,0),
          third_down_failed = if_else(down == 3 & first_down == 0 & touchdown_play == 0,1,0),
          third_down_converted = if_else(down == 3 & first_down == 1,1,0),
          fourth_down_attempt = if_else(down == 4 & punt == 0,1,0),
          fourth_down_failed = if_else(fourth_down_attempt == 1 & first_down == 0,1,0),
          fourth_down_converted = if_else(down == 4 & first_down == 1,1,0),
          sack = if_else(str_detect(value, regex("sacked", ignore_case = TRUE)),1,0),
          interception = if_else(str_detect(value,"intercepted") & !str_detect(value,regex("no play",ignore_case = TRUE)),1,0),
          kickoff_fair_catch = if_else(kickoff_play == 1 & str_detect(value, regex("fair caught", ignore_case = TRUE)),1,0),
          kickoff_out_of_bounds = if_else(kickoff_play == 1 & str_detect(value, regex("out of bounds")),1,0),
          kickoff_return = if_else(kickoff_play == 1 & str_detect(value,regex("return",ignore_case = TRUE)),1,0),
          punt_fair_catch = if_else(punt == 1 & str_detect(value, regex("fair catch", ignore_case = TRUE)),1,0),
          punt_out_of_bound = if_else(punt == 1 & str_detect(value, regex("out of bounds",ignore_case = TRUE)) |
                                                               str_detect(value,regex("out-of-bounds", ignore_case = TRUE)),1,0),
          punt_touchback = if_else(punt == 1 & str_detect(value,regex("touchback",ignore_case = TRUE)),1,0),
          punt_blocked = if_else(punt == 1 & str_detect(value, regex("blocked",ignore_case = TRUE)),1,0),
          punt_return = if_else(punt == 1 & str_detect(value,regex("return",ignore_case = TRUE)),1,0),
          punt_return_td = if_else(punt_return == 1 & str_detect(value,regex("touchdown",ignore_case = TRUE)),1,0),
          field_goal_blocked = if_else(field_goal == 1 & str_detect(value,regex("blocked",ignore_case = TRUE)),1,0),
          extra_point_blocked = if_else(extra_point_attempt == 1 & str_detect(value,regex("blocked",ignore_case = TRUE)),1,0),
          shotgun = if_else(str_detect(value, regex("shotgun",ignore_case = TRUE)),1,0),
          no_huddle = if_else(str_detect(value,regex("no huddle",ignore_case = TRUE)),1,0),
          change_of_poss = if_else(punt == 1 | kickoff_play == 1,1,0),
          turnover_downs = if_else(str_detect(value, regex("turnover on downs",ignore_case = TRUE)),1,0),
          end_of_half = if_else(str_detect(value, regex("end of half|end of game", ignore_case = TRUE)),1,0),
          first_by_penalty = if_else(penalty_accepted == 1 & first_down == 1,1,0),
          first_by_yards = if_else(rush == 1 & first_down == 1 | pass == 1 & first_down == 1,1,0),
          pass_attempt = if_else(pass == 1 & sack == 0 & no_play == 0,1,0),
          pass_breakup = if_else(pass_attempt == 1 & str_detect(value,regex("broken up",ignore_case = TRUE)),1,0),
          interception_return_touchdown = if_else(interception == 1 & str_detect(value, regex("touchdown",ignore_case = TRUE)),1,0),
          fumble_return_touchdown = if_else(fumble == 1 & str_detect(value,regex("touchdown",ignore_case = TRUE)),1,0),
          safety = if_else(str_detect(value,regex("safety",ignore_case = TRUE)),1,0),
          field_goal_return_touchdown = if_else(field_goal == 1 & str_detect(value,regex("touchdown",ignore_case = TRUE)),1,0),
          qb_hurry = if_else(str_detect(value,regex("QB hurried",ignore_case = TRUE)) |
                               str_detect(value, regex("QB hurry", ignore_case = TRUE)),1,0),
          rush_attempt = if_else(rush == 1 & no_play == 0 & !str_detect(value, regex("rush attempt",ignore_case = TRUE)),1,0),
          timeout = if_else(str_detect(value, regex("timeout",ignore_case = TRUE)),1,0),
          qb_kneel = if_else(str_detect(value, regex("kneel",ignore_case = TRUE)),1,0),
          fumble_return = if_else(fumble == 1 & str_detect(value, regex("return",ignore_case = TRUE)),1,0),
          qb_spike = if_else(pass == 1 & str_detect(value, regex("spike", ignore_case = TRUE)),1,0),
          two_point_attempt = if_else(str_detect(value,regex("rush attempt|pass attempt", ignore_case = TRUE)),1,NA_integer_),
          two_point_success = if_else(two_point_attempt == 1 & str_detect(value,regex("success",ignore_case = TRUE)),1,NA_integer_),
          two_point_failed = if_else(two_point_attempt == 1 & str_detect(value, regex("fail",ignore_case = TRUE)),1,NA_integer_))
  
  
  
}


test_ds <- binary_d2_scraper_variables(5366160)


view(test_ds)


binary_d2_scraper_variables_ot_fix <- function(url) {
  d2_pbp <- glue::glue({url})
  d2_pbp_page <- read_html(d2_pbp)
  ##### function to get all of the binary columns from NCAA STATS Org play by play data
  d2_tbl <- d2_pbp_page |> 
    html_elements("section") |> 
    html_nodes(xpath = '//*[@id="OT"]') |> 
    html_table() |> 
    #   html_text2() |>
    list_rbind() |> 
    janitor::clean_names() |> 
    mutate(
      #start_of_2nd_half = if_else(str_detect(start_of_2nd_half, regex("Play overturned",ignore_case = TRUE)),
      #                    str_extract(start_of_2nd_half,"The previous play.*"),
      #                   start_of_2nd_half),
      start_of_2nd_half = if_else(str_detect(start_of_2nd_half,regex("play overturned.",ignore_case = TRUE)),
                      str_remove(start_of_2nd_half,regex("play overturned.*",ignore_case = TRUE)),
                      start_of_2nd_half),
      down = case_when(str_detect(start_of_2nd_half,"1st and") ~ str_extract(start_of_2nd_half, "[0-9]+"),
                       str_detect(start_of_2nd_half,"2nd and") ~ str_extract(start_of_2nd_half, "[0-9]+"),
                       str_detect(start_of_2nd_half,"3rd and") ~ str_extract(start_of_2nd_half, "[0-9]+"),
                       str_detect(start_of_2nd_half,"4th and") ~ str_extract(start_of_2nd_half, "[0-9]+"))) |> 
    mutate(yds_to_go = case_when(str_detect(start_of_2nd_half,"1st and") ~ str_extract(start_of_2nd_half, "[0-9]+ "),
                                 str_detect(start_of_2nd_half,"2nd and") ~ str_extract(start_of_2nd_half, "[0-9]+ "),
                                 str_detect(start_of_2nd_half,"3rd and") ~ str_extract(start_of_2nd_half, "[0-9]+ "),
                                 str_detect(start_of_2nd_half,"4th and") ~ str_extract(start_of_2nd_half, "[0-9]+ "))) |> 
    mutate(field_side = case_when(str_detect(start_of_2nd_half,"1st and") ~ str_extract(start_of_2nd_half, "[A-Z]+"),
                                  str_detect(start_of_2nd_half,"2nd and") ~ str_extract(start_of_2nd_half, "[A-Z]+"),
                                  str_detect(start_of_2nd_half,"3rd and") ~ str_extract(start_of_2nd_half, "[A-Z]+"),
                                  str_detect(start_of_2nd_half,"4th and") ~ str_extract(start_of_2nd_half, "[A-Z]+"))) |> 
    mutate(yard_line = case_when(str_detect(start_of_2nd_half,"1st and") ~ str_extract(start_of_2nd_half, "[0-9]+$"),
                                 str_detect(start_of_2nd_half,"2nd and") ~ str_extract(start_of_2nd_half, "[0-9]+$"),
                                 str_detect(start_of_2nd_half,"3rd and") ~ str_extract(start_of_2nd_half, "[0-9]+$"),
                                 str_detect(start_of_2nd_half,"4th and") ~ str_extract(start_of_2nd_half, "[0-9]+$"))) |> 
 #   mutate(down = lag(down),
  #         yds_to_go = lag(as.numeric(yds_to_go)),
    #       field_side = lag(field_side),
     #      yard_line = lag(yard_line)) |> 
    mutate(pass = if_else(str_detect(x2, regex("pass complete|pass incomplete|intercepted|sacked",ignore_case = TRUE)),1,0),
           rush = if_else(str_detect(x2, "rush"),1,0),
           punt = if_else(str_detect(x2, "punt"),1,0),
           punt_return = if_else(punt == 1 & str_detect(x2, regex("return",ignore_case = TRUE)),1,0),
           punt_muffed = if_else(punt == 1 & str_detect(x2, regex("muff",ignore_case = TRUE)),1,0),
           kickoff_play = if_else(str_detect(x2, "kickoff"),1,0),
           incompletion = if_else(pass == 1 & str_detect(x2,regex("incomplete",ignore_case = TRUE)),1,0),
           completion = if_else(incompletion == 0 & str_detect(x2,regex("complete",ignore_case = TRUE))
                                & !str_detect(x2,regex("no play",ignore_case = TRUE)),1,0),
           touchdown_play = if_else(str_detect(x2, regex("touchdown", ignore_case = TRUE)) &
                                      !str_detect(x2, regex("no play", ignore_case = TRUE)),1,0),
           passing_touchdown = if_else(pass == 1 & touchdown_play == 1 & !str_detect(x2,regex("fumble",ignore_case = TRUE))
                                       &!str_detect(x2, regex("intercepted",ignore_case = TRUE)), 1,0),
           rushing_touchdown = if_else(rush == 1 & touchdown_play == 1 & !str_detect(x2,regex("fumble|fumbled|touchdown nullified",ignore_case = TRUE)), 1,0),
           receiving_touchdown = passing_touchdown,
           field_goal = if_else(str_detect(x2, regex("field goal", ignore_case = TRUE)),1,0),
           field_goal_made = if_else(field_goal == 1 & str_detect(x2, regex("good", ignore_case = TRUE))
                                     & !str_detect(x2,regex("no good",ignore_case = TRUE)),
                                     1,0),
           field_goal_missed = if_else(field_goal == 1 & str_detect(x2, regex("no good", ignore_case = TRUE)),
                                       1,0),
           fumble = if_else(str_detect(x2, regex("fumble", ignore_case = TRUE)),1,0),
           fumble_forced = if_else(fumble == 1 & str_detect(x2, regex("fumble forced", ignore_case = TRUE)) | 
                                     fumble == 1 & str_detect(x2, regex("forced", ignore_case = TRUE)) ,1,0),
           punt_return_td = if_else(punt == 1 &
                                      str_detect(x2, regex("touchdown", ignore_case = TRUE)),
                                    1,0),
           kickoff_return_td = if_else(kickoff_play == 1 &
                                         str_detect(x2, regex("touchdown", ignore_case = TRUE)),
                                       1,0),
           kickoff_touchback = if_else(kickoff_play == 1 & str_detect(x2,regex("touchback", ignore_case = TRUE)),1,0),
           punt_downed = if_else(punt == 1 & 
                                   str_detect(x2, regex("downed", ignore_case = TRUE)),1,0),
           penalty = if_else(str_detect(x2, regex("penalty", ignore_case = TRUE)),1,0),
           penalty_declined = if_else(penalty == 1 & 
                                        str_detect(x2, regex("declined", ignore_case = TRUE)),1,0),
           penalty_accepted = if_else(penalty == 1 & penalty_declined == 0,1,0),
           no_play = if_else(penalty == 1 & penalty_declined == 0 & 
                               str_detect(x2, regex("no play|offsides|false start|delay of game|illegal participation|illegal forward pass", 
                                                       ignore_case = TRUE)),1,0),
           extra_point_attempt = if_else(str_detect(x2, regex("kick attempt", ignore_case = TRUE)),1,0),
           extra_point_made = if_else(str_detect(x2, regex("kick attempt good", ignore_case = TRUE)),1,0),
           extra_point_missed = if_else(str_detect(x2, regex("kick attempt no good", ignore_case = TRUE)),1,0),
           first_down = if_else(str_detect(x2,regex("1st down",ignore_case = TRUE)),1,0),
           first_down_pass = if_else(pass == 1 & first_down == 1,1,0),
           first_down_rush = if_else(rush == 1 & first_down == 1,1,0),
           third_down_failed = if_else(down == 3 & first_down == 0 & touchdown_play == 0,1,0),
           third_down_converted = if_else(down == 3 & first_down == 1,1,0),
           fourth_down_attempt = if_else(down == 4 & punt == 0,1,0),
           fourth_down_failed = if_else(fourth_down_attempt == 1 & first_down == 0,1,0),
           fourth_down_converted = if_else(down == 4 & first_down == 1,1,0),
           sack = if_else(str_detect(x2, regex("sacked", ignore_case = TRUE)),1,0),
           interception = if_else(str_detect(x2,"intercepted") & !str_detect(x2,regex("no play",ignore_case = TRUE)),1,0),
           kickoff_fair_catch = if_else(kickoff_play == 1 & str_detect(x2, regex("fair caught", ignore_case = TRUE)),1,0),
           kickoff_out_of_bounds = if_else(kickoff_play == 1 & str_detect(x2, regex("out of bounds")),1,0),
           kickoff_return = if_else(kickoff_play == 1 & str_detect(x2,regex("return",ignore_case = TRUE)),1,0),
           punt_fair_catch = if_else(punt == 1 & str_detect(x2, regex("fair catch", ignore_case = TRUE)),1,0),
           punt_out_of_bound = if_else(punt == 1 & str_detect(x2, regex("out of bounds",ignore_case = TRUE)) |
                                         str_detect(x2,regex("out-of-bounds", ignore_case = TRUE)),1,0),
           punt_touchback = if_else(punt == 1 & str_detect(x2,regex("touchback",ignore_case = TRUE)),1,0),
           punt_blocked = if_else(punt == 1 & str_detect(x2, regex("blocked",ignore_case = TRUE)),1,0),
           punt_return = if_else(punt == 1 & str_detect(x2,regex("return",ignore_case = TRUE)),1,0),
           punt_return_td = if_else(punt_return == 1 & str_detect(x2,regex("touchdown",ignore_case = TRUE)),1,0),
           field_goal_blocked = if_else(field_goal == 1 & str_detect(x2,regex("blocked",ignore_case = TRUE)),1,0),
           extra_point_blocked = if_else(extra_point_attempt == 1 & str_detect(x2,regex("blocked",ignore_case = TRUE)),1,0),
           shotgun = if_else(str_detect(x2, regex("shotgun",ignore_case = TRUE)),1,0),
           no_huddle = if_else(str_detect(x2,regex("no huddle",ignore_case = TRUE)),1,0),
           change_of_poss = if_else(punt == 1 | kickoff_play == 1,1,0),
           turnover_downs = if_else(str_detect(x2, regex("turnover on downs",ignore_case = TRUE)),1,0),
           end_of_half = if_else(str_detect(x2, regex("end of half|end of game", ignore_case = TRUE)),1,0),
           first_by_penalty = if_else(penalty_accepted == 1 & first_down == 1,1,0),
           first_by_yards = if_else(rush == 1 & first_down == 1 | pass == 1 & first_down == 1,1,0),
           pass_attempt = if_else(pass == 1 & sack == 0 & no_play == 0 & !str_detect(x2, regex("intentional grounding", ignore_case = TRUE)),1,0),
           pass_breakup = if_else(pass_attempt == 1 & str_detect(x2,regex("broken up",ignore_case = TRUE)),1,0),
           interception_return_touchdown = if_else(interception == 1 & str_detect(x2, regex("touchdown",ignore_case = TRUE)),1,0),
           fumble_return_touchdown = if_else(fumble == 1 & str_detect(x2,regex("touchdown",ignore_case = TRUE)),1,0),
           safety = if_else(str_detect(x2,regex("safety",ignore_case = TRUE)),1,0),
           field_goal_return_touchdown = if_else(field_goal == 1 & str_detect(x2,regex("touchdown",ignore_case = TRUE)),1,0),
           qb_hurry = if_else(str_detect(x2,regex("QB hurried",ignore_case = TRUE)) |
                                str_detect(x2, regex("QB hurry", ignore_case = TRUE)),1,0),
           rush_attempt = if_else(rush == 1 & no_play == 0 & !str_detect(x2, regex("rush attempt",ignore_case = TRUE)),1,0),
           timeout = if_else(str_detect(x2, regex("timeout",ignore_case = TRUE)),1,0),
           qb_kneel = if_else(str_detect(x2, regex("kneel",ignore_case = TRUE)),1,0),
           fumble_return = if_else(fumble == 1 & str_detect(x2, regex("return",ignore_case = TRUE)),1,0),
           qb_spike = if_else(pass == 1 & str_detect(x2, regex("spike", ignore_case = TRUE)),1,0),
           two_point_attempt = if_else(str_detect(x2,regex("rush attempt|pass attempt", ignore_case = TRUE)),1,NA_integer_),
           two_point_success = if_else(two_point_attempt == 1 & str_detect(x2,regex("success",ignore_case = TRUE)),1,NA_integer_),
           two_point_failed = if_else(two_point_attempt == 1 & str_detect(x2, regex("fail",ignore_case = TRUE)),1,NA_integer_))
  
  
  
}





d2_pbp <- "https://www.nguathletics.com/sports/football/stats/2024/delta-state-university/boxscore/6993"
d2_pbp_page <- read_html(d2_pbp)
##### function to get all of the binary columns from NCAA STATS Org play by play data
d2_tbl <- d2_pbp_page |> 
  html_elements("section") |> 
  html_nodes(xpath = '//*[@id="OT"]') |> 
  html_table() |> 
  #   html_text2() |>
  list_rbind() |> 
  janitor::clean_names()

class(d2_tbl)
view(d2_tbl)

