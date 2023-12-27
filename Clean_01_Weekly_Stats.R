#### Title: TPL Weekly Stats Tranformation
#### Purpose: Clean and transform variables for visualization
#### Author: Vivien Yuen
#### Date: 19/11/2023

# Set Up -----------------------------------------------------------------------
# Load packages
pacman::p_load(
    vtable,
    rio,
    here,
    janitor,
    lubridate,
    epikit,
    tidyverse,
    skimr,
    visdat,
    tidyxl,
    gtsummary,
    viridis,
    scales,
    gt
    )

set.seed(12345)

# Load data
# Primary sheet
weekly_data <- import(here("data/Weekly Individual Game Stats/per_game_stats.csv"))
weekly_data_clean <- weekly_data

# Get list of variables
vtable(weekly_data_clean, lush = TRUE)

weekly_data_clean <- weekly_data_clean %>%
    dplyr::rename(
        throwaways = tas,
        other_touches = touches,
        second_assists = '2nd_assists'
    )

# Create Variables  -----------------------------------------------------------------------

weekly_data_clean <- weekly_data_clean %>%
    arrange(gameId, teamId) %>%
    mutate(
        all_touches = other_touches + goals + assists + second_assists + drops,
        goal_contributions = goals + assists + second_assists,
        not_goal_contributions = all_touches - goal_contributions,
        turnovers = throwaways + drops,
        not_turnovers = other_touches + goals + assists + second_assists,
        gc_to_touches_ratio = goal_contributions / all_touches,
        turnovers_to_touches_ratio = turnovers / (turnovers + not_turnovers)
    )

weekly_data_clean <- weekly_data_clean %>%
    group_by(gameId, teamId) %>%
    mutate(
        percent_of_team_touches = all_touches / sum(all_touches),
        percent_of_team_contributions = goal_contributions / sum(goal_contributions)
    )

# weekly_data_clean <- weekly_data_clean %>%
#     mutate(
#         gc_to_touches_ratio = percent(gc_to_touches_ratio, accuracy = 0.01, scale = 100),
#         turnovers_to_touches_ratio = percent(turnovers_to_touches_ratio, accuracy = 0.01, scale = 100),
#         percent_of_team_touches = percent(percent_of_team_touches, accuracy = 0.01, scale = 100),
#         percent_of_team_contributions = percent(percent_of_team_contributions, accuracy = 0.01, scale = 100)
#     )
