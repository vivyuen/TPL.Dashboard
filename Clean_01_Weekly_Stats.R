#### Title: TPL Weekly Stats Tranformation
#### Purpose: Clean and transform variables for visualization
#### Author: Vivien Yuen
#### Date: 19/11/2023

# Set Up ----
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
    gt,
    expss,
    stringr
    )

set.seed(12345)

# Load data
# Primary sheet
weekly_data <- import(here("Data/Weekly Individual Game Stats/per_game_stats.csv"))
game_info <- import(here("Data/games.csv"))


# Get list of variables
vtable(weekly_data, lush = TRUE)

weekly_data_clean <- weekly_data %>%
    dplyr::rename(
        throwaways = tas,
        other_touches = touches,
        second_assists = '2nd_assists'
    )

# Create Variables  ----

Change player names to initials
initialsdf <- weekly_data_clean %>%
    select(name) %>%
    mutate(
        split_name = strsplit(name, " ")
    ) %>%
    mutate(
        first_initial = split_name[1]
    ) %>%
    view()

split_name = strsplit(initialsdf$name, " ")

for (fullname in split_name) {
    first_initial = fullname[[1]]
}



weekly_data_clean <- weekly_data_clean %>%
    mutate(
        initials = str_replace(name,
                               pattern = "^(\\w{1})(\\w+)\\s(\\w{1})(\\w+)$",
                               replace = "\\1\\3")
        )

weekly_data_clean %>%
    select(name, initials) %>%
    view()

# Metrics
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
    ) %>%
    ungroup()

# Cumulative season stats
cumulative_season_stats <- weekly_data_clean %>%
    group_by(name) %>%
    mutate(matches_played = n(),
           sum_goals = sum(goals),
           sum_assists = sum(assists),
           sum_sec_assists = sum(second_assists),
           sum_blocks = sum(blocks),
           sum_throwaways = sum(throwaways),
           sum_drops = sum(drops),
           sum_touches = sum(other_touches)
    ) %>%
    select(name, matches_played, sum_goals, sum_assists, sum_sec_assists, sum_blocks, sum_throwaways, sum_drops, sum_touches) %>%
    distinct() %>%
    ungroup()

# Season stats per game

per_game_stats <- cumulative_season_stats %>%
    mutate(goals_pg = num(sum_goals/matches_played, digits = 2),
           assists_pg = num(sum_assists/matches_played, digits = 2),
           sec_assists_pg = num(sum_sec_assists/matches_played, digits = 2),
           blocks_pg = num(sum_blocks/matches_played, digits = 2),
           TAs_pg = num(sum_throwaways/matches_played, digits = 2),
           drops_pg = num(sum_drops/matches_played, digits = 2),
           touches_pg = num(sum_touches/matches_played, digits = 2)) %>%
    select(name, matches_played, goals_pg, assists_pg, sec_assists_pg, blocks_pg, TAs_pg, drops_pg, touches_pg)

# Create week_number column

weekly_data_clean <- weekly_data_clean %>%
    mutate(date = mdy(date)) %>%
    arrange(date) %>%
    group_by(year_week = format(date, "%Y-%U")) %>%
    mutate(week_number = cur_group_id()) %>%
    ungroup()


# Apply variable and value labels ----

teamNames = c(
    "🔥 FaulkenHoeit 451 📚" = 9673,
    "📚📖 EncycLampedia 🤓 BriTangica 📖📚" = 9672,
    "🐒🤔💭Kyra-uos George and the Nam with the Yellow Hat 👷‍♂️👷‍♂️👷‍♂️ at the Aquarium 🐧🐧🐧" = 9675,
    "The CAT 🐈 in the 🧢 hATTANACH" = 9670,
    "ISBeN" = 9668,
    "The Children of Zune" = 9677,
    "🤔🌌👽 Hitchhucker’s Vol. 3: Mike, the Yueniverse and Everything 🚀📚🤔" = 9676,
    "MARTgaret CATwood" = 9669,
    "⚡Harry Peter⚡ and the Jaymber of Secrets🪄🐍🗡📓" = 9674,
    "Tam And A Mat" = 9671
)


# Create gameId labels

weekly_data_clean <- weekly_data_clean %>%
    mutate(gameIdNames = paste("Week", week_number, "-", homeTeamId, "vs.", awayTeamId, sep = " ")) %>%
    arrange(desc(week_number))

GameId <- unique(weekly_data_clean$gameId)
GameName <- unique(weekly_data_clean$gameIdNames)

ListofGames <- setNames(GameId, GameName)


# Apply labels

weekly_data_clean <- weekly_data_clean %>%
    apply_labels(
        teamId = teamNames,
        homeTeamId = teamNames,
        awayTeamId = teamNames
    )



