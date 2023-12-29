#### Title: TPL Weekly Stats Tranformation
#### Purpose: Clean and transform variables for visualization
#### Author: Vivien Yuen
#### Date: 19/11/2023

# Set Up ----
# Load packages
clean_weekly_stats <- function()
{
    set.seed(12345)

    # Load data
    # Primary sheet
    weekly_data <- s21_boxscores
    game_info <- s21_game_IDs


    # Get list of variables
    vtable(weekly_data, lush = TRUE)

    weekly_data_clean <- weekly_data %>%
        dplyr::rename(
            throwaways = tas,
            other_touches = touches,
            second_assists = '2nd_assists'
        )

    # Create Variables  ----

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

    # Create week_number column

    weekly_data_clean <- weekly_data_clean %>%
        mutate(date = mdy(date)) %>%
        arrange(date) %>%
        group_by(year_week = format(date, "%Y-%U")) %>%
        mutate(week_number = cur_group_id())


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

    zevGameId <- unique(weekly_data_clean$gameId)
    zevGameName <- unique(weekly_data_clean$gameIdNames)

    zevList <- setNames(zevGameId, zevGameName)

    gameIdNames <- list(
        gameId = unique(weekly_data_clean$gameId),
        gameName = unique(weekly_data_clean$gameIdNames)
    )

    # Apply labels

    weekly_data_clean <- weekly_data_clean %>%
        apply_labels(
            teamId = teamNames,
            homeTeamId = teamNames,
            awayTeamId = teamNames
        )
}
