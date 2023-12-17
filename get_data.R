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
    httr2,
    jsonlite,
    collections
)


library(httr2)

req <- request("https://tplapp.onrender.com/games/764")
resp <- req_perform(req)

games <- resp_body_json(resp)

game_id_to_date = dict()
for (game in games) {
    game_id_to_date$set(game$id, game$date)
}

game_dict <- dict()
for (game in games) {
    gameId <- game$id
    homeTeamId <- game$homeTeamId
    awayTeamId <- game$awayTeamId
    homeGameUrl <- paste("https://tplapp.onrender.com/gameEvents/",gameId,"/",homeTeamId, sep='')
    awayGameUrl <- paste("https://tplapp.onrender.com/gameEvents/",gameId,"/",awayTeamId, sep='')

    homeGameReq <- request(homeGameUrl)
    homeGameResp <- req_perform(homeGameReq)
    game_dict$set(paste(gameId,homeTeamId, sep='/'), resp_body_json(homeGameResp))

    awayGameReq <- request(awayGameUrl)
    awayGameResp <- req_perform(awayGameReq)
    game_dict$set(paste(gameId,awayTeamId, sep ='/'),resp_body_json(awayGameResp))
}


df <- data.frame(gameId=character(),
                 playerId=character(),
                 teamId=character(),
                 name=character(),
                 date=character(),
                 Goal=integer(),
                 Assist=integer(),
                 Assist2=integer(),
                 D=integer(),
                 TA=integer(),
                 Drop=integer(),
                 passes=integer())

for (game_team in game_dict$keys()) {
    game_id = str_split(a, "/")[[1]][1]
    team_id = str_split(a, "/")[[1]][2]
    game_events_sequence = game_dict$get(game_team)
    for (event in game_events_sequence) {
        if (nrow(df %>% filter(gameId == event$gameId, playerId == event$player$id, teamId==event$teamId)) == 0) {
            df[nrow(df) + 1,] <-c( event$gameId,
                                   event$player$id,
                                   event$teamId,
                                   event$player$playerName,
                                   game_id_to_date$get(game_id),
                                   as.integer(0),
                                   as.integer(0),
                                   as.integer(0),
                                   as.integer(0),
                                   as.integer(0),
                                   as.integer(0),
                                   as.integer(0)
            )
        }

        if (event$eventType == "Goal" ) {
            df <- df %>% mutate(
                Goal = case_when(
                    (gameId == event$gameId & playerId == event$player$id) ~ as.integer(Goal) + 1,
                    (!(gameId == event$gameId & playerId == event$player$id)) ~ as.integer(Goal)
                )
            )
        }
        if (event$eventType == "Assist" ) {
            df <- df %>% mutate(
                Assist = case_when(
                    (gameId == event$gameId & playerId == event$player$id) ~ as.integer(Assist) + 1,
                    (!(gameId == event$gameId & playerId == event$player$id)) ~ as.integer(Assist)

                )
            )
        }
        if (event$eventType == "2nd Assist" ) {
            df <- df %>% mutate(
                Assist2 = case_when(
                    (gameId == event$gameId & playerId == event$player$id) ~ as.integer(Assist2) + 1,
                    (!(gameId == event$gameId & playerId == event$player$id)) ~ as.integer(Assist2)

                )
            )
        }
        if (event$eventType == "D" ) {
            df <- df %>% mutate(
                D = case_when(
                    (gameId == event$gameId & playerId == event$player$id) ~ as.integer(D) + 1,
                    (!(gameId == event$gameId & playerId == event$player$id)) ~ as.integer(D)
                )
            )
        }
        if (event$eventType == "TA" ) {
            df <- df %>% mutate(
                TA = case_when(
                    (gameId == event$gameId & playerId == event$player$id) ~ as.integer(TA) + 1,
                    (!(gameId == event$gameId & playerId == event$player$id)) ~ as.integer(TA)
                )
            )
        }
        if (event$eventType == "Drop" ) {
            df <- df %>% mutate(
                Drop = case_when(
                    (gameId == event$gameId & playerId == event$player$id) ~ as.integer(Drop) + 1,
                    (!(gameId == event$gameId & playerId == event$player$id)) ~ as.integer(Drop)

                )
            )
        }
        if (event$eventType == "" ) {
            df <- df %>% mutate(
                passes = case_when(
                    (gameId == event$gameId & playerId == event$player$id) ~ as.integer(passes) + 1,
                    (!(gameId == event$gameId & playerId == event$player$id)) ~ as.integer(passes)
                )
            )
        }
    }
}


write_csv(df, "Data/Weekly Individual Game Stats/per_game_stats.csv")
# gameId,id,name,2nd_assists,assists,goals,blocks,tas,drops,touches
