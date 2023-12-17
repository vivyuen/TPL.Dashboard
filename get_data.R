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

game_events_sequence <- game_dict$get(paste(gameId,awayTeamId, sep ='/'))
stat_table <-data.frame()

df <- data.frame(gameId=character(),
                 playerId=character(),
                 teamId=character(),
                 Goal=integer(),
                 Assist=integer(),
                 Assist2=integer(),
                 D=integer(),
                 TA=integer(),
                 Drop=integer(),
                 passes=integer())

for (event in game_events_sequence) {
    if (nrow(df %>% filter(gameId == event$gameId, playerId == event$player$id, teamId==event$teamId)) == 0) {
        df[nrow(df) + 1,] <-c( event$gameId,
                        event$player$id,
                        event$teamId,
                        as.integer(0),
                        as.integer(0),
                        as.integer(0),
                        as.integer(0),
                        as.integer(0),
                        as.integer(0),
                        as.integer(0)
                       )
    }

    row = df %>% filter(gameId == event$gameId, playerId == event$player$id, teamId==event$teamId)
    if (event$eventType == "Goal" ) {
        df <- df %>% mutate(
            Goal = case_when(
                (gameId == event$gameId & playerId == event$player$id) ~ as.integer(row[["Goal"]]) + 1
            )
        )
    }
    if (event$eventType == "Assist" ) {
        df <- df %>% mutate(
            Assist = case_when(
                (gameId == event$gameId & playerId == event$player$id) ~ as.integer(row[["Assist"]]) + 1
            )
        )
    }
    if (event$eventType == "2nd Assist" ) {
        df <- df %>% mutate(
            Assist2 = case_when(
                (gameId == event$gameId & playerId == event$player$id) ~ as.integer(row[["Assist2"]]) + 1
            )
        )
    }
    if (event$eventType == "D" ) {
        df <- df %>% mutate(
            D = case_when(
                (gameId == event$gameId & playerId == event$player$id) ~ as.integer(row[["D"]]) + 1
            )
        )
    }
    if (event$eventType == "TA" ) {
        df <- df %>% mutate(
            TA = case_when(
                (gameId == event$gameId & playerId == event$player$id) ~ as.integer(row[["TA"]]) + 1
            )
        )
    }
    if (event$eventType == "Drop" ) {
        df <- df %>% mutate(
            Drop = case_when(
                (gameId == event$gameId & playerId == event$player$id) ~ as.integer(row[["Drop"]]) + 1
            )
        )       }
    if (event$eventType == "" ) {
        df <- df %>% mutate(
            passes = case_when(
                (gameId == event$gameId & playerId == event$player$id) ~ as.integer(row[["passes"]]) + 1
            )
        )
    }
}

df
# gameId,id,name,2nd_assists,assists,goals,blocks,tas,drops,touches
