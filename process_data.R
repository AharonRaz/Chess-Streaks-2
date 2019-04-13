# Extract data files
unzip("./data.zip", overwrite=F)

# Process raw data
require(data.table)
files <- list.files("./data", pattern = "*.csv", full.names = T)
fi <- file.info(files)
files <- files[order(as.POSIXct(fi$ctime))]
cols <- colnames(fread(files[1]))
data <- data.table(id = 1:(length(files)*50))
data[,(cols):= ""]
p <- 1

for (i in 1:length(files)) {
    cat(sprintf("Reading file %d of %d . . .   ", i, length(files)), end="\r")
    f <- fread(files[i], colClasses="character")
    data[p:(p+nrow(f)-1), (cols):=f]
    p = p + nrow(f)
}
cat("Done!", paste(rep(" ", 50), collapse = ""))
fwrite(data[1:p,2:ncol(data)], "data.csv", row.names = F)

rm(list=setdiff(ls(), "data"))
data <- read.csv("data.csv")
data <- data[complete.cases(data),]
ptest <- 0.1 # Percentage of games to mark for test set

# Compute streaks
players <- unique(data$player)
time_categories <- unique(data$time_category)
for (p in 1:length(players)) {
    cat("Processing player", p, "of", length(players), "(0%)", end="                    \r")
    player <- players[p]
    total_rows <- sum(data$player == player)
    rows_done <- 0
    
    for (tc in time_categories) {
        rows <- data$player == player & data$time_category == tc
        if (sum(rows) < 2) next
        player_data <- data[rows,]
        player_data$new_day <- c(player_data$date[-nrow(player_data)] != player_data$date[-1], F)
        player_data$new_opponent <- c(player_data$opponent[-nrow(player_data)] != player_data$opponent[-1], F)
        n_test <- round(ptest*nrow(player_data))
        player_data$test <- 0
        player_data[1:n_test, "test"] <- 1
        player_data[,"streak"] <- 1
        player_data[,"win_streak"] <- 0
        player_data[,"lose_streak"] <- 0
        player_data[,"draw_streak"] <- 0
        player_data[,"streak_daily"] <- 1
        player_data[,"win_streak_daily"] <- 0
        player_data[,"lose_streak_daily"] <- 0
        player_data[,"draw_streak_daily"] <- 0
        player_data[,"streak_opponent"] <- 1
        player_data[,"win_streak_opponent"] <- 0
        player_data[,"lose_streak_opponent"] <- 0
        player_data[,"draw_streak_opponent"] <- 0
        
        for (i in (nrow(player_data) - 1): 1) {
            rows_done <- rows_done + 1
            cat("Processing player", p, "of", length(players), sprintf("(%s - %.0f%% completed)", as.character(player), rows_done/total_rows*100), end="                              \r")
            if (player_data[i, "result"] == player_data[i + 1, "result"]) {
                player_data[i, "streak"] <- player_data[i + 1, "streak"] + 1
                if (!player_data[i, "new_day"]) {
                    player_data[i, "streak_daily"] <- player_data[i + 1, "streak_daily"] + 1
                }
                if (player_data[i, "opponent"] == player_data[i + 1, "opponent"]) {
                    player_data[i, "streak_opponent"] <- player_data[i + 1, "streak_opponent"] + 1
                }
            }
        }
        
        player_data[,"win_streak"] <- player_data[,"streak"] * (player_data$result == "Won")
        player_data[,"lose_streak"] <- player_data[,"streak"] * (player_data$result == "Lost")
        player_data[,"draw_streak"] <- player_data[,"streak"] * (player_data$result == "Draw")
        
        player_data[,"win_streak_daily"] <- player_data[,"streak_daily"] * (player_data$result == "Won")
        player_data[,"lose_streak_daily"] <- player_data[,"streak_daily"] * (player_data$result == "Lost")
        player_data[,"draw_streak_daily"] <- player_data[,"streak_daily"] * (player_data$result == "Draw")
        
        player_data[,"win_streak_opponent"] <- player_data[,"streak_opponent"] * (player_data$result == "Won")
        player_data[,"lose_streak_opponent"] <- player_data[,"streak_opponent"] * (player_data$result == "Lost")
        player_data[,"draw_streak_opponent"] <- player_data[,"streak_opponent"] * (player_data$result == "Draw")
        
        # Shift streaks to predict the next trial
        player_data[,"streak"] <- c(player_data[-1,"streak"], 1)
        player_data[,"win_streak"] <- c(player_data[-1,"win_streak"], 0)
        player_data[,"lose_streak"] <- c(player_data[-1,"lose_streak"], 0)
        player_data[,"draw_streak"] <- c(player_data[-1,"draw_streak"], 0)
        
        player_data[,"streak_daily"] <- c(player_data[-1,"streak_daily"], 1)
        player_data[,"win_streak_daily"] <- c(player_data[-1,"win_streak_daily"], 0)
        player_data[,"lose_streak_daily"] <- c(player_data[-1,"lose_streak_daily"], 0)
        player_data[,"draw_streak_daily"] <- c(player_data[-1,"draw_streak_daily"], 0)
        
        player_data[,"streak_opponent"] <- c(player_data[-1,"streak_opponent"], 1)
        player_data[,"win_streak_opponent"] <- c(player_data[-1,"win_streak_opponent"], 0)
        player_data[,"lose_streak_opponent"] <- c(player_data[-1,"lose_streak_opponent"], 0)
        player_data[,"draw_streak_opponent"] <- c(player_data[-1,"draw_streak_opponent"], 0)
        
        # Save streaks to dataset
        data[rows, "test"] <- player_data$test
        data[rows, "new_day"] <- player_data$new_day
        data[rows, "new_opponent"] <- player_data$new_opponent
        
        data[rows, "streak"] <- player_data[,"streak"]
        data[rows, "win_streak"] <- player_data[,"win_streak"]
        data[rows, "lose_streak"] <- player_data[,"lose_streak"]
        data[rows, "draw_streak"] <- player_data[,"draw_streak"]
        
        data[rows, "streak_daily"] <- player_data[,"streak_daily"]
        data[rows, "win_streak_daily"] <- player_data[,"win_streak_daily"]
        data[rows, "lose_streak_daily"] <- player_data[,"lose_streak_daily"]
        data[rows, "draw_streak_daily"] <- player_data[,"draw_streak_daily"]
        
        data[rows, "streak_opponent"] <- player_data[,"streak_opponent"]
        data[rows, "win_streak_opponent"] <- player_data[,"win_streak_opponent"]
        data[rows, "lose_streak_opponent"] <- player_data[,"lose_streak_opponent"]
        data[rows, "draw_streak_opponent"] <- player_data[,"draw_streak_opponent"]
    }
}

# Prepare variables
data$win <- data$result == "Won"
data$draw <- data$result == "Draw"
data$lose <- data$result == "Lost"

# Fix ratings variables (some low ratings not processed correctly)
data$player_rating <- gsub("[()]", "", data$player_rating)
data$opponent_rating <- gsub("[()]", "", data$opponent_rating)

# Odd missing case
data <- data[complete.cases(data),]

rm(list=setdiff(ls(), "data"))
write.csv(data, "data_processed.csv", row.names = F)
