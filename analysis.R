data <- read.csv("data_processed.csv")
n_total <- nrow(data)
n_total_players <- length(unique(data$player))
# Remove games with less than 10 moves
cat(sprintf("Games with less than 10 moves: %.2f%%\n", mean(data$moves < 10) * 100))
data <- data[data$moves >= 10,]

# Remove games from players with less than 100 games in a category
temp <- as.data.frame(table(data$time_category, data$player))
colnames(temp) <- c("time_category", "player", "ngames")
cat(sprintf("Instances of less than 100 games in category: %d\n", sum(temp$ngames < 100)))
data <- merge(data, temp, by = c("player", "time_category"))
data <- data[data$ngames >= 100,]
cat(sprintf("Players left: %d / %d\n", length(unique(data$player)), n_total_players))

# Removals summary
cat(sprintf("Games removed: %d / %d (%.2f%%)\n", n_total - nrow(data), nrow(data), (n_total - nrow(data)) / n_total * 100))

rm(list=setdiff(ls(), "data"))

# Compute expected game outcomes based on ratings
m <- glm(win~player_rating*opponent_rating, data, family=binomial)
summary(m)
cat(sprintf("win~ratings: r = %.3f, accuracy = %.2f%%, baseline accuracy = %.2f%%\n", cor(m$y, m$fitted.values), mean((m$fitted.values >= 0.5) == m$y) * 100, (0.5 + abs(mean(m$y) - 0.5)) * 100))
data$expected_outcome = m$fitted.values
data$overperformance <- (data$win - data$lose) / 2 - (data$expected_outcome - 0.5)

plot_streaks <- function(data, plot_legend=T, mar=c(5.1,4.1,1.1,1.1), ...) {
    win_win <- merge(aggregate(win~win_streak, data, mean), aggregate(win~win_streak, data, length), by = 1)
    win_win <- merge(win_win, aggregate(win~win_streak, data, sd))
    colnames(win_win) <- c("streak", "p", "n", "sd")
    win_lose <- merge(aggregate(win~lose_streak, data, mean), aggregate(win~lose_streak, data, length), by = 1)
    win_lose <- merge(win_lose, aggregate(win~lose_streak, data, sd), by = 1)
    colnames(win_lose) <- c("streak", "p", "n", "sd")
    
    # Compute standrd errors and 95% confidence intervals
    win_win$se <- win_win$sd / sqrt(win_win$n)
    win_win$lwr <- win_win$p - qnorm(0.975) * win_win$se
    win_win$upr <- win_win$p + qnorm(0.975) * win_win$se
    
    win_lose$se <- win_lose$sd / sqrt(win_lose$n)
    win_lose$lwr <- win_lose$p - qnorm(0.975) * win_lose$se
    win_lose$upr <- win_lose$p + qnorm(0.975) * win_lose$se
    
    par(mar=mar)
    x1 = 1:max(win_win[win_win$n >= 300, "streak"])
    y1 = win_win$p[x1+1]*100
    lwr1 = win_win$lwr[x1+1]*100
    upr1 = win_win$upr[x1+1]*100
    clr1 = rgb(0, 0.5, 0)
    x1 = x1 - 0.05
    
    x2 = 1:max(win_lose[win_lose$n >= 300, "streak"])
    y2 = win_lose$p[x2+1]*100
    lwr2 = win_lose$lwr[x2+1]*100
    upr2 = win_lose$upr[x2+1]*100
    clr2 = rgb(0.8, 0, 0)
    x2 = x2 + 0.05
    
    #lim <- range(upr1, lwr1, upr2, lwr2)
    #lim <- lim + diff(lim)*0.1*c(-1,1)
    lim <- c(0.2, 1) * 100
    
    plot(x1, y1, type="l", xlab = "Streak", ylab = "% Win", ylim = lim, col=clr1, ...)
    points(x1, y1, pch=19, col=clr1)
    segments(x1, lwr1, x1, upr1, col=clr1)
    segments(x1-0.05, lwr1, x1+0.05, lwr1, col=clr1)
    segments(x1-0.05, upr1, x1+0.05, upr1, col=clr1)
    
    lines(x2, y2, col=clr2)
    points(x2, y2, pch=19, col=clr2)
    segments(x2, lwr2, x2, upr2, col=clr2)
    segments(x2-0.05, lwr2, x2+0.05, lwr2, col=clr2)
    segments(x2-0.05, upr2, x2+0.05, upr2, col=clr2)
    abline(a=50, b = 0, col=rgb(0, 0, 0, 0.2), lty=2)
    if (plot_legend)
        legend("bottomright", c("Winning streak", "Losing streak"), lty=1, pch=19, col=c(rgb(0, 0.5, 0), rgb(0.8, 0, 0)), inset=0.01, box.lty=0)
}

plot_overperformance <- function(data, plot_legend=T, mar=c(5.1,4.1,1.1,1.1), ...) {
    win_win <- merge(aggregate(overperformance~win_streak, data, mean), aggregate(overperformance~win_streak, data, length), by = 1)
    win_win <- merge(win_win, aggregate(overperformance~win_streak, data, sd))
    colnames(win_win) <- c("streak", "p", "n", "sd")
    win_lose <- merge(aggregate(overperformance~lose_streak, data, mean), aggregate(overperformance~lose_streak, data, length), by = 1)
    win_lose <- merge(win_lose, aggregate(overperformance~lose_streak, data, sd), by = 1)
    colnames(win_lose) <- c("streak", "p", "n", "sd")
    
    # Compute standrd errors and 95% confidence intervals
    win_win$se <- win_win$sd / sqrt(win_win$n)
    win_win$lwr <- win_win$p - qnorm(0.975) * win_win$se
    win_win$upr <- win_win$p + qnorm(0.975) * win_win$se
    
    win_lose$se <- win_lose$sd / sqrt(win_lose$n)
    win_lose$lwr <- win_lose$p - qnorm(0.975) * win_lose$se
    win_lose$upr <- win_lose$p + qnorm(0.975) * win_lose$se
    
    par(mar=mar)
    x1 = 1:max(win_win[win_win$n >= 300, "streak"])
    y1 = win_win$p[x1+1] * 100
    lwr1 = win_win$lwr[x1+1] * 100
    upr1 = win_win$upr[x1+1] * 100
    clr1 = rgb(0, 0.5, 0)
    x1 = x1 - 0.05
    
    x2 = 1:max(win_lose[win_lose$n >= 300, "streak"])
    y2 = win_lose$p[x2+1] * 100
    lwr2 = win_lose$lwr[x2+1] * 100
    upr2 = win_lose$upr[x2+1] * 100
    clr2 = rgb(0.8, 0, 0)
    x2 = x2 + 0.05
    
    #lim <- range(upr1, lwr1, upr2, lwr2)
    #lim <- lim + diff(lim)*0.1*c(-1,1)
    lim <- c(-0.1, 0.2) * 100
    
    plot(x1, y1, type="l", xlab = "Streak", ylab = "Overperformance", ylim = lim, col=clr1, ...)
    points(x1, y1, pch=19, col=clr1)
    segments(x1, lwr1, x1, upr1, col=clr1)
    segments(x1-0.05, lwr1, x1+0.05, lwr1, col=clr1)
    segments(x1-0.05, upr1, x1+0.05, upr1, col=clr1)
    
    lines(x2, y2, col=clr2)
    points(x2, y2, pch=19, col=clr2)
    segments(x2, lwr2, x2, upr2, col=clr2)
    segments(x2-0.05, lwr2, x2+0.05, lwr2, col=clr2)
    segments(x2-0.05, upr2, x2+0.05, upr2, col=clr2)
    abline(a=0, b = 0, col=rgb(0, 0, 0, 0.2), lty=2)
    
    if (plot_legend)
        legend("bottomright", c("Winning streak", "Losing streak"), lty=1, pch=19, col=c(rgb(0, 0.5, 0), rgb(0.8, 0, 0)), inset=0.01, box.lty = 0)
}

run_analysis <- function(data, prefix) {
    # All streaks
    png(paste(prefix, "streaks.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_streaks(data)
    graphics.off()
    
    # First game on the next day following a streak
    png(paste(prefix, "streaks_new_day.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_streaks(data[data$new_day == T,])
    dev.off()
    
    # First game with new opponent following streak
    png(paste(prefix, "streaks_new_opponent.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_streaks(data[data$new_opponent == T,])
    graphics.off()
    
    # Games with same opponent
    png(paste(prefix, "streaks_same_opponent.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_streaks(data[data$new_opponent == F,])
    graphics.off()
    
    #
    # Taking elo into account
    #
    # All games
    png(paste(prefix, "overperformance.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_overperformance(data)
    graphics.off()
    
    # New day
    png(paste(prefix, "overperformance_new_day.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_overperformance(data[data$new_day == T,])
    graphics.off()
    
    # Against new opponents
    png(paste(prefix, "overperformance_new_opponent.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_overperformance(data[data$new_opponent == T,])
    graphics.off()
    
    # Against same opponents
    png(paste(prefix, "overperformance_same_opponent.png", sep="_"), width = 1500, height = 1000, res=200)
    plot_overperformance(data[data$new_opponent == F,])
    graphics.off()
}

run_analysis(data, "all")
run_analysis(data[data$time_category=="bullet",], "bullet")
run_analysis(data[data$time_category=="blitz",], "blitz")

#
# Combined plots
#
# All streaks
xlim <- c(1, 16)
png("grid_streaks.png", width = 1500, height = 1500, res=200)
layout(matrix(1:6, 3, 2, byrow=T), c(1.11, 1), c(1, 1, 1.22))
# All overperformance
plot_streaks(data[data$time_category == "bullet",], F, c(1.1,4.1,1.1,1.1), main="All - Bullet", cex.main = 0.9, xlim= c(1,16), cex.axis=0.9, xaxt="n")
plot_streaks(data[data$time_category == "blitz",], F, c(1.1,1.1,1.1,1.1), main="All - Blitz", cex.main = 0.9, yaxt="n", xlim= c(1,16), cex.axis=0.9, xaxt="n")
# New day (not interesting enough to clutter the graph)
#plot_streaks(data[data$new_day == T & data$time_category == "bullet",], F, c(1.1,4.1,1.1,1.1), main="New Day - Bullet", cex.main = 0.9, xlim=xlim, cex.axis=0.9, xaxt="n")
#plot_streaks(data[data$new_day == T & data$time_category == "blitz",], F, c(1.1,1.1,1.1,1.1), main="New Day - Blitz", cex.main = 0.9, yaxt="n", xlim=xlim, cex.axis=0.9, xaxt="n")
# New opponent
plot_streaks(data[data$new_opponent == T & data$time_category == "bullet",], F, c(1.1,4.1,1.1,1.1), main="New Opponent - Bullet", cex.main = 0.9, xlim=xlim, cex.axis=0.9, xaxt="n")
plot_streaks(data[data$new_opponent == T & data$time_category == "blitz",], F, c(1.1,1.1,1.1,1.1), main="New Opponent - Blitz", cex.main = 0.9, yaxt="n", xlim=xlim, cex.axis=0.9, xaxt="n")
# Same opponent
plot_streaks(data[data$new_opponent == F & data$time_category == "bullet",], F, c(5.1,4.1,1.1,1.1), main="Same Opponent - Bullet", cex.main = 0.9, xlim=xlim, cex.axis=0.9, xaxt="n")
axis(1, at = seq(2, 18, 2))
plot_streaks(data[data$new_opponent == F & data$time_category == "blitz",], F, c(5.1,1.1,1.1,1.1), main="Same Opponent - Blitz", cex.main = 0.9, yaxt="n", xlim=xlim, cex.axis=0.9, xaxt="n")
axis(1, at = seq(2, 18, 2))
graphics.off()

png("grid_overperformance.png", width = 1500, height = 1500, res=200)
layout(matrix(1:6, 3, 2, byrow=T), c(1.11, 1), c(1, 1, 1.22))
# All overperformance
plot_overperformance(data[data$time_category == "bullet",], F, c(1.1,4.1,1.1,1.1), main="All - Bullet", cex.main = 0.9, xlim= c(1,16), cex.axis=0.9, xaxt="n")
plot_overperformance(data[data$time_category == "blitz",], F, c(1.1,1.1,1.1,1.1), main="All - Blitz", cex.main = 0.9, yaxt="n", xlim= c(1,16), cex.axis=0.9, xaxt="n")
# New day (not interesting enough to clutter the graph)
#plot_overperformance(data[data$new_day == T & data$time_category == "bullet",], F, c(1.1,4.1,1.1,1.1), main="New Day - Bullet", cex.main = 0.9, xlim=xlim, cex.axis=0.9, xaxt="n")
#plot_overperformance(data[data$new_day == T & data$time_category == "blitz",], F, c(1.1,1.1,1.1,1.1), main="New Day - Blitz", cex.main = 0.9, yaxt="n", xlim=xlim, cex.axis=0.9, xaxt="n")
# New opponent
plot_overperformance(data[data$new_opponent == T & data$time_category == "bullet",], F, c(1.1,4.1,1.1,1.1), main="New Opponent - Bullet", cex.main = 0.9, xlim=xlim, cex.axis=0.9, xaxt="n")
plot_overperformance(data[data$new_opponent == T & data$time_category == "blitz",], F, c(1.1,1.1,1.1,1.1), main="New Opponent - Blitz", cex.main = 0.9, yaxt="n", xlim=xlim, cex.axis=0.9, xaxt="n")
# Same opponent
plot_overperformance(data[data$new_opponent == F & data$time_category == "bullet",], F, c(5.1,4.1,1.1,1.1), main="Same Opponent - Bullet", cex.main = 0.9, xlim=xlim, cex.axis=0.9, xaxt="n")
axis(1, at = seq(2, 18, 2))
plot_overperformance(data[data$new_opponent == F & data$time_category == "blitz",], F, c(5.1,1.1,1.1,1.1), main="Same Opponent - Blitz", cex.main = 0.9, yaxt="n", xlim=xlim, cex.axis=0.9, xaxt="n")
axis(1, at = seq(2, 18, 2))
graphics.off()

df <- merge(aggregate(overperformance~player*win_streak*lose_streak, data, mean), aggregate(overperformance~player*win_streak*lose_streak, data, sd), by = c("player", "win_streak", "lose_streak"))
df <- merge(df, aggregate(overperformance~player*win_streak*lose_streak, data, length), by = c("player", "win_streak", "lose_streak"))
colnames(df) <- c("player", "win_streak", "lose_streak", "m", "sd", "n")
df$z <- df$m / sqrt(df$n)
hist(df$z)
