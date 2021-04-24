library(ggplot2)
players <- read.csv("topchesslist.csv")


print("Top Chess Players DATASET")
print(players)


print("Type of Dataset : ")
print(class(players))
print(mode(players))
print(typeof(players))

obs <- nrow(players)
var <- ncol(players)
print("Number of observations : ")
print(obs)
print("Number of variables : ")
print(var)
attach(players)


print("Info about Standard Rating")
print(summary(players$Standard))


# Number of players by Country
agg_country <- aggregate(Standard ~ (Country), players, function(x) which.max(table(x)))
print(agg_country)
barplot(agg_country$Standard,
    width = 1, names.arg = agg_country$Country,
    horiz = FALSE, xlab = "country", ylab = "Number of Players", axes = TRUE,
    col = c("pink", "blue"), ylim = c(0, 20)
)
print("Different range of players by Country")
n <- c()
print("Countries with more than 5 players")
n[1] <- length(agg_country$Standard[agg_country$Standard > 5])
print(n[1])
print("Countries with 5 players")
n[2] <- length(agg_country$Standard[agg_country$Standard == 5])
print(n[2])
print("Countries with 4 players")
n[3] <- length(agg_country$Standard[agg_country$Standard == 4])
print(n[3])
print("Countries with 3 players")
n[4] <- length(agg_country$Standard[agg_country$Standard == 3])
print(n[4])
print("Countries with 2 players")
n[5] <- length(agg_country$Standard[agg_country$Standard == 2])
print(n[5])
print("Countries with only 1 player")
n[6] <- length(agg_country$Standard[agg_country$Standard == 1])
print(n[6])
pie(n, c(">5", "5", "4", "3", "2", "1"), main = "Distribution of Countries by Player Count", col = c("purple", "pink", "blue", "cyan", "green", "red"))


# Number of players by Standard Rating
print("Different range of Standard Ratings")
n <- c()
print("No. of players of rating 2500-2600")
n[1] <- length(Standard[Standard < 2600])
print(n[1])
print("No. of players of rating 2600-2700")
n[2] <- length(Standard[(Standard >= 2600) & (Standard < 2700)])
print(n[2])
print("No. of players of rating 2700-2800")
n[3] <- length(Standard[(Standard >= 2700) & (Standard < 2800)])
print(n[3])
print("No. of players of rating greater 2800")
n[4] <- length(Standard[Standard >= 2800])
print(n[4])
pie(n, c("2500-2600", "2600-2700", "2700-2800", ">=2800"), main = "Number of players of each rating range", col = c("purple", "pink", "blue", "cyan"))


# Number of players by Age Group
players_age <- players
players_age$Birth.Year <- 2020 - players_age$Birth.Year
print("Different Age Range of Players")
n <- c()
print("Number of players of Age less than 30")
n[1] <- length(players_age$Birth.Year[players_age$Birth.Year < 30])
print(n[1])
print("Number of players of Age 30-50")
n[2] <- length(players_age$Birth.Year[(players_age$Birth.Year >= 30) & (players_age$Birth.Year <= 50)])
print(n[2])
print("Number of players of Age greater than 50")
n[3] <- length(players_age$Birth.Year[players_age$Birth.Year > 50])
print(n[3])
pie(n, c("<30", "30-50", ">50"), main = "Number of players of each Age Group", col = c("purple", "pink", "cyan"))


# Comparison Between Standard and Blitz Ratings
players_ratings <- subset(players, select = c("Name", "Standard", "Blitz"))
print("Ratings of the top 5 players")
print(head(players_ratings, 5))

rating_gen <- data.frame(
    format <- c("Standard", "Standard", "Standard", "Standard", "Standard", "Blitz", "Blitz", "Blitz", "Blitz", "Blitz"),
    player <- c(
        "Carlsen, Magnus", "Caruana, Fabiano", "Ding, Liren", "Nepomniachtchi, Ian", "Vachier-Lagrave, Maxime",
        "Carlsen, Magnus", "Caruana, Fabiano", "Ding, Liren", "Nepomniachtchi, Ian", "Vachier-Lagrave, Maxime"
    ),
    rating <-
        c(
            players_ratings$Standard[1], players_ratings$Standard[2],
            players_ratings$Standard[3], players_ratings$Standard[4],
            players_ratings$Standard[5], players_ratings$Blitz[1],
            players_ratings$Blitz[2], players_ratings$Blitz[3],
            players_ratings$Blitz[4], players_ratings$Blitz[5]
        )
)
print(ggplot(rating_gen, aes(factor(player), rating, fill = format)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_brewer(palette = "Set1"))