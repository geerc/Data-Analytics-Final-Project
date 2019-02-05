# Install packages and load libraries
# install.packages("Lahman")
# install.packages("tidyverse")
# install.packages("dpylr")
library(Lahman)
library(tidyverse)
library(dplyr)
library(psych)

####Creating Career Stats####
# Create table of career stats and add extra base hits and singles
career.stats <- Batting %>% 
  filter(G >= 150) %>%
  mutate(X1B = H - X2B - X3B - HR) %>%
  mutate(XBH = X2B + X2B + HR) %>%
  group_by(playerID) %>% 
  summarise(sum(G, na.rm = TRUE), sum(AB, na.rm = TRUE), sum(R, na.rm = TRUE), sum(H, na.rm = TRUE), sum(X1B, na.rm = TRUE), sum(XBH, na.rm = TRUE), sum(X2B, na.rm = TRUE), sum(X3B, na.rm = TRUE),sum(HR, na.rm = TRUE),sum(RBI, na.rm = TRUE), sum(BB, na.rm = TRUE), sum(HBP, na.rm = TRUE), sum(SB, na.rm = TRUE), sum(CS, na.rm = TRUE), sum(SO, na.rm = TRUE), sum(SF, na.rm = TRUE)) 

# Rename the columns
names(career.stats)[names(career.stats) == "sum(G, na.rm = TRUE)"] <- "G"
names(career.stats)[names(career.stats) == "sum(AB, na.rm = TRUE)"] <- "AB"
names(career.stats)[names(career.stats) == "sum(R, na.rm = TRUE)"] <- "R"
names(career.stats)[names(career.stats) == "sum(H, na.rm = TRUE)"] <- "H"
names(career.stats)[names(career.stats) == "sum(XBH, na.rm = TRUE)"] <- "XBH"
names(career.stats)[names(career.stats) == "sum(X2B, na.rm = TRUE)"] <- "X2B"
names(career.stats)[names(career.stats) == "sum(X3B, na.rm = TRUE)"] <- "X3B"
names(career.stats)[names(career.stats) == "sum(HR, na.rm = TRUE)"] <- "HR"
names(career.stats)[names(career.stats) == "sum(RBI, na.rm = TRUE)"] <- "RBI"
names(career.stats)[names(career.stats) == "sum(BB, na.rm = TRUE)"] <- "BB"
names(career.stats)[names(career.stats) == "sum(HBP, na.rm = TRUE)"] <- "HBP"
names(career.stats)[names(career.stats) == "sum(SB, na.rm = TRUE)"] <- "SB"
names(career.stats)[names(career.stats) == "sum(CS, na.rm = TRUE)"] <- "CS"
names(career.stats)[names(career.stats) == "sum(SO, na.rm = TRUE)"] <- "SO"
names(career.stats)[names(career.stats) == "sum(X1B, na.rm = TRUE)"] <- "X1B"
names(career.stats)[names(career.stats) == "sum(SF, na.rm = TRUE)"] <- "SF"

# Calcualate batting average, on base percentage, slugging percentage, on base + slugging, and stolen base percentage
career.stats <- career.stats %>%
  mutate(AVG = H / AB) %>%
  mutate(OBP = (H + BB + HBP) / (AB+BB+HBP+SF)) %>%
  mutate(SLG = ((X1B) + (2 * X2B) + (3 * X3B) + (4 * HR)) / AB) %>%
  mutate(OPS = OBP + SLG) %>%
  mutate(SB.pct = SB / (SB + CS)) %>%
  mutate(SO.pct = SO / AB)

# Round numbers where applicable
career.stats$AVG <- round(career.stats$AVG, digits = 3)
career.stats$SLG <- round(career.stats$SLG, digits = 3)
career.stats$OBP <- round(career.stats$OBP, digits = 3)
career.stats$OPS <- round(career.stats$OPS, digits = 3)
career.stats$SB.pct <- round(career.stats$SB.pct, digits = 2)
career.stats$SO.pct <- round(career.stats$SO.pct, digits = 2)

####Question 1: What statistical catagories correlate with a player being elected to the Hall of Fame?####
hall.of.famers <- filter(HallOfFame, inducted == "Y", category == "Player") %>%
  select(playerID, yearID)

q1.data <- full_join(career.stats, hall.of.famers, by = "playerID") %>%
  filter(G >= 1200) %>%
  select(playerID, R, H, XBH, HR:BB, SB, SO, AVG:yearID) %>%
  group_by(is.na(yearID))

q1.data.cumulative <- select(q1.data, R:SO, `is.na(yearID)`)
q1.data.averages <- select(q1.data, AVG:SO.pct, `is.na(yearID)`)

# Because of how true/false values were evaluated negative and positive signs are switched (negative in plot means positive and vice versa)
pairs.panels(q1.data.cumulative)
pairs.panels(q1.data.averages)

####Question 2: What statistical categories correlate with All-Star selections?#####
all.star.selections <- count(Lahman::AllstarFull, playerID)

q2.data <- left_join(career.stats, all.star.selections, by = "playerID") %>%
  select(playerID, AB,R:BB, SB, SO, AVG:n) %>%
  na.omit()

pairs.panels(q2.data[c(3:4, 6, 9:12, 20)])
pairs.panels(q2.data[c(13:20)])

####Question 3: Who are/were the most under and over paid players?####
salaries <- aggregate(salary ~ playerID, Salaries, mean)
debut <- select(Master, debut, playerID)

q3.data <- right_join(salaries, career.stats, by = "playerID") %>%
  full_join(debut, by = "playerID") %>%
  select(R, AVG, playerID, salary, G)

q3.data <- mutate(q3.data, dollars.per.run = R / salary, dollars.per.avg = AVG / salary)

ggplot(data = q3.data, mapping = aes(x = salary, y = R)) + 
  geom_point() +
  geom_text(aes(label = ifelse(salary > 1.25e+07 & salary < 1.8e+07, as.character(playerID),'')), hjust=0, vjust=0) +
  geom_text(aes(label = ifelse(R > 1500 & salary < 5.0e+06, as.character(playerID),'')), hjust = 0, vjust = 1) +
  geom_text(aes(label = ifelse(salary > 1.8e+07, as.character(playerID),'')), hjust = 1, vjust = 1)

ggplot(data = q3.data, mapping = aes(x = salary, y = AVG)) +
  geom_point() +
  geom_text(aes(label = ifelse(salary > 1.5e+07 & salary < 1.8e+07, as.character(playerID),'')), hjust = 0, vjust = 0) +
  geom_text(aes(label = ifelse(salary > 1.80e+07, as.character(playerID),'')), hjust = 1, vjust = 0) +
  geom_text(aes(label = ifelse(AVG > 0.35 & (salary < 7357563 & salary > 5.0e+06), as.character(playerID),'')), hjust = 1, vjust = 0) +
  geom_text(aes(label = ifelse(AVG > .35  & salary < 5.0e+06, as.character(playerID),'')), hjust = 0, vjust = 0) +
  geom_text(aes(label = ifelse(AVG > .35 & salary > 7357563, as.character(playerID),'')), hjust = 0, vjust = 0)

####Question 4: Who were the best teams for their salary####
team.salaries <- group_by(Salaries, yearID, teamID) %>%
  arrange(teamID) %>%
  unite(new, teamID, yearID, sep = "-") %>%
  group_by(new)

team.salaries <- team.salaries %>%
  summarise(sum(salary, na.rm = TRUE))

team.batting <- filter(Batting, yearID >= 1985) %>%
  arrange(teamID) %>%
  unite(new, teamID, yearID, sep = "-") %>%
  group_by(new) %>%
  summarise(sum(R, na.rm = TRUE))

team.wins <- select(Teams, W, yearID, teamID) %>%
  filter(yearID >= 1985) %>%
  arrange(teamID) %>%
  unite(new, teamID, yearID, sep = "-") %>%
  group_by(new)

names(team.salaries)[names(team.salaries) == "sum(salary, na.rm = TRUE)"] <- "salary"
names(team.batting)[names(team.batting) == "sum(R, na.rm = TRUE)"] <- "R"

q4.data <- full_join(team.salaries, team.batting, by = "new") %>%
  full_join(team.wins, by = "new")

ggplot(data = q4.data, mapping = aes(x = salary, y = W)) +
  geom_point() +
  geom_text(aes(label = ifelse(W > 110 & salary <= 66806867, as.character(new),'')), hjust = 1, vjust = 0) +
  geom_text(aes(label = ifelse(W > 110 & salary > 66806867, as.character(new),'')), hjust = 0, vjust = 0) +
  geom_vline(xintercept = 60042633)
  

####Question 5: How much do individual awards correlate with a player being elected to the Hall of Fame?####
hall.of.famers <- filter(HallOfFame, inducted == "Y", category == "Player") %>%
  select(playerID, yearID, votes, ballots) %>%
  mutate(pct.of.ballots = votes / ballots)

total.awards.players <- count(AwardsPlayers, playerID) %>% 
  arrange(desc(n))

q5.data <- full_join(hall.of.famers, total.awards.players, by = "playerID") %>%
  group_by(is.na(yearID))

# Convert is.na(yearID) to numeric
cols <- sapply(q5.data, is.logical) 
q5.data[,cols] <- lapply(q5.data[,cols], as.numeric)

q5.data[is.na(q5.data)] <- 0 #replace the NA's with zeros

# Plot of all players
ggplot(data = q5.data, mapping = aes(x = pct.of.ballots, y = n)) + 
  geom_point() +
  geom_smooth(method = lm)

# Plot of just Hall of Famers
filter(q5.data, votes > 0) %>%
ggplot(mapping = aes(x = pct.of.ballots, y = n)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Correlation plot
q5.pairspanels <- select(q5.data, ballots, pct.of.ballots, n) %>%
  pairs.panels()
