# Laurel Ayuyao
# Sports Analytics Project

library(readr)
library(datavolley)
library(ggplot2)
library(dplyr)

### DOWNLOADING DATA

setwd("/Users/laurel/Desktop/Sports Analytics Project")
getwd()
# Download data from Github
download.file(url = "https://github.com/macfields/mfields_finalproject/archive/master.zip"
              , destfile = "volleydata.zip")
# Data also shared on Google Drive: https://drive.google.com/file/d/1snF1GFCm_cEa5VJX0ukYeWBiLAIPZjVQ/view?usp=sharing
unzip(zipfile = "volleydata.zip",)
setwd("/Users/laurel/Desktop/Sports Analytics Project/mfields_finalproject-master")

# Read in play-by-play data
CSU <- plays(read_dv("2018dvwfiles/&2018-09-01 63826 CSU-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
MICH <- plays(read_dv("2018dvwfiles/&2018-09-01 66885 MICH-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
EMU <- plays(read_dv("2018dvwfiles/&2018-09-02 59573 EMU-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
CCSU <- plays(read_dv("2018dvwfiles/&2018-09-07 66875 HARV-CCSU(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
MSU <- plays(read_dv("2018dvwfiles/&2018-09-08 62690 HARV-MSU(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
FAIR <- plays(read_dv("2018dvwfiles/&2018-09-08 66876 FAIR-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
BC <- plays(read_dv("2018dvwfiles/&2018-09-13 66877 HARV-BC(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
NEU <- plays(read_dv("2018dvwfiles/&2018-09-14 66231 NEU-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
UMASSL <- plays(read_dv("2018dvwfiles/&2018-09-15 66878 HARV-UMassL(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
DART1 <- plays(read_dv("2018dvwfiles/&2018-09-21 60794 DART-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
CORN1 <- plays(read_dv("2018dvwfiles/&2018-09-28 45079 CORN-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
COL1 <- plays(read_dv("2018dvwfiles/&2018-09-29 63605 COL-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
PENN1 <- plays(read_dv("2018dvwfiles/&2018-10-05 66588 HARV-PENN(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
PRINCE1 <- plays(read_dv("2018dvwfiles/&2018-10-06 66880 HARV-PRIN(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
BROWN1<- plays(read_dv("2018dvwfiles/&2018-10-12 62190 BROWN-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
YALE1 <- plays(read_dv("2018dvwfiles/&2018-10-13 63541 YALE-HARV(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
DART2 <- plays(read_dv("2018dvwfiles/&2018-10-19 66881 HARV-DART(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
COL2 <- plays(read_dv("2018dvwfiles/&2018-10-26 79208 HARV-COL(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
CORN2 <- plays(read_dv("2018dvwfiles/&2018-10-27 45088 HARV-CORN(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
YALE2 <- plays(read_dv("2018dvwfiles/&2018-11-02 66883 HARV-YALE(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))
BROWN2 <- plays(read_dv("2018dvwfiles/&2018-11-03 66884 HARV-BROWN(VM).dvw", insert_technical_timeouts = FALSE)) %>% filter(!is.na(player_name))



### DATA PREPROCESSING

# Combine data from all games into a single data frame
all_games <- rbind(CSU, MICH, EMU, CCSU, MSU, FAIR, BC, NEU, UMASSL, DART1, CORN1, COL1, PENN1, PRINCE1, BROWN1, YALE1, DART2, COL2, CORN2, YALE2, BROWN2)
# Create a unique match-set id for each set
all_games$match_set_id <- paste(all_games$match_id, all_games$set_number, sep="_")
all_games$match_set_point_id <- paste(all_games$match_set_id, all_games$point_id, sep="_")

# Print all variable names
names(all_games)

# CSU_set1 <- all_games %>% filter(match_set_id == "b4ac8dd24742bc48dfba6b9713c2baaa_1")  

# Create variable to show Harvard score, regardless of whether Harvard is home team or visiting team for a match
all_games$harvard_score <- ifelse(all_games$home_team == "Harvard University", all_games$home_team_score, all_games$visiting_team_score)
# Create variable to show opponent's score
all_games$other_team_score <- ifelse(all_games$home_team == "Harvard University", all_games$visiting_team_score, all_games$home_team_score)
# Create variable that shows who Harvard is playing for a match
all_games$oponent <- ifelse(all_games$home_team == "Harvard University", all_games$visiting_team, all_games$home_team)
# Create generic team variable
all_games$HARV_or_other <- ifelse(all_games$team == "Harvard University", "Harvard University", "Opposing Team")

# Only plays by Harvard players
HARV_plays <- all_games %>% filter(team == "Harvard University") 



### DATA EXPLORATION

# Count of types of plays for Harvard players
summary(as.factor(HARV_plays$skill))  

# Count of evaluation codes for Harvard players
# "-": Poor
# "!": OK
# "/": Blocked/Poor, no attack/Positive, no attack
# "#": Perfect
# "+": Positive
# "=": Error
summary(as.factor(HARV_plays$evaluation_code))  

# Count of type of sets by setter
summary(as.factor(HARV_plays$attack_description[HARV_plays$skill == "Attack" & HARV_plays$attack_description != "unknown attack code" & !is.na(HARV_plays$attack_description)]))

# Count of type of hits
summary(as.factor(HARV_plays$skill_subtype[HARV_plays$skill == "Attack"]))

# Summary of Harvard players involved in the most plays
summary(as.factor(HARV_plays$player_name)) 

# Calculate scores for each set 
# Identify Unique teams
sets <- unique(na.omit(all_games$match_set_id))
# Create empty vectors to Harvard's score, opponent's score, opponent team name, set number, and match id
harv_final_score <- other_team_final_score <- other_team <- set_number <- match_id<- rep(NA, length(sets))
# For each match - set
for(i in 1:length(sets)){
  # Calculate Harvard final score
  harv_final_score[i] <- max(all_games$harvard_score[all_games$match_set_id == sets[i]],
                       na.rm = T)
  # Calculate opponent final score
  other_team_final_score[i] <- max(all_games$other_team_score[all_games$match_set_id == sets[i]],
                      na.rm = T)
  other_team[i] <- all_games$oponent[all_games$match_set_id == sets[i]]
  set_number[i] <- all_games$set_number[all_games$match_set_id == sets[i]]
  match_id[i] <- all_games$match_id[all_games$match_set_id == sets[i]]
}
# Join result vectors
set_scores <- cbind.data.frame(other_team, match_id, set_number, harv_final_score, other_team_final_score)
# Set winner
set_scores$winning_team <- ifelse(set_scores$harv_final_score>set_scores$other_team_final_score, "Harvard University", set_scores$other_team)
# Create column for team abbreviations (to be used later for graphing)
set_scores$other_team_abbreviation <- strsplit(other_team, " ")
set_scores$other_team_abbreviation<- sapply(set_scores$other_team_abbreviation, function(x){
  toupper(paste(substring(x, 1, 1), collapse = ""))
})

# Get rid of sets where winner is first to 15 points (5th game)
set_scores_norm <- set_scores %>% filter(harv_final_score >= 25 | other_team_final_score >= 25)  

# Graph all set scores
library(ggrepel)
library(ggdark)
g_1 <- ggplot(set_scores_norm, # Set dataset
              aes(x = harv_final_score,  # Set x-axis as run yards
                  y = other_team_final_score,  # Set y-axis as pass yards
                  color = other_team)) + # Set color as teams
  geom_point(alpha = .25) + # Set geom_point for scatter
  geom_text_repel(aes(label = other_team_abbreviation), size = 2.5, segment.alpha = .15) + # Set labels 
  dark_theme_bw() +
  theme(legend.position="none",
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) +  # Remove grid
  labs(y= "Opponent Score", x ="Harvard Score", # Set labels
       title = "Set Scores",
       subtitle = "2018 Harvard Season")
g_1

# Graph number of types of plays
g_2 <- ggplot(HARV_plays, aes(skill)) + 
  geom_bar() +
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) +  # Remove grid
  labs(title = "Count of Types of Play",
       subtitle = "2018 Harvard Season")
g_2


### MORE GRAPHICAL ANALYSIS

# Create graph of most common area to hit the ball to
attack_rate <- HARV_plays %>% 
  dplyr::filter(skill == "Attack") %>%
  group_by(end_zone, end_subzone) %>% dplyr::summarize(n_attacks = n()) %>% ungroup

attack_rate$rate <- attack_rate$n_attacks/sum(attack_rate$n_attacks)

sum(attack_rate$n_attacks)
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$end_zone, end = "upper", subzones = attack_rate$end_subzone))

g_3 <- ggplot(attack_rate, aes(x, y, fill = rate)) + geom_tile() + ggcourt("upper", labels = NULL) +
  scale_fill_gradient2(name = "Rate: attack\nend location", low = "white", high = "red")

g_3


# Serving arrows
all_games_serves <- all_games %>% filter(skill == "Serve", end_coordinate_y>=1)

## now we can plot these
all_games_serves$evaluation[!all_games_serves$evaluation %in% c("Ace", "Error")] <- "Other"

g_4 <- ggplot(all_games_serves, aes(start_coordinate_x, start_coordinate_y,
                    xend = end_coordinate_x, yend = end_coordinate_y, colour = evaluation)) +
  geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20), alpha = .1) +
  scale_colour_manual(values = c(Ace = "limegreen", Error = "firebrick", Other = "dodgerblue"),
                      name = "Evaluation") +
  ggcourt(labels = c("Serving team", "Receiving team"))

g_4

# Aces, Errors, and Other
g_5 <- ggplot(all_games_serves, aes(x=end_coordinate_x, y=end_coordinate_y, colour = evaluation, shape = skill_type, alpha = .0001)) + 
  geom_point() + 
  scale_colour_manual(values = c(Ace = "green", Error = "red", Other = "gray"),
                      name = "Evaluation") +
  scale_shape_manual(name = "Serve Type",
                     values = c(17, 3, 15, 18)) + #17=triangle, 3=plus, 15=square, 18=diamond
  ggcourt("upper", labels = NULL) + 
  labs(title = "Serving Evaluation",
       subtitle = "2018 Season")
  

g_5

# Hits
all_games_attacks <- all_games %>% filter(skill == "Attack", end_coordinate_y >=1, match_id=="b4ac8dd24742bc48dfba6b9713c2baaa")
g_6 <- ggplot(all_games_attacks, aes(start_coordinate_x, start_coordinate_y,
                                    xend = end_coordinate_x, yend = end_coordinate_y, colour = evaluation,)) +
  geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20), alpha = .3) +
  facet_wrap(~ evaluation) +
  ggcourt(labels = c("Attacking team", "Defensive team"))

g_6

all_games_attacks <- all_games %>% filter(skill == "Attack", end_coordinate_y >=1, evaluation != "Blocked")
g_7 <- ggplot(all_games_attacks, aes(x=end_coordinate_x, y=end_coordinate_y, colour = evaluation, alpha = .001)) + 
  geom_point() + 
  facet_wrap(~ evaluation) +
  ggcourt("upper", labels = NULL)
g_7  


# Individual Server Rate
ind_serve_rate <- HARV_plays %>% 
  dplyr::filter(skill == "Serve") %>%
  group_by(player_name) %>% 
  summarize(n_serves = n(),
            error_serves = sum(evaluation_code == "=")) %>%
  mutate(rate = (n_serves-error_serves)/n_serves) %>% 
  ungroup

g_8 <- ggplot(data=ind_serve_rate, aes(x=player_name, y=rate)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g_8

# Kill rate
ind_kill_rate <- HARV_plays %>% 
  dplyr::filter(skill == "Attack", player_name != "unknown player") %>%
  group_by(player_name) %>% 
  summarize(n_attacks = n(),
            kills = sum(evaluation_code == "#")) %>%
  mutate(rate = (kills)/n_attacks) %>% 
  ungroup

g_9 <- ggplot(data=ind_kill_rate, aes(x=player_name, y=rate)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g_9


# Mabry_kill <- HARV_plays %>% 
#   dplyr::filter(skill == "Attack", player_name == "Mindie Mabry") %>%
#   group_by(match_id) %>% 
#   summarize(n_attacks = n(),
#             kills = sum(evaluation_code == "#")) %>%
#   mutate(rate = (kills)/n_attacks) %>% 
#   ungroup
# 
# 
# kill_area <- HARV_plays %>% 
#   dplyr::filter(skill == "Attack", end_zone>=1) %>%
#   group_by(end_zone) %>% 
#   summarize(n_attacks = n(),
#             kills = sum(evaluation_code == "#")) %>%
#   mutate(rate = (kills)/n_attacks) %>% 
#   ungroup
# 
# kill_area <- cbind(kill_area, dv_xy(kill_area$end_zone, end = "lower"))
# 
# g_10 <- ggplot(kill_area, aes(x, y, fill = rate)) + geom_tile() + ggcourt("lower", labels = NULL) +
#   scale_fill_gradient2(name = "Rate: Kills as \na Percentage \nof Attacks", low="red", high="green", midpoint = .3) 
# 
# g_10

kill_area <- all_games %>% 
  dplyr::filter(skill == "Attack", end_zone>=1) %>%
  group_by(end_zone, end_subzone) %>% 
  summarize(n_attacks = n(),
            kills = sum(evaluation_code == "#")) %>%
  mutate(rate = (kills)/n_attacks) %>% 
  ungroup



kill_area <- cbind(kill_area, dv_xy(kill_area$end_zone, end = "upper", subzones = kill_area$end_subzone))

g_11 <- ggplot(kill_area, aes(x, y, fill = rate)) + geom_tile() + ggcourt("upper", labels = NULL) +
  scale_fill_gradient2(name = "Rate: Kills as \na Percentage \nof Total Attacks", low="red", high="green", midpoint = mean(kill_area$rate)) 

g_11


# Graph kill rate vs. errors digging
off_and_def <- all_games %>% 
  dplyr::filter(skill == "Attack" | skill == "Dig" | skill == "Reception") %>%
  group_by(team) %>% 
  summarize(n_attacks = sum(skill == "Attack"),
            n_defense = sum(skill== "Dig" | skill == "Reception"), #n(),
            kills = sum(skill == "Attack" & evaluation_code == "#"),
            def_errors = sum((skill== "Dig" | skill == "Reception") & evaluation_code == "=")) %>%
  mutate(kill_rate = (kills)/n_attacks,
         def_error_rate = def_errors/n_defense)%>% 
  ungroup
off_and_def$team_abbreviation <- strsplit(off_and_def$team, " ")
off_and_def$team_abbreviation<- sapply(off_and_def$team_abbreviation, function(x){
  toupper(paste(substring(x, 1, 1), collapse = ""))
})


g_12 <- ggplot(off_and_def, # Set dataset
              aes(x = kill_rate, # Set x axis as passes per game 
                  y =  def_error_rate # Set y axis as average pass epa
              )) + 
  geom_point(alpha = 0.3) + # Set geom_point for scatter
  dark_theme_bw() + # Set dark theme
  theme(legend.position="none", # Turn off legend
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank(),
        aspect.ratio = 1/1,
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold")) +  # Remove grid
  geom_text_repel(aes(label = team_abbreviation), size = 2.5, segment.alpha = .15) +
  labs(x= "Kill Percentage (Offense)", y ="Defensive Errors Percentage", # Set labels
       title = "Teams by Offense and Defense Metrics",
       subtitle = "2018 Season")

g_12


def_area <- all_games %>% 
  dplyr::filter((skill == "Dig" | skill == "Reception") & start_zone>=1) %>%
  group_by(skill, start_zone) %>% 
  summarize(n_pass = n(),
            errors = sum(evaluation_code == "=")) %>%
  mutate(rate = (errors)/n_pass) %>% 
  ungroup

def_area <- cbind(def_area, dv_xy(def_area$start_zone, end = "lower"))

g_13 <- ggplot(def_area, aes(x, y, fill = rate)) + geom_tile() + ggcourt("lower", labels = NULL) +
  scale_fill_gradient2(name = "Rate: \nErrors as a \nPercentage of \nTotal Passes", low="green", high="red", midpoint = mean(def_area$rate)) +
  facet_wrap(~ skill)

g_13
