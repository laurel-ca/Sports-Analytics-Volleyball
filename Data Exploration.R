library(readr)
library(datavolley)
library(ggplot2)
library(dplyr)


setwd("/Users/laurel/Desktop/Sports Analytics Project")
getwd()
# Download data from Github
download.file(url = "https://github.com/macfields/mfields_finalproject/archive/master.zip"
              , destfile = "volleydata.zip")
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

# Combine data from all games into a single data frame
all_games <- rbind(CSU, MICH, EMU, CCSU, MSU, FAIR, BC, NEU, UMASSL, DART1, CORN1, COL1, PENN1, PRINCE1, BROWN1, YALE1, DART2, COL2, CORN2, YALE2, BROWN2)
# Create a unique match-set id for each set
all_games$match_set_id <- paste(all_games$match_id, all_games$set_number, sep="_")

# Print all variable names
names(all_games)

# CSU_set1 <- all_games %>% filter(match_set_id == "b4ac8dd24742bc48dfba6b9713c2baaa_1")  

# Create variable to show Harvard score, regardless of whether Harvard is home team or visiting team for a match
all_games$harvard_score <- ifelse(all_games$home_team == "Harvard University", all_games$home_team_score, all_games$visiting_team_score)
# Create variable to show opponent's score
all_games$other_team_score <- ifelse(all_games$home_team == "Harvard University", all_games$visiting_team_score, all_games$home_team_score)
# Create variable that shows who Harvard is playing for a match
all_games$oponent <- ifelse(all_games$home_team == "Harvard University", all_games$visiting_team, all_games$home_team)

# Only plays by Harvard players
HARV_plays <- all_games %>% filter(team == "Harvard University") 

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
