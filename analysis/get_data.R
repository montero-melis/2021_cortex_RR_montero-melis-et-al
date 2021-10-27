## Script to obtain the data that will go into the analysis.

# It involves the following steps:
# - Select the relevant participants (e.g., first 60, 72, ...)
# - Extract the design matrix for those participants
# - Merge with the transcription data to obtain accuracy score per trial
# - Exclude invalid trials or whole participants
# - Obtain a clean dataframe with participant info (excluding any sensitive info)
# - Save data sets to disk
# NB: Most of these steps will call custom functions saved in a separate
# script for clarity.


library("tidyverse")

# load custom functions:
source("analysis/myfunctions/extract_info_fncs.R")


# Extract design matrix for relevant participants -------------------------

# Parse all data files from participants that are not obviously invalid:
fnames <- parse_all_datafiles("data/raw_data/")
head(fnames)

# Select the first 60 (they are ordered by DateTime)
fnames_60 <- fnames %>% head(60) %>% pull(filename)
fnames_60
# Select all
fnames_all <- fnames %>% pull(filename)

# Extract participants' design matrices
data60 <- map(fnames_60, extract_design_matrix) %>%
  bind_rows()
head(data60)

data_all <- map(fnames_all, extract_design_matrix) %>%
  bind_rows()

# Verify that this contains exactly 26 x 4 = 104 trials per participant-block
data60 %>%
  group_by(ID_unique, block) %>%
  count() %>%
  filter(n != 104)
# Good! (Empty tibble means all have 104)

data_all %>%
  group_by(ID_unique, block) %>%
  count() %>%
  filter(n != 104)


# Process transcriptions --------------------------------------------------

# load file with RA transcriptions and process them with prepare_transcriptions()
transcr <- read_tsv("data/transcriptions.tsv") %>%
  prepare_transcriptions()
head(transcr)

# Sanity checks - are all trials for all participants transcribed? Quick check:
# to see if we have equal amount of transcriptions for every block:
transcr %>%
  group_by(block) %>%
  count()
# No. Let's look at which participants are the affected ones (perhaps they 
# haven't been fully transcribed yet)
transcr %>%
  group_by(ID_unique) %>%
  count() %>%
  filter(n != 26 * 3)
# Not a problem: 9 is excluded


# Merge transcriptions into data file -------------------------------------

# Check whether all trials in the data file have been transcribed
data60_trials <- with(data60, unique(paste(ID_unique, block, trial_block, sep = "_")))
data_all_trials <- with(data_all, unique(paste(ID_unique, block, trial_block, sep = "_")))
transcr_trials <- with(transcr, paste(ID_unique, block, trial_block, sep = "_"))
sum(! data60_trials %in% transcr_trials)  # Yes! (0 missing transcriptions)
sum(! data_all_trials %in% transcr_trials)  # Yes! (0 missing transcriptions)

# Merge
data60_full <- left_join(data60, transcr)
head(data60_full)
data_all_full <- left_join(data_all, transcr)
head(data_all_full)

# We can check that there are no missing values among descriptions
# NB: An empty description is an empty string (""), while a missing description
# is NA.
sum(is.na(data_all_full$transcription))

# Any participants that don't have 312 observations at this point?
data_all_full %>%
  group_by(ID_unique) %>%
  count() %>%
  filter(n != 26 * 3 * 4)
# No!


# Score each trial --------------------------------------------------------

# compute the score by checking if the verb in a trial is in transcr_list:
data60_scored <- data60_full %>%
  mutate(error = map2_dbl(verb, transcr_list, ~ ! .x %in% .y))
data_all_scored <- data_all_full %>%
  mutate(error = map2_dbl(verb, transcr_list, ~ ! .x %in% .y))

# "error" column is 1 if there's a recall error, 0 if it's correctly recalled:
data60_scored
data_all_scored


# Exclusion based on paradiddles ------------------------------------------

# load list of trials to be excluded (generated in "trial_exclusion.Rmd"):
parad_excl <- read_csv("data/excluded_trials_bc_of_paradiddle.csv")
head(parad_excl)

# Number of trials to be excluded (each excluded trial removes 4 observations)
# But note that this is an upper bound because it includes all participants,
# not just the first 60 or the transcribed ones, etc.
nrow(parad_excl)

# Exclude trials
parad_excl_trials <- with(parad_excl, 
                          paste(ID_unique, block, trial_block, sep = "_"))
data60_scored_trials <- with(data60_scored, 
                           paste(ID_unique, block, trial_block, sep = "_"))
data_all_scored_trials <- with(data_all_scored, 
                             paste(ID_unique, block, trial_block, sep = "_"))
# number of excluded observations (recall: 1 trial = 4 observations)
sum(data60_scored_trials %in% parad_excl_trials)  # for first 60 participants
sum(data_all_scored_trials %in% parad_excl_trials)  # for all participants
# keep only trials that are not in the to-be-excluded list
data60_scored <- data60_scored[! data60_scored_trials %in% parad_excl_trials, ]
data_all_scored <- data_all_scored[! data_all_scored_trials %in% parad_excl_trials, ]


# Manual exclusion for individual cases -----------------------------------

# There is for now no trial to be excluded manually. This would be needed if
# trials needed to be excluded because of notes taken by the RA during data
# collection, such as participants starting a trial before a beep.

# Check handedness as reported by participants
ppts_hand <- read_csv("data/participant_data_raw.csv") %>%
  select(c(1, 13))
names(ppts_hand)[2] <- "handedness"
table(ppts_hand$handedness)
# Any non right-handed participants?
ppts_hand %>%
  filter(handedness != "Högerhänt")
# No, the comments make it clear that they were right-handed.

# Handedness questionnaire
hand <- read_csv("data/handedness.csv")
hand_quot <- hand %>%
  group_by(Subject) %>%
  summarise(HQ = 100 * sum(Response) / 20)

# any non-right handed participants?
hand_quot %>%
  filter(HQ <= 0)

# descriptives of handedness
summary(hand_quot$HQ)


# participant dataframe ---------------------------------------------------

# Load participant info
ppts <- read_csv("data/participant_data_raw.csv")

# rename columns
names(ppts)
names(ppts) <- c(
  "pptID", "valid", "check_notes", "sex", "age", "birthplace", "arrival_sweden",
  "city_grown_up", "abroad", "reading_difficulties", "hearing_problems",
  "sight_problems", "handedness_self", "highest_education", "educat_status",
  "music", "music_detailed", "problem_sight_hearing", "psychological_disease",
  "BPM_hand_foot", "RA_comments", "languages_acquired", "languages_daily",
  "languages_home", "comments"
  )
# filter out invalid participants
ppts <- ppts %>%
  filter(valid %in% c(1,2))
ppts

# We need to match the participants in "ppts" df to their ID as defined in
# data_all, etc.
head(data_all)

# First, extract the integer that indicates their ppt number:
ppts$ID <- as.numeric(gsub("(\\d+),.*", "\\1", ppts$pptID))
# There is one duplicate:
head(sort(table(ppts$ID), decreasing = TRUE))  # pptID = 9
# check out the two rows with same ID:
ppts %>%
  filter(ID == 9) %>%
  select(pptID)
# They can be differentiated by the output file in Psychopy, indicated in
# parenthesis.
# Compare them with the corresponding participants in data_all; indeed they
# correspond to those two rows:
data_all %>%
  filter(ID == 9) %>%
  select(ID_unique, ID) %>%
  unique()
# create a temporary lookup table that we can later use to join
lookup_IDs <- data_all %>%
  select(ID_unique, ID) %>%
  unique() %>%
  mutate(lookup = ID)
head(lookup_IDs)
# Manually add info that disambiguates between the two "9":
lookup_IDs[lookup_IDs$ID_unique == "9_2020_Nov_13_0848", "lookup"] <- "9_2020_Nov_13_0848"
lookup_IDs[lookup_IDs$ID_unique == "9_2020_Nov_20_1107", "lookup"] <- "9_2020_Nov_20_1107"
# Do the same thing in ppts:
ppts$lookup <- ppts$ID
ppts[grepl("2020_Nov_13_0848", ppts$pptID), "lookup"] <- "9_2020_Nov_13_0848"
ppts[grepl("2020_Nov_20_1107", ppts$pptID), "lookup"] <- "9_2020_Nov_20_1107"

# now join to add the ID_unique
ppts <- left_join(ppts, lookup_IDs)
# and join with hand_quot to add handedness quotient
ppts <- ppts %>%
  left_join(hand_quot %>% rename(lookup = Subject, hand_quot = HQ))

# divide BPM_hand_foot into two columns
ppts$BPM_hands <- as.numeric(gsub("(\\d+)/(\\d+)", "\\1", ppts$BPM_hand_foot))
ppts$BPM_feet <- as.numeric(gsub("(\\d+)/(\\d+)", "\\2", ppts$BPM_hand_foot))

# check
ppts %>%
  select(BPM_hand_foot, BPM_hands, BPM_feet)
summary(ppts$BPM_hands)
summary(ppts$BPM_feet)

# select only the relevant columns to save to disk
ppts_processed <- ppts %>%
  select(
    ID_unique, sex, age, highest_education, BPM_hands, BPM_feet, hand_quot
    )
ppts_processed

# Save to disk ------------------------------------------------------------

data60_scored %>%
  select(-transcr_list) %>%
  rename(word_type = trial_type) %>%
  write_csv("data/data60.csv")

data_all_scored %>%
  select(-transcr_list) %>%
  rename(word_type = trial_type) %>%
  write_csv("data/data_all.csv")

ppts_processed %>%
  write_csv("data/ppt_info.csv")
