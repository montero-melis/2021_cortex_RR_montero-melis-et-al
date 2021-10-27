## Compute inter-rater agreement based on 5% of data from 60 participants.

library("tidyverse")

# NB: Below we call custom functions saved in a separate script for clarity.
source("analysis/myfunctions/extract_info_fncs.R")


# Load transcription files for inter-rater agreement ----------------------

pi <- read_tsv("data/transcription_petrus.tsv") %>%
  arrange(filename)
mb <- read_tsv("data/transcription_MB.tsv") %>%
  arrange(filename)

head(pi)
head(mb)

# Have the same files been transcribed?
sum(! pi$filename %in% mb$filename)
sum(! mb$filename %in% pi$filename)
# Yes!


# Extract information -----------------------------------------------------

# Extract information from transcribed file names
f_info <- extract_fileinfo(pi$filename)
# shows which trials have been transcribed and associated base files
head(f_info)


# Extract the target words for each selected trial.
# pmap() allows us to apply a function taking each row as arguments, see
# http://zevross.com/blog/2019/06/11/the-power-of-three-purrr-poseful-iteration-in-r-with-map-pmap-and-imap/
targets <- pmap(f_info, extract_trial_selection) %>%
  bind_rows()
# NB: warnings are caused by an empty column in csv files

head(targets)
tail(targets)

# arrange the transcriptions in a more convenient format: for each trial,
# associate a character string that contains the transcribed words
clean_transcriptions <- function(df, transcriber) {
  df <- df %>%
    mutate(transcr = str_split(transcription, " ")) %>%
  select(participant, block, trial, transcr)
  names(df)[names(df) == "transcr"] <- paste0("transcr_", transcriber)
  df
}
petrus <- clean_transcriptions(df = pi, transcriber = "petrus")
manne <- clean_transcriptions(df = mb, transcriber = "manne")

head(petrus)
head(manne)
# the last column is a list of strings of variable length
head(petrus$transcr_petrus)


# Compute inter-rater agreement -------------------------------------------

# First, join all the information into a single data frame
targets <- targets %>%
  left_join(petrus) %>%
  left_join(manne)
head(targets)

# convenience function used below
check_transcription <- function(target, transcr) {
  target %in% transcr
}

# compute the agreement
agreement <- targets %>%
  mutate(
    Petrus = map2_dbl(verb, transcr_petrus, check_transcription),
    Manne  = map2_dbl(verb, transcr_manne, check_transcription)
  ) %>%
  mutate(agreement = ifelse(Petrus == Manne, 1, 0))

# for sanity, check the first rows to verify that everything looks good:
for (row in 1:40) {
  print(paste("TARGET: ", agreement$verb[row]))
  print(targets[row, "transcr_petrus"][[1]])
  print(targets[row, "transcr_manne"][[1]])
  cat("\n\n")
}

# for sanity, check the disagreements...
for (row in which(agreement$agreement == 0)) {
  print(paste("TARGET: ", agreement$verb[row]))
  print(targets[row, "transcr_petrus"][[1]])
  print(targets[row, "transcr_manne"][[1]])
  cat("\n\n")
}

# Total agreement in percentage
round(100 * with(agreement, sum(agreement) / length(agreement)), 2)
