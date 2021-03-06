## Functions to analyze performance on paradiddles registered with the pads

library("tidyverse")

# The following parameter names are used for transparency:
# - logf refers to the ".log" files generated by PsychoPy for each participant
# - log_df refers to the output of read_logf(), a dataframe from the log file


# read in log file
read_logf <- function(logf) {
	ncols <- max(count.fields(logf, sep = "\t"))
	log_df <- read.delim(
		logf, sep = "\t", header = FALSE,
	  col.names = c("t", "type", paste("msg", seq_len(ncols-2), sep = ""))
	  )
	log_df
}

# Order in which conditions were carried out
get_order <- function(log_df) {
  order_line <- log_df$msg1[grep("Imported block_order_", log_df$msg1)]
  order <- gsub("Imported block_order_(.*)\\.csv.*", "\\1", order_line)
  tibble(block = 1 : 3, condition = unlist(str_split(order, "-")))
}


# Get the overall block and trial structure
get_structure <- function(log_df) {
  
  # Description of the upcoming trial
  trials <- log_df[grepl("start of memory period", log_df$msg1), ]
  # Lines containing timing for the start of memory period
  start <- log_df[grepl("b_memory_period_2_: autoDraw = True", log_df$msg1), ]
  # Lines containing timing for the *end* of memory period:
  # Function looks for end of the period, but keeps only 1st of 2 repeated lines
  mem_end <- function(df) {
    end_Ls <- grep("b_memory_period_2_: autoDraw = False", df$msg1)
    keep <- rep(c(TRUE, FALSE), length.out = length(end_Ls))
    end_Ls[keep]
  }
  end <- log_df[mem_end(log_df), ]
  
  # Sanity check there should be one start/end timing for each trial:
  if (! ( nrow(trials) == nrow(start) & nrow(trials) == nrow(end) )) {
    stop("Number of trials and timings don't match!")
  } 

  tibble(
    routine = gsub("(.*)/([0-9])/([0-9]+)/.*", "\\1", trials$msg1),
    block   = as.numeric(gsub("(.*)/([0-9])/([0-9]+)/.*", "\\2", trials$msg1)),
    trial   = as.numeric(gsub("(.*)/([0-9])/([0-9]+)/.*", "\\3", trials$msg1)),
    t0      = start$t,
    tend    = end$t,
    # 1st and last lines that contain paradiddle info if carried out:
    L0      = as.numeric(row.names(start)) + 1,
    Lend    = as.numeric(row.names(end)) - 1
  )
}


# Function to extract the relevant tapping info:
# as input it takes a dataframe, information about the block and trial
# it is in, the first and last Lines where it should look for taps, etc.
get_taps <- function(log_df, routine, block, trial,  t0, L0, Lend, ...) {
  # Look only in rows specified as argument
  df <- log_df[L0 : Lend, ]
  # Select only relevant rows but remove rows for when the tap stopped (velocity=0)
  taps <- df[grepl("SamplePad", df$msg1) & !grepl("velocity=0", df$msg1), ]
  # Extract the note played at a point in time and include block+trial info
  out <- if (nrow(taps) == 0) {  # if no taps in the trial
    tibble(
      routine,
      block,
      trial,
      time_exp = NA,
      time     = NA,
      note     = NA,
      pad      = NA
      )
    } else {
      tibble(
        routine = routine,
        block,
        trial,
        time_exp = taps$t,
        time     = time_exp - t0,  # time within memory phase
        note     = as.numeric(gsub(".*note=([0-9]+).*", "\\1", taps$msg1)),
        pad      = gsub(".*/(SamplePad [0-9])/.*", "\\1", taps$msg1)
      )
    }
  out
}

# Wrapper function that takes the logfile as input and outputs a dataframe
# with the number of trials per block that need to be excluded. It uses all the
# functions above.
extract_tap_info <- function(logf = NULL) {
	# if no filename is provided, choose from interactive menu
	if (is.null(logf)) {
		logf <- choose.files(default = "../data/*.*")
	}
	logdf <- read_logf(logf)
	order <- get_order(logdf)
	struct <- get_structure(logdf)
	taps <- pmap(struct, get_taps, log_df = logdf) %>%
  	bind_rows() %>%
  	left_join(order) %>%
    mutate(ppt_file = logf)
	taps
}

# computes valid trials by checking if the first tap occured before 3s (valid)
# or not (invalid) - output is one row per trial
compute_valid <- function(log_df) {
	log_df %>%
		# non-control experimental trials
		filter(routine == "word_presentation", condition != "control") %>%  
		group_by(block, condition, trial) %>%
		slice(1) %>%
		mutate(valid = ifelse((time > 3) | is.na(time), 0, 1))
}

# takes as input the output of compute_valid() - it summarizes valid trials
compute_valid_summary <- function(valid_df) {
	valid_df %>%
		group_by(ppt_file, block, condition) %>%
		summarise(valid = sum(valid), N = n()) %>%
		mutate(prop_invalid = (N - valid) / N)
}

# Function to plot different routines (practice, training, real trials):
plot_taps <- function(log_df, rout)  {
  df <- log_df %>% 
    filter(routine == rout) %>%
    mutate(note = factor(note))
  p <- ggplot(df)
  if (rout == "word_presentation_practice") {
  	p <- p +
      geom_vline(aes(xintercept = time, linetype = note))
  } else {
  	p <- p +
  		geom_vline(aes(xintercept = time, colour = note))+#, size = 1.5) +
  		facet_grid(trial ~ condition)
  }
  p + geom_vline(xintercept = 3) + ggtitle(rout)
}

