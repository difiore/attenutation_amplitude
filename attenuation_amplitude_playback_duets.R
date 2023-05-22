#-------------------------------------------------------------------------------
## Load packages:
#-------------------------------------------------------------------------------

packages <- c("tidyverse",
              "lubridate",
              "tuneR",
              "seewave",
              "ggplot2",
              "dplyr",
              "sf",
              "tmaptools",
              "ggmap",
              "tmap",
              "sp",
              "rgdal",
              "ggspatial") # Create vector of packages

lapply(packages, require, character.only = TRUE)    # Load multiple packages


#-------------------------------------------------------------------------------
## Calibrating playback duets:
#-------------------------------------------------------------------------------
  # We're calculating SPL of 5 sec segment of titi duet and noise with
  # calibration in PAMGuide for specific recorder.

# Load in fresh copy of data
d <- read_csv("playback_SNRmeasurements_20230519.csv", col_names = TRUE) # Load in file

# Load in PAMGuide package with Tony's revisions
source("tonyPAMGuide_Meta_revised_AD3.R")

# Variable defines window length that PAMGuide uses for SNR measurements.
window_length <- 5

# Path to location containign recordings.
d$path <- "~/Downloads/2016 SongMeter Playback Audio"

# Turn date into YMD format.
d$DATE <- parse_date_time(d$DATE, "mdy")

# Creating StartTime column using FileName column.
d$StartTime <- str_split_fixed(d$FileName, "_",3)[,3] # Remove everything before 'HHMMSS.wav' pattern.
d$StartTime <- gsub(".wav","", d$StartTime) # Remove .wav pattern.
d$StartTime <- parse_date_time(d$StartTime, "HMS") # Turn HHMMSS into proper date-time.

d$CallOnset <- parse_date_time(d$start_time, "HMS")

# Calculating RMS levels
d <- d %>% rowwise() %>% mutate(Calibrated_SPL_Duet = tonyPAMGuide_Meta(fullfile = paste0(path, "/", FileName), atype= "Broadband", StartTime=StartTime, CallOnset=CallOnset, seconds = window_length, windowDirection = "after", lcut= 700, hcut= 1400, calib= 1, ctype= "TS", Mh=-36, G=0, vADC=1.0, plottype= "Stats", channel = MicUsed)["RMSlev"]) # NOTE: Here channel is assigned to be MicUsed

# Turning lists into integer
data <- d %>% rowwise() %>% mutate(Calibrated_SPL_Duet = ifelse(!is.null(Calibrated_SPL_Duet[[1]]),Calibrated_SPL_Duet[[1]], NA))

# Plot results in graph
ggplot(data, aes(x= distance)) +
  geom_point(aes(y=Expected), colour="red") +
  geom_point(aes(y=Calibrated_SPL_Duet), colour="blue") +
  geom_boxplot(aes(group = distance, y=Calibrated_SPL_Duet), alpha = 0) +
  geom_smooth(aes (x= distance, y=Calibrated_SPL_Duet), method = lm)

# If you want to take a look at the output in Excel, run this write.csv line:
#write.csv(d,"calibrated_amplitude_playback_duets.csv",na="NA",row.names=TRUE)
