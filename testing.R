d <- read_csv("Calibrated_RMSlevels_homerangeduets_duetvsnoise_may23.csv", col_names = TRUE) # Load in file

# Load in PAMGuide package with Tony's revisions
source("tonyPAMGuide_Meta_revised_AD4.R")

# Variable defines window length that PAMGuide uses for SNR measurements.
window_length <- 5

# Path to location containign recordings.
d$path <- "~/Downloads/2016 SongMeter Playback Audio"

# Turn date into YMD format.
d$DATE <- parse_date_time(d$Date, "mdy")

# Creating StartTime column using FileName column.
d$StartTime <- str_split_fixed(d$FileName, "_",3)[,3] # Remove everything before 'HHMMSS.wav' pattern.
d$StartTime <- gsub(".wav","", d$StartTime) # Remove .wav pattern.
d$StartTime <- parse_date_time(d$StartTime, "HMS") # Turn HHMMSS into proper date-time.

d <- d %>% rowwise() %>% mutate(Calibrated_SPL_Duet = tonyPAMGuide_Meta(fullfile = paste0(path, "/", FileName), atype= "Broadband", StartTime=StartTime, CallOnset=CallOnset, seconds = window_length, windowDirection = "after", lcut= 700, hcut= 1400, calib= 1, ctype= "TS", Mh=-36, G=0, vADC=1.0, plottype= "Stats", channel = MicUsed)["RMSlev"]) # NOTE: Here channel is assigned to be MicUsed

newAmbientWave = readWave(paste0(d[1,]$path, "/", d[1,]$FileName), from = d[1,]$Start_ambient_sec, to = d[1,]$Start_ambient_sec + 5, units = "seconds")

writeWave(newAmbientWave, "ambientWave.wav")

newDuetWave = readWave(paste0(d[1,]$path, "/", d[1,]$FileName), from = d[1,]$Start_duet_sec, to = d[1,]$Start_duet_sec + 5, units = "seconds")

writeWave(newDuetWave, "duetWave.wav")

ambientRMS <- tonyPAMGuide_Meta(fullfile = "ambientWave.wav", atype= "Broadband", lcut= 700, hcut= 1400, calib= 1, ctype= "TS", Mh=-36, G=0, vADC=1.0, plottype= "Stats", channel = 1)["RMSlev"] # NOTE: Here channel is assigned to be MicUsed

duetRMS <- tonyPAMGuide_Meta(fullfile = "duetWave.wav", atype= "Broadband", lcut= 700, hcut= 1400, calib= 1, ctype= "TS", Mh=-36, G=0, vADC=1.0, plottype= "Stats", channel = 1)["RMSlev"] # NOTE: Here channel is assigned to be MicUsed

(ambientSpect <- ggspectro(
  readWave("ambientWave.wav"),
  flim = c(0,10),
  noisereduction = NULL,
  # tlim = c(0,20),
  ovlp = 50
) +
  stat_contour(
    geom = "polygon",
    aes(fill = after_stat(level)),
    bins = 30
  ) +
  scale_fill_continuous(
    name = "Amplitude\n(dB)\n",
    limits = c(-35,0),
    na.value = "transparent",
    low = "white",
    high = "black"
  ) +
  theme_bw() +
  theme(
    plot.margin = margin(t = 20, r = 10, b = 10, l = 20, unit = "pt")
  ))

(duetSpect <- ggspectro(
    readWave("duetWave.wav"),
    flim = c(0,10),
    noisereduction = NULL,
    # tlim = c(0,20),
    ovlp = 50
    ) +
  stat_contour(
    geom = "polygon",
    aes(fill = after_stat(level)),
    bins = 30
  ) +
  scale_fill_continuous(
    name = "Amplitude\n(dB)\n",
    limits = c(-35,0),
    na.value = "transparent",
    low = "white",
    high = "black"
  ) +
  theme_bw() +
  theme(
    plot.margin = margin(t = 20, r = 10, b = 10, l = 20, unit = "pt")
  ))
