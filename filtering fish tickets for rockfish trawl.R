# Processing fish tickets for rock fish trawl
# Blake Feist - created 7 May 2026

# packages
library(dplyr)
library(readr)
library(desc)
library(DescTools)

# load master 1994 - 2023 fish tickets file (used for the paper)
fishtickets <- readRDS("~/Documents/GitHub/VMS-pipeline/Confidential/raw_data/fish_tickets/all_fishtickets_1994_2023.rds")

# filter for bottom trawl gear only
fishtickets_trawl <- fishtickets %>%
  filter(
    PACFIN_GEAR_DESCRIPTION == 'DANISH/SCOTTISH SEINE (TRAWL)' |
      PACFIN_GEAR_DESCRIPTION == 'FLATFISH TRAWL' |
      PACFIN_GEAR_DESCRIPTION == 'GROUNDFISH TRAWL (OTTER)' |
      PACFIN_GEAR_DESCRIPTION == 'GROUNDFISH TRAWL, FOOTROPE < 8 IN.' |
      PACFIN_GEAR_DESCRIPTION == 'GROUNDFISH TRAWL, FOOTROPE > 8 IN.' |
      PACFIN_GEAR_DESCRIPTION == 'ROLLER TRAWL' |
      PACFIN_GEAR_DESCRIPTION == 'SELECTIVE FF TRAWL, SMALL FOOTROPE'
  )

# filter for rockfish only
trawl_rock <- fishtickets_trawl %>%
  filter(COMPLEX == "ROCK")


# filter for fish tix 2002 - 2023
rockfish_tix <- trawl_rock %>%
  filter(LANDING_YEAR > 2001)

# save a tab delimited file
setwd("~/Documents/Projects/Ecosystem Science/RCAs/CONFIDENTIAL PacFIN fish tickets")
write_tsv(trawl_rock, "rockfish_trawl_fish_tix_2022-2023.txt")