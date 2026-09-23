# Processing fish tickets for rock fish trawl
# Blake Feist - created 7 May 2026

# packages
library(dplyr)
library(readr)
library(desc)
library(DescTools)

# load master 1994 - 2023 fish tickets file (used for the paper)
fishtickets <- readRDS("~/Documents/GitHub/VMS-pipeline/Confidential/raw_data/fish_tickets/all_fishtickets_1994_2023.rds")

# load master 2011 - 2025 fish tickets file (to add in 2024 data)
fishtickets <- readRDS("~/Documents/Projects/Ecosystem Science/Whale Entanglement/West Coast Take Reduction Team (TRT)/CONFIDENTIAL fish tickets from PacFIN/all_fishtix_2011to2025.rds")



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
# trawl_rock <- fishtickets_trawl %>%
#   filter(COMPLEX == "ROCK")

# filter for 7 rockfish species in RCA paper
seven_spp <- 
  filter(fishtickets_trawl,
         PACFIN_SPECIES_COMMON_NAME == 'BOCACCIO' |
           PACFIN_SPECIES_COMMON_NAME == 'NOM. BOCACCIO' |
           PACFIN_SPECIES_COMMON_NAME == 'CANARY ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'NOM. CANARY ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'COWCOD ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'NOM. COWCOD ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'DARKBLOTCHED ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'NOM. DARKBLOTCHED ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'PACIFIC OCEAN PERCH' |
           PACFIN_SPECIES_COMMON_NAME == 'NOM. POP' |
           PACFIN_SPECIES_COMMON_NAME == 'WIDOW ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'NOM. WIDOW ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'YELLOWEYE ROCKFISH' |
           PACFIN_SPECIES_COMMON_NAME == 'NOM. YELLOWEYE ROCKFISH'
  )


# filter for fish tix 2002 - 2023
seven_spp <- seven_spp %>%
  filter(LANDING_YEAR > 2001)

# save a tab delimited file
setwd("~/Documents/Projects/Ecosystem Science/RCAs/CONFIDENTIAL PacFIN fish tickets")
write_tsv(trawl_rock, "rockfish_trawl_fish_tix_2022-2023.txt")