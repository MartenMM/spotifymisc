setwd("D:/R/Muziek/Spotify")

suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("rvest"))
suppressPackageStartupMessages(library("glue"))

source('spotify_funs.R')
source('D:/R/[code]/progress_bar.R')
source("D:/R/Muziek/lastfm/lfm_get_toptracks.R")

usernames <- c("HiAndFi", "Maluki1", "KennedyJF", "Maaaaaaaaaax", "Karii-Anne", "jenny-oc", "Afraaaaahhh", "silvanbrummen", "MaggieMacster", "tplazarov")
MEGABLEND_hist <- read_rds("MEGABLEND_hist.rds")

# Remove tracks that have been added in the past 180 days:
remove <- MEGABLEND_hist %>%
  filter(date >= (today() - days(180)), date != today()) %>%
  mutate(across(c(artist, track_title), tolower))

# Calculate waiting period for artists that have been added in the past 90 days:
wait <- MEGABLEND_hist %>%
  filter(date >= (today() - days(90)), date != today()) %>%
  group_by(artist = tolower(artist)) %>%
  summarise(wait       = n(),
            last_added = as_date(max(date)),
            next_add   = last_added + weeks(wait),
            .groups    = "drop") %>%
  filter(next_add > today())

# Load locally cached data:
load("play_history.RData")
play_history <- raw %>%
  count(track.id) %>%
  rename(playcount_spotify = n)
matched <- map_dfr(c("match_exact", "match_full", "match_simple", "search_results"), function(x) {
  read_rds(glue("scrobbles/{x}.rds")) %>%
    mutate(match_type = ifelse(x == "search_results", "search", x))
}) %>%
  left_join(play_history, by = c("track_id" = "track.id")) %>%
  mutate(playcount_spotify = replace_na(playcount_spotify, 0))

track_info <- read_rds("cache/track_info.rds")
album_info <- read_rds("cache/album_info.rds")
artist_info <- read_rds("cache/artist_info.rds")





# Import scrobbles from last.fm
message("Loading scrobbles..")
pb <- progress_bar(length(usernames))
scrobbles <- map_dfr(usernames, function(username) {

  scrobbles <- lfm_get_toptracks(username, period = "3month", top_n = 500)
  pb$tick()

  if (length(scrobbles) == 0) {
    return(
      NULL
    )
  }

  scrobbles %>%
    transmute(username = username,
              order    = as.numeric(rank),
              artist,
              track_title,
              scrobbles = as.numeric(playcount))
}) %>%
  distinct() %>%
  group_by(username) %>%
  mutate(wt = scrobbles / max(scrobbles)) %>%
  ungroup()

# Create 'raw' playlist:
# > remove tracks in `remove` and `wait`
# > calculate relative listening score
# > sort by relative listening score, then by total number of scrobbles
# > at most one track per artist
playlist <- scrobbles %>%
  mutate(across(c(artist, track_title), tolower)) %>%
  anti_join(remove, by = c("artist", "track_title")) %>%
  anti_join(wait, by = "artist") %>%
  group_by(artist, track_title) %>%
  summarise(n_psn     = n(),
            scrobbles = sum(scrobbles),
            wt        = mean(wt),
            .groups = "drop") %>%
  arrange(desc(wt*n_psn), desc(scrobbles)) %>%
  distinct(artist, .keep_all = TRUE) %>%
  left_join(scrobbles %>%
              mutate(across(c(artist, track_title), tolower)) %>%
              group_by(artist, track_title) %>%
              summarise(username = paste(username, collapse = ", "), .groups = "drop"),
            by = c("artist", "track_title")) %>%
  left_join(scrobbles %>%
              mutate(across(c(artist, track_title), tolower)) %>%
              select(username, artist, track_title, order),
            by = c("username", "artist", "track_title")) %>%
  arrange(desc(wt*n_psn), desc(scrobbles), order)

# Check if tracks in cached data have new track IDs:
message("Getting new track_id's for matched tracks..")
playlist_matched <- playlist %>%
  inner_join(matched %>%
               mutate(across(c(artist, track_title), tolower)),
             by = c("artist", "track_title" = "track_title")) %>%
  mutate(track_id_relinked = relink_track_info(track_id, album_id)) %>%
  mutate(track_id = ifelse(!is.na(track_id_relinked), track_id_relinked, track_id)) %>%
  group_by(artist, track_title) %>%
  slice_max(playcount_spotify, with_ties = FALSE) %>%
  ungroup()

# Search track IDs for tracks that are not in cached data:
message("Finding results for unmatched tracks..")
search_results <- playlist %>%
  anti_join(playlist_matched, by = c("artist", "track_title")) %>%
  slice(1:pmin(50, n())) %>%
  mutate(query = glue('artist:"{artist}" track:"{track_title}"'),
         search_results = spotify_search(query, type = "track", nested = TRUE))

# Keep tracks that have a track ID:
playlist_matched <- playlist_matched %>%
  bind_rows(search_results %>%
              unnest(search_results, names_repair = "universal") %>%
              rename(track_title = track_title...2) %>%
              group_by(artist, track_title) %>%
              slice(1) %>%
              ungroup() %>%
              mutate(main_artist_id = map_chr(artists, ~ pull(., artist_id) %>% first)) %>%
              select(artist, track_title, any_of(names(playlist_matched))))

# Exclude children's music
excludes_genres <- playlist_matched %>%
  pull(main_artist_id) %>%
  subset(!is.na(.)) %>%
  spotify_get_artists() %>%
  filter(!map_lgl(genres, is_empty)) %>%
  unnest(genres) %>%
  group_by(main_artist_id = id) %>%
  summarise(kinder = sum(str_detect(genres, c("(child)|(kind)")))) %>%
  filter(kinder > 0) %>%
  pull(main_artist_id)



# Initial playlist: thirty most-listened tracks that have at least two listeners
playlist_multi <- playlist_matched %>%
  filter(!main_artist_id %in% excludes_genres) %>%
  arrange(desc(n_psn*wt), desc(scrobbles), order) %>%
  slice(1:30) %>%
  filter(n_psn > 1)

# Calculate how many tracks a single listener can contribute:
n_multi <- nrow(playlist_multi)
n_per_person <- ceiling((30 - n_multi) / nrow(distinct(scrobbles, username))) + 1

playlist_solo <- playlist_matched %>%
  filter(!main_artist_id %in% excludes_genres) %>%
  arrange(desc(n_psn*wt), desc(scrobbles), order) %>%
  filter(n_psn == 1) %>%
  group_by(username) %>%
  slice_max(n_psn*wt, n = n_per_person, with_ties = FALSE) %>%
  ungroup()

# Add (max. `n_per_person`) tracks until 30 tracks are selected:
playlist_def <- bind_rows(
  playlist_multi,
  playlist_solo
) %>%
  arrange(desc(n_psn*wt), desc(scrobbles), order) %>%
  slice(1:30) %>%
  select(artist:wt, track_id) %>%
  left_join(scrobbles %>%
              mutate(across(c(artist, track_title), tolower)) %>%
              group_by(artist, track_title) %>%
              summarise(username = paste(glue("{username} ({scrobbles})"), collapse = ", "), .groups = "drop"),
            by = c("artist", "track_title"))

# Write history to disk:
bind_rows(MEGABLEND_hist %>% filter(date != today()),
          playlist_def %>% mutate(date = today())) %>%
  write_rds("MEGABLEND_hist.rds")

# Add tracks to playlist using API:
playlist_def %>%
  pull(track_id) %>%
  playlist_add_tracks(playlist_id = "2vB12PcJkkxxAkgOAueH32", remove_all_current = TRUE, verbose = FALSE)

message("Done!")
Sys.sleep(3)



MEGABLEND_hist %>%
  filter(date == today())

# Restjes

# gini <- scrobbles %>%
#   anti_join(remove, by = c("artist", "track_title")) %>%
#   group_by(username) %>%
#   summarise(gini = DescTools::Gini(scrobbles))


read_rds("MEGABLEND_hist.rds") %>%
  filter(date == max(date)) %>%
  select(artiest = artist, titel = track_title, scrobbles, aant_personen = n_psn, details = username) %>%
  print(n = 30)
