library("tidyverse")
library("lubridate")
library("rvest")
library("glue")

# Helper functions (local): source("spotify_funs.R")
# Progress bar (local): source("progress_bar.R")


# Enter usernames for MEGABLEND
usernames <- c("Multiple", "usernames", "here")

# Exclude tracks that have been in MEGABLEND in the last 90 days
MEGABLEND_hist <- read_rds("MEGABLEND_hist.rds")

remove <- MEGABLEND_hist %>% 
  filter(date >= (today() - days(90)), date != today())



# Load spotify history en matched scrobbles (both local)
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
  





# Import scrobbles from last.fm (last 90 days)
message("Loading scrobbles..")
pb <- progress_bar(length(usernames))
scrobbles <- map_dfr(usernames, function(username) {  
  page1 <- read_html(glue("https://www.last.fm/user/{username}/library/tracks?date_preset=LAST_90_DAYS"))
  page2 <- read_html(glue("https://www.last.fm/user/{username}/library/tracks?date_preset=LAST_90_DAYS&page=2"))
  page3 <- read_html(glue("https://www.last.fm/user/{username}/library/tracks?date_preset=LAST_90_DAYS&page=3"))
  page4 <- read_html(glue("https://www.last.fm/user/{username}/library/tracks?date_preset=LAST_90_DAYS&page=4"))
  
  # Rate limiting
  Sys.sleep(3)
  
  # Progress
  pb$tick()
  
  # Extract info from pages
  map_dfr(list(page1, page2, page3, page4), function(html) {
    tibble(
      username = username,
      track_name = html %>% 
        html_nodes(".chartlist-name") %>% 
        html_text(trim = TRUE), 
      artist     = html %>% 
        html_nodes(".chartlist-artist") %>% 
        html_text(trim = TRUE),
      n_scrobbles = html %>% 
        html_nodes(".chartlist-count-bar-value") %>% 
        html_text(trim = TRUE) %>% 
        parse_number()
    )
  })
})

# Initial playlist: 
# - Tracks that have not been added in last 90 days
# - wt = (mean) relative importance for user = n_scrobbles / max(n_scrobbles)
# - n_psn = number of users that have track in top-200 = n()
# - Order (desc.) by wt*n_psn, tiebreaker: total number of scrobbles (n_scrobbles)
# - Max. 1 track per (main) artist
playlist <- scrobbles %>%
  anti_join(remove, by = c("artist", "track_name")) %>%
  group_by(username) %>%
  mutate(wt = n_scrobbles / max(n_scrobbles)) %>%
  group_by(artist, track_name) %>%
  summarise(n_psn     = n(),
            n_scrobbles = sum(n_scrobbles),
            wt        = mean(wt),
            .groups = "drop") %>%
  arrange(desc(wt*n_psn), desc(n_scrobbles)) %>%
  distinct(artist, .keep_all = TRUE)

# Multiple versions of the same track may exist 
# Use 'my' version so my stats won't get messed up ;-)
message("Getting new track_id's for matched tracks..")
playlist_matched <- playlist %>% 
  inner_join(matched, by = c("artist", "track_name" = "track_title")) %>% 
  mutate(track_id_relinked = relink_track_info(track_id, album_id)) %>% 
  mutate(track_id = ifelse(!is.na(track_id_relinked), track_id_relinked, track_id)) %>% 
  group_by(artist, track_name) %>% 
  slice_max(playcount_spotify, n = 1, with_ties = FALSE) %>% 
  ungroup()
  
# Use Spotify Search API if I haven't played track
# Lookup at most 50 tracks to save time
message("Finding results for unmatched tracks..")
search_results <- playlist %>% 
  anti_join(playlist_matched, by = c("artist", "track_name")) %>% 
  slice(1:pmin(50, n())) %>% 
  mutate(query = glue('artist:"{artist}" track:"{track_name}"'),
         search_results = spotify_search(query, type = "track", nested = TRUE))

# Not all tracks will return a result: 
# search_results %>% 
#   mutate(no_result = map_lgl(search_results, is_empty)) %>% 
#   count(no_result)

# Bind results and reorder for final playlist:
playlist_def <- playlist_matched %>% 
  bind_rows(search_results %>% 
              unnest(search_results) %>% 
              group_by(artist, track_name) %>% 
              slice(1) %>% 
              ungroup()) %>% 
  arrange(desc(n_psn*wt), desc(n_scrobbles)) %>% 
  slice(1:30) %>% 
  select(artist:wt, track_id)

# Keep track of MEGABLEND history so tracks aren't recycled:
bind_rows(MEGABLEND_hist %>% filter(date != today()),
          playlist_def %>% mutate(date = today())) %>%
  write_rds("MEGABLEND_hist.rds")

# Update playlist using API:
playlist_def %>%
  pull(track_id) %>%
  playlist_add_tracks(playlist_id = "2vB12PcJkkxxAkgOAueH32", remove_all_current = TRUE)
