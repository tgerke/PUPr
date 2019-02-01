#' pupR
#'
#' Your daily dose of PUP!
#'
#' @return A random track from PUP is served to you to click in the Console
#'
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_detect str_c
#' @importFrom xml2 read_html
#'
#' @examples
#' PUPr()
#'
#' @export
PUPr <- function(){
  # parse album the dream is over and get track links
  dream <- read_html("https://puptheband.bandcamp.com/album/the-dream-is-over")
  urls <- dream %>%
    html_nodes("a") %>%
    html_attr("href")
  dreamtracks <- urls %>%
    str_subset("track") %>%
    str_exclude("action") %>%
    unique() %>%
    str_c("https://puptheband.bandcamp.com", .)

  # parse album pup and get track links
  pup <- read_html("https://puptheband.bandcamp.com/album/pup")
  urls <- pup %>%
    html_nodes("a") %>%
    html_attr("href")
  puptracks <- urls %>%
    str_subset("track") %>%
    str_exclude("action") %>%
    unique() %>%
    str_c("https://puptheband.bandcamp.com", .)

  # add the new track
  kids <- "https://puptheband.bandcamp.com/track/kids"

  # concatenate into the sampling vector
  urls <- c(dreamtracks, puptracks, kids)
  track <- sample(urls, size = 1)

  embed_track(track)
}

str_exclude <- function(string, pattern) {
  string[!stringr::str_detect(string, pattern)]

embed_track <- function(track_url) {
  track <- read_html(track_url)
  track_embed_url <- track %>%
    html_nodes("meta") %>%
    rvest::html_attrs() %>%
    purrr::keep(~ "property" %in% names(.)) %>%
    purrr::keep(~ .["property"] == "og:video") %>%
    purrr::map_chr("content") %>%
    .[1]

  htmltools::tagList(
    htmltools::tags$iframe(
      src = track_embed_url,
      style="border: 0; width: 350px; height: 150px;"
    ),
    htmltools::tags$a(
      href = track_url, track_url
    )
  ) %>%
  htmltools::browsable()
}
