#' pupR
#'
#' Your daily dose of PUP!
#'
#' @return A random track from PUP is served to you to click in the Console
#'
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom stringr str_detect
#'
#' @examples
#' PUPr()
#'
#' @export
PUPr <- function(){
  library(rvest)
  library(stringr)

  # parse album the dream is over and get track links
  dream <- read_html("https://puptheband.bandcamp.com/album/the-dream-is-over")
  urls <- dream %>%
    html_nodes("a") %>%
    html_attr("href")
  dreamtracks <- urls[which(str_detect(urls, 'track') & !str_detect(urls, 'action'))] %>%
    unique() %>%
    str_c("https://puptheband.bandcamp.com", .)

  # parse album pup and get track links
  pup <- read_html("https://puptheband.bandcamp.com/album/pup")
  urls <- pup %>%
    html_nodes("a") %>%
    html_attr("href")
  puptracks <- urls[which(str_detect(urls, 'track') & !str_detect(urls, 'action'))] %>%
    unique() %>%
    str_c("https://puptheband.bandcamp.com", .)

  # add the new track
  kids <- "https://puptheband.bandcamp.com/track/kids"

  # concatenate into the sampling vector
  urls <- c(dreamtracks, puptracks, kids)
  print(sample(urls, size = 1))
}

