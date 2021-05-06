library(rvest)
library(glue)
library(tidyverse)
library(curl)
library(lubridate)

# Some setup and read the current page of posts.
base_url = "https://www.donaldjtrump.com/desk"
page = read_html(base_url)
done = FALSE
offset = 0
posts = list()

# Why do a while loop? Because we don't know if the next page will exist
# until we read the current page, so it's impossible to know a priori how
# many pages there will be.
while(!done) {
  # We can think of page 1 = post 0, page 2 = post 10, etc. I use offset
  # for the number of posts / the URL but id for internally storing the
  # page number
  id = (offset / 10) + 1
  print(glue("Reading page {id}"))
  
  # Read the page
  posts[[id]] = page %>%
    # Single page data
    html_nodes("article") %>%
    # Single post data
    map_dfr(function(one_post) {
      # Pretty simple CSS selectors
      date = one_post %>% 
        html_node("div.date p") %>% 
        html_text()
      text = one_post %>% 
        html_node("div.ftdli-main-content p.ftd-post-text") %>%
        html_text()
      # The links use JavaScript rather than normal links, so easier
      # to extract them this way
      link = one_post %>% html_node("div.title") %>%
        html_attr("onclick") %>%
        str_match("location.href='(.*)';$") %>%
        .[, 2]
      post_id = one_post %>% html_attr("data-id")
      
      # Videos have a fancy video player, but also list the raw video
      # URL so we can just grab this
      has_video = one_post %>% html_attr("data-video-url")
      
      tibble(
        post_id = post_id,
        link = link,
        text = text,
        date = date,
        has_video = has_video
      )
    })
  

  # Check if there's a next page
  check_has_more = page %>%
    html_node("a.ftd-next") %>%
    html_attr("href")
  
  # If not, we're done (later on we should basically stop when the current
  # page overlaps with an already-downloaded post, but for now, let's just
  # grab everything)
  if(is.null(check_has_more) ||
     is.na(check_has_more)) break
  
  # Read next page
  offset = offset + 10
  blend_url = glue("{base_url}/P{offset}")
  page = read_html(blend_url)
}

# Use lubridate to parse dates
all_posts = bind_rows(posts) %>%
  mutate(date = parse_date_time(date, "I:Mp m d, y"))

# Download the videos using curl
all_posts %>%
  pull(has_video) %>%
  unique() %>%
  setdiff(NA_character_) %>%
  map(function(download_url) {
    dest_file = download_url %>%
      str_match("(.*)/([^/]*)$") %>%
      .[, 3]
    print(glue("Downloading {download_url}"))
    
    # This unfortunately prints an output rather than being silent,
    # TODO: use a sink to redirect the output
    curl_download(download_url, glue("video/{dest_file}"))
  })

# Write post output
write_csv(all_posts, "posts.csv")
