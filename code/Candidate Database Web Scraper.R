
# ------------------------------------------------------------
# Web Scraper : Profils des candidats aux élections municipales de Montréal 2025 (N = 421, Contenu FR)
# Code écrit par Alan Nemirovski avec l'aide de Microsoft Copilot
# ------------------------------------------------------------

library(rvest)
library(xml2)
library(httr)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(robotstxt)
library(urltools)
library(tibble)

has_memoise <- requireNamespace("memoise", quietly = TRUE)
if (has_memoise) library(memoise)

# --- Configurations initiales ---

BASE_INDEX <- "https://elections.montreal.ca/fr/repertoire-des-candidats/"
DOMAIN     <- "https://elections.montreal.ca"

# Enter researcher name and email below
UA_STRING  <- "FirstnameLastname-ResearchBot/2.0 (+mailto:youremail@example.com) R/rvest"

OUT_DIR <- "data"
OUT_CSV <- file.path(OUT_DIR, "mtlcandidates2025_textdata_RAW.csv")  # only two columns

VERBOSE <- TRUE
PAUSE_DIR      <- 2.2
PAUSE_PROFILE  <- 2.2
RETRY_TIMES    <- 6
RETRY_PAUSEMIN <- 2
RETRY_PAUSECAP <- 20

SILENCE_ROBOTSTXT <- TRUE
TOTAL_PAGES       <- 15 # confirm total number of pages on website

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
log_msg <- function(...) if (VERBOSE) message(...)

# --- Sessions et en-têtes ---

SESSION <- httr::handle(DOMAIN)
make_headers <- function(referer) {
  c(
    "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "fr-CA,fr;q=0.9,en;q=0.8",
    "Cache-Control"   = "no-cache",
    "Pragma"          = "no-cache",
    "Connection"      = "keep-alive",
    "DNT"             = "1",
    "Referer"         = referer
  )
}

# --- robots.txt ---

is_allowed_raw <- function(url) robotstxt::paths_allowed(paths = url, user_agent = UA_STRING)
if (has_memoise) is_allowed_raw <- memoise::memoise(is_allowed_raw)

is_allowed <- function(url) {
  out <- if (SILENCE_ROBOTSTXT) {
    tmp <- NULL
    invisible(capture.output({ tmp <- is_allowed_raw(url) }))
    tmp
  } else {
    is_allowed_raw(url)
  }
  if (is.na(out)) {
    message("robots check indeterminate, defaulting TRUE: ", url)
    TRUE
  } else out
}

# --- GET robuste avec jitter + RETRY + Retry-After ---

safe_GET <- function(url, pause = 1.0, referer = BASE_INDEX, times = RETRY_TIMES,
                     pause_min = RETRY_PAUSEMIN, pause_cap = RETRY_PAUSECAP) {
  Sys.sleep(pause + runif(1, 0.2, 0.8))
  allowed <- tryCatch(is_allowed(url), error = function(e) NA)
  if (is.na(allowed)) {
    message("robots indeterminate; allow: ", url)
  } else if (!allowed) {
    message("Blocked by robots.txt, skipping: ", url); return(NULL)
  }
  resp <- tryCatch(
    httr::RETRY(
      "GET", url,
      times        = times,
      pause_min    = pause_min,
      pause_cap    = pause_cap,
      terminate_on = c(404),
      httr::user_agent(UA_STRING),
      httr::add_headers(.headers = make_headers(referer)),
      handle       = SESSION,
      httr::timeout(60)
    ),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  sc <- httr::status_code(resp)
  if (sc %in% c(429, 503)) {
    ra <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
    if (!is.na(ra) && ra > 0) {
      message("Server asked to wait (Retry-After=", ra, "s) for: ", url)
      Sys.sleep(ra + 1)
      resp <- tryCatch(
        httr::GET(url,
                  httr::user_agent(UA_STRING),
                  httr::add_headers(.headers = make_headers(referer)),
                  handle = SESSION,
                  httr::timeout(60)),
        error = function(e) NULL
      )
    }
  }
  if (is.null(resp) || httr::http_error(resp)) {
    message("Final failure for: ", url, " (status ", if (!is.null(resp)) httr::status_code(resp) else "NA", ")")
    return(NULL)
  }
  resp
}

read_html_safe <- function(url, pause = 1.0, referer = BASE_INDEX) {
  resp <- safe_GET(url, pause = pause, referer = referer)
  if (is.null(resp)) return(NULL)
  tryCatch(read_html(resp), error = function(e) NULL)
}

# --- Découverte de liens candidats (CSS + repli vers HTML brut) ---

rx_is_candidate <- paste0("^", DOMAIN, "/fr/candidates/[^/]+/?$")

extract_links_css <- function(doc, base_url) {
  main <- doc %>% html_element("main"); if (is.null(main)) main <- doc
  hrefs <- main %>% html_elements("a[href]") %>% html_attr("href") %>% discard(is.na)
  if (!length(hrefs)) return(character())
  abs <- unique(map_chr(hrefs, ~ xml2::url_absolute(.x, base_url)))
  abs[str_detect(abs, rx_is_candidate)]
}

extract_links_regex <- function(doc, base_url) {
  txt <- as.character(doc)
  hits <- stringr::str_match_all(txt, "/fr/candidates/[a-z0-9\\-]+/?")[[1]]
  if (is.null(hits) || !nrow(hits)) return(character())
  rels <- unique(hits[,1])
  unique(map_chr(rels, ~ xml2::url_absolute(.x, base_url)))
}

extract_candidate_links <- function(doc, base_url) {
  links <- extract_links_css(doc, base_url)
  if (!length(links)) links <- extract_links_regex(doc, base_url)
  unique(links)
}

# --- Créer des URL d'index avec pagination PATH (1 à 15) ---

index_urls <- c(
  BASE_INDEX,
  paste0(BASE_INDEX, "page/", 2:TOTAL_PAGES, "/")   # <-- key change
)

candidate_urls <- character()
for (u in index_urls) {
  log_msg("Directory page: ", u)
  doc <- read_html_safe(u, pause = PAUSE_DIR, referer = u)  # dynamic Referer
  if (is.null(doc)) { message("Skipping unreachable directory page: ", u); next }
  new_links <- extract_candidate_links(doc, u)
  before <- length(candidate_urls)
  candidate_urls <- unique(c(candidate_urls, new_links))
  after  <- length(candidate_urls)
  log_msg("  links on page: ", length(new_links), " | new total: ", after, " (added ", after - before, ")")
}
message("Total candidate URLs collected: ", length(candidate_urls))

if (!length(candidate_urls)) {
  stop("No candidate URLs were collected. Raise PAUSE_* or check connectivity.")
}

# --- Minimal parser: ONLY cand_name + complete_text ---

parse_candidate_page <- function(url) {
  doc <- read_html_safe(url, pause = PAUSE_PROFILE, referer = BASE_INDEX)
  if (is.null(doc)) return(tibble(cand_name = NA_character_, complete_text = NA_character_))
  main <- doc %>% html_element("main"); if (is.null(main)) main <- doc
  
  h2s    <- main %>% html_elements("h2")
  titles <- if (length(h2s)) map_chr(h2s, html_text2) else character()
  titles <- str_squish(titles)
  
  is_prompt <- str_detect(
    titles,
    regex("^Présentation|^Pourquoi\\b|^Comment\\b|^Quelles?\\s+sont\\s+les\\s+deux\\s+principales\\s+priorités|^Quel\\s+est\\s+le\\s+lieu\\s+préféré",
          ignore_case = TRUE)
  )
  if (any(is_prompt, na.rm = TRUE)) titles <- titles[is_prompt]
  
  cand_name   <- if (length(titles)) paste(titles, collapse = " || ") else NA_character_
  complete_text <- main %>% html_text2() %>% str_squish()
  
  tibble(cand_name = cand_name, complete_text = complete_text)
}

# --- Analyseur minimal : UNIQUEMENT cand_name + complete_text ---

message("Fetching ", length(candidate_urls), " profiles ...")
safe_parse <- purrr::safely(
  parse_candidate_page,
  otherwise = tibble(cand_name = NA_character_, complete_text = NA_character_),
  quiet = FALSE
)
chunks  <- purrr::map(candidate_urls, ~ safe_parse(.x))

errs <- purrr::keep(chunks, ~ !is.null(.x$error))
if (length(errs)) { message("Some profiles failed to parse. First error shown:"); print(errs[[1]]$error) }

results <- dplyr::bind_rows(purrr::map(chunks, "result")) %>%
  select(cand_name, complete_text)

# Vérification de cohérence : exactement deux colonnes, aucun nom vide
stopifnot(identical(names(results), c("cand_name", "complete_text")))
stopifnot(all(nzchar(names(results))))

# --- Enregistrer la sortie dans un fichier .csv ---

write_csv(results, OUT_CSV, na = "")
message("Saved: ", OUT_CSV)
message("Rows:  ", nrow(results))
