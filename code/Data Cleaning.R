
# ------------------------------------------------------------
# Nettoyage des données: Analyse de contenu (Candidats 2025), Élections Montréal
# Code écrit par Alan Nemirovski avec l'aide de Microsoft Copilot
# ------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(stringr)

# --- Lecture des données et initialisation ---

cands.data <- read.csv("data/mtlcandidates2025_textdata_RAW.csv", stringsAsFactors = FALSE)

questions <- c("Présentation de la personne candidate",
               "Pourquoi la personne candidate souhaite représenter les Montréalais\\(es\\)\\?",
               "Comment la personne candidate souhaite améliorer la qualité de vie à Montréal\\?",
               "Quelles sont les deux principales priorités de la personne candidate\\?",
               "Quel est le lieu préféré de la personne candidate à Montréal\\?")

footer <- "Élections Montréal est un organisme neutre et non partisan"


cands.data.clean <- cands.data %>%
  mutate(header = str_extract(complete_text, paste0("^.*?(?=", questions[1], ")")),
         answers = str_extract(complete_text, paste0("(?=", questions[1], ").*?(?=", footer, ")")),
         footerx = str_extract(complete_text, paste0(footer, ".*$")))


extract_section <- function(text, start, end = NULL) {
  if (is.null(end))
    {
    str_trim(str_extract(text, paste0("(?<=", start, ").*$")))
    } 
  else {
    str_trim(str_extract(text, paste0("(?<=", start, ").*?(?=", end, ")")))
    }
}

# --- Séparation des réponses des candidats dans différentes colonnes ---

cands.data.clean <- cands.data.clean |> 
  rename(cand.name = cand_name) |> 
  mutate(presentation = extract_section(answers, questions[1], questions[2]), 
         motivation  = extract_section(answers, questions[2], questions[3]),
         comment.ameliorer.mtl  = extract_section(answers, questions[3], questions[4]),
         prioritees  = extract_section(answers, questions[4], questions[5]),
         lieu.prefere = extract_section(answers, questions[5]))

# --- Enregistrement des données des candidats dans des colonnes distinctes --- 

role_markers <- c("Mairesse", "Conseillère")
district_markers <- c("de ville", "de l'arrondissement", "d'arrondissement", "l'arrondissement")
contact_terms <- c("courriel", "linkedin", "facebook", "instagram", "twitter", "site", "x")

role.pat <- paste0("\\b(", paste(role_markers, collapse = "|"), ")\\b")
district.pat <- paste(district_markers, collapse = "|")
contact.pat <- paste(contact_terms, collapse = "|")

cands.data.clean <- cands.data.clean |>
  mutate(cand.info = str_trim(str_remove(header, paste0("^.*", fixed(cand.name)))), 
         party = str_trim(str_extract(cand.info, paste0("(?i)^.*?(?=", role.pat, ")"))),
         role.info = str_trim(str_extract(cand.info, paste0("(?i)", role.pat, ".*$"))),
         district = str_trim(str_extract(role.info, paste0("(?i)(?<=", district.pat, ")\\s+.*$"))),
         district = str_trim(str_remove(district, paste0("(?i)\\b(", contact.pat, ")\\b.*$"))),
         role = str_trim(str_remove(role.info, paste0("(?i)\\s+(", district.pat, ").*$"))))

cands.data.clean[cands.data.clean == "Non disponible"] <- NA
cands.data.clean[cands.data.clean == ""] <- NA


# --- Sélection des colonnes d'intérêt et nettoyage final ---

cands.data.clean <- cands.data.clean |> 
  select(cand.name, role, district, party, presentation, motivation, comment.ameliorer.mtl, prioritees, lieu.prefere) |> 
  mutate(role = case_when(is.na(district) ~ "Mairesse ou maire de la ville", TRUE ~ role),
         role = case_when(role == "Mairesse ou maire" ~ "Mairesse ou maire de l'arrondissement", TRUE ~ role),
         role = as.factor(role),
         district = as.factor(district),
         party = as.factor(party))
