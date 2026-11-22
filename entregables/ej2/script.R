#Leer API Key

readRenviron(file.path(getwd(), ".Renviron"))
api_key <- Sys.getenv("NYT_API_KEY")
nchar(api_key)

# Instalar librerías (quitar comentario solo si es necesario)
# install.packages(c(
#   "httr2",
#   "tidyverse",
#   "tidytext",
#   "wordcloud2"
# ))

#Cargar librerías

library(httr2)
library(tidyverse)
library(tidytext)
library(wordcloud2)

#Parámetros para API

query <- "hasan piker"
begin_date <- "20260901"
end_date <- "20260930"

#Realizar petición a API

response <- request(
  "https://api.nytimes.com/svc/search/v2/articlesearch.json"
) %>%
  req_url_query(
    q = query,
    begin_date = begin_date,
    end_date = end_date,
    page = 0,
    'api-key' = api_key
  ) %>%
req_perform()
resp_status(response)

#Convertir JSON a lista de R

data <- response %>%
  resp_body_json()

str(data, max.level = 3)

#Obtener todos los artículos anidados en la respuesta

responses <- map(0:3, function(page) {
  
  Sys.sleep(15)
  
  request(
    "https://api.nytimes.com/svc/search/v2/articlesearch.json"
  ) %>%
    req_url_query(
      q = query,
      begin_date = begin_date,
      end_date = end_date,
      page = page,
      `api-key` = api_key
    ) %>%
    req_perform() %>%
    resp_body_json()
})

articles <- purrr::map(
  responses,
  ~ .x$response$docs
) |>
  purrr::flatten()

length(articles)

#Generar tabla para nube de palabras

df <- map_dfr(articles, function(article) {
  tibble(
    headline = article$headline$main %||% NA_character_,
    abstract = article$abstract %||% NA_character_,
    pub_date = article$pub_date %||% NA_character_,
    section = article$section_name %||% NA_character_,
    subsection = article$subsection_name %||% NA_character_,
    url = article$web_url %||% NA_character_
  )
})

nrow(df)

#Preparación para nube de palabras

text <- df |>
  mutate(
    text = paste(headline, abstract, sep = " ")
  ) |>
  pull(text) |>
  paste(collapse = " ")

words <- tibble(text = text) |>
  unnest_tokens(word, text)

words_clean <- words |>
  anti_join(stop_words, by = "word")

word_freq <- words_clean |>
  count(word, sort = TRUE) |>
  rename(freq = n)

#Crear nube de palabras

wordcloud2(
  data = word_freq,
  size = 1,
  minSize = 0,
  color = "random-dark",
  backgroundColor = "white"
)

#Revisar titulares

View(df |> select(pub_date, headline, abstract))
df |>
  mutate(text = str_to_lower(paste(
    coalesce(headline, ""),
    coalesce(abstract, "")
  ))) |>
  filter(str_detect(text, "hasan|piker")) |>
  select(pub_date, headline, abstract)