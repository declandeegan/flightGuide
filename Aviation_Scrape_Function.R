# Libraries
install.packages("janitor")
library(tidyverse)
library(stringr)
library(rvest)
library(httr2)
library(polite)
library(jsonlite)
library(writexl)
library(readxl)
library(readr)
library(janitor)

# Gameplan:
# 1. Run the function with LIMIT=100, grab the number of total flights from it.
# 2. Build our pages enumeration (e.g. 1-2399).
# 3. Run the map loop.
# 4. bind_rows to create big dataframe.
# 5. Save the data.
# 6. TODO: Redo for other dates.
# 7. TODO: Read in and bind together all daily flight dataframes.



#------------------------------

#all airports in alphabetical order by iata code - we can use this to join later
minnick_api_key <- "d9754f293db4dafccf1e9419b9cf3257"

airport_request <- request("http://api.aviationstack.com/v1/airports")

airport_request <- airport_request |>
  req_url_query(
    "access_key" = minnick_api_key,
    "limit" = 6706
  )

response <- req_perform(airport_request)

airports_all <- response |> resp_body_json(simplifyVector = TRUE)

full_airport_df <- airports_all$data |> filter(country_iso2 == "US")

#scraping and joining wikipedia page for all US airports and the cities they serve

# every airport in the US w/ city it serves
url <- "https://en.wikipedia.org/wiki/List_of_airports_in_the_United_States"

html <- read_html(url)

table <- html |>
  html_element("table.wikitable") |>
  html_table()

clean_table <- table |>
  select(City, IATA, Role, Enplanements) |>
  filter(IATA != "") |>
  mutate(Role = str_remove(Role, pattern = "P-"))  |>
  mutate(Enplanements = parse_number(Enplanements)) |>
  rename(Enplanements_2022 = Enplanements)

airport_city_df <- full_airport_df |>
  left_join(clean_table, join_by(iata_code == IATA)) |>
  filter(!is.na(City), Role != "N", Enplanements_2022 > 2000000) |> #removing nonhubs that serve less that .05% of total US passenger enplanements, as well as some small hubs who serve < 850k enplanements per year (top 100 airports)
  arrange(desc(Enplanements_2022))


# Turn airports into a list.
airport_list <- airport_city_df |> pull(iata_code)
airport_list
# # Using this list to filter our scrape only for the 100 largest US airports; removed all that service <800k enplanements or <.35% of yearly US enplanements 
#--------------------------




# Variables UPDATE PROJECT FOLDER path for ur laptop 
PROJECT_FOLDER <- "/Users/henryminnick/Desktop/Data_Mgmt_Final_Project"
IS_DEBUG <- TRUE
api_key <- "d9754f293db4dafccf1e9419b9cf3257"
api_base <- "https://api.aviationstack.com/v1"
endpoint_flights <- "/flights"
URL_FLIGHTS <- str_glue("{api_base}{endpoint_flights}")
LIMIT <- 100


politely_get_flights_page <- function(url, page, dest_airport, date){
  # Build request object
  req <- request(URL_FLIGHTS) |>
    req_url_query(
      access_key = api_key,
      flight_date = date,
      dep_iata = dest_airport,
      limit = LIMIT,
      offset = LIMIT * page # This needs to be equal to LIMIT to move forward 1 full page - otherwise we get duplicates.
      # Can include other parameters here, such as flight_date
    ) |>
    req_throttle(10 / 60, realm = URL_FLIGHTS) # This makes sure we only make 10 requests per minute to stay within rate limitations.
  # Source: https://httr2.r-lib.org/articles/wrapping-apis.html
  
  # Make the request
  resp <- req_perform(req)
  
  # Put data into a dataframe
  results <- resp |>
    resp_body_json(simplifyVector = TRUE)
  
  num_flights <- results$pagination$total
  
  df_results <- results$data
  
  return(list(df_results, num_flights)) 
  # We return the page number for page 0 to help us build the correctly-sized
  # vector of page numbers.
}

politely_get_flights <- function(url, dest_airport, date){
  results_p0 <- politely_get_flights_page(
    url = url, 
    page = 0,
    dest_airport = dest_airport,
    date = date)
  
  # Get the page 0 data.
  df_p0 <- results_p0[[1]]
  # Get the total page number
  NUM_FLIGHTS <- results_p0[[2]]
  
  if (NUM_FLIGHTS == 0) {
    print("There were no flights on Date {date} for airport {dest_airport}.")
    return(data.frame())
  }
  
  print(str_glue("For Airport {dest_airport} on Date {date}, there are {NUM_FLIGHTS} flights."))
  
  # Build the vector of page numbers
  total_pages <- 1:floor(NUM_FLIGHTS / LIMIT)
  
  # Loop over the result pages:
  print(str_glue("Getting data for {date}, {dest_airport}."))
  results_p1N <- purrr::map(
    .x = total_pages, 
    .f = politely_get_flights_page, 
    url = url,
    dest_airport = dest_airport,
    date = date, 
    .progress = TRUE)
  
  # Bind all pages together.
  print(str_glue("Cleaning data for {date}, {dest_airport}."))
  dfs_p1N <- map(results_p1N, 1)
  # Bind them all together
  df_p1N <- bind_rows(dfs_p1N, .id = "page")
  
  # Bind page 0 to the front.
  df_p0N <- bind_rows(
    df_p0 |> mutate(page = "0", .before = 1), 
    df_p1N) |>
    mutate(
      page = parse_number(page))
  
  # Unnest nested columns.
  df_p0N_clean <- df_p0N |> 
    unnest(cols = c(departure, arrival, airline, flight, aircraft, live), 
           keep_empty = TRUE,
           names_sep = "_")
  
  # Save data
  print(str_glue("Saving data for {date}, {dest_airport}."))
  df_p0N_clean |>
    write_xlsx(
      path = str_glue("{PROJECT_FOLDER}/data/{date}_{dest_airport}.xlsx"))
  
  return(df_p0N_clean)
}


# update DATES variable, airport_reduced
airport_reduced <- airport_list[1:65]

DATE <- "2023-11-25"


daily_flights <- purrr::map(.x = airport_reduced, .f = politely_get_flights, 
                            url = URL_FLIGHTS, date = DATE, .progress = TRUE)

combined_dataframe <- bind_rows(daily_flights)


# Old functions we no longer need since we're writing them in xlsx:
#write.csv(combined_dataframe, file = str_glue("{PROJECT_FOLDER}/data/{DATE}.csv"), row.names = FALSE)



# Once all data for a particular day is in individual xlsx sheets, complete the following to write the sheets into one csv file

# List all files in a folder.
files_to_append <- list.files(path = str_glue("{PROJECT_FOLDER}/data"), pattern = "*.xlsx", full.names = TRUE)

# Create dataframe from the list of files.
dfs <- map(.x = files_to_append, .f = read_xlsx)
df <- bind_rows(dfs) 

# TODO: Do stuff with the final df.

write.csv(df, file = str_glue("{DATE}.csv"), row.names = FALSE)






