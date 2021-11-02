# sample data
f <- system.file("extdata/ebd-sample.txt", package = "auk")
# define an EBD reference and a set of filters
ebd <- auk_ebd(f) %>%
  # species: common and scientific names can be mixed
  auk_species(species = c("Nannopterum auritum"), taxonomy_version = 2021) %>%
  auk_species(species=c("Phalacrocorax carbo/Nannopterum auritum",
                        "Nannopterum auritum x brasilianum",
                        "Nannopterum auritum/brasilianum"), taxonomy_version = 2021) %>%
  # country: codes and names can be mixed; case insensitive
  auk_country(country = c("United States", "Canada")) %>%
  # restrict to Stationary and Traveling checklists
  auk_protocol(c("Stationary", "Traveling")) %>%
  # restrict to complete checklists (needed for zero-filled data)
  auk_complete() ## JLB: need to make sure this has zeroes..

output_file <- "data-raw/temp/ebd_filtered_test.txt"
ebd_filtered <- system.file("extdata/ebd-sample.txt", package = "auk") %>%
  auk_species(species=c("Nannopterum auritum",
                        "Phalacrocorax carbo/Nannopterum auritum",
                        "Nannopterum auritum x brasilianum",
                        "Nannopterum auritum/brasilianum")) %>%
  # country: codes and names can be mixed; case insensitive
  auk_country(country = c("United States", "Canada")) %>%
  auk_protocol(c("Stationary", "Traveling")) %>%
  auk_filter(file = output_file)
