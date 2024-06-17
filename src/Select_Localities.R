library(sf)
library(xml2)
library(dplyr)

# Bestandspad naar de KML-file
kml_file <- "./data/input/localities.kml"

# Lezen van de KML-bestand
kml_data <- st_read(kml_file)

# Functie om de HTML-gecodeerde string om te zetten naar een lijst
parse_description <- function(desc) {
  desc_list <- strsplit(desc, "<br>")[[1]]
  desc_list <- lapply(desc_list, function(x) {
    parts <- strsplit(x, ": ", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      return(setNames(list(parts[2]), parts[1]))
    } else {
      return(NULL)
    }
  })
  desc_list <- do.call(c, desc_list)
  return(desc_list)
}

# Toepassen van de parse functie op de beschrijving kolom
parsed_data <- lapply(kml_data$Description, parse_description)



# Omzetten naar een dataframe
parsed_data <- lapply(parsed_data, function(x) {
  as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
})
# Binden van alle rijen in een enkele dataframe
df <- bind_rows(parsed_data) 

# Verwijder de Description kolom van kml_data
kml_data <- kml_data %>% select(-Description)

# Voeg de nieuwe attributen toe aan kml_data
kml_data <- cbind(kml_data, df)


###Transform before doing intersects with other layers
kml_data <- st_transform(kml_data, crs = 31370)

###Voer de bewerkingen uit voor de nieuw gecreëerde punten
#Lees de locaties
provincies_path = "./data/input/shapefiles/provincies.shp"
postkantons_path = "./data/input/shapefiles/postkantons.shp"
gemeenten_path = "./data/input/shapefiles/gemeenten.shp"

#Laad de files op
# Shapefiles inlezen
provincies_shape <- st_read(provincies_path)
postkantons_shape <- st_read(postkantons_path)
gemeenten_shape <- st_read(gemeenten_path)


###UPDATE DE TOEGEKENDE WAARDEN VOOR isRESERVED
# Hernoem de kolom 'Name' naar 'isReserved'
names(kml_data)[names(kml_data) == "Name"] <- "isReserved"

# Wijs dezelfde waarden toe aan de nieuwe kolom 'isReserved'
kml_data$isReserved <- kml_data$updateRes

###VOEG DE WAARDEN VOOR VELDEN provincies, gemeenten en postkantons toe

# Voeg een tijdelijke index kolom toe
kml_data <- kml_data %>%
  mutate(temp_id = row_number())

# Intersect met provincies_shape
provincies_intersect <- st_intersection(kml_data, provincies_shape)
# Intersect met postkantons_shape
postkantons_intersect <- st_intersection(kml_data, postkantons_shape)
# Intersect met gemeenten_shape
gemeenten_intersect <- st_intersection(kml_data, gemeenten_shape)

# Voeg velden toe indien ze niet bestaan
if (!"provincie" %in% colnames(kml_data)) {
  kml_data$provincie <- NA
}
if (!"postcode" %in% colnames(kml_data)) {
  kml_data$postcode <- NA
}
if (!"gemeente" %in% colnames(kml_data)) {
  kml_data$gemeente <- NA
}

# Maak dictionaries van de intersecties
prov_dict <- setNames(provincies_intersect$PROVNAAM, provincies_intersect$temp_id)
post_dict <- setNames(postkantons_intersect$nouveau_PO, postkantons_intersect$temp_id)
gem_dict <- setNames(gemeenten_intersect$GEMNAAM, gemeenten_intersect$temp_id)

# Update de velden in kml_data gebaseerd op de intersecties alleen als de velden leeg zijn
kml_data <- kml_data %>%
  rowwise() %>%
  mutate(
    gemeente = if_else(is.na(gemeente) | gemeente == "", gem_dict[as.character(temp_id)], gemeente),
    provincie = if_else(is.na(provincie) | provincie == "", prov_dict[as.character(temp_id)], provincie),
    postcode = if_else(is.na(postcode) | postcode == "", post_dict[as.character(temp_id)], postcode)
  ) %>%
  ungroup() %>%
  select(-temp_id) 

print("Province, postcode, and gemeenten names successfully added to Localiteiten")

####UPDATE LOC_ID######

# Voeg het veld 'locID' toe als het niet bestaat
if (!"locID" %in% colnames(kml_data)) {
  kml_data <- kml_data %>% mutate(locID = NA_character_)
}

# Create a dictionary to keep track of unique sequence numbers for each postcode
unique_numbers <- list()

# First pass to populate the unique_numbers list with existing locIDs
for (i in 1:nrow(kml_data)) {
  row <- kml_data[i, ]
  postcode <- as.character(row$postcode)
  locID <- as.character(row$locID)
  
  if (!is.na(locID) && locID != "") {
    tryCatch({
      unique_number <- as.integer(strsplit(locID, "_")[[1]][3])
      if (is.null(unique_numbers[[postcode]])) {
        unique_numbers[[postcode]] <- unique_number
      } else {
        unique_numbers[[postcode]] <- max(unique_numbers[[postcode]], unique_number)
      }
    }, error = function(e) {
      # Skip locID values that are not in the expected format
    })
  }
}

# Update the 'locID' field
kml_data <- kml_data %>%
  rowwise() %>%
  mutate(
    locID = ifelse(
      is.na(locID) | locID == "",
      {
        if (is.null(unique_numbers[[as.character(postcode)]])) {
          unique_numbers[[as.character(postcode)]] <- 1
        } else {
          unique_numbers[[as.character(postcode)]] <- unique_numbers[[as.character(postcode)]] + 1
        }
        
        unique_number <- unique_numbers[[as.character(postcode)]]
        prefix <- ifelse(SugbyINBO == "YES", "I", "V")
        sprintf("%s_%s_%s", prefix, as.character(postcode), unique_number)
      },
      locID
    )
  ) %>%
  ungroup()

print("LocID toegevoegd aan nieuwe locaties")

#Save als shape en als csv

## Save als een csv bestand
# Definieer het pad naar het CSV-bestand
csv_path <- "./data/output/localities.csv"

# Schrijf kml_data naar een CSV-bestand
write.csv(st_drop_geometry(kml_data), file = csv_path, row.names = FALSE, sep = ",", dec = ".")

print("CSV file successfully saved.")


