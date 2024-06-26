library(sf)
library(xml2)
library(dplyr)
library(lwgeom)

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
waterlopen_path = "./data/input/shapefiles/VhaCattraj.shp"
polder_path = "./data/input/shapefiles/polder.shp"
watering_path = "./data/input/shapefiles/watering.shp"

#Laad de files op
# Shapefiles inlezen
provincies_shape <- st_read(provincies_path)
postkantons_shape <- st_read(postkantons_path)
gemeenten_shape <- st_read(gemeenten_path)
waterlopen_shape <- st_read(waterlopen_path) %>%
  st_transform(crs = st_crs(kml_data))
waterlopen_buffer <- waterlopen_shape %>%
  st_buffer(dist = 5)
watering_shape <- st_read(watering_path) %>%
                  st_transform(crs = 31370)
polder_shape <- st_read(polder_path)%>%
                st_transform(crs = 31370)  


###UPDATE DE TOEGEKENDE WAARDEN VOOR isRESERVED
# Hernoem de kolom 'Name' naar 'isReserved'
names(kml_data)[names(kml_data) == "Name"] <- "isReserved"

# Wijs dezelfde waarden toe =aan de nieuwe kolom 'isReserved'
kml_data$isReserved <- kml_data$updateRes

#########UPDATA VELDEN provincies, gemeenten en postkantons ####################
# Voeg een tijdelijke index kolom toe
kml_data <- kml_data %>%
  mutate(temp_id = row_number())
# Intersect met provincies_shape
provincies_intersect <- st_intersection(kml_data, provincies_shape)
# Intersect met postkantons_shape
postkantons_intersect <- st_intersection(kml_data, postkantons_shape)
# Intersect met gemeenten_shape
gemeenten_intersect <- st_intersection(kml_data, gemeenten_shape)
# Intersect met waterlopen_buffer
waterlopen_intersect <- st_intersection(kml_data, waterlopen_buffer)
# Intersect met watervlakken_shape
watering_intersect <- st_intersection(kml_data, watering_shape)
polder_intersect <- st_intersection(kml_data, polder_shape)

# Bewerk zodat je voor elke temp_id slechts één punt krijgt, het intersect & buffer kan ervoor zorgen dat punten in meerdere polygonen liggen
# Controleer of alle waarden in waterlopen_intersect$temp_id uniek zijn
is_unique <- length(unique(waterlopen_intersect$temp_id)) == nrow(waterlopen_intersect)
# Print het resultaat
if (is_unique) {
  print("Alle waarden in waterlopen_intersect$temp_id zijn uniek.")
} else {
  print("Niet alle waarden in waterlopen_intersect$temp_id zijn uniek.")
}

# Optioneel: Als je de niet-unieke waarden wilt zien
duplicated_ids <- waterlopen_intersect$temp_id[duplicated(waterlopen_intersect$temp_id)]
if (length(duplicated_ids) > 0) {
  print("Niet-unieke temp_id waarden:")
  print(duplicated_ids)
  
  # Filter de duplicaten
  filtered_waterlopen_intersect <- waterlopen_intersect %>%
    group_by(temp_id) %>%
    filter(row_number(st_equals(geometry, first(geometry))) == 1) %>%
    filter(CATC == min(CATC)) %>%
    ungroup()
  
  # Controleer het resultaat
  print(filtered_waterlopen_intersect)
  
  # Als je wilt zien hoeveel rijen er zijn verwijderd
  print(paste("Aantal rijen voor filtering:", nrow(waterlopen_intersect)))
  print(paste("Aantal rijen na filtering:", nrow(filtered_waterlopen_intersect)))
  
  # Werk de originele dataset bij
  waterlopen_intersect <- filtered_waterlopen_intersect
}

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
if (!"CATC" %in% colnames(kml_data)) {
  kml_data$CATC <- NA
}
if (!"PolWat" %in% colnames(kml_data)) {
  kml_data$PolWat <- NA
}
if (!"PWtelefoon" %in% colnames(kml_data)) {
  kml_data$PWtel <- NA
}
if (!"PWemail" %in% colnames(kml_data)) {
  kml_data$PWemail <- NA
}

# Maak dictionaries van de intersecties
prov_dict <- setNames(provincies_intersect$PROVNAAM, provincies_intersect$temp_id)
post_dict <- setNames(postkantons_intersect$nouveau_PO, postkantons_intersect$temp_id)
gem_dict <- setNames(gemeenten_intersect$GEMNAAM, gemeenten_intersect$temp_id)
wat_catc_dict <- setNames(waterlopen_intersect$CATC, waterlopen_intersect$temp_id)
wat_naam_dict <- setNames(waterlopen_intersect$NAAM, waterlopen_intersect$temp_id)
wat_vhag_dict <- setNames(waterlopen_intersect$VHAG, waterlopen_intersect$temp_id)
pol_email_dict <- setNames(polder_intersect$EMAIL, polder_intersect$temp_id)
pol_tel_dict <- setNames(polder_intersect$TELEFOON, polder_intersect$temp_id)
pol_naam_dict <- setNames(polder_intersect$NAAMPOL, polder_intersect$temp_id)
wat_email_dict <- setNames(watering_intersect$EMAIL, watering_intersect$temp_id)
wat_tel_dict <- setNames(watering_intersect$TELEFOON, watering_intersect$temp_id)
wat_naam_dict <- setNames(watering_intersect$NAAMWAT, watering_intersect$temp_id)

# Update de velden in kml_data gebaseerd op de intersecties alleen als de velden leeg zijn
kml_data <- kml_data %>%
  rowwise() %>%
  mutate(
    gemeente = if_else(is.na(gemeente) | gemeente == "", gem_dict[as.character(temp_id)], gemeente),
    provincie = if_else(is.na(provincie) | provincie == "", prov_dict[as.character(temp_id)], provincie),
    postcode = if_else(is.na(postcode) | postcode == "", post_dict[as.character(temp_id)], postcode),
    CATC = if_else(is.na(CATC) | CATC == "", wat_catc_dict[as.character(temp_id)], CATC), 
    NAAM = if_else(is.na(NAAM) | NAAM == "", wat_naam_dict[as.character(temp_id)], NAAM),
    VHAG = if_else(is.na(VHAG) | VHAG == 0 | VHAG == "", wat_vhag_dict[as.character(temp_id)], VHAG),
    PolWat = if_else(CATC %in% c(2, 3) & (is.na(PolWat) | PolWat == ""), pol_naam_dict[as.character(temp_id)], PolWat),
    PWemail = if_else(CATC %in% c(2, 3) & (is.na(PWemail) | PWemail == ""), pol_email_dict[as.character(temp_id)], PWemail),
    PWtel = if_else(CATC %in% c(2, 3) & (is.na(PWtel) | PWtel == ""), pol_tel_dict[as.character(temp_id)], PWtel)
  ) %>%
  ungroup() %>%
  select(-temp_id) 

print("VHAG, CATC, Province, postcode, and gemeenten successfully added to localities")

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


#Save als csv

## Save als een csv bestand
# Definieer het pad naar het CSV-bestand
csv_path <- "./data/output/localities.csv"

# Schrijf kml_data naar een CSV-bestand
write.csv(st_drop_geometry(kml_data), file = csv_path, row.names = FALSE, sep = ",", dec = ".")

print("CSV file successfully saved.")


