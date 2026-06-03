# Load the primary data science framework and Excel import library
library(tidyverse)
library(readxl)
library(here)

# Practice Import A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv(here::here("../data/reef_cover_log.csv"))

# Practice Import B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("../data/acoustic_telemetry_stream.txt"))

# Practice Import C: Targeting a specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("../data/fish_catch_data.xlsx"), sheet = "Commercial_2026")

# Read in mangrove_data
mangrove_data <- read_csv(file = here::here("../data/mangrove_survey_raw.csv"))
# Reading in this dataset created an issue as the first 5 lines are formatted differently, they appear to contain the meta data. This meta data then sets the format for the following data, setting it in a single column. 

# Use args within read_csv to skip headers and declare missing flags
mangrove_data <- read_csv(
  here::here("../data/mangrove_survey_raw.csv"),
  skip = 5,   # Skip the first 5 lines of field notes
  na = c(".", "NA", "9999", "ND", "blank"))  # Convert known text alts to true NA

#------------------------------------------------------------------

# Force a modern tibble to degrade into a legacy base R data frame structure
benthic_cover_df <- as.data.frame(benthic_cover)


# Print the old-style dataframe structure to view
print(benthic_cover_df)
# And compare with tibble alternative
print(benthic_cover)

#-----------------------------------------------------------------
  
# Install the data package (execute this command once in your console pane and then delete!)
#install.packages("palmerpenguins")

# Load the package data into active memory
library(palmerpenguins)
data("penguins")

# Examine the structure of the dataset - always do this when loading a new dataset!
glimpse(penguins) # tidyverse version (from dplyr package)
str(penguins) # base R version

# Generate an exploratory summary matrix
summary(penguins)

#-----------------------------------------------------------------

# Vertically slice specific morphometric variables by explicit name
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g)
glimpse(morphology_metrics)

# Retain a continuous block of attributes using the colon operator
spatial_block <- select(penguins, species:island)

# Discard logistics tracking attributes while preserving everything else using the minus sign
clean_scientific_fields <- select(penguins, -year)


#-----------------------------------------------------------------

# Isolate observations belonging to a single categorical target group
adelie_cohort <- filter(penguins, species == "Adelie")

# Sift out individuals using continuous numerical boundary thresholds
# Preserves only large penguins whose mass exceeds 4500 grams
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Combine multiple conditional parameters across separate attributes
# Preserves records matching Gentoo penguins sampled explicitly on Biscoe Island
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")

# Sift records matching multiple targeting flags within an explicit set
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))


#----------------------------------------------------------------

# Sort penguins by ascending body mass (Default setting: Smallest mass first)
lightest_first <- arrange(penguins, body_mass_g)

# Sort penguins in descending sequence using the desc() layout wrapper
heaviest_first <- arrange(penguins, desc(body_mass_g))

# Execute nested sorting criteria: Group by species, then sort by descending bill length
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))


#----------------------------------------------------------------
## Pipes
#Instead of:
penguins_subset <- mutate(penguins, bill_ratio = bill_length_mm / bill_depth_mm)
penguins_final <- filter(penguins_subset, species == "Adelie")

#You can simply write:
penguins_final <- penguins %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  filter(species == "Adelie")

#----------------------------------------------------------------
# Mutate

# Calculate a new morphological ratio in our environment
penguin_ratios <- penguins  |> 
  mutate(body_mass_kg = body_mass_g / 1000,   # Convert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm  # Bill ratio
  )

# View your newly engineered variables appended to the far-right columns
glimpse(penguin_ratios)



#----------------------------------------------------------------

# Grouping our active memory penguins by species
grouped_penguins <- group_by(penguins, species)

# Notice that the table looks identical, but metadata notes 'Groups: species [3]'
print(grouped_penguins)

# Collapsing the buckets into explicit summary metrics
species_mass_summary <- summarise(grouped_penguins,
                                  mean_mass_g = mean(body_mass_g)
)

print(species_mass_summary)


# Overcoming the missing value trap using na.rm = TRUE
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),    # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE), # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)  # Standard deviation calculation
  )

print(biological_signal) #the species will have a group that gives an NA for sex - i believe this is to count those samples that have an NA for sex


#----------------------------------------------------------------

