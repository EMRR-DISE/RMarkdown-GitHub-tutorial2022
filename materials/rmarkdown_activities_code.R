# Code to use in activities for the R Markdown tutorial


# Code block 1: Load packages ---------------------------------------------

# Load packages
library(tidyverse)
library(knitr)
library(here)

# Code block 2: Import fish data ------------------------------------------

df_monthly <- read_csv(here("Monthly_Catch_CPUE.csv"))

# Code block 3: Inline code -----------------------------------------------

# This data contains monthly fish counts and CPUE for three different gear types
# (`r unique(df_monthly$Gear)`) from `r min(df_monthly$year)` to `r
# max(df_monthly$year)`.

# Code block 4: Add code block to plot monthly CPUE -----------------------

plt_cpue_month <- df_monthly %>%
  ggplot(aes(x = Date, y = CPUE)) +
  geom_col() +
  facet_wrap(vars(Gear), ncol = 1, scales = "free_y") +
  theme_bw()

plt_cpue_month

# Code block 5: Summarize and plot annual CPUE ----------------------------

# Summarize count and CPUE as annual totals
df_annual <- df_monthly %>%
  group_by(year, Gear) %>%
  summarize(across(c(mo.count, CPUE), sum), .groups = "drop") %>%
  rename(
    Year = year,
    Count = mo.count
  )

# Plot annual CPUE
plt_cpue_yr <- df_annual %>%
  ggplot(aes(x = Year, y = CPUE)) +
  geom_col() +
  facet_wrap(vars(Gear), ncol = 1, scales = "free_y") +
  theme_bw()

plt_cpue_yr

# Code block 6: Print df and plot of annual CPUE --------------------------

# Show glimpse of df_annual
glimpse(df_annual)

# Print plot of annual CPUE
plt_cpue_yr

# Code block 7: Figure output options -------------------------------------

plt_cpue_yr

# Code block 8: Modify basic YAML options ---------------------------------

# ---
# title: "Your title"
# author: "Your name"
# date: "October 17, 2022"
# output: html_document
# ---

# Code block 9: Add dynamic date to YAML options --------------------------

# ---
# title: "Your title"
# author: "Your name"
# date: "`r Sys.Date()`"
# output: html_document
# ---

# Code block 10: Add table of contents to html document -------------------

# ---
# title: "Your title"
# author: "Your name"
# date: "`r Sys.Date()`"
# output:
#   html_document:
#     toc: true
#     toc_float:
#       collapsed: false
# ---

# Code block 11: Add code folding and download to html document -----------

# ---
# title: "Your title"
# author: "Your name"
# date: "`r Sys.Date()`"
# output:
#   html_document:
#     code_folding: show
#     code_download: true
# ---

# Code block 12: Add bootswatch theme to html document --------------------

# ---
# title: "Your title"
# author: "Your name"
# date: "`r Sys.Date()`"
# output:
#   html_document:
#     theme:
#       bootswatch: solar
# ---

bslib::bootswatch_themes()

# Code block 13: Print dataframe of annual CPUE ----------------------------

df_annual

# Code block 14: Print dataframe of annual CPUE as kable -------------------

kable(df_annual, caption = "Annual Counts and CPUE of YBFMP survey data")

# Code block 15: Add format options to kable -------------------------------

kable(
  df_annual,
  caption = "Annual Counts and CPUE of YBFMP survey data",
  # Round CPUE column to 1 digit
  digits = 1,
  # Align all columns to the left
  align = "l"
)

# Code block 16: Add format options to kable with kableExtra --------------

library(kableExtra)

df_annual %>%
  kbl(
    caption = "Annual Counts and CPUE of YBFMP survey data",
    # Round CPUE column to 1 digit
    digits = 1,
    # Align all columns to the left
    align = "l"
  ) %>%
  kable_styling(
    # appearance option
    bootstrap_options = c("striped", "hover"),
    # change width
    full_width = FALSE,
    # change position
    position = "left",
    # fix the header row at the top when scrolling, useful for longer tables
    fixed_thead = TRUE
  ) %>%
  # Format the header row (0) to be aligned in the center
  row_spec(0, align = "center") %>%
  # Make the Year column bold
  column_spec(1, bold = TRUE) %>%
  # add a scroll box for easier viewing
  scroll_box(width = "40%", height = "550px")


# Code block 17: Convert dataframe of annual CPUE to interactive datatable ------

library(DT)
datatable(df_annual)

# Code block 18: Add format options to interactive datatable ----------------

df_annual %>%
  # Convert Year and Gear variables to factors to allow for select lists in the
  # datatable filters
  mutate(across(c(Year, Gear), factor)) %>%
  datatable(
    # remove rownames
    rownames = FALSE,
    # add column filters
    filter = "top",
    options = list(autoWidth = TRUE)
  )

# Code block 19: Convert plot of annual CPUE to interactive plot -----------

library(plotly)
ggplotly(plt_cpue_yr)

# Code block 20: Create an interactive map of YBFMP sampling locations --------

# Import YBFMP station information from EDI data package
df_stations <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc")

# Convert to sf object
library(sf)
sf_stations <- df_stations %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Create interactive map using leaflet
library(leaflet)
sf_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 5,
    fillOpacity = 0.8,
    weight = 0.5,
    color = "black",
    opacity = 1
  )

# Code block 21: Add format options to leaflet map --------------------------

# Define color palette for MethodCode
color_pal <- colorFactor(palette = "viridis", domain = sf_stations$MethodCode)

sf_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 5,
    fillColor = ~color_pal(MethodCode),
    fillOpacity = 0.8,
    weight = 0.5,
    color = "black",
    opacity = 1,
    label = paste0("Station Code: ", sf_stations$StationCode)
  ) %>%
  addLegend(
    position = "topright",
    pal = color_pal,
    values = sf_stations$MethodCode,
    title = "Gear Type:"
  )

# Code block 22: Adding tabs to html document -------------------------------

## YBFMP Data Summary {.tabset}

### Monthly CPUE

plt_cpue_month

### Annual CPUE

plt_cpue_yr

### Stations Map

sf_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 5,
    fillColor = ~color_pal(MethodCode),
    fillOpacity = 0.8,
    weight = 0.5,
    color = "black",
    opacity = 1,
    label = paste0("Station Code: ", sf_stations$StationCode)
  ) %>%
  addLegend(
    position = "topright",
    pal = color_pal,
    values = sf_stations$MethodCode,
    title = "Gear Type:"
  )

