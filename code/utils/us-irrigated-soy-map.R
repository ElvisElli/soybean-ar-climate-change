## ============================================================
## us-irrigated-soy-map.R
## US county-level maps of soybean irrigated area from USDA Census
## of Agriculture (most recent = 2022).
##
## Figures:
##   fig_soy_irrigated_area.tiff       — irrigated soybean area (acres) per county
##   fig_soy_irrigated_pct.tiff        — irrigated as % of total soybean area
## ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(sf)
  library(tigris)
  library(ggplot2)
  library(scales)
  library(viridis)
})

options(tigris_use_cache = TRUE)
dir.create("figures", showWarnings = FALSE)

NASS_API_KEY <- "D228A372-93ED-3BF7-9699-D2D0DDD3C88D"
CENSUS_YEAR  <- 2022

## ── Step 1: Download county-level soybean data from NASS Census ──────────────
fetch_nass <- function(short_desc) {
  message("[Map] Downloading: ", short_desc)
  resp <- tryCatch(
    GET("https://quickstats.nass.usda.gov/api/api_GET/",
        query = list(key             = NASS_API_KEY,
                     commodity_desc  = "SOYBEANS",
                     short_desc      = short_desc,
                     agg_level_desc  = "COUNTY",
                     source_desc     = "CENSUS",
                     year            = CENSUS_YEAR,
                     format          = "JSON"),
        timeout(60)),
    error = function(e) { message("API error: ", e$message); NULL }
  )
  if (is.null(resp) || http_error(resp)) {
    message("  HTTP error: ", status_code(resp))
    return(NULL)
  }
  j <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  df <- as.data.frame(j$data)
  message("  ", nrow(df), " county records")
  df
}

raw_total    <- fetch_nass("SOYBEANS - ACRES HARVESTED")
raw_irrigated <- fetch_nass("SOYBEANS, IRRIGATED - ACRES HARVESTED")

## ── Step 2: Clean and join ───────────────────────────────────────────────────
clean_nass <- function(df, value_col) {
  df %>%
    transmute(
      state_fips  = state_fips_code,
      county_fips = county_code,
      fips        = paste0(state_fips_code, county_code),
      !!value_col := suppressWarnings(as.numeric(gsub(",", "", Value)))
    ) %>%
    filter(!is.na(.data[[value_col]]))
}

total_df    <- clean_nass(raw_total,     "acres_total")
irrigated_df <- clean_nass(raw_irrigated, "acres_irrigated")

soy_df <- full_join(total_df, irrigated_df, by = "fips") %>%
  mutate(
    acres_irrigated = ifelse(is.na(acres_irrigated), 0, acres_irrigated),
    pct_irrigated   = ifelse(acres_total > 0,
                             100 * acres_irrigated / acres_total, NA_real_)
  )

message(sprintf("[Map] %d counties with total area | %d with irrigated area",
                sum(!is.na(soy_df$acres_total)),
                sum(soy_df$acres_irrigated > 0, na.rm = TRUE)))

## ── Step 3: Get US county shapefile (tigris) ─────────────────────────────────
message("[Map] Downloading US county boundaries...")
counties_sf <- counties(cb = TRUE, resolution = "5m", year = 2022, progress_bar = FALSE) %>%
  select(GEOID, geometry) %>%
  rename(fips = GEOID)

## Join spatial + NASS data; exclude non-contiguous territories for cleaner map
map_sf <- counties_sf %>%
  left_join(soy_df, by = "fips") %>%
  filter(!substr(fips, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78"))

## Reproject to Albers Equal Area (standard for US thematic maps)
map_sf <- st_transform(map_sf, 5070)

## State outlines for reference
states_sf <- states(cb = TRUE, resolution = "5m", year = 2022, progress_bar = FALSE) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "MP", "AS", "VI")) %>%
  st_transform(5070)

## ── Shared map theme ─────────────────────────────────────────────────────────
map_theme <- theme_void() +
  theme(
    legend.position   = "right",
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    plot.title        = element_text(size = 13, face = "bold", hjust = 0.5,
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 9, hjust = 0.5, colour = "grey40",
                                     margin = margin(b = 8)),
    plot.margin       = margin(5, 5, 5, 5)
  )

## ── Figure 1: irrigated soybean area (acres) ─────────────────────────────────
message("[Map] Building fig_soy_irrigated_area...")

plot_area <- ggplot() +
  geom_sf(data = map_sf, aes(fill = acres_irrigated / 1000),
          colour = NA, linewidth = 0) +
  geom_sf(data = states_sf, fill = NA, colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option    = "plasma",
    direction = -1,
    na.value  = "grey88",
    name      = "Thousand\nacres",
    trans     = "sqrt",           # square-root scale to show skewed distribution
    labels    = label_number(accuracy = 1),
    breaks    = c(0, 10, 50, 100, 200, 400)
  ) +
  labs(
    title    = "Irrigated Soybean Area — US Counties",
    subtitle = sprintf("USDA Census of Agriculture %d  |  square-root colour scale", CENSUS_YEAR)
  ) +
  map_theme

ggsave("figures/fig_soy_irrigated_area.tiff", plot = plot_area,
       width = 25, height = 15, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
message("[Map] Saved: figures/fig_soy_irrigated_area.tiff")

## ── Figure 2: irrigated as % of total soybean area ───────────────────────────
message("[Map] Building fig_soy_irrigated_pct...")

plot_pct <- ggplot() +
  geom_sf(data = map_sf, aes(fill = pct_irrigated),
          colour = NA, linewidth = 0) +
  geom_sf(data = states_sf, fill = NA, colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option   = "mako",
    direction = -1,
    na.value = "grey88",
    name     = "% irrigated",
    limits   = c(0, 100),
    breaks   = c(0, 25, 50, 75, 100),
    labels   = label_percent(scale = 1, accuracy = 1)
  ) +
  labs(
    title    = "Irrigated Soybean as % of Total Soybean Area — US Counties",
    subtitle = sprintf("USDA Census of Agriculture %d  |  grey = no soybean reported", CENSUS_YEAR)
  ) +
  map_theme

ggsave("figures/fig_soy_irrigated_pct.tiff", plot = plot_pct,
       width = 25, height = 15, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
message("[Map] Saved: figures/fig_soy_irrigated_pct.tiff")

## ── Console summary ───────────────────────────────────────────────────────────
cat(sprintf("\n── US Irrigated Soybean Summary (%d Census) ──\n", CENSUS_YEAR))
soy_df %>%
  summarise(
    total_soy_Macres      = round(sum(acres_total,    na.rm=TRUE) / 1e6, 2),
    irrigated_soy_Macres  = round(sum(acres_irrigated,na.rm=TRUE) / 1e6, 2),
    pct_us_irrigated      = round(100 * sum(acres_irrigated,na.rm=TRUE) /
                                       sum(acres_total,na.rm=TRUE), 1),
    n_counties_with_irrig = sum(acres_irrigated > 0, na.rm=TRUE)
  ) %>%
  print()
