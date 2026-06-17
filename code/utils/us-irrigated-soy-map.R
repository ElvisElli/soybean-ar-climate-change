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
## NASS withholds county data as "(D)" when fewer than 3 operations exist,
## to protect individual farm confidentiality. These counties DO have irrigated
## soybeans but the amount is unknown. We track them separately so they can
## be shown distinctly on the map rather than lumped with "no irrigation".

clean_nass_total <- function(df) {
  df %>%
    transmute(
      fips        = paste0(state_fips_code, county_code),
      acres_total = suppressWarnings(as.numeric(gsub(",", "", Value)))
    ) %>%
    filter(!is.na(acres_total))
}

clean_nass_irrigated <- function(df) {
  df %>%
    transmute(
      fips            = paste0(state_fips_code, county_code),
      acres_irrigated = suppressWarnings(as.numeric(gsub(",", "", Value))),
      irrig_withheld  = is.na(suppressWarnings(as.numeric(gsub(",", "", Value)))) &
                        trimws(Value) != ""   # "(D)" rows: data exists but suppressed
    )
}

total_df     <- clean_nass_total(raw_total)
irrigated_df <- clean_nass_irrigated(raw_irrigated)

soy_df <- left_join(total_df, irrigated_df, by = "fips") %>%
  mutate(
    state_fips    = substr(fips, 1, 2),
    pct_irrigated = ifelse(acres_total > 0 & !is.na(acres_irrigated),
                           100 * acres_irrigated / acres_total, NA_real_)
  )

n_known     <- sum(!is.na(soy_df$acres_irrigated), na.rm = TRUE)
n_withheld  <- sum(isTRUE(soy_df$irrig_withheld) | soy_df$irrig_withheld, na.rm = TRUE)
message(sprintf("[Map] %d counties with total soybean | %d with known irrigated area | %d withheld (D)",
                nrow(total_df), n_known, n_withheld))

## ── State-level statistics ───────────────────────────────────────────────────
## State FIPS → abbreviation lookup (contiguous US only)
fips_to_state <- c(
  "01"="AL","04"="AZ","05"="AR","06"="CA","08"="CO","09"="CT","10"="DE",
  "12"="FL","13"="GA","16"="ID","17"="IL","18"="IN","19"="IA","20"="KS",
  "21"="KY","22"="LA","23"="ME","24"="MD","25"="MA","26"="MI","27"="MN",
  "28"="MS","29"="MO","30"="MT","31"="NE","32"="NV","33"="NH","34"="NJ",
  "35"="NM","36"="NY","37"="NC","38"="ND","39"="OH","40"="OK","41"="OR",
  "42"="PA","44"="RI","45"="SC","46"="SD","47"="TN","48"="TX","49"="UT",
  "50"="VT","51"="VA","53"="WA","54"="WV","55"="WI","56"="WY"
)

state_stats <- soy_df %>%
  group_by(state_fips) %>%
  summarise(
    state            = fips_to_state[state_fips[1]],
    total_soy_acres  = sum(acres_total,    na.rm = TRUE),
    irrig_acres      = sum(acres_irrigated, na.rm = TRUE),
    pct_irrigated    = round(100 * irrig_acres / total_soy_acres, 1),
    n_counties_known = sum(!is.na(acres_irrigated)),
    n_counties_withheld = sum(irrig_withheld, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!state_fips %in% c("02","15","60","66","69","72","78")) %>%
  arrange(desc(irrig_acres))

cat(sprintf("\n── State-level irrigated soybean summary (%d Census) ──\n", CENSUS_YEAR))
print(state_stats, n = Inf)

write.csv(state_stats, "figures/state_irrigated_soy_stats.csv", row.names = FALSE)
message("[Map] Saved: figures/state_irrigated_soy_stats.csv")

MIN_ACRES <- 500  # suppress counties with < 500 known irrigated acres

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

## Split map_sf into layers for clean rendering:
##   withheld  = county has irrigated soy but amount suppressed by NASS "(D)"
##   known     = county has >= MIN_ACRES known irrigated area (suppress noise)
map_withheld <- filter(map_sf, !is.na(irrig_withheld) & irrig_withheld)
map_known    <- filter(map_sf, !is.na(acres_irrigated) & acres_irrigated >= MIN_ACRES)

WITHHELD_COL <- "#d4b483"   # warm tan — "data exists but undisclosed"

## ── Figure 1: irrigated soybean area (acres) ─────────────────────────────────
message("[Map] Building fig_soy_irrigated_area...")

plot_area <- ggplot() +
  geom_sf(data = map_sf,       fill = "grey90", colour = NA, linewidth = 0) +
  geom_sf(data = map_withheld, fill = WITHHELD_COL, colour = NA, linewidth = 0) +
  geom_sf(data = map_known,    aes(fill = acres_irrigated / 1000),
          colour = NA, linewidth = 0) +
  geom_sf(data = states_sf, fill = NA, colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option    = "plasma",
    direction = -1,
    na.value  = "grey90",
    name      = "Thousand\nacres",
    trans     = "sqrt",
    labels    = label_number(accuracy = 1),
    breaks    = c(0, 10, 50, 100, 200, 400)
  ) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.5, size = 2.8,
           colour = "grey30",
           label = paste0("■ = irrigation present, amount withheld by NASS (n = ", n_withheld, " counties)")) +
  labs(
    title    = "Irrigated Soybean Area — US Counties",
    subtitle = paste0("USDA Census of Agriculture ", CENSUS_YEAR,
                      "  |  grey = no irrigation / <500 acres  |  tan = data withheld  |  sqrt colour scale")
  ) +
  map_theme

ggsave("figures/fig_soy_irrigated_area.tiff", plot = plot_area,
       width = 25, height = 15, units = "cm",
       dpi = 600, compression = "lzw", bg = "white")
message("[Map] Saved: figures/fig_soy_irrigated_area.tiff")

## ── Figure 2: irrigated as % of total soybean area ───────────────────────────
message("[Map] Building fig_soy_irrigated_pct...")

plot_pct <- ggplot() +
  geom_sf(data = map_sf,       fill = "grey90", colour = NA, linewidth = 0) +
  geom_sf(data = map_withheld, fill = WITHHELD_COL, colour = NA, linewidth = 0) +
  geom_sf(data = map_known,    aes(fill = pct_irrigated),
          colour = NA, linewidth = 0) +
  geom_sf(data = states_sf, fill = NA, colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option    = "mako",
    direction = -1,
    na.value  = "grey90",
    name      = "% irrigated",
    limits    = c(0, 100),
    breaks    = c(0, 25, 50, 75, 100),
    labels    = label_percent(scale = 1, accuracy = 1)
  ) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.5, size = 2.8,
           colour = "grey30",
           label = paste0("■ = irrigation present, amount withheld by NASS (n = ", n_withheld, " counties)")) +
  labs(
    title    = "Irrigated Soybean as % of Total Soybean Area — US Counties",
    subtitle = paste0("USDA Census of Agriculture ", CENSUS_YEAR,
                      "  |  grey = no irrigation / <500 acres  |  tan = data withheld  |  % of county soybean area")
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
    n_counties_with_irrig = sum(!is.na(acres_irrigated))
  ) %>%
  print()
