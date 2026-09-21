#!/usr/bin/env Rscript
# kc_map_v2/build_v2.R
#
# Generates programs.json and layer GeoJSONs for the v2 map.
# Programs data is separated so non-R collaborators can update programs.json
# directly without running R (for simple edits) or re-run this script
# (after updating the spreadsheet).
#
# Run from the King County folder:  Rscript kc_map_v2/build_v2.R
#
# Outputs (all under kc_map_v2/):
#   programs.json          — all programs + Someone to Call entries
#   layers/kc_zones.geojson
#   layers/psap_kc.geojson
#   layers/leg_kc.geojson
#   layers/fire_districts.geojson
#
# Deploy: copy kc_map_v2/ to ~/kc-crisis-map/v2/ and push.

suppressMessages({
  library(dplyr); library(jsonlite); library(sf); library(rmapshaper)
})

BHCORE_ROOT <- "../landscape2026"
KC_ROOT     <- "."
V2_DIR      <- "kc_map_v2"
LAYER_DIR   <- file.path(V2_DIR, "layers")

# ── Subtype classifiers (mirrors build_kc_map.R) ─────────────────────────────
classify_stc <- function(pt) dplyr::case_when(
  grepl("^Law",                          pt, ignore.case=TRUE) ~ "LE co-response",
  grepl("^Fire",                         pt, ignore.case=TRUE) ~ "Fire/EMS co-response",
  grepl("^Alternative|City.based civil", pt, ignore.case=TRUE) ~ "Alternative/community",
  grepl("Crisis evaluation|^Designated", pt, ignore.case=TRUE) ~ "Designated Crisis Responder (DCR)",
  grepl("^Behavioral|^Mobile Rapid",     pt, ignore.case=TRUE) ~ "Mobile Rapid Response Crisis Team (MRRCT)",
  grepl("^Child|^Youth",                 pt, ignore.case=TRUE) ~ "Mobile Response and Stabilization Services (MRSS)",
  TRUE ~ "Other"
)
classify_oe <- function(type, program) dplyr::case_when(
  grepl("City.Based", type, ignore.case=TRUE) |
    grepl("City Outreach Team", program, ignore.case=TRUE) ~ "City-Based Outreach",
  grepl("Outreach|Civilian", type, ignore.case=TRUE) ~ "Outreach",
  TRUE ~ "Other"
)
classify_ptg <- function(type) dplyr::case_when(
  grepl("Emergency Department",                   type, ignore.case=TRUE) ~ "Emergency Department",
  grepl("Crisis Care|Crisis Stabiliz|Not open yet", type, ignore.case=TRUE) ~ "Crisis Stabilization",
  grepl("E&T|SWMS|Evaluation",                    type, ignore.case=TRUE) ~ "Evaluation & Treatment",
  grepl("Withdrawal|\\bWM\\b|Sobering",           type, ignore.case=TRUE) ~ "Withdrawal Management",
  grepl("Inpatient",                              type, ignore.case=TRUE) ~ "Inpatient Psychiatry",
  TRUE ~ "Other"
)
classify_pcc <- function(type) dplyr::case_when(
  grepl("Fire.Based|Integrated Health|\\bMIH\\b|FD MIH", type, ignore.case=TRUE) ~ "Fire-Based / MIH",
  grepl("Post.Crisis Follow",                     type, ignore.case=TRUE) ~ "Post-Crisis Follow-Up",
  grepl("Community Stabiliz|Navigation|Diversion|Outreach", type, ignore.case=TRUE) ~ "Community Stabilization",
  TRUE ~ NA_character_
)

# ── Load spreadsheet data ─────────────────────────────────────────────────────
kcs_raw <- read.csv(file.path(KC_ROOT, "data_prep/kc_crisis_system.csv"),
                    stringsAsFactors = FALSE)
cat("kc_crisis_system.csv rows:", nrow(kcs_raw), "\n")

# ── REDCap enrichment ─────────────────────────────────────────────────────────
EXCLUDED <- c("16","25","57","37","44","50","53","84","85")
pd_raw <- read.csv(file.path(BHCORE_ROOT, "program_data.csv"), stringsAsFactors = FALSE)

pd_enrich <- pd_raw |>
  filter(!as.character(record_id) %in% EXCLUDED) |>
  select(record_id, program_year_start, annual_num_encounters,
         coverage_summary, coverage_zips, survey_status) |>
  rename(rc_year_start    = program_year_start,
         rc_annual_calls  = annual_num_encounters,
         rc_serves        = coverage_summary,
         rc_zips          = coverage_zips,
         rc_survey_status = survey_status)

crosswalk <- read.csv(file.path(KC_ROOT, "data_prep/kc_redcap_crosswalk.csv"),
                      stringsAsFactors = FALSE)

# ── Someone to Call: accordion-only, no map markers ──────────────────────────
stc_info <- kcs_raw |>
  filter(category == "Someone to Call") |>
  select(category, program, parent_org, type, population,
         geographic_area, how_accessed, refers_to, website) |>
  distinct(program, .keep_all = TRUE)

# ── Mappable programs ─────────────────────────────────────────────────────────
kcs_enriched <- kcs_raw |>
  filter(category != "Someone to Call", !is.na(lat), !is.na(lon)) |>
  left_join(crosswalk, by = c("program" = "spreadsheet_program")) |>
  left_join(pd_enrich, by = "record_id")

# Collapse multi-zone rows → one record per program with a zones list.
# Multi-zone rows are identical except for crz_zone; group_by lat/lon
# to merge them.
zones_df <- kcs_enriched |>
  group_by(program, category, lat, lon) |>
  summarise(zones = list(sort(unique(crz_zone[!is.na(crz_zone) & nzchar(crz_zone)]))),
            .groups = "drop")

base_df <- kcs_enriched |>
  group_by(program, category, lat, lon) |>
  slice(1) |>
  ungroup() |>
  select(-crz_zone) |>
  left_join(zones_df, by = c("program","category","lat","lon")) |>
  mutate(
    zones = lapply(zones, function(z) {
      if (length(z) == 0) return("Countywide")
      if ("Countywide" %in% z) return("Countywide")
      z
    }),
    dot_style   = ifelse(!is.na(rc_survey_status) & rc_survey_status == "not_surveyed",
                         "hollow", "solid"),
    stc_subtype = ifelse(category == "Someone to Respond",   classify_stc(type),         NA_character_),
    stc_subtype = ifelse(type == "FD MIH/CARES",            "Fire/EMS co-response",      stc_subtype),
    oe_subtype  = ifelse(category == "Outreach/Engage",      classify_oe(type, program),  NA_character_),
    ptg_subtype = ifelse(category == "Somewhere Safe to Go", classify_ptg(type),          NA_character_),
    pcc_subtype = ifelse(category == "Post-Crisis",          classify_pcc(type),          NA_character_)
  )

cat("Mappable programs (deduplicated):", nrow(base_df), "\n")

# ── Build JSON list ───────────────────────────────────────────────────────────
nn <- function(x) !is.na(x) && nzchar(trimws(as.character(x)))

row_to_obj <- function(r, include_map_fields = TRUE) {
  obj <- list(
    category       = r$category,
    program        = r$program,
    parent_org     = if (nn(r$parent_org))      r$parent_org      else NULL,
    type           = if (nn(r$type))            r$type            else NULL,
    population     = if (nn(r$population))      r$population      else NULL,
    geographic_area= if (nn(r$geographic_area)) r$geographic_area else NULL,
    how_accessed   = if (nn(r$how_accessed))    r$how_accessed    else NULL,
    refers_to      = if (nn(r$refers_to))       r$refers_to       else NULL,
    address        = if (nn(r$address))         r$address         else NULL,
    website        = if (nn(r$website))         r$website         else NULL
  )
  if (include_map_fields) {
    obj$lat         <- if (!is.na(r$lat))              r$lat            else NULL
    obj$lon         <- if (!is.na(r$lon))              r$lon            else NULL
    obj$zones       <- as.list(r$zones[[1]])
    obj$dot_style   <- if (nn(r$dot_style))            r$dot_style      else "solid"
    obj$stc_subtype <- if (nn(r$stc_subtype))          r$stc_subtype    else NULL
    obj$oe_subtype  <- if (nn(r$oe_subtype))           r$oe_subtype     else NULL
    obj$ptg_subtype <- if (nn(r$ptg_subtype))          r$ptg_subtype    else NULL
    obj$pcc_subtype <- if (nn(r$pcc_subtype))          r$pcc_subtype    else NULL
    obj$rc_year_start   <- if (!is.na(r$rc_year_start))   r$rc_year_start   else NULL
    obj$rc_annual_calls <- if (!is.na(r$rc_annual_calls)) r$rc_annual_calls else NULL
    obj$rc_serves   <- if (nn(r$rc_serves))            r$rc_serves      else NULL
    obj$rc_zips     <- if (nn(r$rc_zips))              r$rc_zips        else NULL
  }
  obj
}

mappable_list <- lapply(seq_len(nrow(base_df)), function(i) {
  row_to_obj(as.list(base_df[i, ]))
})

stc_list <- lapply(seq_len(nrow(stc_info)), function(i) {
  r <- as.list(stc_info[i, ])
  list(
    category        = r$category,
    program         = r$program,
    parent_org      = if (nn(r$parent_org))      r$parent_org      else NULL,
    type            = if (nn(r$type))            r$type            else NULL,
    population      = if (nn(r$population))      r$population      else NULL,
    geographic_area = if (nn(r$geographic_area)) r$geographic_area else NULL,
    how_accessed    = if (nn(r$how_accessed))    r$how_accessed    else NULL,
    refers_to       = if (nn(r$refers_to))       r$refers_to       else NULL,
    website         = if (nn(r$website))         r$website         else NULL,
    lat = NULL, lon = NULL, zones = list()
  )
})

all_programs <- c(mappable_list, stc_list)
out_json <- file.path(V2_DIR, "programs.json")
write(toJSON(all_programs, auto_unbox = TRUE, pretty = TRUE, null = "null"),
      out_json)
cat("Wrote", out_json, "—", length(all_programs), "records\n")
cat("  Mappable:", length(mappable_list),
    "| Someone to Call:", length(stc_list), "\n")

# ── Layer GeoJSONs ────────────────────────────────────────────────────────────
KC_SF <- st_bbox(c(xmin=-122.55, ymin=47.07, xmax=-121.06, ymax=47.82),
                 crs=4326) |> st_as_sfc()

# CRZ zones (already built)
file.copy(file.path(KC_ROOT, "kc_zones.geojson"),
          file.path(LAYER_DIR, "kc_zones.geojson"), overwrite=TRUE)
cat("Copied kc_zones.geojson\n")

# PSAP (clipped + simplified)
psap <- st_read(file.path(BHCORE_ROOT,"psap.geojson"), quiet=TRUE) |>
  ms_simplify(keep=0.12, keep_shapes=TRUE)
psap_kc <- psap[lengths(st_intersects(psap, KC_SF)) > 0, ]
st_write(psap_kc, file.path(LAYER_DIR,"psap_kc.geojson"),
         delete_dsn=TRUE, quiet=TRUE)
cat("Wrote psap_kc.geojson —", nrow(psap_kc), "features\n")

# Legislative districts (clipped)
leg <- st_read(file.path(BHCORE_ROOT,"leg_districts_simplified.geojson"), quiet=TRUE)
leg_kc <- leg[lengths(st_intersects(leg, KC_SF)) > 0, ]
st_write(leg_kc, file.path(LAYER_DIR,"leg_kc.geojson"),
         delete_dsn=TRUE, quiet=TRUE)
cat("Wrote leg_kc.geojson —", nrow(leg_kc), "features\n")

# Fire districts (FPDs + RFAs + municipal)
frd <- bind_rows(
  st_read(file.path(KC_ROOT,"data_prep/fire_districts/FIRDST_AREA_407_-2212399412707482500.geojson"),
          quiet=TRUE) |> mutate(fire_type="Fire Protection District") |> select(NAME,fire_type),
  st_read(file.path(KC_ROOT,"data_prep/RFA Districts of King County/RFADST_AREA_2577_7779909782687922957.geojson"),
          quiet=TRUE) |> mutate(fire_type="Regional Fire Authority") |> select(NAME,fire_type),
  st_read(file.path(BHCORE_ROOT,"wa_places.geojson"), quiet=TRUE) |>
    filter(NAME %in% c("Seattle","Bellevue","Kirkland","Mercer Island")) |>
    mutate(NAME=paste0(NAME," Fire Department"), fire_type="Municipal Fire Department") |>
    select(NAME,fire_type)
)
st_write(frd, file.path(LAYER_DIR,"fire_districts.geojson"),
         delete_dsn=TRUE, quiet=TRUE)
cat("Wrote fire_districts.geojson —", nrow(frd), "features\n")

cat("\nDone. To publish:\n")
cat("  cp -r kc_map_v2/ ~/kc-crisis-map/v2/ && cd ~/kc-crisis-map && git add v2/ && git push\n")
