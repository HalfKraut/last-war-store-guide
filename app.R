library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

bundled_workbook <- file.path("data", "Last War Price Guide.xlsx")
app_build_label <- "Build: 2026-05-30 train icon cache refresh"
icon_cache_bust <- "20260530a"
source_workbook <- if (file.exists(bundled_workbook)) {
  bundled_workbook
} else {
  file.path(Sys.getenv("USERPROFILE"), "Desktop", "Last War Price Guide.xlsx")
}

parse_num <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9.\\-]", "")
  out <- suppressWarnings(as.numeric(x))
  out[is.na(x) | x == ""] <- NA_real_
  out
}

clean_item_key <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  x <- str_replace_all(x, "décor", "decor")
  x <- str_replace_all(x, "®", "")
  x <- str_replace_all(x, "skil chip", "skill chip")
  x <- str_replace_all(x, "dialectric|dialetric", "dielectric")
  x <- str_replace_all(x, "\\bdrone part\\b", "drone parts")
  x <- str_replace_all(x, "\\blevel\\b", "lv")
  x <- str_replace_all(x, "\\b1 hour\\b", "1h")
  x <- str_replace_all(x, "\\bhour\\b", "h")
  x <- str_replace_all(x, "universal ur hero shard|ur hero universal shard|universal ur hero universal shard", "ur hero universal shard")
  x <- str_replace_all(x, "ssr resource choice chest|resource choice chest \\(ssr\\)", "resource choice chest ssr")
  x <- str_replace_all(x, "ur resource choice chest|resource choice chest \\(ur\\)", "resource choice chest ur")
  x <- str_replace_all(x, "research choice chest \\(ur\\)", "research choice chest ur")
  x <- str_replace_all(x, "\\((sr|ssr|ur|mr|10k)\\)", "\\1")
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

pretty_item_name <- function(key) {
  small <- c("ur", "ssr", "sr", "mr", "lv", "h", "m", "exp")
  words <- str_split(key, " ", simplify = FALSE)
  vapply(words, function(w) {
    paste(vapply(w, function(part) {
      if (part %in% small) toupper(part) else str_to_title(part)
    }, character(1)), collapse = " ")
  }, character(1))
}

item_menu_label <- function(x) {
  x %>%
    str_replace_all("^Universal Decor Component Equivalent$", "Decoration Chest/Components") %>%
    str_replace_all("\\s*\\([^)]*Equivalent\\)", "") %>%
    str_replace_all("\\s+Equivalent\\b", "") %>%
    str_squish()
}

standardize_display_item <- function(item) {
  if (is.na(item) || item == "") return(item)
  key <- clean_item_key(item)
  tier <- detect_tier(key)
  hours <- duration_hours(item)

  if (key == "survivor s token") {
    return("Survivor's Token")
  }

  if (str_detect(key, "drone combat boost")) {
    return("Drone Combat Boost EXP")
  }

  speed_type <- case_when(
    str_detect(key, "construction speed up") ~ "Construction",
    str_detect(key, "research speed up") ~ "Research",
    str_detect(key, "training speed up") ~ "Training",
    str_detect(key, "healing speed up") ~ "Healing",
    str_detect(key, "universal speed up|\\bspeed up\\b") ~ "Universal",
    TRUE ~ NA_character_
  )

  if (!is.na(speed_type) && !is.na(hours)) {
    duration_label <- if (hours < 1) {
      paste0(fmt_num(hours * 60, 0), "m")
    } else {
      paste0(fmt_num(hours, 0), "h")
    }
    return(paste(duration_label, speed_type, "Speed-Up"))
  }

  if (str_detect(key, "hero exp chest") && !is.na(tier)) {
    return(paste0("Hero EXP Chest (", toupper(tier), ")"))
  }

  if (str_detect(key, "resource choice chest") && !is.na(tier)) {
    return(paste0("Resource Choice Chest (", toupper(tier), ")"))
  }

  component_level <- str_match(key, "\\blv\\s*([0-9]+)\\s*(?:drone\\s*)?component")[, 2]
  if (!is.na(component_level)) {
    if (str_detect(key, "choice chest")) {
      return(paste0("Lv ", component_level, " Drone Component Choice Chest"))
    }
    return(paste0("Lv ", component_level, " Drone Component Chest"))
  }

  if (key == "drone part" || key == "drone parts") {
    return("Drone Parts")
  }

  if (str_detect(key, "dielectric ceramic")) {
    return("Dielectric Ceramic")
  }

  if (key %in% c("universal ur hero shard", "ur hero universal shard")) {
    return("UR Hero Universal Shard")
  }

  item
}

duration_hours <- function(item) {
  item_text <- str_to_lower(str_squish(as.character(item)))
  duration <- str_match(item_text, "\\b([0-9]+(?:\\.[0-9]+)?)\\s*-?\\s*(m|min|minute|minutes|h|hr|hour|hours)\\b")
  amount <- suppressWarnings(as.numeric(duration[, 2]))
  unit <- duration[, 3]

  case_when(
    unit %in% c("m", "min", "minute", "minutes") ~ amount / 60,
    unit %in% c("h", "hr", "hour", "hours") ~ amount,
    TRUE ~ NA_real_
  )
}

resource_chest_amounts <- function(hq_level = 29) {
  # Only HQ 29 has explicit quantities so far. Other levels keep this table visible
  # until level-specific amounts are added.
  tibble(
    hq_level = 29,
    resource = rep(c("iron", "food", "coins"), each = 4),
    tier = rep(c("r", "sr", "ssr", "ur"), times = 3),
    amount = c(
      16560, 165600, 1320000, 3970000,
      16560, 165600, 1320000, 3970000,
      9930, 99360, 794880, 2380000
    )
  ) %>%
    filter(hq_level == 29)
}

detect_tier <- function(key) {
  case_when(
    str_detect(key, "\\bur\\b") ~ "ur",
    str_detect(key, "\\bssr\\b") ~ "ssr",
    str_detect(key, "\\bsr\\b") ~ "sr",
    str_detect(key, "\\br\\b") ~ "r",
    TRUE ~ NA_character_
  )
}

resource_tier_multiplier <- function(tier) {
  case_when(
    tier == "r" ~ 0.1,
    tier == "sr" ~ 1,
    tier == "ssr" ~ 8,
    tier == "ur" ~ 24,
    TRUE ~ NA_real_
  )
}

normalize_one_listing <- function(row, hq_level = 29) {
  item <- row$item
  qty <- row$qty
  key <- clean_item_key(item)
  hours <- duration_hours(item)

  speed_type <- case_when(
    str_detect(key, "construction speed up") ~ "construction",
    str_detect(key, "research speed up") ~ "research",
    str_detect(key, "training speed up") ~ "training",
    str_detect(key, "healing speed up") ~ "healing",
    str_detect(key, "universal speed up") ~ "universal",
    str_detect(key, "\\bspeed up\\b") ~ "universal",
    TRUE ~ NA_character_
  )

  if (!is.na(speed_type) && !is.na(hours)) {
    return(tibble(
      item_key = paste(speed_type, "speed up hour"),
      item_canonical = paste0(str_to_title(speed_type), " Speed-Up (1h Equivalent)"),
        comparable_qty = qty * hours,
        comparable_unit = "1h",
        flexibility_rank = if_else(speed_type == "universal", 0L, 1L),
        anchor_group = item_key,
        normalization_note = paste0("Normalized from ", item, " to 1 hour.")
    ))
  }

  if (str_detect(key, "\\bshield\\b") && !is.na(hours)) {
    return(tibble(
      item_key = "shield 8 hour equivalent",
      item_canonical = "Shield (8-Hour Equivalent)",
      comparable_qty = qty * hours / 8,
      comparable_unit = "8-Hour Shield",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = paste0("Normalized from ", item, " to 8-hour shields.")
    ))
  }

  if (str_detect(key, "battle data 10k")) {
    return(tibble(
      item_key = "battle data",
      item_canonical = "Battle Data (10k Equivalent)",
      comparable_qty = 1,
      comparable_unit = "10k Battle Data",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Battle Data (10K) is the base denomination."
    ))
  }

  if (str_detect(key, "battle data 100k")) {
    return(tibble(
      item_key = "battle data",
      item_canonical = "Battle Data (10k Equivalent)",
      comparable_qty = 10,
      comparable_unit = "10k Battle Data",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Battle Data (100K) is treated as 10 10K Battle Data units."
    ))
  }

  if (key == "battle data") {
    return(tibble(
      item_key = "battle data",
      item_canonical = "Battle Data (10k Equivalent)",
      comparable_qty = qty / 10000,
      comparable_unit = "10k Battle Data",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = ""
    ))
  }

  direct_resource <- case_when(
    key == "iron" ~ "iron",
    key == "food" ~ "food",
    key == "coins" ~ "coins",
    TRUE ~ NA_character_
  )

  if (!is.na(direct_resource)) {
    sr_amount <- resource_chest_amounts(hq_level) %>%
      filter(resource == direct_resource, tier == "sr") %>%
      pull(amount)
    if (!length(sr_amount)) {
      sr_amount <- resource_chest_amounts(29) %>%
        filter(resource == direct_resource, tier == "sr") %>%
        pull(amount)
    }

    return(tibble(
      item_key = paste(direct_resource, "resource"),
      item_canonical = paste0(str_to_title(direct_resource), " Resource"),
      comparable_qty = qty / sr_amount,
      comparable_unit = "SR Resource Chest",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = ""
    ))
  }

  chest_resource <- case_when(
    str_detect(key, "iron chest") ~ "iron",
    str_detect(key, "food chest") ~ "food",
    str_detect(key, "coin chest") ~ "coins",
    TRUE ~ NA_character_
  )
  is_resource_choice <- str_detect(key, "resource choice chest")
  tier <- detect_tier(key)

  if ((!is.na(chest_resource) || is_resource_choice) && !is.na(tier)) {
    resources <- if (is_resource_choice) c("iron", "food", "coins") else chest_resource
    chest_anchor_group <- if (is_resource_choice) paste("resource choice chest", tier) else key
    amounts <- resource_chest_amounts(hq_level) %>%
      filter(resource %in% resources, tier == !!tier)

    if (nrow(amounts) == 0) {
      amounts <- resource_chest_amounts(29) %>%
        filter(resource %in% resources, tier == !!tier)
      resource_note <- "HQ 29 chest content is used as a placeholder until this HQ level is entered."
    } else {
      resource_note <- paste0("Chest content uses HQ ", hq_level, " values.")
    }

    return(amounts %>%
      left_join(
        resource_chest_amounts(hq_level) %>%
          filter(tier == "sr") %>%
          select(resource, sr_amount = amount),
        by = "resource"
      ) %>%
      transmute(
        item_key = paste(resource, "resource"),
        item_canonical = paste0(str_to_title(resource), " Resource"),
        comparable_qty = qty * resource_tier_multiplier(tier),
        comparable_unit = "SR Resource Chest",
        flexibility_rank = if_else(is_resource_choice, 0L, 1L),
        anchor_group = chest_anchor_group,
        normalization_note = paste0(toupper(tier), " ", resource_note)
      ))
  }

  if (str_detect(key, "hero exp chest")) {
    hero_exp_multiplier <- case_when(
      tier == "ur" ~ 24,
      tier == "ssr" ~ 8,
      tier == "sr" ~ 1,
      TRUE ~ NA_real_
    )
    if (!is.na(hero_exp_multiplier)) {
      return(tibble(
        item_key = "hero exp chest sr equivalent",
        item_canonical = "Hero EXP Chest (SR Equivalent)",
        comparable_qty = qty * hero_exp_multiplier,
        comparable_unit = "SR Hero EXP Chest",
        flexibility_rank = 1L,
        anchor_group = item_key,
        normalization_note = "Hero EXP chest tiers use SSR = 8x SR and UR = 3x SSR."
      ))
    }
  }

  drone_level <- str_match(key, "\\blv\\s*([0-9]+)\\s*(?:drone\\s*)?component")
  if (!is.na(drone_level[, 2])) {
    level <- as.numeric(drone_level[, 2])
    is_component_choice <- str_detect(key, "choice chest")
    return(tibble(
      item_key = if_else(is_component_choice, "drone component choice level 1 equivalent", "drone component level 1 equivalent"),
      item_canonical = if_else(is_component_choice, "Drone Component Choice (Lv 1 Equivalent)", "Drone Component (Lv 1 Equivalent)"),
      comparable_qty = qty * (3 ^ (level - 1)),
      comparable_unit = if_else(is_component_choice, "Lv 1 Component Choices", "Lv 1 Components"),
      flexibility_rank = if_else(is_component_choice, 0L, 1L),
      anchor_group = item_key,
      normalization_note = if_else(
        is_component_choice,
        "Drone component choice chests use 3 lower-level components per next level, but fit as a flexible choice item.",
        "Drone components use 3 lower-level components per next level."
      )
    ))
  }

  material_power <- case_when(
    key == "superalloy" ~ 0,
    key == "synthetic resin" ~ 1,
    str_detect(key, "dielectric ceramic") ~ 2,
    TRUE ~ NA_real_
  )
  if (!is.na(material_power)) {
    return(tibble(
      item_key = "superalloy equivalent",
      item_canonical = "Superalloy / Resin / Ceramic Materials",
      comparable_qty = qty * (4 ^ material_power),
      comparable_unit = "superalloy",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Crafting-material chain uses 4 of each level for 1 of the next."
    ))
  }

  if (key == "universal decor component") {
    return(tibble(
      item_key = "universal decor component equivalent",
      item_canonical = "Universal Decor Component Equivalent",
      comparable_qty = qty,
      comparable_unit = "Universal Decor Component",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Universal decor components are the base decoration unit."
    ))
  }

  if (key == "decoration chest ur") {
    return(tibble(
      item_key = "universal decor component equivalent",
      item_canonical = "Universal Decor Component Equivalent",
      comparable_qty = qty * 130,
      comparable_unit = "Universal Decor Component",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "1 UR decoration chest is treated as 130 universal decor components."
    ))
  }

  if (key %in% c("hero choice chest", "ur hero universal shard")) {
    return(tibble(
      item_key = "ur hero shard equivalent",
      item_canonical = "UR Hero Shard Equivalent",
      comparable_qty = qty,
      comparable_unit = "shard",
      flexibility_rank = if_else(key == "hero choice chest", 0L, 1L),
      anchor_group = item_key,
      normalization_note = "Hero choice chests and UR universal shards are treated as equivalent for now."
    ))
  }

  if (str_detect(key, "skill chip chest") && !is.na(tier)) {
    skill_chip_ev <- case_when(
      tier == "r" ~ "EV: 65% R, 30% SR, 4.8% SSR, 0.2% UR + 1 material",
      tier == "sr" ~ "EV: 68% SR, 30% SSR, 2% UR + 5 material",
      tier == "ssr" ~ "EV: 85% SSR, 15% UR + 30 material",
      tier == "ur" ~ "100% UR + 100 material",
      TRUE ~ paste0(toupper(tier), " Skill Chip Chest")
    )
    return(tibble(
      item_key = paste("skill chip chest", tier),
      item_canonical = paste0("Skill Chip Chest (", toupper(tier), ")"),
      comparable_qty = qty,
      comparable_unit = skill_chip_ev,
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Skill chip chest baseline uses expected chip/material contents."
    ))
  }

  if (str_detect(key, "skill chip chest") && is.na(tier)) {
    return(tibble(
      item_key = "skill chip chest r",
      item_canonical = "Skill Chip Chest (R)",
      comparable_qty = qty,
      comparable_unit = "EV: 65% R, 30% SR, 4.8% SSR, 0.2% UR + 1 material",
      flexibility_rank = 1L,
      anchor_group = item_key,
      normalization_note = "Untiered skill chip chest label is treated as R."
    ))
  }

  if (key == "survivor s token") {
    return(tibble(
      item_key = key,
      item_canonical = "Survivor's Token",
      comparable_qty = qty,
      comparable_unit = "unit",
      flexibility_rank = 1L,
      anchor_group = key,
      normalization_note = ""
    ))
  }

  tibble(
    item_key = key,
    item_canonical = pretty_item_name(key),
    comparable_qty = qty,
    comparable_unit = "unit",
    flexibility_rank = 1L,
    anchor_group = key,
    normalization_note = ""
  )
}

load_prices <- function(path = source_workbook, hq_level = 29) {
  raw <- read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  starts <- which(!is.na(as.character(raw[1, ])) & as.character(raw[2, ]) == "Item")

  rows <- lapply(starts, function(st) {
    block <- raw[3:nrow(raw), st:(st + 4)]
    names(block) <- c("item", "qty", "price", "curr", "limit")
    block %>%
      mutate(
        store = str_replace(as.character(raw[[st]][1]), "Invation", "Invasion"),
        item = as.character(item),
        item = if_else(str_detect(clean_item_key(item), "skill chip chest") & !str_detect(clean_item_key(item), "\\bsr\\b|\\bssr\\b|\\bur\\b"),
                       "Skill Chip Chest (R)", item),
        item = case_when(
          str_detect(clean_item_key(item), "battle data 10k") ~ "Battle Data (10k)",
          clean_item_key(item) == "battle data" & parse_num(qty) == 100000 ~ "Battle Data (100K)",
          clean_item_key(item) == "battle data" & parse_num(qty) == 10000 ~ "Battle Data (10k)",
          str_replace(as.character(raw[[st]][1]), "Invation", "Invasion") == "Campaign Storefront" &
            str_detect(clean_item_key(item), "hero exp chest") ~ "Hero EXP Chest (SSR)",
          TRUE ~ item
        ),
        item = vapply(item, standardize_display_item, character(1)),
        qty = parse_num(qty),
        qty = case_when(
          clean_item_key(item) == "battle data 100k" & qty == 100000 ~ 1,
          clean_item_key(item) == "battle data 10k" & qty %in% c(10, 10000) ~ 1,
          TRUE ~ qty
        ),
        qty = if_else(str_replace(as.character(raw[[st]][1]), "Invation", "Invasion") == "Alliance Storefront" &
                        str_detect(clean_item_key(item), "\\bshield\\b"),
                      1, qty),
        price = parse_num(price),
        curr = str_to_upper(str_squish(as.character(curr))),
        limit = parse_num(limit)
      ) %>%
      filter(!is.na(item), item != "")
  })

  parsed <- bind_rows(rows) %>%
    mutate(
      row_id = row_number()
    )

  normalized <- bind_rows(lapply(seq_len(nrow(parsed)), function(i) {
    bind_cols(parsed[i, ], normalize_one_listing(parsed[i, ], hq_level = hq_level))
  }))

  normalized %>%
    mutate(unit_native = price / comparable_qty) %>%
    select(row_id, store, item, item_key, item_canonical, qty, comparable_qty,
           comparable_unit, flexibility_rank, anchor_group, normalization_note, price, curr, limit, unit_native)
}

build_value_model <- function(prices, efficient_percentile = 0.80) {
  include_in_network <- function(x) {
    !(x$store == "Bounty Hunter Trade Store" &
        clean_item_key(x$item) == "drone parts" &
        x$curr == "BOUN" &
        !is.na(x$price) &
        x$price == 32) &
      (
        clean_item_key(x$item) != "battle data 10k" |
          (
            x$store == "Campaign Storefront" &
              x$curr == "CAM" &
              !is.na(x$price) &
              x$price == 2000
          )
      )
  }

  with_network_keys <- function(x) {
    x %>%
      mutate(
        network_item_key = case_when(
          item_key %in% c("construction speed up hour", "research speed up hour", "training speed up hour", "healing speed up hour") ~ "specific speed up hour",
          item_key %in% c("iron resource", "food resource", "coins resource") ~ "resource chest sr equivalent",
          TRUE ~ item_key
        )
      )
  }

  network_rows <- prices %>%
    with_network_keys() %>%
    filter(store != "Wandering Merchant", include_in_network(.), !is.na(unit_native), unit_native > 0, !is.na(item_key), item_key != "", !is.na(curr), curr != "") %>%
    mutate(
      expansion_weight = 1 / ave(row_id, row_id, FUN = length),
      finite_limit = if_else(is.na(limit), NA_real_, limit),
      listed_base_volume = comparable_qty * finite_limit
    ) %>%
    group_by(network_item_key) %>%
    mutate(
      fallback_base_volume = median(listed_base_volume[!is.na(listed_base_volume) & listed_base_volume > 0], na.rm = TRUE),
      fallback_base_volume = if_else(is.finite(fallback_base_volume), fallback_base_volume, comparable_qty),
      listed_base_volume = if_else(is.na(listed_base_volume) | listed_base_volume <= 0, fallback_base_volume, listed_base_volume),
      item_median_base_volume = median(listed_base_volume[listed_base_volume > 0], na.rm = TRUE),
      item_median_base_volume = if_else(is.finite(item_median_base_volume) & item_median_base_volume > 0, item_median_base_volume, listed_base_volume),
      volume_weight = sqrt(listed_base_volume / item_median_base_volume),
      volume_weight = pmin(pmax(volume_weight, 0.35), 3)
    ) %>%
    ungroup() %>%
    mutate(
      base_weight = if_else(curr == "DIA", 1.5, 1) * expansion_weight * volume_weight
    )

  item_levels <- sort(unique(network_rows$network_item_key))
  curr_levels <- sort(setdiff(unique(network_rows$curr), "DIA"))
  n_items <- length(item_levels)
  n_currs <- length(curr_levels)
  x <- matrix(0, nrow = nrow(network_rows), ncol = n_items + n_currs)
  colnames(x) <- c(paste0("item::", item_levels), paste0("curr::", curr_levels))

  item_idx <- match(network_rows$network_item_key, item_levels)
  x[cbind(seq_len(nrow(network_rows)), item_idx)] <- 1
  curr_idx <- match(network_rows$curr, curr_levels)
  has_curr <- !is.na(curr_idx)
  x[cbind(which(has_curr), n_items + curr_idx[has_curr])] <- -1

  y <- log(network_rows$unit_native)
  weights <- network_rows$base_weight
  fit <- NULL
  fitted <- rep(NA_real_, nrow(network_rows))
  resid <- rep(NA_real_, nrow(network_rows))

  for (iter in seq_len(8)) {
    fit <- lm.wfit(x, y, w = weights)
    coef <- fit$coefficients
    coef[is.na(coef)] <- 0
    fitted <- as.vector(x %*% coef)
    resid <- y - fitted
    scale <- median(abs(resid - median(resid, na.rm = TRUE)), na.rm = TRUE) / 0.6745
    if (!is.finite(scale) || scale <= 0) scale <- sd(resid, na.rm = TRUE)
    if (!is.finite(scale) || scale <= 0) scale <- 1
    robust_weight <- pmin(1, (1.5 * scale) / pmax(abs(resid), 1e-9))
    weights <- network_rows$base_weight * robust_weight
  }

  coef <- fit$coefficients
  coef[is.na(coef)] <- 0
  item_values <- tibble(
    network_item_key = item_levels,
    direct_dia_unit = exp(coef[seq_len(n_items)]),
    direct_dia_sources = "Network model"
  )

  item_value_lookup <- prices %>%
    distinct(item_key) %>%
    with_network_keys() %>%
    left_join(item_values, by = "network_item_key") %>%
    select(item_key, network_item_key, direct_dia_unit, direct_dia_sources)
  curr_values <- tibble(
    curr = c("DIA", curr_levels),
    dia_per_currency = c(1, exp(coef[n_items + seq_len(n_currs)]))
  )

  network_fit <- network_rows %>%
    mutate(
      network_predicted_unit_native = exp(fitted),
      network_log_residual = resid,
      network_weight = weights,
      network_value_ratio = exp(-network_log_residual)
    ) %>%
    transmute(row_id, item_key, network_item_key, network_predicted_unit_native, network_log_residual, network_weight, network_value_ratio)

  anchor_rows <- prices %>%
    with_network_keys() %>%
    filter(curr != "DIA", !is.na(unit_native), unit_native > 0) %>%
    left_join(item_value_lookup, by = c("item_key", "network_item_key")) %>%
    left_join(curr_values, by = "curr") %>%
    left_join(network_fit, by = c("row_id", "item_key", "network_item_key")) %>%
    mutate(
      observed_dia_per_currency = direct_dia_unit / unit_native,
      observed_value_index = 100 * observed_dia_per_currency,
      listing_key = clean_item_key(item),
      network_value_ratio = observed_dia_per_currency / dia_per_currency
    ) %>%
    filter(!is.na(direct_dia_unit), !is.na(dia_per_currency), is.finite(observed_dia_per_currency), observed_dia_per_currency > 0) %>%
    group_by(item_key) %>%
    mutate(item_price_rank = min_rank(desc(network_value_ratio))) %>%
    ungroup() %>%
    group_by(listing_key) %>%
    mutate(
      max_other_listing_limit = vapply(
        seq_along(limit),
        function(i) safe_max(limit[-i]),
        numeric(1)
      )
    ) %>%
    ungroup() %>%
    mutate(
      excluded_on_special = !is.na(limit) & !is.na(max_other_listing_limit) &
        limit < 0.10 * max_other_listing_limit & item_price_rank <= 2 &
        network_value_ratio >= 1.5
    )

  anchors <- anchor_rows %>%
    group_by(curr, store, anchor_group) %>%
    arrange(desc(network_weight), abs(network_log_residual), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup()

  rates <- curr_values %>%
    left_join(
      anchor_rows %>%
        group_by(curr) %>%
        summarise(
          anchor_items = n_distinct(item_key),
          observations = n(),
          min_anchor = min(observed_dia_per_currency, na.rm = TRUE),
          median_anchor = median(observed_dia_per_currency, na.rm = TRUE),
          max_anchor = max(observed_dia_per_currency, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "curr"
    ) %>%
    mutate(
      anchor_items = if_else(curr == "DIA", NA_integer_, anchor_items),
      observations = if_else(curr == "DIA", NA_integer_, observations),
      min_anchor = if_else(curr == "DIA", 1, min_anchor),
      median_anchor = if_else(curr == "DIA", 1, median_anchor),
      max_anchor = if_else(curr == "DIA", 1, max_anchor)
    )

  anchor_candidates <- anchor_rows %>%
    left_join(
      anchors %>% transmute(row_id, selected_anchor = TRUE),
      by = "row_id"
    ) %>%
    mutate(
      selected_anchor = if_else(is.na(selected_anchor), FALSE, selected_anchor),
      excluded_throwaway = network_value_ratio < 0.5,
      excluded_reason = case_when(
        excluded_on_special ~ "On special: network value is high and limit is below 10% of another store's limit",
        !selected_anchor ~ "Duplicate comparable listing: weaker row in same store/item family",
        excluded_throwaway ~ "Costly outlier: less than 0.5x network expectation",
        TRUE ~ ""
      ),
      is_excluded = excluded_reason != ""
    )

  no_direct_candidates <- prices %>%
    filter(curr != "DIA", !is.na(unit_native), unit_native > 0) %>%
    with_network_keys() %>%
    anti_join(item_value_lookup %>% select(item_key), by = "item_key") %>%
    mutate(
      direct_dia_unit = NA_real_,
      direct_dia_sources = "",
      dia_per_currency = NA_real_,
      observed_dia_per_currency = NA_real_,
      observed_value_index = NA_real_,
      network_value_ratio = NA_real_,
      network_log_residual = NA_real_,
      network_weight = NA_real_,
      item_price_rank = NA_integer_,
      excluded_on_special = FALSE,
      selected_anchor = FALSE,
      excluded_throwaway = FALSE,
      excluded_reason = "No network comparison",
      is_excluded = TRUE
    )

  anchor_candidates <- bind_rows(anchor_candidates, no_direct_candidates)

  valued <- prices %>%
    with_network_keys() %>%
    left_join(rates %>% select(curr, dia_per_currency), by = "curr") %>%
    left_join(item_value_lookup, by = c("item_key", "network_item_key")) %>%
    left_join(network_fit, by = c("row_id", "item_key", "network_item_key")) %>%
    mutate(
      effective_dia_unit = unit_native * dia_per_currency,
      direct_value_vs_rate = if_else(
        !is.na(direct_dia_unit) & !is.na(effective_dia_unit) & effective_dia_unit > 0,
        100 * direct_dia_unit / effective_dia_unit,
        NA_real_
      ),
      network_value_ratio = direct_value_vs_rate / 100
    )

  best_known <- valued %>%
    filter(!is.na(effective_dia_unit), effective_dia_unit > 0) %>%
    group_by(item_key) %>%
    summarise(
      best_effective_dia_unit = min(effective_dia_unit),
      best_store = paste(sort(unique(store[effective_dia_unit == min(effective_dia_unit)])), collapse = ", "),
      .groups = "drop"
    )

  valued <- valued %>%
    left_join(best_known, by = "item_key") %>%
    mutate(
      normal_effective_dia_unit = direct_dia_unit,
      priority_score = if_else(
        !is.na(best_effective_dia_unit) & !is.na(effective_dia_unit) & effective_dia_unit > 0,
        100 * best_effective_dia_unit / effective_dia_unit,
        NA_real_
      ),
      normal_value_ratio = if_else(
        !is.na(normal_effective_dia_unit) & !is.na(effective_dia_unit) & effective_dia_unit > 0,
        normal_effective_dia_unit / effective_dia_unit,
        NA_real_
      ),
      total_effective_dia = price * dia_per_currency
    )

  diagnostics <- valued %>%
    filter(!is.na(network_value_ratio)) %>%
    mutate(
      direction = case_when(
        network_value_ratio >= 1.5 ~ "Cheaper than network",
        network_value_ratio <= 0.5 ~ "Costlier than network",
        TRUE ~ "Near network"
      )
    )

  list(rates = rates, anchors = anchors, anchor_candidates = anchor_candidates,
       valued = valued, direct = item_value_lookup, diagnostics = diagnostics)
}

fmt_num <- function(x, digits = 2) {
  out <- ifelse(
    is.na(x),
    "",
    format(round(x, digits), big.mark = ",", nsmall = 0, scientific = FALSE, trim = TRUE)
  )
  out <- ifelse(str_detect(out, "\\."), str_replace(out, "0+$", ""), out)
  str_replace(out, "\\.$", "")
}

fmt_sig <- function(x, digits = 2) {
  out <- ifelse(
    is.na(x),
    "",
    format(signif(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
  out <- ifelse(str_detect(out, "\\."), str_replace(out, "0+$", ""), out)
  str_replace(out, "\\.$", "")
}

fmt_dia_equiv <- function(x) {
  fmt_num(round(x / 100) * 100, 0)
}

fmt_dia_compact <- function(x) {
  x <- round(x / 100) * 100
  ifelse(abs(x) >= 1000, paste0(fmt_num(x / 1000, 1), "k"), fmt_num(x, 0))
}

fmt_limit <- function(limit, store = "") {
  ifelse(
    store == "Diamond Storefront" & is.na(limit),
    "None",
    fmt_num(limit, 0)
  )
}

bold_cell <- function(x) {
  ifelse(x == "" | is.na(x), "", paste0("<strong>", x, "</strong>"))
}

excluded_cell <- function(x, excluded) {
  x <- ifelse(is.na(x), "", x)
  x
}

excluded_short_label <- function(reason) {
  case_when(
    reason == "" ~ "Fits network",
    str_detect(reason, "^On special") ~ "Special",
    str_detect(reason, "^Throwaway|^Costly outlier") ~ "Costly outlier",
    str_detect(reason, "^Duplicate") ~ "Duplicate",
    str_detect(reason, "^No direct|^No network") ~ "No network",
    TRUE ~ reason
  )
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else max(x)
}

dia_unit_col <- function(curr = NA_character_) {
  ifelse(!is.na(curr) & curr == "DIA", "DIA/unit", "~DIA/unit")
}

fmt_game_qty <- function(x) {
  out <- case_when(
    is.na(x) ~ "",
    abs(x) >= 1e9 ~ paste0(fmt_num(x / 1e9, 2), "b"),
    abs(x) >= 1e6 ~ paste0(fmt_num(x / 1e6, 2), "m"),
    abs(x) >= 1e3 ~ paste0(fmt_num(x / 1e3, 2), "k"),
    TRUE ~ fmt_num(x, 2)
  )
  out
}

base_unit_qty <- function(qty, unit) {
  unit_label <- case_when(
    unit == "1h" ~ "1h Speed-Up",
    unit == "8-Hour Shield" ~ "8-Hour Shield",
    unit == "resource" ~ "resource",
    unit == "10k Battle Data" ~ "10k Battle Data",
    unit == "Lv 1 Components" ~ "Lv 1 Component",
    unit == "Lv 1 Component Choices" ~ "Lv 1 Component Choice",
    unit == "Universal Decor Component" ~ "Universal Decor Component",
    unit == "superalloy" ~ "Superalloy",
    unit == "shard" ~ "shard",
    unit == "SR Hero EXP Chest" ~ "SR Hero EXP Chest",
    str_detect(unit, "^(EV:|100% UR \\+)") ~ "Skill Chip Chest",
    TRUE ~ unit
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & str_detect(unit_label, "Chest$"),
    paste0(unit_label, "s"),
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "Universal Decor Component",
    "Universal Decor Components",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "8-Hour Shield",
    "8-Hour Shields",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "1h Speed-Up",
    "1h Speed-Ups",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "Lv 1 Component",
    "Lv 1 Components",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "Lv 1 Component Choice",
    "Lv 1 Component Choices",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "unit",
    "units",
    unit_label
  )
  unit_label <- ifelse(
    !is.na(qty) & qty > 1 & unit_label == "shard",
    "shards",
    unit_label
  )
  paste(fmt_game_qty(qty), unit_label)
}

deal_badge <- function(score) {
  label <- ifelse(is.na(score), "", paste0(fmt_num(score, 2), "x"))
  cls <- case_when(
    is.na(score) ~ "",
    score >= 1.25 ~ "deal-great",
    score >= 0.85 ~ "deal-mid",
    TRUE ~ "deal-bad"
  )
  ifelse(label == "", "", paste0("<span class='deal-badge ", cls, "'>", label, "</span>"))
}

best_store_cell <- function(best_store, current_store = NA_character_) {
  is_current <- !is.na(best_store) & !is.na(current_store) &
    vapply(strsplit(best_store, ",\\s*"), function(stores) current_store %in% stores, logical(1))
  ifelse(
    is_current,
    paste0("<span class='best-current'>", best_store, "</span>"),
    best_store
  )
}

js_string <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\\\", "\\\\\\\\")
  x <- str_replace_all(x, "'", "\\\\'")
  x <- str_replace_all(x, "[\r\n]+", " ")
  paste0("'", x, "'")
}

table_link_html <- function(html, nav_kind, value, title = "") {
  if (is.na(value) || value == "") return(html)
  href <- paste0("?view=", nav_kind, "&", nav_kind, "=", utils::URLencode(value, reserved = TRUE))
  paste0(
    "<a href='", htmltools::htmlEscape(href), "' class='lw-table-link' title='", htmltools::htmlEscape(title),
    "' onclick='window.location.href=this.href; return false;",
    "' data-nav-kind='", htmltools::htmlEscape(nav_kind),
    "' data-nav-value='", htmltools::htmlEscape(value), "'>", html, "</a>"
  )
}

icon_badge <- function(label, cls = "misc", title = "") {
  paste0(
    "<span class='lw-icon icon-", cls, "' title='", htmltools::htmlEscape(title), "'>",
    htmltools::htmlEscape(label),
    "</span>"
  )
}

icon_img <- function(file, title = "") {
  if (!file.exists(file.path("www", "icons", file))) {
    return("")
  }
  image_size <- ifelse(str_detect(file, "\\.svg$"), "92%", "150%")
  paste0(
    "<span class='lw-icon lw-img' title='", htmltools::htmlEscape(title),
    "' style=\"background-image:url('icons/", htmltools::htmlEscape(file),
    "?v=", icon_cache_bust, "'); background-size:", image_size, ";\"></span>"
  )
}

icon_or_badge <- function(file, label, cls = "misc", title = "") {
  img <- icon_img(file, title)
  ifelse(img == "", icon_badge(label, cls, title), img)
}

item_icon <- function(item, item_key = "") {
  if (length(item) == 0) {
    return(character(0))
  }
  if (length(item) > 1 || length(item_key) > 1) {
    n <- max(length(item), length(item_key))
    return(mapply(
      item_icon,
      rep_len(item, n),
      rep_len(item_key, n),
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    ))
  }

  item_l <- clean_item_key(item)
  key_l <- clean_item_key(item_key)
  component_level <- str_match(item_l, "\\blv\\s*([0-9]+)\\s*(?:drone\\s*)?component")[, 2]
  case_when(
    item_l == "diamonds" ~ icon_or_badge("diamonds.svg", "DIA", "diamond", "Diamonds"),
    str_detect(item_l, "alliance contribution") ~ icon_or_badge("currency-alliance-contribution.svg", "ALL", "alliance", "Alliance Contributions"),
    str_detect(item_l, "(drone )?component choice chest") & component_level %in% c("3", "5") ~
      icon_or_badge(paste0("drone-component-choice-chest-lv", component_level, ".svg"), paste0("Lv", component_level), "drone", paste0("Lv ", component_level, " component choice chest")),
    str_detect(item_l, "(drone )?component choice chest") ~ icon_badge(if_else(is.na(component_level), "Lv", paste0("Lv", component_level)), "drone", "Component choice chest"),
    str_detect(item_l, "(drone )?component chest") & component_level %in% c("1", "3", "5") ~
      icon_or_badge(paste0("drone-component-chest-lv", component_level, ".svg"), paste0("Lv", component_level), "drone", paste0("Lv ", component_level, " component chest")),
    str_detect(item_l, "(drone )?component chest") ~ icon_badge(if_else(is.na(component_level), "Lv", paste0("Lv", component_level)), "drone", "Component chest"),
    item_l == "stamina" ~ icon_or_badge("stamina.svg", "STA", "energy", "Stamina"),
    item_l == "advanced teleporter" ~ icon_or_badge("advanced-teleporter.svg", "ADV", "teleport", "Advanced teleporter"),
    item_l == "alliance teleporter" ~ icon_or_badge("alliance-teleporter.svg", "ALL", "teleport", "Alliance teleporter"),
    item_l == "random teleporter" ~ icon_or_badge("random-teleporter.svg", "?", "teleport", "Random teleporter"),
    str_detect(item_l, "8 hour shield|8 h shield") ~ icon_or_badge("shield-8h.svg", "8h", "shield", "8-hour shield"),
    str_detect(item_l, "12 hour shield|12 h shield") ~ icon_or_badge("shield-12h.svg", "12h", "shield", "12-hour shield"),
    str_detect(item_l, "24 hour shield|24 h shield") ~ icon_or_badge("shield-24h.svg", "24h", "shield", "24-hour shield"),
    item_l == "trade contract" ~ icon_badge("TRK", "contract", "Trade contract"),
    item_l == "universal decor component" ~ icon_or_badge("universal-decor-component.svg", "DEC", "decor", "Universal decor component"),
    str_detect(item_l, "decoration chest") ~ icon_or_badge("decoration-chest-ur.svg", "DEC", "decor", "Decoration chest"),
    item_l == "valor badge" ~ icon_or_badge("valor-badge.webp", "VAL", "badge", "Valor badge"),
    item_l == "survivor s token" ~ icon_or_badge("survivor-token.svg", "TOK", "token", "Survivor's Token"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("skill-chip-chest-ur.svg", "UR", "chip", "Skill Chip Chest (UR)"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("skill-chip-chest-ssr.svg", "SSR", "chip", "Skill Chip Chest (SSR)"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\bsr\\b") ~ icon_or_badge("skill-chip-chest-sr.svg", "SR", "chip", "Skill Chip Chest (SR)"),
    str_detect(item_l, "skill chip chest") & str_detect(item_l, "\\br\\b") ~ icon_or_badge("skill-chip-chest-r.svg", "R", "chip", "Skill Chip Chest (R)"),
    str_detect(item_l, "skill chip chest") ~ icon_or_badge("skill-chip-chest-r.svg", "CHP", "chip", "Skill chip chest"),
    item_l == "basic chip material" ~ icon_or_badge("basic-chip-material.webp", "BAS", "chip", "Basic chip material"),
    item_l == "premium chip material" ~ icon_or_badge("premium-chip-material.webp", "PRM", "chip", "Premium chip material"),
    item_l == "skill medal" ~ icon_or_badge("skill-medal.webp", "MED", "medal", "Skill medal"),
    str_detect(item_l, "hero exp") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("hero-exp-ur.svg", "EXP", "hero", "Hero EXP Chest (UR)"),
    str_detect(item_l, "hero exp") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("hero-exp-ssr.svg", "EXP", "hero", "Hero EXP Chest (SSR)"),
    str_detect(item_l, "hero exp") & str_detect(item_l, "\\bsr\\b") ~ icon_or_badge("hero-exp-sr.svg", "EXP", "hero", "Hero EXP Chest (SR)"),
    str_detect(item_l, "hero exp") ~ icon_or_badge("hero-exp-ssr.svg", "EXP", "hero", "Hero EXP"),
    item_l == "ssr gear chest" ~ icon_or_badge("gear-chest-ssr.svg", "SSR", "gear", "SSR Gear Chest"),
    item_l == "gear chest sr" ~ icon_or_badge("gear-chest-sr.svg", "SR", "gear", "Gear Chest (SR)"),
    item_l == "ur campaign chest" | item_l == "campaign chest ur" ~ icon_or_badge("campaign-chest-ur.svg", "UR", "chest", "UR Campaign Chest"),
    item_l == "luxury choice chest" ~ icon_or_badge("choice-chest-luxury.svg", "LUX", "chest", "Luxury Choice Chest"),
    item_l == "deluxe choice chest" ~ icon_or_badge("choice-chest-deluxe.svg", "DEL", "chest", "Deluxe Choice Chest"),
    item_l == "sr food iron coin chest" ~ icon_or_badge("resource-chest-sr-combo.svg", "SR", "chest", "SR Food/Iron/Coin Chest"),
    item_l == "resource chest sr" ~ icon_or_badge("resource-chest-sr.svg", "SR", "chest", "Resource Chest (SR)"),
    item_l == "hero recruitment ticket" ~ icon_or_badge("hero-recruitment-ticket.svg", "HRT", "ticket", "Hero recruitment ticket"),
    item_l == "survivor recruitment ticket" ~ icon_or_badge("survivor-ticket.webp", "SRT", "ticket", "Survivor recruitment ticket"),
    str_detect(item_l, "\\bur\\b.*hero.*shard|hero choice chest|universal ur hero shard") ~ icon_or_badge("shard-ur.svg", "UR", "shard", "UR hero shard"),
    str_detect(item_l, "\\bssr\\b.*hero.*shard|violet shard") ~ icon_or_badge("shard-ssr.svg", "SSR", "shard", "SSR hero shard"),
    str_detect(item_l, "\\bsr\\b.*hero.*shard") ~ icon_or_badge("shard-sr.svg", "SR", "shard", "SR hero shard"),
    str_detect(item_l, "hero universal shard|hero shard") ~ icon_or_badge("shard-ur.svg", "SHD", "shard", "Hero shard"),
    item_l == "5 minute speed up chest" | item_l == "5 min speed up chest" | item_l == "5m speed up chest" ~ icon_or_badge("speed-chest-5m.svg", "5m", "speed", "5-minute speed-up chest"),
    str_detect(item_l, "5m research speed up") ~ icon_or_badge("speed-research-5m.svg", "5m", "speed", "5m research speed-up"),
    str_detect(item_l, "5m construction speed up") ~ icon_or_badge("speed-construction-5m.svg", "5m", "speed", "5m construction speed-up"),
    str_detect(item_l, "research speed up") ~ icon_or_badge("speed-research.svg", "R&D", "speed", "Research speed-up"),
    str_detect(item_l, "training speed up") ~ icon_or_badge("speed-training.svg", "TRN", "speed", "Training speed-up"),
    str_detect(item_l, "construction speed up") ~ icon_or_badge("speed-construction.svg", "BLD", "speed", "Construction speed-up"),
    str_detect(item_l, "healing speed up") ~ icon_or_badge("speed-healing.svg", "HL", "speed", "Healing speed-up"),
    str_detect(item_l, "universal speed up|\\bspeed up\\b") ~ icon_or_badge("speed-universal.svg", ">>", "speed", "Universal speed-up"),
    str_detect(item_l, "resource choice chest") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("resource-choice-chest-ur.svg", "UR", "chest", "Resource Choice Chest (UR)"),
    str_detect(item_l, "resource choice chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("resource-choice-chest-ssr.svg", "SSR", "chest", "Resource Choice Chest (SSR)"),
    str_detect(item_l, "resource choice chest") ~ icon_or_badge("resource-choice-chest-ssr.svg", "RSC", "chest", "Resource choice chest"),
    str_detect(item_l, "food chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("food-chest-ssr.svg", "FD", "food", "SSR Food Chest"),
    str_detect(item_l, "iron chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("iron-chest-ssr.svg", "FE", "iron", "SSR Iron Chest"),
    str_detect(item_l, "coin chest") & str_detect(item_l, "\\bssr\\b") ~ icon_or_badge("coin-chest-ssr.svg", "$", "coin", "SSR Coin Chest"),
    item_l == "food" | str_detect(key_l, "food resource") ~ icon_or_badge("food-50k.svg", "FD", "food", "Food resource"),
    item_l == "iron" | str_detect(key_l, "iron resource") ~ icon_or_badge("iron-50k.svg", "FE", "iron", "Iron resource"),
    item_l == "coins" | str_detect(key_l, "coins resource") ~ icon_or_badge("coins-18k.svg", "$", "coin", "Coins resource"),
    str_detect(item_l, "food chest") ~ icon_badge("FD", "food", "Food"),
    str_detect(item_l, "iron chest") ~ icon_badge("FE", "iron", "Iron"),
    str_detect(item_l, "coin chest") ~ icon_badge("$", "coin", "Coins"),
    item_l == "drone parts" ~ icon_or_badge("drone-parts.webp", "GER", "drone", "Drone parts"),
    str_detect(item_l, "drone combat boost") ~ icon_or_badge("battle-data.webp", "DAT", "drone", "Drone data"),
    str_detect(item_l, "^battle data$|battle data 10k|battle data 100k") ~ icon_or_badge("battle-data.webp", "DAT", "drone", "Drone data"),
    str_detect(item_l, "gear blueprint") & str_detect(item_l, "\\bmr\\b") ~ icon_or_badge("gear-blueprint-mr.svg", "MR", "gear", "MR gear blueprint"),
    str_detect(item_l, "gear blueprint") & str_detect(item_l, "\\bur\\b") ~ icon_or_badge("gear-blueprint-ur.svg", "UR", "gear", "UR gear blueprint"),
    str_detect(item_l, "gear blueprint") ~ icon_or_badge("gear-blueprint-ur.svg", "BP", "gear", "Gear blueprint"),
    str_detect(item_l, "dielectric ceramic") ~ icon_or_badge("dielectric-ceramic.webp", "CER", "material", "Dielectric ceramic"),
    item_l == "synthetic resin" ~ icon_or_badge("synthetic-resin.webp", "RES", "material", "Synthetic resin"),
    item_l == "superalloy" ~ icon_or_badge("superalloy.svg", "ALY", "material", "Superalloy"),
    item_l == "upgrade ore" ~ icon_or_badge("upgrade-ore.svg", "ORE", "ore", "Upgrade ore"),
    TRUE ~ icon_badge("·", "misc", "Item")
  )
}

currency_icon <- function(curr) {
  case_when(
    curr == "DIA" ~ icon_or_badge("diamonds.svg", "DIA", "diamond", "Diamonds"),
    curr == "ALL" ~ icon_or_badge("currency-alliance-contribution.svg", "ALL", "alliance", "Alliance Contributions"),
    curr == "CAM" ~ icon_or_badge("currency-campaign-medal.svg", "CAM", "campaign", "Campaign Medals"),
    curr == "COUR" ~ icon_or_badge("currency-courage-medal.svg", "CRG", "courage", "Courage Medals"),
    curr == "GLIT" ~ icon_or_badge("currency-glitter-coin.svg", "GL", "glitter", "Glitter Coins"),
    curr == "BOUN" ~ icon_or_badge("currency-bounty-voucher.svg", "BV", "bounty", "Bounty Vouchers"),
    curr == "HON" ~ icon_or_badge("currency-honor.svg", "HON", "honor", "Honor Points"),
    curr == "MOB" ~ icon_badge("MOB", "campaign", "Total Mobilization"),
    curr == "ID" ~ icon_badge("ID", "badge", "ID Points"),
    curr == "DEL" ~ icon_or_badge("choice-chest-deluxe.svg", "DEL", "chest", "Deluxe Choice Chest pick"),
    curr == "LUX" ~ icon_or_badge("choice-chest-luxury.svg", "LUX", "chest", "Luxury Choice Chest pick"),
    TRUE ~ icon_badge(curr, "misc", curr)
  )
}

item_cell <- function(item, item_key = "") {
  paste0("<span class='icon-cell'>", item_icon(item, item_key), "<span>", htmltools::htmlEscape(item), "</span></span>")
}

train_item_cell <- function(label, icon_item = label) {
  paste0("<span class='icon-cell'>", item_icon(icon_item), "<span>", htmltools::htmlEscape(label), "</span></span>")
}

item_link_cell <- function(item, item_key = "", target_item = "") {
  mapply(
    function(item_one, key_one, target_one) {
      table_link_html(item_cell(item_one, key_one), "item", target_one, "Open item view")
    },
    item, item_key, target_item,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
}

store_link_cell <- function(store) {
  vapply(store, function(store_one) {
    table_link_html(htmltools::htmlEscape(store_one), "store", store_one, "Open storefront")
  }, character(1))
}

store_links_cell <- function(stores, current_store = NA_character_) {
  current_store <- ifelse(length(current_store) == 0, NA_character_, current_store[1])
  vapply(stores, function(stores_one) {
    if (is.na(stores_one) || stores_one == "") return("")
    parts <- str_split(stores_one, ",\\s*", simplify = FALSE)[[1]]
    links <- vapply(parts, function(store) {
      html <- htmltools::htmlEscape(store)
      if (!is.na(current_store) && store == current_store) {
        html <- paste0("<span class='best-current'>", html, "</span>")
      }
      table_link_html(html, "store", store, "Open storefront")
    }, character(1))
    paste(links, collapse = ", ")
  }, character(1))
}

best_dia_store_cell <- function(value, stores) {
  paste0(fmt_num(value, 2), " (", store_links_cell(stores), ")")
}

currency_anchor_link <- function(curr) {
  vapply(curr, function(curr_one) {
    if (is.na(curr_one) || curr_one == "DIA") return("")
    href <- paste0("?view=model&currency=", utils::URLencode(curr_one, reserved = TRUE))
    paste0(
      "<a href='", htmltools::htmlEscape(href),
      "' class='lw-table-link' title='View network observations' onclick='window.location.href=this.href; return false;'>View</a>"
    )
  }, character(1))
}

currency_cell <- function(curr) {
  paste0("<span class='icon-cell'>", currency_icon(curr), "<span>", curr, "</span></span>")
}

currency_label <- function(curr) {
  labels <- c(
    DIA = "Diamonds",
    ALL = "Alliance Contributions",
    HON = "Honor Points",
    CAM = "Campaign Medals",
    MOB = "Total Mobilization",
    ID = "ID Points",
    DEL = "Deluxe Choice Chest pick",
    LUX = "Luxury Choice Chest pick",
    COUR = "Courage Medals",
    BOUN = "Bounty Hunter Tokens",
    GLIT = "Glittering Market Tokens"
  )
  out <- labels[curr]
  ifelse(is.na(out), curr, out)
}

train_items <- function(hq_level = 29) {
  sr_food <- resource_chest_amounts(hq_level) %>%
    filter(resource == "food", tier == "sr") %>%
    pull(amount)
  if (!length(sr_food) || is.na(sr_food)) {
    sr_food <- resource_chest_amounts(29) %>%
      filter(resource == "food", tier == "sr") %>%
      pull(amount)
  }

  top_tier_ids <- c(
    "battle_data_10",
    "gear_ssr_1",
    "ur_decoration_1",
    "ur_resource_choice_3",
    "skill_medal_3000",
    "universal_decor_component_20",
    "upgrade_ore_2500",
    "dielectric_ceramic_50",
    "drone_parts_6",
    "ur_hero_shard_2",
    "drone_component_lv3_1"
  )

  tibble::tribble(
    ~id, ~label, ~icon_item, ~reward_qty, ~value_kind, ~item_key, ~comparable_qty, ~currency, ~flat_dia,
    "battle_data_10", "10k Battle Data (x10)", "Battle Data (10k)", "10", "item", "battle data", 10, NA_character_, NA_real_,
    "battle_data_5", "10k Battle Data (x5)", "Battle Data (10k)", "5", "item", "battle data", 5, NA_character_, NA_real_,
    "hero_ticket_1", "Hero Recruitment Ticket (x1)", "Hero Recruitment Ticket", "1", "item", "hero recruitment ticket", 1, NA_character_, NA_real_,
    "resource_chest_50", "Resource Chest (x50)", "Resource Chest (SR)", "50", "item", "food resource", 50 * 10000 / sr_food, NA_character_, NA_real_,
    "speed_5m_20", "5-min Speed Up Chest (x20)", "5m Speed Up Chest", "20", "item", "construction speed up hour", 20 * 5 / 60, NA_character_, NA_real_,
    "diamonds_100", "100 Diamonds", "Diamonds", "100", "flat", NA_character_, 1, NA_character_, 100,
    "gear_ssr_1", "SSR Gear Chest (x1)", "SSR Gear Chest", "1", "item", "superalloy equivalent", 150 * 4, NA_character_, NA_real_,
    "gear_sr_1", "Gear Chest (SR) (x1)", "Gear Chest SR", "1", "item", "superalloy equivalent", 40, NA_character_, NA_real_,
    "ur_decoration_1", "UR Decoration Chest (x1)", "Decoration Chest (UR)", "1", "item", "universal decor component equivalent", 130, NA_character_, NA_real_,
    "alliance_contribution_2500", "Alliance Contribution (x2500)", "Alliance Contribution", "2.5k", "currency", NA_character_, 2500, "ALL", NA_real_,
    "ur_resource_choice_3", "UR Resource Choice Chest (x3)", "Resource Choice Chest (UR)", "3", "item", "food resource", 3 * resource_tier_multiplier("ur"), NA_character_, NA_real_,
    "ur_hero_shard_1", "UR Universal Hero Shard (x1)", "UR Hero Universal Shard", "1", "item", "ur hero shard equivalent", 1, NA_character_, NA_real_,
    "sr_hero_exp_32", "SR Hero EXP Chest (x32)", "Hero EXP Chest (SR)", "32", "item", "hero exp chest sr equivalent", 32, NA_character_, NA_real_,
    "sr_resource_chest_32", "SR Food/Iron/Coin Chest (x32)", "SR Food/Iron/Coin Chest", "32", "item", "coins resource", 32 * resource_tier_multiplier("sr"), NA_character_, NA_real_,
    "ssr_coin_chest_5", "SSR Coin Chest (x5)", "SSR Coin Chest", "5", "item", "coins resource", 5 * resource_tier_multiplier("ssr"), NA_character_, NA_real_
    ,
    "drone_parts_3", "Drone Parts (x3)", "Drone Parts", "3", "item", "drone parts", 3, NA_character_, NA_real_,
    "drone_parts_6", "Drone Parts (x6)", "Drone Parts", "6", "item", "drone parts", 6, NA_character_, NA_real_,
    "upgrade_ore_2000", "Upgrade Ore (x2.0k)", "Upgrade Ore", "2.0k", "item", "upgrade ore", 2000, NA_character_, NA_real_,
    "upgrade_ore_2500", "Upgrade Ore (x2.5k)", "Upgrade Ore", "2.5k", "item", "upgrade ore", 2500, NA_character_, NA_real_,
    "drone_component_lv3_1", "Lv 3 Drone Component Chest (x1)", "Lv 3 Drone Component Chest", "1", "item", "drone component level 1 equivalent", 9, NA_character_, NA_real_,
    "ur_hero_shard_2", "UR Universal Hero Shard (x2)", "UR Hero Universal Shard", "2", "item", "ur hero shard equivalent", 2, NA_character_, NA_real_,
    "skill_medal_2400", "Skill Medal (x2.4k)", "Skill Medal", "2.4k", "item", "skill medal", 2400, NA_character_, NA_real_,
    "skill_medal_3000", "Skill Medal (x3.0k)", "Skill Medal", "3.0k", "item", "skill medal", 3000, NA_character_, NA_real_,
    "universal_decor_component_20", "Universal Decor Component (x20)", "Universal Decor Component", "20", "item", "universal decor component equivalent", 20, NA_character_, NA_real_,
    "dielectric_ceramic_50", "Dielectric Ceramic (x50)", "Dielectric Ceramic", "50", "item", "superalloy equivalent", 50 * 16, NA_character_, NA_real_
  ) %>%
    mutate(
      tier = if_else(id %in% top_tier_ids, "top", "standard"),
      tier_rank = if_else(tier == "top", 1L, 2L),
      original_order = row_number()
    ) %>%
    arrange(tier_rank, label)
}

item_choices <- function(prices_df) {
  values <- sort(unique(prices_df$item_canonical))
  labels <- item_menu_label(values)
  stats::setNames(values, labels)
}







