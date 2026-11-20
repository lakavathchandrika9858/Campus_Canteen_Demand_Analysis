# Campus Canteen Demand Analysis - DMA Lab
# Student survey + owner survey (CSV or ZIP)
# Figures are survey estimates, not actual sales records.

# 0. Settings
CONFIG <- list(
  student_file = NULL,                
  owner_file = NULL,                   
  show_plots = TRUE,
  seed = 42L,
  min_model_rows = 25L,
  open_ended_purchase_score = 6,       # Assumption for '6 or more times'
  open_ended_spending_score = 175,     # Assumption for 'More than Rs 150'
  include_optional_forest = TRUE
)
set.seed(CONFIG$seed)
library(ggplot2)
theme_set(theme_minimal(base_size = 12))

# Simple color names used throughout the graphs.
# Change them here if you want another color scheme.
teal <- "cadetblue"
purple <- "mediumpurple4"
gold <- "goldenrod"
coral <- "red3"
dark_red <- "red1"
light_teal <- "turquoise3"
light_grey <- "grey92"
pale_grey <- "grey94"

# 1. Read and clean the survey files
log_step <- function(title) cat("\n", strrep("=", 68), "\n", title, "\n",
                                strrep("=", 68), "\n", sep = "")
clean_text <- function(x) {
  x <- as.character(x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)
  x[tolower(x) %in% c("", "na", "n/a", "null", "none provided", "not answered")] <- NA_character_
  x
}
normalize <- function(x) {
  x <- tolower(clean_text(x))
  x <- gsub("[\u2013\u2014\u2212]", "-", x)
  x <- gsub("[\u2018\u2019]", "'", x)
  x
}
read_survey <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    listing <- unzip(path, list = TRUE)
    candidates <- listing$Name[grepl("\\.csv$", listing$Name, ignore.case = TRUE) &
                                 !grepl("(^|/)__MACOSX/", listing$Name)]
    if (length(candidates) != 1L) stop("Expected exactly one CSV inside ZIP: ", path)
    con <- unz(path, candidates[1], open = "r")
    on.exit(close(con), add = TRUE)
    df <- read.csv(con, check.names = FALSE, colClasses = "character",
                   na.strings = "", strip.white = TRUE)
    df[] <- lapply(df, clean_text)
    return(df)
  }
  df <- read.csv(path, check.names = FALSE, colClasses = "character",
                 fileEncoding = "UTF-8-BOM", na.strings = "", strip.white = TRUE)
  df[] <- lapply(df, clean_text)
  df
}
remove_identifiers <- function(df) {
  keep <- !grepl("^(timestamp|username|name|student name|email|e-mail|email address|roll no\\.?|roll number|phone|mobile)(\\b|$)",
                 names(df), ignore.case = TRUE, perl = TRUE)
  df[, keep, drop = FALSE]
}
# Remove repeated submissions only when an identifier matches.
# Identical survey answers alone are not considered duplicates.
dedup_survey <- function(df) {
  identity_columns <- grep("^(e-?mail( address)?|roll( number| no\\.?)?|timestamp)$",
                           names(df), ignore.case = TRUE, value = TRUE)
  identity_columns <- identity_columns[!grepl("timestamp", identity_columns, ignore.case = TRUE)]
  if (length(identity_columns)) {
    key <- do.call(paste, c(lapply(df[identity_columns], normalize), sep = "|"))
    valid <- !is.na(df[[identity_columns[1]]])
    df <- df[!(duplicated(key) & valid), , drop = FALSE]
  }
  df
}
get_field <- function(df, pattern, label = pattern, required = FALSE) {
  ix <- grep(pattern, names(df), ignore.case = TRUE, perl = TRUE)
  if (length(ix) == 1L) return(df[[ix]])
  if (length(ix) > 1L) {
    warning("Ambiguous field ", label, ": ", paste(names(df)[ix], collapse = ", "))
  } else if (required) {
    stop("Required survey column missing: ", label,
         "\nInspect names(student_raw) or names(owner_raw) to update the mapping.")
  } else {
    warning("Optional field missing: ", label)
  }
  rep(NA_character_, nrow(df))
}

show_plot <- function(p) {
  if (CONFIG$show_plots) print(p)
  invisible(p)
}
show_table <- function(df, name) {
  cat("\n", gsub("_", " ", name), "\n", sep = "")
  print(utils::head(df, 15), row.names = FALSE)
  invisible(df)
}
frequency_table <- function(x, variable, include_missing = FALSE) {
  x <- as.character(x)
  if (include_missing) x[is.na(x)] <- "Missing"
  x <- x[!is.na(x)]
  if (!length(x)) return(data.frame(variable = character(), category = character(),
                                    percentage = numeric()))
  counts <- sort(table(x), decreasing = TRUE)
  data.frame(variable = variable, category = names(counts),
             percentage = 100 * as.numeric(counts) / sum(counts))
}
plot_distribution <- function(x, title, xlab = "Responses") {
  dat <- frequency_table(x, title)
  if (!nrow(dat)) return(invisible(NULL))
  p <- ggplot(dat, aes(x = reorder(category, percentage), y = percentage)) +
    geom_col(fill = teal, width = .7) +
    coord_flip() + labs(title = title, x = xlab, y = "Percentage of valid responses") +
    geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -.12, size = 3.4) +
    expand_limits(y = max(dat$percentage) * 1.17)
  show_plot(p)
}

show_panel <- function(plots, ncol = 2L) {
  plots <- Filter(Negate(is.null), plots)
  if (!length(plots) || !CONFIG$show_plots) return(invisible(NULL))
  if (length(plots) == 1L) { print(plots[[1]]); return(invisible(NULL)) }
  nrow <- ceiling(length(plots) / ncol)
  grid::grid.newpage()
  lay <- grid::grid.layout(nrow, ncol)
  grid::pushViewport(grid::viewport(layout = lay))
  for (i in seq_along(plots)) {
    r <- (i - 1L) %/% ncol + 1L
    c <- (i - 1L) %% ncol + 1L
    print(plots[[i]], newpage = FALSE,
          vp = grid::viewport(layout.pos.row = r, layout.pos.col = c))
  }
  grid::popViewport()
  invisible(NULL)
}
bar_panel <- function(x, title, color = teal, levels = NULL) {
  dat <- frequency_table(x, title)
  if (!is.null(levels) && nrow(dat)) {
    dat <- dat[match(levels, dat$category, nomatch = 0), , drop = FALSE]
  }
  if (!nrow(dat)) return(NULL)
  dat$category <- factor(dat$category, levels = rev(dat$category))
  ggplot(dat, aes(category, percentage)) +
    geom_col(fill = color, width = .7) + coord_flip() +
    geom_text(aes(label = sprintf("%.0f", percentage)), hjust = -.15, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, .22))) +
    labs(title = title, x = NULL, y = "% of valid responses") +
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.text.y = element_text(size = 8.5),
          axis.title.x = element_text(size = 9))
}
# Select one or many slots from owner answers; don't mark unreported slots as peaks.
owner_hour_tokens <- function(value) {
  if (is.na(value)) return(character())
  z <- normalize(value)
  z <- gsub("([ap]m) +(?=[0-9])", "\\1;", z, perl = TRUE)
  out <- slot_names[vapply(slot_names, function(slot) {
    key <- tolower(slot)
    grepl(key, z, fixed = TRUE)
  }, logical(1))]
  out <- union(out, parse_slots(value))
  if (grepl("after\\s*8", z)) out <- union(out, "After 8 PM")
  out
}
owner_day_tokens <- function(value) {
  if (is.na(value)) return(character())
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  z <- normalize(value)
  days[vapply(days, function(day) grepl(tolower(day), z, fixed = TRUE) ||
                grepl(paste0("\\b", tolower(substr(day, 1, 3)), "\\b"), z, perl = TRUE), logical(1))]
}
owner_heatmap <- function(data, column, levels, title, parser) {
  if (!nrow(data)) return(NULL)
  keep <- !is.na(data$outlet) & !is.na(data[[column]])
  if (!any(keep)) return(NULL)
  dat <- data[keep, , drop = FALSE]
  peak <- lapply(dat[[column]], parser)
  tiles <- expand.grid(slot = levels, outlet = unique(dat$outlet), stringsAsFactors = FALSE)
  tiles$peak <- vapply(seq_len(nrow(tiles)), function(i) {
    which_outlet <- which(dat$outlet == tiles$outlet[i])
    any(vapply(peak[which_outlet], function(items) tiles$slot[i] %in% items, logical(1)))
  }, logical(1))
  tiles$slot <- factor(tiles$slot, levels = levels)
  ggplot(tiles, aes(slot, outlet, fill = peak)) +
    geom_tile(color = "white", linewidth = 1) +
    scale_fill_manual(values = c("FALSE" = pale_grey, "TRUE" = purple), guide = "none") +
    labs(title = title, subtitle = "Colored cells = reported peak; Nescafe details supplied separately", x = NULL, y = NULL) +
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
          axis.text.y = element_text(size = 9))
}

parse_purchase <- function(x) {
  k <- normalize(x)
  scores <- c("0 times" = 0, "1 time" = 1, "2-3 times" = 2.5,
              "4-5 times" = 4.5, "6 or more times" = CONFIG$open_ended_purchase_score)
  ans <- unname(scores[k])
  unrecognized <- unique(k[!is.na(k) & is.na(ans)])
  if (length(unrecognized)) warning("Unknown purchase bands treated as missing: ",
                                    paste(unrecognized, collapse = "; "))
  as.numeric(ans)
}
parse_spending <- function(x) {
  k <- normalize(x)
  k <- gsub("[\u20b9,]", "", k)
  k <- gsub("\\s+", "", k)
  out <- rep(NA_real_, length(k))
  for (i in seq_along(k)) {
    if (is.na(k[i])) next
    match_range <- regexec("^(?:rs\\.?|inr)?([0-9]+)-([0-9]+)$", k[i])
    mm <- regmatches(k[i], match_range)[[1]]
    if (length(mm) == 3L) out[i] <- mean(as.numeric(mm[2:3]))
    if (grepl("morethan|above|over|150\\+", k[i])) out[i] <- CONFIG$open_ended_spending_score
  }
  if (any(!is.na(k) & is.na(out)))
    warning("Some spending bands could not be parsed; inspect data-quality report")
  out
}
standard_canteen <- function(x) {
  key <- gsub("[^a-z]", "", iconv(normalize(x), to = "ASCII//TRANSLIT", sub = ""))
  out <- as.character(x)
  out[!is.na(key) & grepl("snacker", key)] <- "Snackers"
  out[!is.na(key) & grepl("domino", key)] <- "Dominos"
  out[!is.na(key) & grepl("nescafe", key)] <- "Nescafe"
  out
}
slot_names <- c("9-11 AM", "11 AM-1 PM", "1-3 PM", "3-5 PM", "5-7 PM", "After 7 PM")
parse_slots <- function(x) {
  z <- normalize(x)
  if (is.na(z)) return(character(0))
  z <- gsub("([ap]m) +(?=[0-9])", "\\1;", z, perl = TRUE)
  patterns <- c("9\\s*:?00?\\s*-\\s*11\\s*:?00?\\s*am|9-11\\s*am",
                "11\\s*:?00?\\s*am\\s*-\\s*1\\s*:?00?\\s*pm|11\\s*am-1\\s*pm",
                "1\\s*:?00?\\s*-\\s*3\\s*:?00?\\s*pm|1-3\\s*pm",
                "3\\s*:?00?\\s*-\\s*5\\s*:?00?\\s*pm|3-5\\s*pm",
                "5\\s*:?00?\\s*-\\s*7\\s*:?00?\\s*pm|5-7\\s*pm",
                "after\\s*7\\s*:?00?\\s*pm|after\\s*7\\s*pm")
  found <- vapply(patterns, function(pattern) grepl(pattern, z, perl = TRUE), logical(1))
  slot_names[found]
}
make_slot_matrix <- function(answers) {
  vals <- lapply(answers, parse_slots)
  mat <- t(vapply(vals, function(v) as.integer(slot_names %in% v), integer(length(slot_names))))
  colnames(mat) <- slot_names
  mat[is.na(answers), ] <- NA_integer_
  mat
}

run_analysis <- function(student_file, owner_file) {
  log_step("1. LOAD DATA AND AUDIT")
  student_raw <- read_survey(student_file)
  owner_raw <- read_survey(owner_file)
  if (!nrow(student_raw) || !nrow(owner_raw)) stop("Both input surveys must have responses")
  student_dedup <- dedup_survey(student_raw)
  owner_dedup <- dedup_survey(owner_raw)
  student <- remove_identifiers(student_dedup)
  owner <- remove_identifiers(owner_dedup)
  cat("Data imported. Student response counts are not displayed.\n")
  quality <- rbind(
    data.frame(source = "student", column = names(student),
               missing_percent = round(vapply(student, function(v) mean(is.na(v))*100, 0.0), 1),
               distinct_nonmissing = vapply(student, function(v) length(unique(v[!is.na(v)])), 0L)),
    data.frame(source = "owner", column = names(owner),
               missing_percent = round(vapply(owner, function(v) mean(is.na(v))*100, 0.0), 1),
               distinct_nonmissing = vapply(owner, function(v) length(unique(v[!is.na(v)])), 0L))
  )
  show_table(quality, "01_data_quality")
  cat("Mean missing percentage: ",
      round(mean(quality$missing_percent), 1), "%\n", sep = "")
  s <- function(p, label = p, required = FALSE) get_field(student, p, label, required)
  o <- function(p, label = p, required = FALSE) get_field(owner, p, label, required)
  
  log_step("2. TRANSFORM AND ENGINEER FEATURES")
  visits <- data.frame(
    Snackers = parse_purchase(s("^Q3\\..*\\[Snackers\\]$", "Snackers visits", TRUE)),
    Dominos = parse_purchase(s("^Q3\\..*\\[Dominos\\]$", "Dominos visits", TRUE)),
    Nescafe = parse_purchase(s("^Q3\\..*\\[Nescafe\\]$", "Nescafe visits", TRUE))
  )
  # Missing visits cannot become '0 times'. At least one known score is needed.
  complete_visits <- complete.cases(visits)
  visits_total <- rowSums(visits)
  visits_total[!complete_visits] <- NA_real_
  spending_raw <- s("^Q9\\.", "spending per visit")
  spending <- parse_spending(spending_raw)
  student_features <- data.frame(
    residence = clean_text(s("^Q2\\.", "residence")),
    preferred_canteen = standard_canteen(s("^Q4\\.", "preferred canteen")),
    preferred_times = s("^Q5\\.", "purchase times"),
    week_pattern = s("^Q6\\.", "purchase day"),
    food = s("^Q7\\.", "food type"),
    spend_band = spending_raw,
    spend_midpoint_inr = spending,
    motivation = s("^Q10\\.", "reason for choice"),
    waiting = s("^Q11\\.", "waiting time"),
    stockouts = s("^Q12\\.", "stockout experience"),
    stockout_action = s("^Q13\\.", "stockout reaction"),
    improvement = s("^Q14\\.", "requested improvements"),
    snackers_weekly = visits$Snackers,
    dominos_weekly = visits$Dominos,
    nescafe_weekly = visits$Nescafe,
    weekly_purchases = visits_total,
    weekly_spend_proxy_inr = visits_total * spending,
    stringsAsFactors = FALSE
  )
  # Spending and visit counts are estimates based on survey ranges.
  bins <- quantile(student_features$weekly_purchases, probs = c(1/3, 2/3),
                   na.rm = TRUE, names = FALSE)
  if (length(unique(bins)) == 2L) {
    student_features$purchase_group <- cut(student_features$weekly_purchases,
                                           breaks = c(-Inf, bins[1], bins[2], Inf),
                                           labels = c("Low", "Medium", "High"), include.lowest = TRUE)
  } else {
    student_features$purchase_group <- factor(NA_character_, levels = c("Low", "Medium", "High"))
  }
  slots <- make_slot_matrix(student_features$preferred_times)
  slot_data <- as.data.frame(slots, check.names = FALSE)
  names(slot_data) <- paste0("slot_", seq_along(slot_names))
  student_features <- cbind(student_features, slot_data)
  show_table(student_features, "02_processed_student_features")
  
  owner_fields <- c(
    outlet="^Q1\\.", operating_hours="^Q2\\.", reported_closing_time="^Q2b\\.",
    customers_per_day="^Q3\\.", food_categories="^Q4\\.", best_sellers="^Q5\\.",
    low_demand="^Q6\\.", peak_hours="^Q7\\.", peak_days="^Q8\\.",
    demand_change="^Q9\\.", preparation_method="^Q10\\.",
    items_that_sell_out="^Q11\\.", items_left_unsold="^Q12\\.",
    unsold_share="^Q13\\.", unsold_handling="^Q14\\.",
    sales_record_method="^Q15\\.", sales_history="^Q16\\.",
    menu_price_change="^Q17\\.", challenge="^Q18\\.",
    sales_data_available="^Q19\\.", prices_available="^Q20\\.",
    quantity_tracking="^Q21\\.", unexpected_demand="^Q22\\."
  )
  owner_features <- as.data.frame(lapply(owner_fields, function(pattern) o(pattern)),
                                  stringsAsFactors = FALSE, check.names = FALSE)
  owner_features$outlet <- standard_canteen(owner_features$outlet)
  nescafe_supplied <- c(customers_per_day = "100-200",
                        items_that_sell_out = "Maggie",
                        items_left_unsold = "Nothing",
                        peak_hours = "After 8 PM",
                        peak_days = "Friday;Saturday;Sunday")
  owner_features$information_source <- "Owner survey"
  nescafe_row <- which(!is.na(owner_features$outlet) &
                         owner_features$outlet == "Nescafe")
  if (!length(nescafe_row)) {
    new_owner <- as.list(rep(NA_character_, ncol(owner_features)))
    names(new_owner) <- names(owner_features)
    new_owner$outlet <- "Nescafe"
    new_owner$information_source <- "Separately supplied project information"
    for (field in names(nescafe_supplied)) new_owner[[field]] <- unname(nescafe_supplied[field])
    owner_features <- rbind(owner_features, as.data.frame(new_owner, stringsAsFactors = FALSE))
  } else {
    for (idx in nescafe_row) {
      supplemented <- FALSE
      for (field in names(nescafe_supplied)) {
        if (is.na(owner_features[[field]][idx])) {
          owner_features[[field]][idx] <- unname(nescafe_supplied[field])
          supplemented <- TRUE
        }
      }
      if (supplemented) owner_features$information_source[idx] <-
          "Owner survey + separately supplied project information"
    }
  }
  # Keep the same order in every owner graph.
  owner_features <- owner_features[order(match(owner_features$outlet,
                                               c("Dominos", "Campus canteen", "Nescafe")), na.last = TRUE), , drop = FALSE]
  rownames(owner_features) <- NULL
  cat("\nNote: Nescafe supplementary values came from project information supplied earlier,\n",
      "not from the owner CSV. They are labeled separately in the results.\n", sep = "")
  show_table(owner_features, "03_processed_owner_features")
  
  log_step("3. EXPLORATORY ANALYSIS")
  cat("Estimated purchases/week (complete visit records): ",
      round(mean(student_features$weekly_purchases, na.rm = TRUE), 2), "\n", sep = "")
  cat("Estimated spending/visit (band midpoint): Rs ",
      round(mean(spending, na.rm = TRUE), 2), "\n", sep = "")
  # Weekly purchases, preferences and purchase bands
  mean_by_canteen <- data.frame(canteen = names(visits),
                                mean = colMeans(visits, na.rm = TRUE))
  p_mean <- ggplot(mean_by_canteen, aes(reorder(canteen, mean), mean)) +
    geom_col(fill = purple, width = .65) + coord_flip() +
    geom_text(aes(label = sprintf("%.2f", mean)), hjust = -.2, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, .25))) +
    labs(title = "Estimated weekly purchases", x = NULL, y = "Mean purchases/student")
  visit_bins <- c("0 times", "1 time", "2-3 times", "4-5 times", "6 or more times")
  visit_original <- data.frame(
    Snackers = s("^Q3\\..*\\[Snackers\\]$"),
    Dominos = s("^Q3\\..*\\[Dominos\\]$"),
    Nescafe = s("^Q3\\..*\\[Nescafe\\]$")
  )
  visit_records <- do.call(rbind, lapply(names(visit_original), function(nm) {
    valid <- normalize(visit_original[[nm]])
    valid <- valid[!is.na(valid)]
    if (!length(valid)) return(NULL)
    counts <- table(factor(valid, levels = visit_bins))
    data.frame(canteen = nm, band = names(counts),
               percentage = 100 * as.numeric(counts) / length(valid))
  }))
  p_bands <- if (!is.null(visit_records) && nrow(visit_records)) ggplot(visit_records,
                                                                        aes(canteen, percentage, fill = band)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c("0 times" = light_grey, "1 time" = light_teal,
                                 "2-3 times" = teal, "4-5 times" = gold, "6 or more times" = purple),
                      breaks = visit_bins) +
    labs(title = "Weekly purchase bands", x = NULL, y = "% of valid responses", fill = "Visits") +
    theme(legend.position = "bottom", legend.text = element_text(size = 7)) else NULL
  show_panel(list(p_mean, bar_panel(student_features$preferred_canteen,
                                    "Canteen visited most often"), p_bands), ncol = 3)
  
  # Food choices, spending, reasons and suggested improvements
  show_panel(list(
    bar_panel(student_features$food, "Food and drinks students buy"),
    bar_panel(student_features$spend_band, "Spending per visit", purple),
    bar_panel(student_features$motivation, "Why students choose a canteen"),
    bar_panel(student_features$improvement, "Most requested improvement", gold)
  ), ncol = 2)
  
  # Waiting times and unavailable items
  show_panel(list(
    bar_panel(student_features$waiting, "Reported waiting time"),
    bar_panel(student_features$stockouts, "Times an item was unavailable", purple),
    bar_panel(student_features$stockout_action, "What students do after stockouts", coral)
  ), ncol = 3)
  
  long_visits <- data.frame(canteen = rep(names(visits), each = nrow(visits)),
                            purchases = unlist(visits, use.names = FALSE))
  long_visits <- long_visits[is.finite(long_visits$purchases), , drop = FALSE]
  if (nrow(long_visits)) {
    p <- ggplot(long_visits, aes(x = canteen, y = purchases, fill = canteen)) +
      geom_boxplot(width = .58, outlier.alpha = .4) +
      labs(title = "Estimated weekly purchases by canteen", x = NULL,
           y = "Purchases/week (category midpoints)") + guides(fill = "none")
    show_plot(p)
  }
  weekly_valid <- student_features[is.finite(student_features$weekly_purchases), , drop = FALSE]
  if (nrow(weekly_valid)) {
    p <- ggplot(weekly_valid, aes(x = weekly_purchases)) +
      geom_histogram(binwidth = 2, fill = teal, color = "white", boundary = 0) +
      geom_vline(xintercept = mean(weekly_valid$weekly_purchases), linetype = "dashed",
                 color = dark_red) +
      labs(title = "Distribution of estimated weekly purchases", x = "Approx. purchases/week", y = "Survey responses")
    show_plot(p)
  }
  slot_valid <- colMeans(slots, na.rm = TRUE) * 100
  slot_valid[!is.finite(slot_valid)] <- NA_real_
  slot_df <- data.frame(slot = factor(slot_names, levels = slot_names), percentage = slot_valid)
  p_slot <- ggplot(slot_df, aes(x = slot, y = percentage)) +
    geom_col(fill = teal, na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(percentage), "", sprintf("%.0f", percentage))),
              hjust = -.15, size = 3, na.rm = TRUE) + coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, .2))) +
    labs(title = "When students usually buy", x = NULL, y = "% selecting each slot")
  p_days <- bar_panel(student_features$week_pattern, "When students buy during the week", purple)
  show_panel(list(p_slot, p_days), ncol = 1)
  show_table(slot_df, "04_student_time_slot_shares")
  
  # Spend-vs-frequency describes an association only, not causal demand.
  spend_frequency <- student_features[complete.cases(student_features[, c("weekly_purchases", "spend_midpoint_inr")]), ]
  if (nrow(spend_frequency) >= 4L) {
    p <- ggplot(spend_frequency, aes(x = weekly_purchases, y = spend_midpoint_inr)) +
      geom_jitter(width = .15, height = 1, alpha = .5, color = teal) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = dark_red) +
      labs(title = "Purchase frequency vs typical spending", x = "Approx. purchases/week",
           y = "Typical spending/visit (Rs, midpoint)")
    show_plot(p)
  }
  numeric_features <- student_features[, c("snackers_weekly", "dominos_weekly", "nescafe_weekly",
                                           "weekly_purchases", "spend_midpoint_inr")]
  if (sum(complete.cases(numeric_features)) >= 3L) {
    cm <- cor(numeric_features, use = "pairwise.complete.obs")
    cm[!is.finite(cm)] <- NA_real_
    show_table(data.frame(feature = rownames(cm), cm), "05_correlation_matrix")
    heat <- as.data.frame(as.table(cm))
    p <- ggplot(heat, aes(Var1, Var2, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = ifelse(is.na(Freq), "", sprintf("%.2f", Freq))), size = 3) +
      scale_fill_gradient2(low = coral, mid = "white", high = teal, midpoint = 0,
                           limits = c(-1, 1), na.value = "grey90") +
      coord_equal() + labs(title = "Numeric feature correlations", x = NULL, y = NULL, fill = "r") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    show_plot(p)
  }
  # Owner customer estimates and unsold items
  if (nrow(owner_features)) {
    owner_peak <- owner_features[, c("outlet", "customers_per_day", "peak_hours", "peak_days",
                                     "items_that_sell_out", "items_left_unsold", "information_source")]
    print(owner_peak, row.names = FALSE)
    range_rows <- lapply(seq_len(nrow(owner_features)), function(i) {
      val <- normalize(owner_features$customers_per_day[i])
      if (is.na(val) || is.na(owner_features$outlet[i])) return(NULL)
      m <- regmatches(val, regexec("([0-9]+)\\s*-\\s*([0-9]+)", val))[[1]]
      if (length(m) != 3) return(NULL)
      data.frame(outlet = owner_features$outlet[i], lower = as.numeric(m[2]),
                 upper = as.numeric(m[3]))
    })
    ranges <- do.call(rbind, range_rows)
    p_range <- NULL
    if (!is.null(ranges) && nrow(ranges)) {
      ranges$outlet <- factor(ranges$outlet, levels = rev(unique(ranges$outlet)))
      p_range <- ggplot(ranges, aes(y = outlet)) +
        geom_segment(aes(x = lower, xend = upper, yend = outlet),
                     linewidth = 1.5, color = teal) +
        geom_point(aes(x = lower), size = 2.5, color = teal) +
        geom_point(aes(x = upper), size = 2.5, color = teal) +
        geom_text(aes(x = (lower + upper) / 2, label = paste0(lower, "–", upper)),
                  vjust = -1, size = 3) +
        scale_x_continuous(expand = expansion(mult = c(.1, .2))) +
        labs(title = "Daily customers (owners + supplied Nescafe)",
             subtitle = "Nescafe: separately supplied estimate",
             x = "Customer range/day", y = NULL)
    }
    items <- owner_features[!is.na(owner_features$outlet),
                            c("outlet", "items_that_sell_out", "items_left_unsold"), drop = FALSE]
    item_lines <- do.call(rbind, lapply(seq_len(nrow(items)), function(i) {
      a <- items$items_that_sell_out[i]
      b <- items$items_left_unsold[i]
      data.frame(outlet = items$outlet[i],
                 detail = c(paste0("Sells out: ", ifelse(is.na(a), "Not reported", a)),
                            paste0("Left unsold: ", ifelse(is.na(b), "Not reported", b))),
                 line = c(2, 1))
    }))
    p_items <- NULL
    if (!is.null(item_lines) && nrow(item_lines)) {
      item_lines$outlet <- factor(item_lines$outlet, levels = rev(unique(items$outlet)))
      p_items <- ggplot(item_lines, aes(x = .04, y = line, label = detail)) +
        geom_text(hjust = 0, size = 3.2) +
        facet_grid(outlet ~ ., switch = "y") +
        coord_cartesian(xlim = c(0, 1), ylim = c(.5, 2.5), clip = "off") +
        theme_void() + theme(strip.text.y.left = element_text(angle = 0, face = "bold"),
                             plot.margin = margin(7, 10, 7, 10)) +
        labs(title = "What sells out and what remains",
             subtitle = "Nescafe: separately supplied information")
    }
    show_panel(list(p_range, p_items), ncol = 2)
    
    # Owner peak times and days (including the supplied Nescafe details)
    p_owner_hours <- owner_heatmap(owner_features, "peak_hours",
                                   c(slot_names, "After 8 PM"), "Reported peak hours (incl. Nescafe)", owner_hour_tokens)
    p_owner_days <- owner_heatmap(owner_features, "peak_days",
                                  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                  "Reported peak days (incl. Nescafe)", owner_day_tokens)
    show_panel(list(p_owner_hours, p_owner_days), ncol = 1)
  }
  
  log_step("4. ASSOCIATION RULES - APRIORI")
  transaction_features <- student_features[, c("residence", "preferred_canteen", "spend_band",
                                               "waiting", "stockouts", "motivation")]
  transaction_features[] <- lapply(transaction_features, function(v) {
    v <- clean_text(v)
    v[is.na(v)] <- "Unknown"
    factor(v)
  })
  if (requireNamespace("arules", quietly = TRUE) && nrow(transaction_features) >= 10L) {
    trans <- methods::as(transaction_features, "transactions")
    support_min <- max(0.03, min(0.12, 4 / nrow(transaction_features)))
    rules <- arules::apriori(trans,
                             parameter = list(supp = support_min, conf = .55, minlen = 2, maxlen = 4,
                                              target = "rules"), control = list(verbose = FALSE))
    if (length(rules)) {
      rules <- arules::sort(rules, by = "lift", decreasing = TRUE)
      rules <- subset(rules, subset = lift > 1.05)
    }
    if (length(rules)) {
      rules_df <- as(rules, "data.frame")
      show_table(rules_df, "06_apriori_rules")
      print(utils::head(rules_df, 10), row.names = FALSE)
      rule_plot <- utils::head(rules_df, 15)
      p <- ggplot(rule_plot, aes(x = reorder(rules, lift), y = lift)) +
        geom_col(fill = teal) + coord_flip() +
        labs(title = "Top Apriori associations by lift", x = "Association rule", y = "Lift") +
        theme(axis.text.y = element_text(size = 8))
      show_plot(p)
    } else cat("No association rules meet the support, confidence and lift criteria.\n")
  } else cat("Apriori skipped (install 'arules' or provide more observations).\n")
  
  log_step("5. UNSUPERVISED MINING - K-MEANS")
  cluster_columns <- c("snackers_weekly", "dominos_weekly", "nescafe_weekly", "spend_midpoint_inr")
  cluster_data <- student_features[, cluster_columns, drop = FALSE]
  usable <- complete.cases(cluster_data)
  x <- cluster_data[usable, , drop = FALSE]
  varying <- vapply(x, function(v) is.finite(stats::sd(v)) && stats::sd(v) > 0, logical(1))
  x <- x[, varying, drop = FALSE]
  if (nrow(x) >= 12L && ncol(x) >= 2L && requireNamespace("cluster", quietly = TRUE) &&
      nrow(unique(x)) >= 3L) {
    z <- scale(x)
    candidates <- 2:min(6L, nrow(unique(x)) - 1L, nrow(z) - 1L)
    if (length(candidates)) {
      scores <- lapply(candidates, function(k) {
        fit <- kmeans(z, centers = k, nstart = 30, iter.max = 100)
        sil <- cluster::silhouette(fit$cluster, dist(z))
        c(k = k, wss = fit$tot.withinss, silhouette = mean(sil[, "sil_width"]))
      })
      scores <- as.data.frame(do.call(rbind, scores))
      show_table(scores, "07_cluster_selection")
      p <- ggplot(scores, aes(x = k, y = wss)) + geom_line(color = teal) +
        geom_point(size = 3, color = teal) +
        scale_x_continuous(breaks = candidates) +
        labs(title = "Elbow method - within-cluster sum of squares", x = "Clusters (k)", y = "WSS")
      p_elbow <- p
      p <- ggplot(scores, aes(x = k, y = silhouette)) + geom_line(color = dark_red) +
        geom_point(size = 3, color = dark_red) +
        scale_x_continuous(breaks = candidates) +
        labs(title = "Mean silhouette width by cluster count", x = "Clusters (k)", y = "Silhouette")
      show_panel(list(p_elbow, p), ncol = 2)
      chosen_k <- scores$k[which.max(scores$silhouette)]
      fit <- kmeans(z, centers = chosen_k, nstart = 50)
      student_features$cluster <- NA_integer_
      student_features$cluster[usable] <- fit$cluster
      cluster_profile <- aggregate(cluster_data[usable, , drop = FALSE],
                                   by = list(cluster = fit$cluster), FUN = mean)
      show_table(cluster_profile, "08_cluster_profiles")
      print(cluster_profile, row.names = FALSE)
      pc <- prcomp(z, center = FALSE, scale. = FALSE)
      pcs <- data.frame(PC1 = pc$x[, 1], PC2 = pc$x[, 2], cluster = factor(fit$cluster))
      p <- ggplot(pcs, aes(PC1, PC2, color = cluster)) + geom_point(alpha = .75, size = 2) +
        labs(title = "Student purchase patterns - PCA view of K-Means clusters",
             subtitle = "PCA is for visualization; clusters fitted on standardized features")
      show_plot(p)
    }
  } else cat("Clustering skipped: insufficient complete, varying observations or 'cluster' missing.\n")
  show_table(student_features, "09_student_features_with_clusters")
  
  log_step("6. SUPERVISED MINING - OUT-OF-SAMPLE PREDICTION")
  predictors <- c("residence", "preferred_canteen", "week_pattern", "food",
                  "spend_band", "motivation", "waiting", "stockouts")
  model_df <- student_features[, c("weekly_purchases", predictors)]
  model_df <- model_df[is.finite(model_df$weekly_purchases), , drop = FALSE]
  if (nrow(model_df) >= CONFIG$min_model_rows) {
    idx <- sample(seq_len(nrow(model_df)), size = max(10L, floor(.8*nrow(model_df))))
    train <- model_df[idx, , drop = FALSE]
    test <- model_df[-idx, , drop = FALSE]
    for (v in predictors) {
      a <- clean_text(train[[v]])
      b <- clean_text(test[[v]])
      a[is.na(a)] <- "Unknown"
      b[is.na(b)] <- "Unknown"
      # Combine categories with very few responses.
      counts <- table(a)
      keep_levels <- names(counts)[counts >= 3]
      a[!a %in% keep_levels] <- "Other"
      b[!b %in% keep_levels] <- "Other"
      lv <- unique(c(a, "Other"))
      train[[v]] <- factor(a, levels = lv)
      test[[v]] <- factor(b, levels = lv)
    }
    usable_predictors <- predictors[vapply(train[predictors], function(v) nlevels(v) > 1L, logical(1))]
    if (!length(usable_predictors)) {
      train$intercept_only <- 1
      test$intercept_only <- 1
    }
    reg_formula <- if (length(usable_predictors)) reformulate(usable_predictors, response = "weekly_purchases") else weekly_purchases ~ 1
    baseline <- rep(mean(train$weekly_purchases), nrow(test))
    estimates <- data.frame(actual = test$weekly_purchases, Baseline = baseline)
    # Regression tree: non-linear and interpretable, if available.
    if (requireNamespace("rpart", quietly = TRUE)) {
      tree <- rpart::rpart(reg_formula, data = train, method = "anova",
                           control = rpart::rpart.control(cp = .01, minsplit = 8))
      estimates$DecisionTree <- as.numeric(predict(tree, newdata = test))
      cat("\nRegression tree summary:\n")
      print(tree)
    }
    if (CONFIG$include_optional_forest && requireNamespace("randomForest", quietly = TRUE)) {
      forest <- randomForest::randomForest(reg_formula, data = train, ntree = 300,
                                           importance = TRUE, nodesize = 5)
      estimates$RandomForest <- as.numeric(predict(forest, newdata = test))
      importance <- data.frame(feature = rownames(randomForest::importance(forest)),
                               importance = randomForest::importance(forest)[, 1], row.names = NULL)
      show_table(importance, "11_random_forest_importance")
      p <- ggplot(importance, aes(x = reorder(feature, importance), y = importance)) +
        geom_col(fill = teal) + coord_flip() +
        labs(title = "Random forest feature importance", x = "Survey feature", y = "Importance (increase in MSE)")
      show_plot(p)
    }
    n_parameters <- ncol(stats::model.matrix(reg_formula, data = train))
    if (n_parameters < nrow(train) / 2) {
      linear <- tryCatch(stats::lm(reg_formula, data = train), error = function(e) NULL)
      if (!is.null(linear)) {
        pred <- suppressWarnings(tryCatch(predict(linear, newdata = test),
                                          error = function(e) rep(NA_real_, nrow(test))))
        if (all(is.finite(pred))) estimates$LinearRegression <- as.numeric(pred)
      }
    }
    metric <- function(actual, predicted) c(
      MAE = mean(abs(actual - predicted)),
      RMSE = sqrt(mean((actual - predicted)^2)),
      R2 = if (sum((actual - mean(actual))^2) > 0)
        1 - sum((actual-predicted)^2) / sum((actual-mean(actual))^2) else NA_real_)
    models <- setdiff(names(estimates), "actual")
    metrics <- as.data.frame(do.call(rbind, lapply(models, function(m) metric(estimates$actual, estimates[[m]]))))
    metrics$model <- models
    metrics <- metrics[, c("model", "MAE", "RMSE", "R2")]
    show_table(metrics, "12_regression_holdout_metrics")
    show_table(estimates, "13_regression_holdout_predictions")
    print(metrics, row.names = FALSE)
    plot_metrics <- data.frame(model = metrics$model, MAE = metrics$MAE)
    p <- ggplot(plot_metrics, aes(x = reorder(model, MAE), y = MAE)) +
      geom_col(fill = teal) + coord_flip() +
      labs(title = "Held-out prediction error (lower is better)", x = "Model", y = "MAE (purchases/week)")
    show_plot(p)
    pred_long <- do.call(rbind, lapply(models, function(m) {
      data.frame(model = m, actual = estimates$actual, predicted = estimates[[m]])
    }))
    p <- ggplot(pred_long, aes(x = actual, y = predicted)) +
      geom_point(alpha = .65, color = teal) +
      geom_abline(intercept = 0, slope = 1, color = dark_red, linetype = "dashed") +
      facet_wrap(~ model) +
      labs(title = "Actual versus predicted weekly purchases (holdout)",
           x = "Survey-derived actual approximation", y = "Predicted purchases/week")
    show_plot(p)
  } else {
    cat("Prediction skipped: too few complete target records (minimum ",
        CONFIG$min_model_rows, "). Do not invent an accuracy score.\n", sep = "")
  }
  
  log_step("7. SURVEY-BASED DEMAND ESTIMATES AND RECOMMENDATIONS")
  canteen_demand <- data.frame(
    canteen = names(visits),
    mean_weekly_purchases = as.numeric(colMeans(visits, na.rm = TRUE)),
    median_weekly_purchases = as.numeric(vapply(visits, median, numeric(1), na.rm = TRUE)),
    share_reporting_any_purchase_pct = as.numeric(vapply(visits, function(v) {
      if (!any(!is.na(v))) return(NA_real_)
      mean(v[!is.na(v)] > 0) * 100
    }, numeric(1)))
  )
  show_table(canteen_demand, "14_relative_canteen_demand")
  print(canteen_demand, row.names = FALSE)
  if (all(is.finite(canteen_demand$mean_weekly_purchases))) {
    p <- ggplot(canteen_demand, aes(x = reorder(canteen, mean_weekly_purchases),
                                    y = mean_weekly_purchases)) +
      geom_col(fill = teal) + coord_flip() +
      labs(title = "Relative canteen demand from survey bands",
           subtitle = "Approximate purchases per responding student/week, not total campus sales",
           x = "Canteen", y = "Mean estimated purchases/week")
    show_plot(p)
  }
  recommendation <- c(
    "CAMPUS CANTEEN DEMAND ANALYSIS - INTERPRETATION",
    "Interpret every frequency and spending figure as an interval-midpoint estimate.",
    "The survey is not a randomly sampled POS dataset; do not claim campus-wide totals.",
    "Peak slot percentages are multi-select and need not add to 100%.",
    "Stockout and waiting-time plots suggest operational priorities, not causal effects.",
    "Nescafe values were supplied separately, not collected in the owner CSV.",
    "Prediction error describes out-of-sample fit to SURVEY-ESTIMATED weekly frequency,",
    "not validated future demand or exact transactions.",
    "Use the highest selected time slots to investigate staffing/preparation needs.",
    "Compare the student-reported preferences with owner-reported best sellers and peak periods.",
    "Use association rules for co-occurrence hypotheses, not causal recommendations."
  )
  cat(paste(recommendation, collapse = "\n"), "\n")
  cat("\nAnalysis complete. All results displayed in Console / Plots pane.\n")
  invisible(list(student = student_features, owner = owner_features,
                 quality = quality, demand = canteen_demand))
}

# 8. Run the analysis
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2L) {
  student_file <- args[1]
  owner_file <- args[2]
} else if (!is.null(CONFIG$student_file) && !is.null(CONFIG$owner_file)) {
  student_file <- CONFIG$student_file
  owner_file <- CONFIG$owner_file
} else if (interactive()) {
  message("Select the STUDENT Google Forms CSV")
  student_file <- file.choose()
  message("Select the OWNER survey ZIP or CSV")
  owner_file <- file.choose()
} else {
  stop("Usage: Rscript Campus_Canteen_DMA_Readable.R student.csv owner.csv-or-zip")
}
results <- run_analysis(student_file, owner_file)

