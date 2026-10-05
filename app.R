# ════════════════════════════════════════════════════════════════
# Football Spatial Analysis
# Χωρική ανάλυση αγώνων ποδοσφαίρου με χρήση Συστημάτων Γεωγραφικών Πληροφοριών
#
# Διαδραστική εφαρμογή R/Shiny της διπλωματικής εργασίας του Παράσχου Τσουράκη
# Εθνικό Μετσόβιο Πολυτεχνείο,
# Σχολή Αγρονόμων και Τοπογράφων Μηχανικών – Μηχανικών Γεωπληροφορικής, 2026
#
# Δεδομένα: StatsBomb Open Data (https://github.com/hudl/open-data)
#
# Δομή του αρχείου:
#   1. Πακέτα και σταθερές
#   2. Βοηθητικές συναρτήσεις (υπολογισμοί και γραφήματα των αναλύσεων)
#   3. Διεπαφή (ui)
#   4. Εξυπηρετητής (server)
# ════════════════════════════════════════════════════════════════

# ── Πακέτα ──────────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(tidyverse)
library(StatsBombR)
library(ggsoccer)
library(ggplot2)
library(igraph)
library(sf)

# Σταθερή αντιστοίχιση αποτελέσματος σουτ → χρώμα (παλέτα Set1· το γκολ σε χρυσό, όπως στον χάρτη γκολ σεζόν)
SHOT_OUTCOME_COLOURS <- c(
  "Goal"             = "#FFD700",
  "Saved"            = "#377EB8",
  "Saved to Post"    = "#984EA3",
  "Saved Off Target" = "#A65628",
  "Post"             = "#FF7F00",
  "Off T"            = "#E41A1C",
  "Wayward"          = "#F781BF",
  "Blocked"          = "#999999"
)

# Πεδία τα οποία διατηρούνται από τα γεγονότα μιας σεζόν (όσα χρησιμοποιεί η εφαρμογή)
SEASON_FIELDS <- c(
  "match_id", "period", "minute", "second", "type.name", "team.name", "player.name",
  "location.x", "location.y", "pass.end_location.x", "pass.end_location.y",
  "pass.recipient.name", "pass.outcome.name", "shot.outcome.name", "shot.statsbomb_xg",
  "substitution.replacement.name", "foul_committed.card.name", "bad_behaviour.card.name"
)

# ════════════════════════════════════════════════════════════════
# ΒΟΗΘΗΤΙΚΕΣ ΣΥΝΑΡΤΗΣΕΙΣ
# ════════════════════════════════════════════════════════════════

# ── Κοινό σκούρο θέμα των γραφημάτων ────────────────────────────
dark_theme <- function() {
  theme(
    plot.background   = element_rect(fill = "#1e1e1e", color = NA),
    panel.background  = element_rect(fill = "#1e1e1e", color = NA),
    text              = element_text(color = "white"),
    legend.background = element_rect(fill = "#2d2d2d", color = NA),
    legend.key        = element_rect(fill = "#2d2d2d", color = NA)
  )
}

# ── Γράφημα με επεξηγηματικό μήνυμα στη θέση του αποτελέσματος ───
no_data_plot <- function(msg = "Φορτώστε δεδομένα για να εμφανιστεί το γράφημα") {
  ggplot(data.frame(x = 0.5, y = 0.5, label = msg)) +
    geom_text(aes(x, y, label = label), color = "#888888", size = 5.5, hjust = 0.5) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#1e1e1e", color = NA))
}

# ── Ανάκτηση γεγονότων ──────────────────────────────────────────
# Τα αρχεία γεγονότων ανακτώνται διαδοχικά, με τη get.matchFree() του StatsBombR.
# Η free_allevents() του ίδιου πακέτου δημιουργεί σε κάθε κλήση μία διεργασία της R ανά πυρήνα
# του επεξεργαστή και δεν τις τερματίζει, οπότε εξαντλεί τη μνήμη ενός εξυπηρετητή φιλοξενίας.
# Όπως και στη free_allevents(), ένας αγώνας του οποίου η ανάκτηση αποτυγχάνει παραλείπεται.
.fetch_events <- function(matches) {
  bind_rows(lapply(seq_len(nrow(matches)), function(i)
    tryCatch(get.matchFree(matches[i, ]), error = function(e) NULL)))
}

# ── Έλεγχος συνέπειας: γεγονότα αναπληρωματικών πριν από την είσοδό τους ───
# Απορρίπτονται τα γεγονότα με συντεταγμένες ενός αναπληρωματικού, ο χρόνος των οποίων
# (περίοδος, λεπτό, δευτερόλεπτο) είναι προγενέστερος της εισόδου του (γεγονός Substitution).
# Τα γεγονότα χωρίς συντεταγμένες (π.χ. κάρτα στον πάγκο) διατηρούνται.
.drop_pre_entry <- function(events) {
  if (!"substitution.replacement.name" %in% names(events)) return(events)
  entries <- events %>%
    filter(type.name == "Substitution", !is.na(substitution.replacement.name)) %>%
    transmute(match_id, player.name = substitution.replacement.name,
              t_in = period * 10000 + minute * 60 + second)
  if (nrow(entries) == 0) return(events)
  events %>%
    left_join(entries, by = c("match_id", "player.name")) %>%
    filter(is.na(t_in) | is.na(location.x) |
             period * 10000 + minute * 60 + second >= t_in) %>%
    select(-t_in)
}

# ── Γεγονότα ενός αγώνα της σεζόν ───────────────────────────────
# Ανάκτηση, καθαρισμός, έλεγχος συνέπειας και επιλογή των πεδίων SEASON_FIELDS.
# Η επεξεργασία γίνεται ανά αγώνα και διατηρούνται μόνο τα πεδία τα οποία χρησιμοποιεί η εφαρμογή,
# ώστε η μνήμη η οποία απαιτείται για μια σεζόν να παραμένει μικρή. Αν η ανάκτηση αποτύχει,
# επιστρέφεται NULL και ο αγώνας παραλείπεται.
.season_match_events <- function(match) {
  ev <- .fetch_events(match)
  if (nrow(ev) == 0) return(NULL)
  ev %>% allclean() %>% .drop_pre_entry() %>% select(any_of(SEASON_FIELDS))
}

# ── Χάρτης ροής μεταβιβάσεων παίκτη ─────────────────────────────
make_player_pass_map <- function(events, player, team, subtitle) {
  df <- events %>%
    filter(player.name == player, type.name == "Pass") %>%
    mutate(
      location.x          = as.numeric(location.x),
      location.y          = as.numeric(location.y),
      pass.end_location.x = as.numeric(pass.end_location.x),
      pass.end_location.y = as.numeric(pass.end_location.y),
      pass_status = ifelse(is.na(pass.outcome.name), "Επιτυχημένη", "Ανεπιτυχής")
    ) %>%
    filter(!is.na(location.x), !is.na(pass.end_location.x))
  if (nrow(df) == 0) return(NULL)

  ggplot(df) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#224422") +
    geom_segment(
      aes(x = location.x, y = location.y,
          xend = pass.end_location.x, yend = pass.end_location.y,
          color = pass_status),
      lineend = "round", linewidth = 0.6, alpha = 0.8,
      arrow = arrow(length = unit(0.08, "inches"), ends = "last", type = "closed")
    ) +
    scale_color_manual(values = c("Επιτυχημένη" = "#00FF00", "Ανεπιτυχής" = "#FF0000")) +
    coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
    scale_y_reverse() +
    theme_pitch(aspect_ratio = NULL) + dark_theme() +
    labs(title = paste("Χάρτης ροής μεταβιβάσεων:", player),
         subtitle = paste(team, "|", subtitle),
         color = "Αποτέλεσμα",
         caption = "Δεδομένα: StatsBomb")
}

# ── Χάρτης σουτ ομάδας ή παίκτη ─────────────────────────────────
make_shot_map <- function(events, mode, team, player, subtitle) {
  df <- events %>%
    filter(type.name == "Shot", team.name == team, period < 5)

  if (mode == "player") df <- df %>% filter(player.name == player)

  df <- df %>%
    mutate(location.x = as.numeric(location.x),
           location.y = as.numeric(location.y)) %>%
    filter(!is.na(location.x))
  if (nrow(df) == 0) return(NULL)

  title_str <- if (mode == "player")
    paste("Χάρτης σουτ:", player) else
    paste("Χάρτης σουτ:", team)
  sub_str <- if (mode == "player")
    paste(team, "|", subtitle) else subtitle

  ggplot(df) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#224422") +
    geom_point(aes(location.x, location.y,
                   color = shot.outcome.name, size = shot.statsbomb_xg), alpha = 0.8) +
    scale_color_manual(values = SHOT_OUTCOME_COLOURS,
                       breaks = names(SHOT_OUTCOME_COLOURS)) +
    coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
    scale_y_reverse() +
    theme_pitch(aspect_ratio = NULL) + dark_theme() +
    labs(title = title_str,
         subtitle = sub_str,
         color = "Αποτέλεσμα", size = "Αναμενόμενα γκολ (xG)",
         caption = "Δεδομένα: StatsBomb")
}

# ── Χάρτης πυκνότητας ενεργειών παίκτη ──────────────────────────
make_action_heatmap <- function(events, player, subtitle) {
  df <- events %>%
    filter(player.name == player) %>%
    mutate(location.x = as.numeric(location.x),
           location.y = as.numeric(location.y)) %>%
    filter(!is.na(location.x), !is.na(location.y))
  if (nrow(df) == 0) return(NULL)

  ggplot(df, aes(location.x, location.y)) +
    geom_density_2d_filled(alpha = 0.8, show.legend = FALSE) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = NA) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(expand = c(0, 0)) +
    coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
    theme_pitch(aspect_ratio = NULL) + dark_theme() +
    labs(title = paste("Χάρτης πυκνότητας ενεργειών:", player),
         subtitle = paste("Ενέργειες:", nrow(df), "|", subtitle),
         caption = "Δεδομένα: StatsBomb")
}

# ── Χάρτης γκολ σεζόν ───────────────────────────────────────────
make_goals_map <- function(events, matches, team, season) {
  df <- events %>%
    filter(team.name == team, type.name == "Shot", shot.outcome.name == "Goal", period < 5) %>%
    mutate(location.x = as.numeric(location.x),
           location.y = as.numeric(location.y)) %>%
    filter(!is.na(location.x))
  if (nrow(df) == 0) return(NULL)

  n_matches <- matches %>%
    filter(home_team.home_team_name == team | away_team.away_team_name == team) %>%
    nrow()

  ggplot(df) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#224422") +
    geom_point(aes(location.x, location.y, size = shot.statsbomb_xg),
               color = "gold", alpha = 0.9) +
    coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
    scale_y_reverse() +
    theme_pitch(aspect_ratio = NULL) + dark_theme() +
    theme(legend.position = "bottom") +
    labs(title = paste("Χάρτης γκολ σεζόν:", team),
         subtitle = paste("Σεζόν:", season,
                          "| Αγώνες:", n_matches,
                          "| Γκολ:", nrow(df)),
         size = "Αναμενόμενα γκολ (xG)",
         caption = "Δεδομένα: StatsBomb")
}


# ── Δίκτυο μεταβιβάσεων: χρονικό παράθυρο και κατηγορία μήκους ───
# Περιορισμός των γεγονότων της ομάδας στο χρονικό παράθυρο ανάλυσης
.pn_filter_time <- function(team_ev, time_window, custom_range) {
  if (time_window == "first_sub") {
    first_sub <- team_ev %>%
      filter(type.name == "Substitution") %>%
      pull(minute) %>% min(na.rm = TRUE)
    if (!is.infinite(first_sub)) team_ev <- team_ev %>% filter(minute < first_sub)
  } else if (time_window == "custom") {
    team_ev <- team_ev %>% filter(minute >= custom_range[1], minute <= custom_range[2])
  }
  team_ev
}

# Επιλογή των μεταβιβάσεων ανά κατηγορία μήκους (όρια 16,4 και 32,8 μονάδες, δηλαδή 15 m και 30 m)
.pn_filter_pass_type <- function(passes, pass_type) {
  switch(pass_type,
    "short"  = passes %>% filter(pass_dist <  16.4),
    "medium" = passes %>% filter(pass_dist >= 16.4, pass_dist <= 32.8),
    "long"   = passes %>% filter(pass_dist >  32.8),
    passes
  )
}

# ── Δίκτυο μεταβιβάσεων: κατασκευή και απεικόνιση ───────────────
make_pass_network <- function(events, team, subtitle,
                               time_window  = "first_sub",
                               custom_range = c(0, 90),
                               pass_type    = "all",
                               accent_color = "#4fc3f7",
                               show_heatmap = FALSE) {
  team_ev <- events %>%
    filter(team.name == team) %>%
    mutate(location.x          = as.numeric(location.x),
           location.y          = as.numeric(location.y),
           pass.end_location.x = as.numeric(pass.end_location.x),
           pass.end_location.y = as.numeric(pass.end_location.y))

  team_ev <- .pn_filter_time(team_ev, time_window, custom_range)

  nodes <- team_ev %>%
    filter(!is.na(player.name), !is.na(location.x), !is.na(location.y)) %>%
    group_by(player.name) %>%
    summarise(x = mean(location.x), y = mean(location.y), .groups = "drop")

  succ_passes <- team_ev %>%
    filter(type.name == "Pass", is.na(pass.outcome.name),
           !is.na(player.name), !is.na(pass.recipient.name),
           !is.na(pass.end_location.x)) %>%
    mutate(pass_dist = sqrt((pass.end_location.x - location.x)^2 +
                             (pass.end_location.y - location.y)^2))

  succ_passes <- .pn_filter_pass_type(succ_passes, pass_type)
  if (nrow(succ_passes) == 0) return(NULL)

  given    <- succ_passes %>% count(player.name, name = "given")
  received <- succ_passes %>%
    count(pass.recipient.name, name = "received") %>%
    rename(player.name = pass.recipient.name)

  node_involvement <- full_join(given, received, by = "player.name") %>%
    mutate(across(c(given, received), ~replace_na(., 0)),
           involvement = given + received)

  nodes <- nodes %>%
    left_join(node_involvement, by = "player.name") %>%
    mutate(involvement = replace_na(involvement, 0),
           label = sapply(strsplit(player.name, " "), function(x) tail(x, 1)))

  edges_raw <- succ_passes %>%
    mutate(from = pmin(player.name, pass.recipient.name),
           to   = pmax(player.name, pass.recipient.name)) %>%
    count(from, to, name = "passes") %>%
    filter(passes >= 3)

  if (nrow(edges_raw) == 0) return(NULL)

  edges <- edges_raw %>%
    left_join(select(nodes, player.name, x, y), by = c("from" = "player.name")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(select(nodes, player.name, x, y), by = c("to" = "player.name")) %>%
    rename(x_to = x, y_to = y) %>%
    filter(!is.na(x_from), !is.na(x_to))

  if (nrow(edges) == 0) return(NULL)

  caption_txt <- switch(time_window,
    "first_sub" = "Επιτυχημένες μεταβιβάσεις πριν από την πρώτη αλλαγή",
    "full"      = "Επιτυχημένες μεταβιβάσεις | Ολόκληρος ο αγώνας",
    paste0("Επιτυχημένες μεταβιβάσεις | Λεπτά ", custom_range[1], "–", custom_range[2])
  )

  p <- ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#1a2a1a")

  if (isTRUE(show_heatmap) && nrow(succ_passes) >= 5) {
    p <- p +
      geom_density_2d_filled(
        data         = succ_passes,
        aes(x = location.x, y = location.y),
        alpha        = 0.35,
        show.legend  = FALSE
      ) +
      scale_fill_viridis_d(option = "plasma")
  }

  p +
    geom_segment(data = edges,
                 aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                     linewidth = passes),
                 color = accent_color, lineend = "round", alpha = 0.7,
                 show.legend = FALSE) +
    scale_linewidth_continuous(range = c(0.5, 4)) +
    geom_point(data = nodes,
               aes(x = x, y = y, size = involvement),
               color = accent_color, fill = "#1e1e1e", shape = 21,
               stroke = 2, show.legend = FALSE) +
    scale_size_continuous(range = c(3, 12)) +
    geom_text(data = nodes,
              aes(x = x, y = y, label = label),
              color = "white", size = 2.8, fontface = "bold",
              vjust = -1.2, hjust = 0.5) +
    coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
    scale_y_reverse() +
    theme_pitch(aspect_ratio = NULL) + dark_theme() +
    labs(title    = paste("Δίκτυο μεταβιβάσεων:", team),
         subtitle = subtitle,
         caption  = paste("Δεδομένα: StatsBomb |", caption_txt))
}

# ── Δίκτυο μεταβιβάσεων: μετρικές (igraph) ──────────────────────
compute_network_stats <- function(events, team,
                                   time_window  = "first_sub",
                                   custom_range = c(0, 90),
                                   pass_type    = "all") {
  team_ev <- events %>%
    filter(team.name == team) %>%
    mutate(location.x          = as.numeric(location.x),
           location.y          = as.numeric(location.y),
           pass.end_location.x = as.numeric(pass.end_location.x),
           pass.end_location.y = as.numeric(pass.end_location.y))

  team_ev <- .pn_filter_time(team_ev, time_window, custom_range)

  all_passes   <- team_ev %>% filter(type.name == "Pass", !is.na(player.name))
  total_passes <- nrow(all_passes)
  succ_count   <- sum(is.na(all_passes$pass.outcome.name))
  pct_success  <- if (total_passes > 0) round(100 * succ_count / total_passes, 1) else 0

  succ_passes <- all_passes %>%
    filter(is.na(pass.outcome.name), !is.na(pass.recipient.name),
           !is.na(pass.end_location.x)) %>%
    mutate(pass_dist = sqrt((pass.end_location.x - location.x)^2 +
                             (pass.end_location.y - location.y)^2))

  succ_filtered <- .pn_filter_pass_type(succ_passes, pass_type)

  empty <- list(
    betweenness  = data.frame(Παίκτης = character(0), Betweenness = numeric(0)),
    degree       = data.frame(Παίκτης = character(0), Συνδέσεις   = integer(0)),
    key_passer   = "Δεν βρέθηκαν δεδομένα",
    top_pairs    = data.frame(Ζεύγος = character(0), Πάσες = integer(0)),
    total_passes = total_passes,
    pct_success  = pct_success
  )
  if (nrow(succ_filtered) < 2) return(empty)

  # Κατασκευή του γράφου και μετατροπή του σε μη κατευθυνόμενο
  edge_df <- succ_filtered %>%
    count(from = player.name, to = pass.recipient.name, name = "weight")
  g  <- igraph::graph_from_data_frame(edge_df, directed = TRUE)
  gu <- igraph::as.undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum", "ignore"))

  # Κεντρικότητα ενδιαμεσότητας (χωρίς βάρη, ώστε το πλήθος των μεταβιβάσεων να μην ερμηνεύεται ως απόσταση)
  btw    <- igraph::betweenness(gu, normalized = TRUE, weights = NA)
  btw_df <- data.frame(
    Παίκτης     = sapply(strsplit(names(btw), " "), function(x) tail(x, 1)),
    Betweenness = round(btw, 3)
  ) %>% arrange(desc(Betweenness)) %>% head(5)

  # Κεντρικότητα βαθμού
  deg    <- igraph::degree(gu)
  deg_df <- data.frame(
    Παίκτης   = sapply(strsplit(names(deg), " "), function(x) tail(x, 1)),
    Συνδέσεις = as.integer(deg)
  ) %>% arrange(desc(Συνδέσεις)) %>% head(5)

  # Προωθητικές μεταβιβάσεις (πρόοδος ≥ 10 m, δηλαδή ≥ 10,94 μονάδες κατά τον άξονα x)
  prog_df <- succ_passes %>%
    filter((pass.end_location.x - location.x) >= 10.94) %>%
    count(player.name, name = "prog") %>%
    arrange(desc(prog)) %>%
    head(3) %>%
    mutate(label = paste0(
      sapply(strsplit(player.name, " "), function(x) tail(x, 1)),
      " (", prog, ")"
    ))
  key_passer <- if (nrow(prog_df) == 0) "Δεν βρέθηκαν" else
    paste(prog_df$label, collapse = ", ")

  # Τα τρία συχνότερα ζεύγη
  top_pairs <- succ_filtered %>%
    mutate(p1 = pmin(player.name, pass.recipient.name),
           p2 = pmax(player.name, pass.recipient.name)) %>%
    count(p1, p2, name = "Πάσες") %>%
    arrange(desc(Πάσες)) %>%
    head(3) %>%
    mutate(Ζεύγος = paste(
      sapply(strsplit(p1, " "), function(x) tail(x, 1)), "↔",
      sapply(strsplit(p2, " "), function(x) tail(x, 1))
    )) %>%
    select(Ζεύγος, Πάσες)

  list(betweenness = btw_df, degree = deg_df, key_passer = key_passer,
       top_pairs = top_pairs, total_passes = total_passes, pct_success = pct_success)
}

# ── Δίκτυο μεταβιβάσεων: πλαίσιο με τις μετρικές ────────────────
.build_stats_ui <- function(stats) {
  if (is.null(stats))
    return(tags$p(style = "color:#888; padding:15px;", "Φορτώστε πρώτα δεδομένα."))

  make_tbl <- function(df, col_name, col_val, header) {
    if (nrow(df) == 0) {
      rows <- list(tags$tr(tags$td(colspan = "2",
        style = "color:#888; padding:5px 8px; text-align:center;", "—")))
    } else {
      rows <- lapply(seq_len(nrow(df)), function(i)
        tags$tr(
          tags$td(style = "padding:3px 8px; border-bottom:1px solid #3a3a3a;",
                  as.character(df[[col_name]][i])),
          tags$td(style = "padding:3px 8px; border-bottom:1px solid #3a3a3a; text-align:right; font-weight:bold;",
                  as.character(df[[col_val]][i]))
        )
      )
    }
    tags$div(
      tags$div(style = "color:#aaa; font-size:11px; font-weight:bold; text-transform:uppercase; margin-bottom:5px;",
               header),
      tags$table(style = "width:100%; border-collapse:collapse; color:white; font-size:12px;",
        tags$tbody(rows))
    )
  }

  tagList(
    tags$div(
      style = "display:flex; gap:10px; flex-wrap:wrap; margin-bottom:16px;",
      tags$div(
        style = "background:#0d2a1a; border:1px solid #1a5a2a; border-radius:6px; padding:10px 16px; flex:1; min-width:110px; text-align:center;",
        tags$div(style = "color:#888; font-size:10px; text-transform:uppercase;", "Μεταβιβάσεις"),
        tags$div(style = "font-size:26px; font-weight:bold; color:#4fc3f7; line-height:1.2;",
                 as.character(stats$total_passes))
      ),
      tags$div(
        style = "background:#0d2a1a; border:1px solid #1a5a2a; border-radius:6px; padding:10px 16px; flex:1; min-width:110px; text-align:center;",
        tags$div(style = "color:#888; font-size:10px; text-transform:uppercase;", "Ευστοχία"),
        tags$div(style = "font-size:26px; font-weight:bold; color:#2ecc71; line-height:1.2;",
                 paste0(stats$pct_success, "%"))
      )
    ),
    tags$div(
      style = "background:#2a1e00; border-left:3px solid #f39c12; border-radius:0 6px 6px 0; padding:9px 13px; margin-bottom:16px;",
      tags$div(style = "color:#f39c12; font-size:11px; font-weight:bold; text-transform:uppercase; margin-bottom:3px;",
               "Προωθητικές μεταβιβάσεις (≥ 10 m)"),
      tags$div(style = "color:white; font-size:13px;", stats$key_passer)
    ),
    fluidRow(
      column(4, make_tbl(stats$betweenness, "Παίκτης", "Betweenness", "Κεντρικότητα ενδιαμεσότητας")),
      column(4, make_tbl(stats$degree,      "Παίκτης", "Συνδέσεις",   "Κεντρικότητα βαθμού")),
      column(4, make_tbl(stats$top_pairs,   "Ζεύγος",  "Πάσες",       "Συχνότερα ζεύγη"))
    )
  )
}


# ── Διάγραμμα Voronoi: στιγμιότυπο των θέσεων των παικτών ───────
.voronoi_snapshot <- function(events, minute_val, ref_team) {
  snap <- events %>%
    filter(!is.na(location.x), !is.na(location.y), !is.na(player.name)) %>%
    mutate(location.x = as.numeric(location.x),
           location.y = as.numeric(location.y)) %>%
    filter(minute <= minute_val) %>%
    arrange(minute) %>%
    group_by(player.name, team.name) %>%
    slice_tail(n = 1) %>%
    ungroup()

  # Αποχωρήσεις έως το minute_val (από τον πλήρη πίνακα events, όχι τον snap,
  # γιατί τα γεγονότα Substitution δεν έχουν συντεταγμένες)
  subs_out <- events %>%
    filter(type.name == "Substitution", minute <= minute_val) %>%
    pull(player.name)

  card_cols <- intersect(c("foul_committed.card.name", "bad_behaviour.card.name"), names(events))
  sent_off <- character(0)
  for (col in card_cols) {
    sent_off <- c(sent_off, events %>%
      filter(minute <= minute_val, .data[[col]] %in% c("Red Card", "Second Yellow")) %>%
      pull(player.name))
  }

  departed <- unique(c(subs_out, sent_off))
  snap <- snap %>% filter(!(player.name %in% departed))

  # Ομάδα αναφοράς: αν είναι NULL, κενή ή άκυρη, χρησιμοποιείται η πρώτη ομάδα αλφαβητικά
  teams <- sort(unique(snap$team.name))
  ref_team <- if (is.null(ref_team) || !nzchar(ref_team) || !(ref_team %in% teams)) teams[1] else ref_team

  # Μεταφορά των θέσεων της αντίπαλης ομάδας στο σύστημα της ομάδας αναφοράς (στροφή κατά 180°)
  snap <- snap %>%
    mutate(opp        = team.name != ref_team,
           location.x = if_else(opp, 120 - location.x, location.x),
           location.y = if_else(opp,  80 - location.y, location.y))

  snap
}

# ── Διάγραμμα Voronoi: κατασκευή και απεικόνιση ─────────────────
make_voronoi_plot <- function(events, minute_val, ref_team) {
  df <- .voronoi_snapshot(events, minute_val, ref_team) %>%
    distinct(location.x, location.y, .keep_all = TRUE)

  if (nrow(df) < 3) return(NULL)

  pts_sf <- suppressWarnings(
    st_as_sf(df, coords = c("location.x", "location.y"))
  )

  pitch_env_sf <- st_sfc(st_polygon(list(
    matrix(c(-10,-10, 130,-10, 130,90, -10,90, -10,-10), ncol = 2, byrow = TRUE)
  )))
  pitch_clip_poly <- st_polygon(list(
    matrix(c(0,0, 120,0, 120,80, 0,80, 0,0), ncol = 2, byrow = TRUE)
  ))

  tryCatch({
    multipt   <- st_union(pts_sf)
    vor_geom  <- st_voronoi(multipt, envelope = pitch_env_sf)
    vor_polys <- st_sf(geometry = st_collection_extract(vor_geom, "POLYGON"))

    vor_clipped <- suppressWarnings(st_sf(
      geometry = st_intersection(vor_polys$geometry, pitch_clip_poly)
    ))

    nearest_idx <- st_nearest_feature(vor_clipped, pts_sf)
    vor_clipped$player.name <- df$player.name[nearest_idx]
    vor_clipped$team.name   <- df$team.name[nearest_idx]

    vor_df <- do.call(rbind, lapply(seq_len(nrow(vor_clipped)), function(i) {
      co <- st_coordinates(vor_clipped$geometry[i])
      data.frame(x = co[, "X"], y = co[, "Y"], grp = i,
                 player.name = vor_clipped$player.name[i],
                 team.name   = vor_clipped$team.name[i],
                 stringsAsFactors = FALSE)
    }))

    df$short_name <- sapply(strsplit(df$player.name, " "), tail, 1)
    ref_team_resolved <- unique(df$team.name[!df$opp])[1]
    team_colors <- setNames(
      c("#4fc3f7", "#ff9800"),
      c(ref_team_resolved, setdiff(unique(df$team.name), ref_team_resolved))
    )  # χρώματα βάσει ομάδας αναφοράς και αντίπαλης, όχι βάσει της σειράς εμφάνισης

    ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#1a2a1a") +
      geom_polygon(data = vor_df,
                   aes(x = x, y = y, group = grp, fill = team.name),
                   alpha = 0.30, color = "white", linewidth = 0.4) +
      geom_point(data = df,
                 aes(x = location.x, y = location.y, color = team.name),
                 size = 5, shape = 21, fill = "#1e1e1e", stroke = 2.5) +
      geom_text(data = df,
                aes(x = location.x, y = location.y, label = short_name),
                color = "white", size = 2.6, fontface = "bold", vjust = -1.4) +
      scale_fill_manual(values  = team_colors) +
      scale_color_manual(values = team_colors) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_reverse(expand = c(0, 0)) +
      coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
      theme_pitch(aspect_ratio = NULL) + dark_theme() +
      labs(title    = paste("Διάγραμμα Voronoi — Λεπτό:", minute_val),
           subtitle = paste0("Τελευταία γνωστή θέση κάθε παίκτη έως το λεπτό ", minute_val,
                             " | Ομάδα αναφοράς: ", ref_team_resolved, " (επίθεση προς τα δεξιά)"),
           fill     = "Ομάδα",
           color    = "Ομάδα",
           caption  = "Δεδομένα: StatsBomb")
  }, error = function(e) {
    no_data_plot(paste("Σφάλμα στο διάγραμμα Voronoi:", conditionMessage(e)))
  })
}

# ── Κυρτό πολύγωνο: πολύγωνο και εμβαδόν ενός παίκτη ────────────
.get_player_hull <- function(events, player, time_range, ref_team = NULL) {
  df <- events %>%
    filter(player.name == player,
           !is.na(location.x), !is.na(location.y)) %>%
    mutate(location.x = as.numeric(location.x),
           location.y = as.numeric(location.y)) %>%
    filter(minute >= time_range[1], minute <= time_range[2])

  team_name <- if (nrow(df) > 0) df$team.name[1] else NA_character_

  if (!is.null(ref_team) && !is.na(ref_team) && !is.na(team_name) && team_name != ref_team) {
    df <- df %>% mutate(location.x = 120 - location.x,
                        location.y =  80 - location.y)
  }

  if (nrow(df) < 3)
    return(list(points = df, hull_df = NULL, area_m2 = NA,
                sf_poly = NULL, team = team_name, n = nrow(df)))

  # Κορυφές του κυρτού πολυγώνου και κλείσιμο του δακτυλίου
  hidx     <- chull(df$location.x, df$location.y)
  hull_pts <- df[hidx, ]
  hull_pts <- rbind(hull_pts, hull_pts[1, ])

  sf_poly <- tryCatch(
    st_polygon(list(cbind(hull_pts$location.x, hull_pts$location.y))),
    error = function(e) NULL
  )

  # Μετατροπή του εμβαδού από τετραγωνικές μονάδες του πλαισίου (yd²) σε m²: 1 yd² = 0,836127 m²
  area_m2 <- if (!is.null(sf_poly))
    round(as.numeric(st_area(sf_poly)) * 0.836127, 1) else NA

  list(points = df, hull_df = hull_pts, area_m2 = area_m2,
       sf_poly = sf_poly, team = team_name, n = nrow(df))
}

# ── Κυρτό πολύγωνο: απεικόνιση και κοινή επιφάνεια ──────────────
make_hull_result <- function(events, player1, player2, time_range) {
  h1 <- .get_player_hull(events, player1, time_range)

  has_p2 <- !is.null(player2) && nchar(trimws(player2)) > 0 && player2 != player1
  h2     <- if (has_p2) .get_player_hull(events, player2, time_range, ref_team = h1$team) else NULL

  overlap_m2 <- NA_real_
  if (!is.null(h2) && !is.null(h1$sf_poly) && !is.null(h2$sf_poly)) {
    overlap_m2 <- tryCatch({
      inter <- suppressWarnings(
        st_intersection(st_sfc(h1$sf_poly), st_sfc(h2$sf_poly))
      )
      if (length(inter) > 0 && !all(st_is_empty(inter)))
        round(sum(as.numeric(st_area(inter))) * 0.836127, 1)
      else 0
    }, error = function(e) NA_real_)
  }

  col1 <- "#4fc3f7"
  col2 <- "#ff9800"

  p <- ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#1a2a1a") +
    coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
    scale_y_reverse() +
    theme_pitch(aspect_ratio = NULL) + dark_theme()

  if (!is.null(h2) && !is.null(h2$hull_df)) {
    p <- p +
      geom_polygon(data = h2$hull_df,
                   aes(x = location.x, y = location.y),
                   fill = col2, alpha = 0.20, color = col2,
                   linewidth = 1.4, linetype = "dashed") +
      geom_point(data = h2$points,
                 aes(x = location.x, y = location.y),
                 color = col2, alpha = 0.55, size = 1.8, shape = 17)
  }

  if (!is.null(h1$hull_df)) {
    p <- p +
      geom_polygon(data = h1$hull_df,
                   aes(x = location.x, y = location.y),
                   fill = col1, alpha = 0.20, color = col1, linewidth = 1.4) +
      geom_point(data = h1$points,
                 aes(x = location.x, y = location.y),
                 color = col1, alpha = 0.55, size = 1.8)
  }

  title_str <- if (has_p2)
    paste("Κυρτό πολύγωνο:", player1, "–", player2) else
    paste("Κυρτό πολύγωνο:", player1)

  sub_str <- paste0(
    "Λεπτά: ", time_range[1], "–", time_range[2],
    "  |  Γαλάζιο: ", player1,
    if (has_p2) paste0("  |  Πορτοκαλί: ", player2) else "",
    if (has_p2 && isTRUE(h2$team != h1$team))
      " | Θέσεις του παίκτη 2 στο σύστημα αναφοράς της ομάδας του παίκτη 1" else ""
  )

  p <- p + labs(
    title    = title_str,
    subtitle = sub_str,
    caption  = "Δεδομένα: StatsBomb | Εμβαδόν σε m² (1 yd² ≈ 0,836 m²)"
  )

  list(plot = p, h1 = h1, h2 = h2, has_p2 = has_p2, overlap_m2 = overlap_m2)
}

# ════════════════════════════════════════════════════════════════
# ΔΙΕΠΑΦΗ (UI)
# ════════════════════════════════════════════════════════════════

ui <- dashboardPage(
  skin = "black",

  dashboardHeader(title = "Football Spatial Analysis"),

  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Μεταβιβάσεις παίκτη", tabName = "player_pass",    icon = icon("arrow-right")),
      menuItem("Χάρτης σουτ",        tabName = "shot_map",       icon = icon("crosshairs")),
      menuItem("Ενέργειες παίκτη",   tabName = "action_heatmap", icon = icon("map")),
      menuItem("Δίκτυο μεταβιβάσεων", tabName = "pass_network",   icon = icon("share-alt")),
      menuItem("Γκολ σεζόν",         tabName = "goals_map",      icon = icon("futbol")),
      menuItem("Χωρική ανάλυση",     tabName = "spatial_analysis", icon = icon("globe"))
    ),

    hr(),

    selectInput("competition", "Διοργάνωση:", choices = NULL),
    selectInput("season",      "Σεζόν:",      choices = NULL),

    # Επιλογή αγώνα και κουμπί φόρτωσης (αποκρύπτονται στην ενότητα «Γκολ σεζόν»)
    conditionalPanel(
      condition = "input.tabs !== 'goals_map'",
      selectInput("match_choice", "Αγώνας:", choices = NULL, width = "100%"),
      actionButton("load_match", "Φόρτωση αγώνα",
                   class = "btn-primary btn-block",
                   icon  = icon("download"),
                   style = "margin: 5px 15px 10px; width: calc(100% - 30px);")
    ),

    # Κουμπί φόρτωσης σεζόν (εμφανίζεται μόνο στην ενότητα «Γκολ σεζόν»)
    conditionalPanel(
      condition = "input.tabs === 'goals_map'",
      tags$div(style = "padding:8px 15px; color:#f39c12; font-size:11px;",
               icon("exclamation-triangle"),
               " Η φόρτωση σεζόν μπορεί να διαρκέσει αρκετά λεπτά."),
      actionButton("load_season", "Φόρτωση σεζόν",
                   class = "btn-warning btn-block",
                   icon  = icon("clock"),
                   style = "margin: 5px 15px 10px; width: calc(100% - 30px);")
    ),

    hr(),

    selectInput("team", "Ομάδα:", choices = NULL),

    # Επιλογή παίκτη (μόνο στις αναλύσεις παίκτη)
    conditionalPanel(
      condition = "input.tabs === 'player_pass' ||
                   input.tabs === 'action_heatmap' ||
                   (input.tabs === 'shot_map' && input.shot_map_mode === 'player')",
      selectInput("player", "Παίκτης:", choices = NULL)
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side  { background-color: #1a1a1a; }
      .box                           { background-color: #2d2d2d; border: 1px solid #444; }
      .box-header                    { background-color: #333 !important; }
      .box-title                     { color: #ffffff !important; }
      .skin-black .main-header .logo { background-color: #111 !important; }
      .selectize-input               { background-color: #333 !important;
                                       color: #fff !important;
                                       border: 1px solid #555 !important; }
      .selectize-dropdown            { background-color: #333 !important; color: #fff !important; }
      .selectize-dropdown-content .option       { color: #fff !important; }
      .selectize-dropdown-content .option:hover { background-color: #555 !important; }
      label                          { color: #cccccc !important; }
      hr                             { border-color: #444; }
      .sidebar .btn                  { max-width: 100%; overflow: hidden; }
      .sidebar .shiny-input-container { padding-right: 0; }
      table.shiny-table              { width: 100%; border-collapse: collapse; color: #fff; }
      table.shiny-table thead th     { background-color: #444; color: #fff;
                                       padding: 8px 12px; text-align: left;
                                       border-bottom: 2px solid #666; }
      table.shiny-table tbody tr     { border-bottom: 1px solid #444; }
      table.shiny-table tbody tr:hover { background-color: #3a3a3a; }
      table.shiny-table tbody td     { padding: 7px 12px; }
    "))),

    tabItems(

      tabItem("player_pass",
        box(width = 12, solidHeader = TRUE, status = "primary",
            title = "Χάρτης ροής μεταβιβάσεων παίκτη",
            plotOutput("plot_player_pass", height = "auto"))
      ),

      tabItem("shot_map",
        box(width = 12, solidHeader = TRUE, status = "primary",
            title = "Χάρτης σουτ",
            radioButtons("shot_map_mode", "Επίπεδο ανάλυσης:",
                         choices  = c("Ομάδα" = "team", "Παίκτης" = "player"),
                         selected = "team", inline = TRUE),
            plotOutput("plot_shot_map", height = "auto"))
      ),

      tabItem("action_heatmap",
        box(width = 12, solidHeader = TRUE, status = "primary",
            title = "Χάρτης πυκνότητας ενεργειών παίκτη",
            plotOutput("plot_action_heatmap", height = "auto")),
        box(width = 12, solidHeader = TRUE, status = "primary",
            title = "Ενέργειες ανά τύπο",
            tableOutput("action_table"))
      ),

      tabItem("pass_network",

        # ── Ρυθμίσεις ───────────────────────────────────────────────────
        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "info",
              title = "Ρυθμίσεις δικτύου μεταβιβάσεων",
              fluidRow(
                column(3,
                  radioButtons("pn_time_window", "Παράθυρο ανάλυσης:",
                               choices  = c("Μέχρι την πρώτη αλλαγή" = "first_sub",
                                            "Ολόκληρος ο αγώνας"     = "full",
                                            "Προσαρμοσμένο (λεπτά)"  = "custom"),
                               selected = "first_sub")
                ),
                column(3,
                  conditionalPanel(
                    condition = "input.pn_time_window === 'custom'",
                    br(),
                    sliderInput("pn_custom_range", "Εύρος λεπτών:",
                                min = 0, max = 120, value = c(0, 45), step = 1)
                  )
                ),
                column(3,
                  selectInput("pn_pass_type", "Κατηγορία μήκους:",
                              choices  = c("Όλες οι μεταβιβάσεις" = "all",
                                           "Κοντινές (< 15 m)"    = "short",
                                           "Μεσαίες (15–30 m)"    = "medium",
                                           "Μεγάλες (> 30 m)"     = "long"),
                              selected = "all")
                ),
                column(3,
                  tags$div(style = "padding-top:25px;",
                    checkboxInput("pn_compare",
                                  "Σύγκριση των δύο ομάδων",
                                  value = FALSE),
                    checkboxInput("pn_show_heatmap",
                                  "Υπόβαθρο πυκνότητας μεταβιβάσεων",
                                  value = FALSE)
                  )
                )
              )
          )
        ),

        # ── Γραφήματα (δυναμικά) ────────────────────────────────────────
        uiOutput("pn_plots_ui"),

        # ── Μετρικές (δυναμικά) ─────────────────────────────────────────
        uiOutput("pn_stats_ui")
      ),

      tabItem("goals_map",
        box(width = 12, solidHeader = TRUE, status = "warning",
            title = "Χάρτης γκολ σεζόν",
            plotOutput("plot_goals_map", height = "auto"))
      ),

      tabItem("spatial_analysis",

        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "info",
              title = "Ρυθμίσεις χωρικής ανάλυσης",
              fluidRow(
                column(3,
                  radioButtons("spatial_mode", "Τύπος ανάλυσης:",
                               choices  = c("Διάγραμμα Voronoi" = "voronoi",
                                            "Κυρτό πολύγωνο"    = "convex_hull"),
                               selected = "voronoi")
                ),
                column(3,
                  conditionalPanel(
                    condition = "input.spatial_mode === 'voronoi'",
                    sliderInput("voronoi_minute", "Λεπτό στιγμιοτύπου:",
                                min = 0, max = 90, value = 45, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.spatial_mode === 'convex_hull'",
                    sliderInput("hull_time_range", "Χρονικό διάστημα (λεπτά):",
                                min = 0, max = 90, value = c(0, 90), step = 1)
                  )
                ),
                column(3,
                  conditionalPanel(
                    condition = "input.spatial_mode === 'convex_hull'",
                    selectInput("hull_player1", "Παίκτης 1:",
                                choices = NULL, width = "100%"),
                    br(),
                    selectInput("hull_player2", "Παίκτης 2 (προαιρετικό):",
                                choices = c("— Κανένας —" = ""), width = "100%")
                  )
                ),
                column(3,
                  conditionalPanel(
                    condition = "input.spatial_mode === 'voronoi'",
                    tags$div(
                      style = "background:#0d1a2a; border-left:3px solid #4fc3f7; padding:10px 13px; border-radius:0 6px 6px 0; margin-top:5px;",
                      tags$div(style = "color:#4fc3f7; font-size:11px; font-weight:bold; text-transform:uppercase; margin-bottom:5px;",
                               "Διάγραμμα Voronoi"),
                      tags$div(style = "color:#ccc; font-size:12px; line-height:1.5;",
                               "Κάθε κυψέλη δείχνει την περιοχή του γηπέδου η οποία βρίσκεται πλησιέστερα σε κάθε παίκτη, με βάση την τελευταία γνωστή θέση του έως το επιλεγμένο λεπτό. Οι θέσεις της αντίπαλης ομάδας μεταφέρονται στο σύστημα αναφοράς της επιλεγμένης ομάδας.")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.spatial_mode === 'convex_hull'",
                    tags$div(
                      style = "background:#1a0d00; border-left:3px solid #ff9800; padding:10px 13px; border-radius:0 6px 6px 0; margin-top:5px;",
                      tags$div(style = "color:#ff9800; font-size:11px; font-weight:bold; text-transform:uppercase; margin-bottom:5px;",
                               "Κυρτό πολύγωνο"),
                      tags$div(style = "color:#ccc; font-size:12px; line-height:1.5;",
                               "Το μικρότερο κυρτό πολύγωνο το οποίο περιέχει όλες τις ενέργειες του παίκτη στο επιλεγμένο διάστημα. Το εμβαδόν του, σε m², μετρά την έκταση της περιοχής δράσης του.")
                    )
                  )
                )
              )
          )
        ),

        fluidRow(
          box(width = 8, solidHeader = TRUE, status = "primary",
              title = "Χωρική ανάλυση",
              plotOutput("plot_spatial", height = "auto")
          ),
          box(width = 4, solidHeader = TRUE, status = "primary",
              title = "Στατιστικά χώρου",
              uiOutput("spatial_stats_ui")
          )
        )
      )
    )
  )
)


# ════════════════════════════════════════════════════════════════
# ΕΞΥΠΗΡΕΤΗΤΗΣ (SERVER)
# ════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # Δυναμικό ύψος γραφήματος: πλάτος × (80/120) + 90 px για τον τίτλο, τον υπότιτλο και την υποσημείωση
  .ph <- function(id) function() {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    if (is.null(w) || w < 10) return(500)
    round(w * 80 / 120) + 90
  }

  # Τιμές αντίδρασης: γεγονότα αγώνα, γεγονότα σεζόν και κατάλογος αγώνων της σεζόν
  match_events_rv  <- reactiveVal(NULL)
  season_events_rv <- reactiveVal(NULL)
  season_matches_rv <- reactiveVal(NULL)

  # ── Κατάλογος διοργανώσεων (κατά την εκκίνηση) ──────────────────
  comps <- reactive({
    withProgress(message = "Σύνδεση με StatsBomb...", FreeCompetitions())
  })

  observe({
    req(comps())
    updateSelectInput(session, "competition",
                      choices  = sort(unique(comps()$competition_name)),
                      selected = "Champions League")
  })

  # ── Σεζόν της επιλεγμένης διοργάνωσης ───────────────────────────
  observe({
    req(input$competition)
    seasons <- comps() %>%
      filter(competition_name == input$competition) %>%
      pull(season_name) %>% unique() %>% sort(decreasing = TRUE)
    updateSelectInput(session, "season", choices = seasons)
  })

  # ── Αγώνες της επιλεγμένης διοργάνωσης και σεζόν ────────────────
  matches_rv <- reactive({
    req(input$competition, input$season)
    comp_row <- comps() %>%
      filter(competition_name == input$competition,
             season_name      == input$season)
    # Ο υπολογισμός αναστέλλεται όταν η σεζόν δεν ανήκει στην επιλεγμένη διοργάνωση
    # (μεταβατικό διάστημα αμέσως μετά την αλλαγή διοργάνωσης)
    req(nrow(comp_row) > 0)
    tryCatch(
      FreeMatches(comp_row) %>% arrange(match_date),
      error = function(e) {
        msg <- if (grepl("404", conditionMessage(e))) {
          paste0("Τα δεδομένα για «", input$competition,
                 "» δεν είναι διαθέσιμα στο StatsBomb Free Open Data.")
        } else {
          paste("Σφάλμα φόρτωσης αγώνων:", conditionMessage(e))
        }
        showNotification(msg, type = "warning", duration = 12)
        NULL
      }
    )
  })

  match_labels <- reactive({
    req(matches_rv())
    m <- matches_rv()
    tryCatch({
      home <- if ("home_team.home_team_name" %in% names(m)) m$home_team.home_team_name else rep("?", nrow(m))
      away <- if ("away_team.away_team_name" %in% names(m)) m$away_team.away_team_name else rep("?", nrow(m))
      paste(m$match_date, "|", home, "vs", away)
    }, error = function(e) paste("Αγώνας", seq_len(nrow(m))))
  })

  observe({
    req(input$competition, input$season)
    m <- matches_rv()
    if (is.null(m)) {
      updateSelectInput(session, "match_choice", choices = character(0))
      return()
    }
    updateSelectInput(session, "match_choice", choices = match_labels())
  })

  # ── Φόρτωση γεγονότων αγώνα (κουμπί) ────────────────────────────
  observeEvent(input$load_match, {
    req(input$match_choice, matches_rv())
    tryCatch({
      idx <- match(input$match_choice, match_labels())
      withProgress(message = "Ανάκτηση γεγονότων αγώνα...", {
        events <- .fetch_events(matches_rv()[idx, ]) %>%
          allclean() %>% .drop_pre_entry()
        match_events_rv(events)
      })
      teams <- sort(unique(events$team.name))
      updateSelectInput(session, "team", choices = teams)

      # Ενημέρωση των χρονικών ρυθμιστικών και των καταλόγων παικτών του κυρτού πολυγώνου
      max_min <- max(events$minute, na.rm = TRUE)
      updateSliderInput(session, "pn_custom_range",
                        max = max_min, value = c(0, min(45, max_min)))
      updateSliderInput(session, "voronoi_minute",
                        max = max_min, value = min(45, max_min))
      updateSliderInput(session, "hull_time_range",
                        max = max_min, value = c(0, max_min))
      all_pl <- sort(unique(na.omit(events$player.name)))
      updateSelectInput(session, "hull_player1", choices = all_pl,
                        selected = if (length(all_pl) > 0) all_pl[1] else NULL)
      updateSelectInput(session, "hull_player2",
                        choices  = c("— Κανένας —" = "", all_pl),
                        selected = "")
    }, error = function(e) {
      showNotification(
        paste("Σφάλμα φόρτωσης αγώνα:", conditionMessage(e)),
        type = "error", duration = 10
      )
    })
  })

  # ── Παίκτες της επιλεγμένης ομάδας ──────────────────────────────
  observeEvent(input$team, {
    if (is.null(match_events_rv())) return()
    players <- match_events_rv() %>%
      filter(team.name == input$team, !is.na(player.name)) %>%
      pull(player.name) %>% unique() %>% sort()
    updateSelectInput(session, "player", choices = players)
  })

  # ── Φόρτωση γεγονότων σεζόν (κουμπί, χρονοβόρα) ─────────────────
  observeEvent(input$load_season, {
    req(input$competition, input$season)
    tryCatch({
      comp_row <- comps() %>%
        filter(competition_name == input$competition,
               season_name      == input$season)
      s_matches <- withProgress(message = "Φόρτωση αγώνων σεζόν...",
                                FreeMatches(comp_row))
      season_matches_rv(s_matches)
      withProgress(message = "Ανάκτηση γεγονότων σεζόν (χρονοβόρο)...", value = 0, {
        n_m <- nrow(s_matches)
        events <- bind_rows(lapply(seq_len(n_m), function(i) {
          incProgress(1 / n_m, detail = paste("Αγώνας", i, "από", n_m))
          .season_match_events(s_matches[i, ])
        }))
        if (nrow(events) == 0) stop("δεν ανακτήθηκαν γεγονότα για κανέναν αγώνα της σεζόν")
        season_events_rv(events %>% arrange(match_id))
      })
      teams <- sort(unique(season_events_rv()$team.name))
      updateSelectInput(session, "team", choices = teams)
    }, error = function(e) {
      showNotification(
        paste("Σφάλμα φόρτωσης σεζόν:", conditionMessage(e)),
        type = "error", duration = 10
      )
    })
  })


  # ════════════════════════════════════════════════════════════════
  # ΓΡΑΦΗΜΑΤΑ ΚΑΙ ΑΠΟΤΕΛΕΣΜΑΤΑ
  # ════════════════════════════════════════════════════════════════

  output$plot_player_pass <- renderPlot({
    if (is.null(match_events_rv()))
      return(no_data_plot("Επιλέξτε αγώνα και πατήστε «Φόρτωση αγώνα»"))
    req(input$team, input$player)
    p <- make_player_pass_map(match_events_rv(), input$player, input$team, input$match_choice)
    if (is.null(p)) return(no_data_plot(paste(input$player, "— δεν πραγματοποίησε μεταβιβάσεις σε αυτόν τον αγώνα")))
    p
  }, bg = "#1e1e1e", height = .ph("plot_player_pass"))

  output$plot_shot_map <- renderPlot({
    if (is.null(match_events_rv()))
      return(no_data_plot("Επιλέξτε αγώνα και πατήστε «Φόρτωση αγώνα»"))
    req(input$team)
    mode <- input$shot_map_mode %||% "team"
    if (mode == "player") req(input$player)
    p <- make_shot_map(match_events_rv(), mode, input$team, input$player, input$match_choice)
    if (is.null(p)) {
      msg <- if (mode == "player")
        paste(input$player, "— δεν εκτέλεσε σουτ σε αυτόν τον αγώνα") else
        "Η ομάδα δεν εκτέλεσε σουτ σε αυτόν τον αγώνα"
      return(no_data_plot(msg))
    }
    p
  }, bg = "#1e1e1e", height = .ph("plot_shot_map"))

  output$plot_action_heatmap <- renderPlot({
    if (is.null(match_events_rv()))
      return(no_data_plot("Επιλέξτε αγώνα και πατήστε «Φόρτωση αγώνα»"))
    req(input$team, input$player)
    p <- make_action_heatmap(match_events_rv(), input$player, input$match_choice)
    if (is.null(p)) return(no_data_plot(paste(input$player, "— δεν έχει ενέργειες με συντεταγμένες")))
    p
  }, bg = "#1e1e1e", height = .ph("plot_action_heatmap"))

  output$action_table <- renderTable({
    req(match_events_rv(), input$player)
    match_events_rv() %>%
      filter(player.name == input$player,
             !is.na(location.x)) %>%
      count(Ενέργεια = type.name, name = "Πλήθος") %>%
      arrange(desc(Πλήθος))
  }, striped = FALSE, hover = FALSE, bordered = FALSE, spacing = "m",
     width = "100%", align = "lr")

  # ── Δίκτυο μεταβιβάσεων ─────────────────────────────────────────
  pn_team2 <- reactive({
    events <- match_events_rv()
    if (is.null(events) || is.null(input$team) || input$team == "") return(NULL)
    other <- setdiff(unique(events$team.name), input$team)
    if (length(other) == 0) return(NULL)
    other[1]
  })

  pn_params <- reactive({
    list(
      time_window  = input$pn_time_window  %||% "first_sub",
      custom_range = input$pn_custom_range %||% c(0, 90),
      pass_type    = input$pn_pass_type    %||% "all"
    )
  })

  output$pn_plots_ui <- renderUI({
    t1 <- paste("Δίκτυο μεταβιβάσεων:", input$team %||% "—")
    if (isTRUE(input$pn_compare)) {
      t2_name <- pn_team2()
      t2 <- if (!is.null(t2_name)) paste("Δίκτυο μεταβιβάσεων:", t2_name) else "Δίκτυο μεταβιβάσεων: αντίπαλος"
      fluidRow(
        column(6,
          box(width = 12, solidHeader = TRUE, status = "info", title = t1,
              plotOutput("plot_pass_network",   height = "auto"))
        ),
        column(6,
          box(width = 12, solidHeader = TRUE, status = "warning", title = t2,
              plotOutput("plot_pass_network_2", height = "auto"))
        )
      )
    } else {
      fluidRow(
        box(width = 12, solidHeader = TRUE, status = "info", title = t1,
            plotOutput("plot_pass_network", height = "auto"))
      )
    }
  })

  output$plot_pass_network <- renderPlot({
    if (is.null(match_events_rv()))
      return(no_data_plot("Επιλέξτε αγώνα και πατήστε «Φόρτωση αγώνα»"))
    req(input$team)
    p  <- pn_params()
    pl <- make_pass_network(match_events_rv(), input$team, input$match_choice,
                             p$time_window, p$custom_range, p$pass_type, "#4fc3f7",
                             show_heatmap = isTRUE(input$pn_show_heatmap))
    if (is.null(pl)) return(no_data_plot("Δεν υπάρχουν αρκετές μεταβιβάσεις (τουλάχιστον 3 ανά ζεύγος παικτών)"))
    pl
  }, bg = "#1e1e1e", height = .ph("plot_pass_network"))

  output$plot_pass_network_2 <- renderPlot({
    if (is.null(match_events_rv())) return(no_data_plot("Φορτώστε πρώτα αγώνα"))
    team2 <- pn_team2()
    if (is.null(team2)) return(no_data_plot("Δεν βρέθηκε δεύτερη ομάδα"))
    p  <- pn_params()
    pl <- make_pass_network(match_events_rv(), team2, input$match_choice,
                             p$time_window, p$custom_range, p$pass_type, "#ff9800",
                             show_heatmap = isTRUE(input$pn_show_heatmap))
    if (is.null(pl)) return(no_data_plot("Δεν υπάρχουν αρκετές μεταβιβάσεις (τουλάχιστον 3 ανά ζεύγος παικτών)"))
    pl
  }, bg = "#1e1e1e", height = .ph("plot_pass_network_2"))

  output$pn_stats_ui <- renderUI({
    if (is.null(match_events_rv()) || is.null(input$team) || input$team == "") return(NULL)
    if (isTRUE(input$pn_compare)) {
      team2 <- pn_team2()
      if (!is.null(team2)) {
        fluidRow(
          column(6,
            box(width = 12, solidHeader = TRUE, status = "info",
                title = paste("Μετρικές δικτύου:", input$team),
                uiOutput("pn_stats_1"))
          ),
          column(6,
            box(width = 12, solidHeader = TRUE, status = "warning",
                title = paste("Μετρικές δικτύου:", team2),
                uiOutput("pn_stats_2"))
          )
        )
      } else {
        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = paste("Μετρικές δικτύου:", input$team),
              uiOutput("pn_stats_1"))
        )
      }
    } else {
      fluidRow(
        box(width = 12, solidHeader = TRUE, status = "primary",
            title = paste("Μετρικές δικτύου:", input$team),
            uiOutput("pn_stats_1"))
      )
    }
  })

  output$pn_stats_1 <- renderUI({
    req(match_events_rv(), input$team)
    p <- pn_params()
    stats <- compute_network_stats(match_events_rv(), input$team,
                                   p$time_window, p$custom_range, p$pass_type)
    .build_stats_ui(stats)
  })

  output$pn_stats_2 <- renderUI({
    req(match_events_rv())
    team2 <- pn_team2()
    req(team2)
    p <- pn_params()
    stats <- compute_network_stats(match_events_rv(), team2,
                                   p$time_window, p$custom_range, p$pass_type)
    .build_stats_ui(stats)
  })

  # ── Χωρική ανάλυση ──────────────────────────────────────────────

  spatial_hull_data <- reactive({
    req(match_events_rv())
    req(isTRUE(input$spatial_mode == "convex_hull"))
    req(input$hull_player1, nchar(trimws(input$hull_player1)) > 0)
    time_range <- input$hull_time_range %||% c(0, 90)
    player2    <- input$hull_player2    %||% ""
    make_hull_result(match_events_rv(), input$hull_player1, player2, time_range)
  })

  output$plot_spatial <- renderPlot({
    if (is.null(match_events_rv()))
      return(no_data_plot("Επιλέξτε αγώνα και πατήστε «Φόρτωση αγώνα»"))

    mode <- input$spatial_mode %||% "voronoi"

    if (mode == "voronoi") {
      minute_val <- input$voronoi_minute %||% 45
      pl <- make_voronoi_plot(match_events_rv(), minute_val, input$team)
      if (is.null(pl)) return(no_data_plot("Δεν βρέθηκαν αρκετοί παίκτες (χρειάζονται ≥3 με γνωστή θέση)"))
      pl
    } else {
      req(input$hull_player1)
      data <- spatial_hull_data()
      if (is.null(data)) return(no_data_plot("Δεν βρέθηκαν δεδομένα για το κυρτό πολύγωνο"))
      data$plot
    }
  }, bg = "#1e1e1e", height = .ph("plot_spatial"))

  output$spatial_stats_ui <- renderUI({
    if (is.null(match_events_rv()))
      return(tags$p(style = "color:#888; padding:15px;", "Φορτώστε πρώτα αγώνα."))

    mode <- input$spatial_mode %||% "voronoi"

    if (mode == "voronoi") {
      minute_val <- input$voronoi_minute %||% 45
      df_snap <- .voronoi_snapshot(match_events_rv(), minute_val, input$team)

      teams_snap <- unique(df_snap$team.name)
      if (length(teams_snap) > 0) {
        ref_team_snap <- unique(df_snap$team.name[!df_snap$opp])[1]
        other_teams_snap <- setdiff(teams_snap, ref_team_snap)
        pal_snap <- setNames(c("#4fc3f7", rep("#ff9800", length(other_teams_snap))),
                             c(ref_team_snap, other_teams_snap))
      } else {
        pal_snap <- setNames(character(0), character(0))
      }

      tagList(
        tags$div(
          style = "color:#aaa; font-size:11px; font-weight:bold; text-transform:uppercase; margin-bottom:10px;",
          paste("Στιγμιότυπο — λεπτό", minute_val)
        ),
        tags$div(
          style = "background:#0d2a1a; border:1px solid #1a5a2a; border-radius:6px; padding:10px 14px; margin-bottom:10px; text-align:center;",
          tags$div(style = "color:#888; font-size:10px; text-transform:uppercase;",
                   "Παίκτες στο στιγμιότυπο"),
          tags$div(style = "font-size:28px; font-weight:bold; color:#2ecc71;",
                   nrow(df_snap))
        ),
        lapply(seq_along(teams_snap), function(i) {
          t  <- teams_snap[i]
          n  <- sum(df_snap$team.name == t, na.rm = TRUE)
          co <- pal_snap[[t]]
          tags$div(
            style = paste0("background:#111; border-left:3px solid ", co,
                           "; border-radius:0 6px 6px 0; padding:8px 12px; margin-bottom:6px;"),
            tags$div(style = paste0("color:", co, "; font-size:12px; font-weight:bold;"), t),
            tags$div(style = "color:#ccc; font-size:13px;", paste(n, "παίκτες"))
          )
        })
      )

    } else {
      if (is.null(input$hull_player1) || nchar(trimws(input$hull_player1 %||% "")) == 0)
        return(tags$p(style = "color:#888; padding:15px;", "Επιλέξτε παίκτη."))

      data <- spatial_hull_data()
      if (is.null(data))
        return(tags$p(style = "color:#888; padding:15px;", "Φορτώστε πρώτα αγώνα."))

      stat_card <- function(lbl, val, col = "#4fc3f7") {
        tags$div(
          style = "background:#0d2a1a; border:1px solid #1a5a2a; border-radius:6px; padding:9px 12px; margin-bottom:7px; text-align:center;",
          tags$div(style = "color:#888; font-size:10px; text-transform:uppercase;", lbl),
          tags$div(style = paste0("font-size:22px; font-weight:bold; color:", col, "; line-height:1.2;"), val)
        )
      }

      area_fmt <- function(x) if (!is.na(x)) paste0(x, " m²") else "< 3 σημεία"

      tagList(
        tags$div(style = "color:#4fc3f7; font-size:12px; font-weight:bold; margin-bottom:6px;",
                 paste0(if (is.null(data$h1$team) || is.na(data$h1$team)) "—" else data$h1$team, " — ", data$h1$n, " ενέργειες")),
        stat_card("Εμβαδόν", area_fmt(data$h1$area_m2), "#4fc3f7"),
        stat_card("Ενέργειες",   as.character(data$h1$n),    "#4fc3f7"),

        if (isTRUE(data$has_p2) && !is.null(data$h2)) {
          tagList(
            tags$hr(style = "border-color:#444; margin:10px 0;"),
            tags$div(style = "color:#ff9800; font-size:12px; font-weight:bold; margin-bottom:6px;",
                     paste0(if (is.null(data$h2$team) || is.na(data$h2$team)) "—" else data$h2$team, " — ", data$h2$n, " ενέργειες")),
            stat_card("Εμβαδόν", area_fmt(data$h2$area_m2), "#ff9800"),
            stat_card("Ενέργειες",   as.character(data$h2$n),    "#ff9800"),

            tags$hr(style = "border-color:#444; margin:10px 0;"),
            tags$div(style = "color:#2ecc71; font-size:11px; font-weight:bold; text-transform:uppercase; margin-bottom:6px;",
                     "Επικάλυψη"),
            stat_card(
              "Κοινή επιφάνεια",
              if (!is.na(data$overlap_m2)) paste0(data$overlap_m2, " m²") else "—",
              "#2ecc71"
            )
          )
        }
      )
    }
  })

  output$plot_goals_map <- renderPlot({
    if (is.null(season_events_rv()))
      return(no_data_plot("Επιλέξτε διοργάνωση και σεζόν και πατήστε «Φόρτωση σεζόν»"))
    req(input$team)
    p <- make_goals_map(season_events_rv(), season_matches_rv(), input$team, input$season)
    if (is.null(p)) return(no_data_plot("Δεν βρέθηκαν γκολ της ομάδας στη σεζόν"))
    p
  }, bg = "#1e1e1e", height = .ph("plot_goals_map"))
}

shinyApp(ui, server)
