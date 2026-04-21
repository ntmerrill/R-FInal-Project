# =============================================================================
# Stephen Curry Postseason Stats Analysis
# =============================================================================
# This script ingests, cleans, manipulates, analyzes, and visualizes
# Stephen Curry's NBA postseason game-by-game stats across 9 playoff seasons.
# =============================================================================

# ---- 0. Install & Load Required Packages ------------------------------------

required_packages <- c("dplyr", "ggplot2", "tidyr", "scales", "ggthemes")
installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg, repos = "https://cran.r-project.org")
}

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(ggthemes)

# ---- 1. INGEST DATA ---------------------------------------------------------

# Read the CSV file. Strings kept as character for cleaning.
df_raw <- read.csv("Stephen_Curry_Postseason_Stats.csv",
                   stringsAsFactors = FALSE,
                   na.strings = c("", "NA", "N/A"))

cat("=== RAW DATA OVERVIEW ===\n")
cat("Rows:", nrow(df_raw), "| Columns:", ncol(df_raw), "\n")
cat("Seasons covered:", paste(unique(df_raw$Season_year), collapse = ", "), "\n\n")

# ---- 2. DATA CLEANING -------------------------------------------------------

df <- df_raw

# 2a. Remove redundant composite shooting columns (FG, 3PT, FT are "made-att"
#     strings; the individual FGM/FGA/3PTM/3PTA/FTM/FTA cols already exist)
df <- df %>% select(-FG, -X3PT, -FT)

# 2b. Fix column names that may have been read with X prefix for 3PT columns
#     (read.csv converts leading digits/special chars)
df <- df %>%
  rename_with(~ gsub("^X3", "3", .x))   # restore "3P%" etc.

# 2c. Convert Result to a logical Win flag (W = TRUE, L = FALSE)
df <- df %>%
  mutate(Win = ifelse(Result == "W", TRUE, FALSE))

# 2d. Ensure numeric types for shooting percentages and minutes
#     (they should already be numeric but verify)
numeric_cols <- c("MIN", "FG.", "X3P.", "FT.", "REB", "AST", "BLK",
                  "STL", "PF", "TO", "PTS", "FGM", "FGA",
                  "X3PTM", "X3PTA", "FTM", "FTA")
# Use actual column names present after cleaning
num_check <- intersect(numeric_cols, colnames(df))
for (col in num_check) {
  df[[col]] <- as.numeric(df[[col]])
}

# 2e. Parse Season_year into a clean integer start year for ordering
df <- df %>%
  mutate(Season_start = as.integer(substr(Season_year, 1, 4)))

# 2f. Check for and remove exact duplicate rows
dup_count <- sum(duplicated(df))
cat(sprintf("Duplicate rows found and removed: %d\n", dup_count))
df <- df %>% distinct()

# 2g. Verify no missing values in key numeric columns
cat("Missing values in key columns:\n")
key_cols <- c("PTS", "REB", "AST", "MIN", "FGM", "FGA", "X3PTM", "X3PTA")
print(colSums(is.na(df[, intersect(key_cols, colnames(df))])))

cat("\n=== CLEANED DATA READY ===\n")
cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n\n")

# ---- 3. DATA MANIPULATION (dplyr) -------------------------------------------

# 3a. FILTERING — Restrict to games where Curry played at least 30 minutes
#     Purpose: isolate meaningful starter-level performances
df_30min <- df %>%
  filter(MIN >= 30)
cat(sprintf("Games with 30+ minutes played: %d / %d\n",
            nrow(df_30min), nrow(df)))

# 3b. ARRANGING — Sort by points descending to find top performances
df_top_games <- df %>%
  arrange(desc(PTS)) %>%
  select(Season_year, Date, OPP, Result, MIN, PTS, X3PTM, REB, AST)
cat("\nTop 5 scoring games:\n")
print(head(df_top_games, 5))

# 3c. SELECTING — Slim dataset to core box-score columns for analysis
df_core <- df %>%
  select(Season_year, Season_start, Date, OPP, Result, Win,
         MIN, PTS, FGM, FGA, FG., X3PTM, X3PTA, X3P.,
         FTM, FTA, FT., REB, AST, STL, BLK, TO, PF)

# 3d. MUTATING — Derive new analytical columns
df_core <- df_core %>%
  mutate(
    # Points per minute (efficiency measure)
    PTS_per_MIN = round(PTS / MIN, 3),
    # 3-point attempt rate: share of FGA that are 3s
    ThreePT_rate = round(X3PTA / FGA, 3),
    # True Shooting % = PTS / (2 * (FGA + 0.44 * FTA))
    TS_pct = round(PTS / (2 * (FGA + 0.44 * FTA)), 3),
    # Score margin (positive = Curry's team winning)
    Margin = T.Score - O.Score
  )

# 3e. GROUPED SUMMARIES — Season-level averages
season_summary <- df_core %>%
  group_by(Season_year, Season_start) %>%
  summarise(
    Games       = n(),
    Wins        = sum(Win),
    Avg_PTS     = round(mean(PTS), 1),
    Avg_REB     = round(mean(REB), 1),
    Avg_AST     = round(mean(AST), 1),
    Avg_3PTM    = round(mean(X3PTM), 1),
    Avg_3P_pct  = round(mean(X3P., na.rm = TRUE), 1),
    Avg_TS_pct  = round(mean(TS_pct, na.rm = TRUE), 3),
    Avg_MIN     = round(mean(MIN), 1),
    .groups = "drop"
  ) %>%
  arrange(Season_start)

cat("\n=== SEASON-BY-SEASON POSTSEASON AVERAGES ===\n")
print(season_summary)

# ---- 4. STATISTICAL ANALYSIS ------------------------------------------------

cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# 4a. Descriptive stats — Points
pts_stats <- df_core %>%
  summarise(
    N          = n(),
    Mean_PTS   = round(mean(PTS), 2),
    Median_PTS = median(PTS),
    SD_PTS     = round(sd(PTS), 2),
    Min_PTS    = min(PTS),
    Max_PTS    = max(PTS),
    Q1_PTS     = quantile(PTS, 0.25),
    Q3_PTS     = quantile(PTS, 0.75)
  )
cat("Points per game distribution:\n")
print(pts_stats)

# 4b. Descriptive stats — True Shooting %
ts_stats <- df_core %>%
  filter(!is.na(TS_pct)) %>%
  summarise(
    Mean_TS   = round(mean(TS_pct), 3),
    Median_TS = round(median(TS_pct), 3),
    SD_TS     = round(sd(TS_pct), 3),
    Min_TS    = round(min(TS_pct), 3),
    Max_TS    = round(max(TS_pct), 3)
  )
cat("\nTrue Shooting % distribution:\n")
print(ts_stats)

# 4c. Mode helper (R has no built-in mode)
calc_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
cat(sprintf("\nMode of Points scored: %d\n", calc_mode(df_core$PTS)))
cat(sprintf("Mode of 3-pointers made: %d\n", calc_mode(df_core$X3PTM)))

# ---- Hypothesis Test --------------------------------------------------------
# Q: Does Curry score significantly MORE points in WINS than LOSSES?
# H0: mean PTS in wins == mean PTS in losses
# H1: mean PTS in wins  > mean PTS in losses (one-tailed t-test)

wins_pts   <- df_core$PTS[df_core$Win == TRUE]
losses_pts <- df_core$PTS[df_core$Win == FALSE]

cat("\n=== HYPOTHESIS TEST: Points in Wins vs. Losses ===\n")
cat(sprintf("Wins   — N: %d | Mean: %.1f | SD: %.1f\n",
            length(wins_pts), mean(wins_pts), sd(wins_pts)))
cat(sprintf("Losses — N: %d | Mean: %.1f | SD: %.1f\n",
            length(losses_pts), mean(losses_pts), sd(losses_pts)))

t_result <- t.test(wins_pts, losses_pts,
                   alternative = "greater",   # one-tailed: wins > losses
                   var.equal   = FALSE)        # Welch's t-test (safer default)
cat(sprintf("\nt = %.3f, df = %.1f, p-value = %.4f\n",
            t_result$statistic, t_result$parameter, t_result$p.value))
if (t_result$p.value < 0.05) {
  cat("Conclusion: Reject H0. Curry scores significantly more points in WINS (p < 0.05).\n")
} else {
  cat("Conclusion: Fail to reject H0. No significant difference in scoring by outcome.\n")
}

# ---- Linear Regression ------------------------------------------------------
# Q: Does higher 3-point volume (3PTA) predict more total points scored?
# Model: PTS ~ X3PTA

cat("\n=== LINEAR REGRESSION: PTS ~ 3-Point Attempts ===\n")
lm_model <- lm(PTS ~ X3PTA, data = df_core)
lm_sum   <- summary(lm_model)
print(lm_sum)

cat(sprintf(
  "\nInterpretation: For each additional 3-point attempt, Curry scores approximately %.2f more points.\n",
  coef(lm_model)[["X3PTA"]]
))
cat(sprintf("R² = %.3f — 3PA explains %.1f%% of variance in points scored.\n",
            lm_sum$r.squared, lm_sum$r.squared * 100))

# ---- 5. VISUALIZATIONS (ggplot2) --------------------------------------------

# Common theme for all plots
curry_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, hjust = 0.5,
                                 margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40",
                                 margin = margin(b = 10)),
    plot.caption  = element_text(color = "grey55", size = 9, hjust = 1),
    axis.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

GSW_GOLD  <- "#FFC72C"
GSW_BLUE  <- "#1D428A"
GSW_GRAY  <- "#BAC3C9"

# ---- Plot 1: Avg Points Per Postseason Season (bar chart) -------------------
p1 <- ggplot(season_summary,
             aes(x = reorder(Season_year, Season_start), y = Avg_PTS)) +
  geom_col(fill = GSW_GOLD, color = GSW_BLUE, width = 0.7) +
  geom_text(aes(label = Avg_PTS), vjust = -0.4, fontface = "bold",
            color = GSW_BLUE, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Stephen Curry — Average Postseason Points by Season",
    subtitle = "All NBA Playoff Seasons (2013–2023)",
    x        = "Season",
    y        = "Average Points Per Game",
    caption  = "Source: Stephen_Curry_Postseason_Stats.csv"
  ) +
  curry_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("visualizations/plot1_avg_pts_by_season.png", p1,
       width = 10, height = 6, dpi = 150)
cat("\nPlot 1 saved.\n")

# ---- Plot 2: Points Distribution — Wins vs. Losses (density/violin) ---------
p2 <- df_core %>%
  mutate(Outcome = ifelse(Win, "Win", "Loss")) %>%
  ggplot(aes(x = Outcome, y = PTS, fill = Outcome)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = 21, outlier.fill = "white",
               outlier.size = 2, color = "grey30") +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 4, fill = "white", color = "black") +
  scale_fill_manual(values = c("Win" = GSW_GOLD, "Loss" = GSW_BLUE),
                    guide = "none") +
  labs(
    title    = "Curry's Points Distribution: Wins vs. Losses",
    subtitle = "Violin + Box plot  |  Diamond = mean",
    x        = "Game Outcome",
    y        = "Points",
    caption  = "Source: Stephen_Curry_Postseason_Stats.csv"
  ) +
  curry_theme

ggsave("visualizations/plot2_pts_win_vs_loss.png", p2,
       width = 8, height = 6, dpi = 150)
cat("Plot 2 saved.\n")

# ---- Plot 3: 3-PT Attempts vs. Points (scatter + regression line) -----------
p3 <- ggplot(df_core, aes(x = X3PTA, y = PTS)) +
  geom_point(aes(color = ifelse(Win, "Win", "Loss")), alpha = 0.65, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = GSW_BLUE,
              fill = GSW_GRAY, linewidth = 1.1) +
  scale_color_manual(name = "Outcome",
                     values = c("Win" = GSW_GOLD, "Loss" = "tomato3")) +
  labs(
    title    = "3-Point Attempts vs. Points Scored",
    subtitle = "Linear regression with 95% confidence band",
    x        = "3-Point Attempts",
    y        = "Points",
    caption  = "Source: Stephen_Curry_Postseason_Stats.csv"
  ) +
  curry_theme

ggsave("visualizations/plot3_3pta_vs_pts_regression.png", p3,
       width = 9, height = 6, dpi = 150)
cat("Plot 3 saved.\n")

# ---- Plot 4 (Bonus): Shooting Efficiency Over Seasons (TS%) line chart ------
p4 <- ggplot(season_summary, aes(x = reorder(Season_year, Season_start),
                                  y = Avg_TS_pct, group = 1)) +
  geom_line(color = GSW_BLUE, linewidth = 1.3) +
  geom_point(size = 4, shape = 21, fill = GSW_GOLD, color = GSW_BLUE,
             stroke = 1.5) +
  geom_text(aes(label = scales::percent(Avg_TS_pct, accuracy = 0.1)),
            vjust = -1.1, fontface = "bold", color = GSW_BLUE, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.45, 0.85)) +
  labs(
    title    = "Curry's True Shooting % — Postseason Trend",
    subtitle = "TS% = PTS / [2 × (FGA + 0.44 × FTA)]",
    x        = "Season",
    y        = "True Shooting %",
    caption  = "Source: Stephen_Curry_Postseason_Stats.csv"
  ) +
  curry_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("visualizations/plot4_ts_pct_trend.png", p4,
       width = 10, height = 6, dpi = 150)
cat("Plot 4 saved.\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All 4 visualizations saved to visualizations/\n")
