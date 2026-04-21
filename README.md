# Stephen Curry Postseason Statistical Analysis

Used CHat gpt and CLaude for some assistance


## Overview

This project analyzes Stephen Curry's NBA postseason game-by-game performance across **9 playoff seasons** (2013–2023), covering **147 individual games**. The analysis covers data ingestion and cleaning, dplyr-style manipulation, descriptive statistics, hypothesis testing, linear regression, and four ggplot2-style visualizations.

**Script:** `curry_analysis.R`  
**Dataset:** `Stephen_Curry_Postseason_Stats.csv`  
**Visualizations:** `visualizations/` folder (4 PNG files)

---

## Dataset Description

| Column | Description |
|--------|-------------|
| `Season_year` | Playoff season (e.g., 2014-2015) |
| `Date` | Game date |
| `OPP` | Opponent abbreviation |
| `Result` | W / L |
| `MIN` | Minutes played |
| `PTS` | Points scored |
| `FGM / FGA / FG%` | Field goals made, attempted, pct |
| `3PTM / 3PTA / 3P%` | 3-point made, attempted, pct |
| `FTM / FTA / FT%` | Free throws made, attempted, pct |
| `REB / AST / STL / BLK` | Rebounds, assists, steals, blocks |
| `TO / PF` | Turnovers, personal fouls |

---

## How to Run

```r
# Set your working directory to the project folder, then:
source("curry_analysis.R")
```

**Required packages** (auto-installed by the script if missing):
`dplyr`, `ggplot2`, `tidyr`, `scales`, `ggthemes`

---

## 1. Data Ingestion

```r
df_raw <- read.csv("Stephen_Curry_Postseason_Stats.csv",
                   stringsAsFactors = FALSE)
```

The raw CSV contains 147 rows × 27 columns spanning 9 playoff seasons.

---

## 2. Data Cleaning

| Step | Action | Rationale |
|------|--------|-----------|
| Drop composite columns | Remove `FG`, `3PT`, `FT` (e.g., "10-25") | Redundant; individual made/attempted columns already present |
| Add `Win` flag | `Win = (Result == "W")` | Logical column for win/loss comparisons |
| Add `Season_start` | Integer year from `Season_year` | Enables chronological ordering |
| Type coercion | Ensure numeric for all stat columns | Guards against string-parsing surprises |
| Deduplication | `distinct()` | 0 exact duplicates found in this dataset |
| Derived stats | `TS_pct`, `PTS_per_MIN`, `ThreePT_rate`, `Margin` | New analytical features |

**Missing values:** None detected in any key column.

---

## 3. Data Manipulation (dplyr)

### 3a. Filtering — 30+ Minute Games
```r
df_30min <- df %>% filter(MIN >= 30)
```
Isolates full-starter performances (excludes blowout garbage-time minutes).

### 3b. Arranging — Top Scoring Games
```r
df_top_games <- df %>% arrange(desc(PTS))
```
Surfaces Curry's highest-impact individual performances.

### 3c. Selecting — Core Box-Score Columns
```r
df_core <- df %>% select(Season_year, Season_start, Date, OPP, Result,
                          Win, MIN, PTS, FGM, FGA, FG., ...)
```
Reduces the working frame to the analytically relevant columns.

### 3d. Mutating — Derived Features
```r
df_core <- df_core %>% mutate(
  PTS_per_MIN  = PTS / MIN,
  ThreePT_rate = X3PTA / FGA,
  TS_pct       = PTS / (2 * (FGA + 0.44 * FTA)),
  Margin       = T.Score - O.Score
)
```

### 3e. Grouped Summaries — Per-Season Averages
```r
season_summary <- df_core %>%
  group_by(Season_year, Season_start) %>%
  summarise(Avg_PTS = mean(PTS), Avg_3PTM = mean(X3PTM), ...)
```

| Season | Games | Wins | Avg PTS | Avg REB | Avg AST | Avg 3PM | Avg 3P% |
|--------|-------|------|---------|---------|---------|---------|---------|
| 2012-13 | 12 | 6 | 23.4 | 3.8 | 8.1 | 3.5 | 38.1% |
| 2013-14 | 7  | 3 | 23.0 | 3.6 | 8.4 | 3.1 | 37.2% |
| 2014-15 | 21 | 16 | 28.3 | 5.0 | 6.4 | 4.7 | 43.2% |
| 2015-16 | 18 | 11 | 25.1 | 5.5 | 5.2 | 4.4 | 40.9% |
| 2016-17 | 17 | 16 | 28.1 | 6.2 | 6.7 | 4.2 | 41.6% |
| 2017-18 | 15 | 11 | 25.5 | 6.1 | 5.4 | 4.3 | 37.4% |
| 2018-19 | 22 | 14 | 28.2 | 6.0 | 5.7 | 4.2 | 38.6% |
| 2021-22 | 22 | 16 | 27.4 | 5.2 | 5.9 | 4.1 | 39.3% |
| 2022-23 | 13 | 6  | 30.5 | 5.2 | 6.1 | 4.4 | 37.4% |

---

## 4. Statistical Analysis

### 4a. Descriptive Statistics — Points Per Game

| Statistic | Value |
|-----------|-------|
| N | 147 |
| Mean | **27.0 pts** |
| Median | **28.0 pts** |
| Mode | **29 pts** |
| Std Dev | 7.95 |
| Min | 6 pts |
| Max | 50 pts |
| Q1 / Q3 | 22.0 / 32.5 |

### 4b. Descriptive Statistics — True Shooting %

| Statistic | Value |
|-----------|-------|
| Mean TS% | **60.8%** |
| Median TS% | 61.3% |
| Std Dev | 12.6% |
| Min TS% | 31.0% |
| Max TS% | 95.2% |

*TS% above 60% is considered elite efficiency; Curry's mean clears this benchmark across all postseasons.*

### 4c. Hypothesis Test — Do Wins Correlate with Higher Scoring?

**Question:** Does Curry score significantly more points in wins versus losses?

- **H₀:** Mean PTS in wins = Mean PTS in losses  
- **H₁:** Mean PTS in wins > Mean PTS in losses (one-tailed)  
- **Test:** Welch's independent-samples t-test (`var.equal = FALSE`)

| Group | N | Mean PTS | SD |
|-------|---|----------|----|
| Wins  | 99 | 28.2 | 7.9 |
| Losses | 48 | 24.4 | 7.6 |

> **t = 2.796, p = 0.0031**

**Conclusion:** Reject H₀ at α = 0.05. Curry scores significantly more points in wins than losses (p < 0.01). The ~3.8-point gap suggests his scoring output is meaningfully tied to team outcomes.

### 4d. Linear Regression — PTS ~ 3-Point Attempts

**Question:** Does attempting more 3-pointers predict higher point totals?

```r
lm_model <- lm(PTS ~ X3PTA, data = df_core)
```

| Term | Estimate | Std Error | p-value |
|------|----------|-----------|---------|
| Intercept | 10.985 | 1.39 | < 0.001 |
| 3PTA (slope) | 1.510 | 0.14 | < 0.001 |

> **R² = 0.330** — 3-point attempts explain **33% of variance** in points scored.

**Interpretation:** For each additional 3-point attempt, Curry scores approximately **1.51 more points**. This strong, significant relationship (p < 0.001) confirms that Curry's 3-point volume is a primary engine of his scoring.

---

## 5. Visualizations

### Plot 1 — Average Points by Season
![Avg PTS by Season](visualizations/plot1_avg_pts_by_season.png)

**Chart type:** Bar chart  
**Insight:** Curry's scoring has trended upward across his playoff career. His 2022-23 season (30.5 ppg) represents a career postseason high, and seasons with championship runs (2014-15, 2016-17, 2021-22) all show 27+ ppg averages.

---

### Plot 2 — Points Distribution: Wins vs. Losses
![Wins vs Losses](visualizations/plot2_pts_win_vs_loss.png)

**Chart type:** Violin + box plot  
**Insight:** The distribution of points in wins is noticeably higher and more concentrated in the 25–35 range, while losses show a wider, lower-centered spread. The t-test confirms this ~3.8-point gap is statistically significant (p = 0.003).

---

### Plot 3 — 3-Point Attempts vs. Points (Regression)
![Regression](visualizations/plot3_3pta_vs_pts_regression.png)

**Chart type:** Scatter plot with regression line and 95% CI  
**Insight:** A clear positive linear relationship exists between 3-point attempts and points scored. The regression equation (y = 1.51x + 10.99, R² = 0.33) shows that Curry's 3-point volume is a primary driver of his scoring output. High-scoring games cluster in the upper-right.

---

### Plot 4 — True Shooting % Trend
![TS% Trend](visualizations/plot4_ts_pct_trend.png)

**Chart type:** Line chart  
**Insight:** Curry's efficiency dipped in 2013-14 (his smallest playoff sample) and peaked in the championship seasons. His ability to maintain 58–64% TS% across a decade of postseasons underscores his elite shot quality and selection.

---

## Key Takeaways

1. **Elite and improving scorer:** Curry averages **27.0 points** per playoff game overall, with his most recent seasons (2021-22, 2022-23) being his highest-volume.
2. **Scoring drives winning:** His output in wins (28.2 ppg) is significantly higher than in losses (24.4 ppg), a statistically confirmed gap (p = 0.003).
3. **3-point volume = points:** Each additional 3-point attempt translates to ~1.5 more points (R² = 0.33), validating the Golden State offense's strategy of feeding Curry three-point opportunities.
4. **Sustained efficiency:** A career postseason TS% of 60.8% places him among the most efficient high-volume scorers in playoff history.
