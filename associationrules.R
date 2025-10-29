# --- Packages ---
suppressPackageStartupMessages({
  library(arules)
  library(dplyr)
})

# --- Load your .RData (adjust path if needed) ---
load("~/data mining/progetto/df_outliers.RData")

# --- Pick the first data.frame found (replace with the known name if you have it) ---
objs <- mget(ls(), inherits = TRUE)
df_name <- NA
for (nm in names(objs)) {
  if (is.data.frame(objs[[nm]])) { df_name <- nm; break }
}
if (is.na(df_name)) stop("No data.frame found in the loaded workspace.")
df <- objs[[df_name]]
message("Using data.frame: ", df_name)

# --- Quick sanity checks ---
cat("\nDimensions (rows, cols):\n"); print(dim(df))
cat("\nColumn names:\n"); print(names(df))
cat("\nStructure (truncated):\n"); str(df, give.attr = FALSE, strict.width = "cut")

cat("\nSummary (first 10 columns if many):\n")
show_cols <- head(names(df), 10)
print(summary(df[show_cols]))

# Optional: peek at missingness
cat("\nMissing values per column (head):\n")
print(head(sort(colSums(is.na(df)), decreasing = TRUE)))


# --- STEP 2: Prepare data for association rules ---

# 2.1 Keep TRAIN rows only (test has NA in song_popularity)
df_tr <- df %>% dplyr::filter(dataset == "train")

# 2.2 Drop identifier / non-feature columns
# (We keep 'is_outlier' for now; we can remove it later if needed.)
drop_cols <- c("global_ID", "ID", "dataset")
df_tr <- df_tr[, setdiff(names(df_tr), drop_cols), drop = FALSE]

# 2.3 Make a working copy
dd <- df_tr

# 2.4 Discretize target 'song_popularity' into 3 classes (quantile-based)
y_col <- "song_popularity"
dd[[y_col]] <- arules::discretize(dd[[y_col]],
                                  method = "frequency",
                                  categories = 3,
                                  labels = c("Low","Medium","High"))

# 2.5 Discretize all OTHER numeric features into 3 bins (Low/Medium/High)
is_num <- sapply(dd, is.numeric)
num_others <- setdiff(names(dd)[is_num], y_col)

for (nm in num_others) {
  v <- dd[[nm]]
  # If too few unique numeric values, just coerce to factor
  if (length(na.omit(unique(v))) >= 5) {
    dd[[nm]] <- tryCatch(
      arules::discretize(v, method = "frequency", categories = 3,
                         labels = c("Low","Medium","High")),
      error = function(e) {
        arules::discretize(v, method = "interval", categories = 3,
                           labels = c("Low","Medium","High"))
      }
    )
  } else {
    dd[[nm]] <- factor(v)
  }
}

# 2.6 Ensure ALL columns are factors (including already-categorical ones)
for (nm in names(dd)) if (!is.factor(dd[[nm]])) dd[[nm]] <- factor(dd[[nm]])

# 2.7 Convert factor levels into explicit "col=value" item labels
make_item <- function(xname, f) factor(paste0(xname, "=", as.character(f)))
for (nm in names(dd)) dd[[nm]] <- make_item(nm, dd[[nm]])

# --- Quick checks we need for the next step ---
cat("\nRows x Cols (TRAIN only, post-discretization):\n"); print(dim(dd))

cat("\nTarget levels (should be 'song_popularity=Low/Medium/High'):\n")
print(levels(dd[[y_col]])[1:3])

cat("\nClass balance of target:\n")
print(table(dd[[y_col]]))

cat("\nSample rows (first 3):\n")
print(head(dd, 3))

# (We will build transactions in Step 3)

# --- STEP 3: Build transactions and explore items ---

# 3.1 Convert to transactions
trans <- as(dd, "transactions")

# 3.2 Basic information
cat("\nTransaction summary:\n")
summary(trans)

# 3.3 Most frequent items (top 15)
cat("\nTop 15 most frequent items:\n")
itemFrequencyPlot(trans, topN = 15, type = "absolute", main = "Top 15 Items (Absolute Frequency)")

# 3.4 Check that 'song_popularity=High' exists in item labels
cat("\nConfirm target item presence:\n")
print(any(grepl("song_popularity=High", itemLabels(trans))))


# ---- STEP 4: Generate association rules for High popularity ----

# 4.1 Generate rules with moderate thresholds
rules_all <- apriori(
  trans,
  parameter = list(supp = 0.02, conf = 0.6, maxlen = 4, target = "rules")
)

cat("\nTotal number of rules generated:", length(rules_all), "\n")

# 4.2 Filter only rules that predict high popularity
rules_pop_high <- subset(rules_all, rhs %in% "song_popularity=song_popularity=High")

cat("\nRules with RHS = song_popularity=High:\n")
print(length(rules_pop_high))

# 4.3 Show top 10 by lift
inspect(head(sort(rules_pop_high, by = "lift"), 10))

# --- STEP 4b: Check available popularity labels ---
grep("song_popularity", itemLabels(trans), value = TRUE)


# --- STEP 5: Rules with RHS fixed to High popularity ---

# exact RHS label from your grep() output
target_high <- "song_popularity=song_popularity=High"

# 5.1 Mine rules with RHS fixed to target_high, and prevent popularity from appearing on the LHS
rules_high <- apriori(
  trans,
  parameter = list(supp = 0.01, conf = 0.6, maxlen = 4, target = "rules"),
  appearance = list(default = "lhs", rhs = target_high),
  control = list(verbose = TRUE)
)

cat("\nTotal rules with RHS = High:\n"); print(length(rules_high))
cat("\nSummary of rules_high:\n"); print(summary(rules_high))

# 5.2 Top rules by lift and by confidence (first 10)
cat("\nTop 10 by lift:\n")
inspect(head(sort(rules_high, by = "lift"), 10))

cat("\nTop 10 by confidence:\n")
inspect(head(sort(rules_high, by = "confidence"), 10))

# --- STEP 6: Sweep thresholds to find non-empty rule sets ---

target_high <- "song_popularity=song_popularity=High"

cfgs <- list(
  list(supp=0.010, conf=0.55, maxlen=4),
  list(supp=0.0075, conf=0.55, maxlen=4),
  list(supp=0.005, conf=0.50, maxlen=4),
  list(supp=0.005, conf=0.45, maxlen=5),
  list(supp=0.003, conf=0.45, maxlen=5)
)

results <- data.frame(supp=numeric(), conf=numeric(), maxlen=integer(), n_rules=integer())

rule_sets <- list()

for (i in seq_along(cfgs)) {
  p <- cfgs[[i]]
  cat(sprintf("\n>>> Trying supp=%.4f, conf=%.2f, maxlen=%d ...\n", p$supp, p$conf, p$maxlen))
  rs <- apriori(
    trans,
    parameter = list(supp=p$supp, conf=p$conf, maxlen=p$maxlen, target="rules"),
    appearance = list(default="lhs", rhs=target_high),
    control = list(verbose=FALSE)
  )
  n <- length(rs)
  cat("Rules found:", n, "\n")
  results <- rbind(results, data.frame(supp=p$supp, conf=p$conf, maxlen=p$maxlen, n_rules=n))
  rule_sets[[paste(p$supp, p$conf, p$maxlen, sep="_")]] <- rs
}

cat("\nSweep summary:\n"); print(results)

# If any non-empty, pick the first non-empty set and show top rules
first_nonempty_key <- names(Filter(function(x) length(x)>0, rule_sets))[1]
if (!is.null(first_nonempty_key)) {
  cat("\nShowing top rules for config:", first_nonempty_key, "\n")
  rs <- rule_sets[[first_nonempty_key]]
  
  cat("\nTop 10 by lift:\n")
  inspect(head(sort(rs, by="lift"), 10))
  
  cat("\nTop 10 by confidence:\n")
  inspect(head(sort(rs, by="confidence"), 10))
} else {
  cat("\nNo rules found across this sweep. We will loosen further in the next step.\n")
}


# --- STEP 7 (fixed): Filter, prune, and review high-quality rules ---

rs <- rule_sets[["0.005_0.5_4"]]

# 7.1 Remove rules with 'is_outlier' on the LHS
rs1 <- subset(rs, !(lhs %pin% "is_outlier="))

# 7.2 Keep only rules with lift >= 1.45
rs2 <- subset(rs1, lift >= 1.45)

cat("\nCounts:\n")
cat("Original:", length(rs),
    "| no-outlier LHS:", length(rs1),
    "| lift>=1.45:", length(rs2), "\n")

# 7.3 Remove redundant rules
rs3 <- rs2[!is.redundant(rs2, measure = "confidence")]
cat("After redundancy pruning:", length(rs3), "\n")

# 7.4 Inspect strongest rules
cat("\nTop 10 by lift (post-filter):\n")
inspect(head(sort(rs3, by = "lift"), 10))

cat("\nTop 10 by confidence (post-filter):\n")
inspect(head(sort(rs3, by = "confidence"), 10))

# 7.5 Convert to data.frame + compute lengths safely
df_rules <- as(rs3, "data.frame")

lhs_len <- tryCatch(arules::size(lhs(rs3)), error = function(e) NULL)
rhs_len <- tryCatch(arules::size(rhs(rs3)), error = function(e) NULL)

if (is.null(lhs_len) || is.null(rhs_len)) {
  lhs_len <- lengths(arules::LIST(lhs(rs3)))
  rhs_len <- lengths(arules::LIST(rhs(rs3)))
}

df_rules$lhs_len <- lhs_len
df_rules$rhs_len <- rhs_len

df_rules <- df_rules[order(-df_rules$lift, -df_rules$confidence), ]

cat("\nPreview of rules table (top 10 rows):\n")
print(head(df_rules, 10))

# --- STEP 8: Mine & compare Medium / Low with same thresholds and pruning ---

target_med  <- "song_popularity=song_popularity=Medium"
target_low  <- "song_popularity=song_popularity=Low"

mine_target <- function(target_label, supp=0.005, conf=0.50, maxlen=4, min_lift=1.45) {
  rs <- apriori(
    trans,
    parameter = list(supp=supp, conf=conf, maxlen=maxlen, target="rules"),
    appearance = list(default="lhs", rhs=target_label),
    control = list(verbose = FALSE)
  )
  # remove 'is_outlier' on LHS and keep strong lift
  rs1 <- subset(rs, !(lhs %pin% "is_outlier="))
  rs2 <- subset(rs1, lift >= min_lift)
  # prune redundancy
  rs3 <- rs2[!is.redundant(rs2, measure = "confidence")]
  list(raw=rs, filtered=rs3)
}

med <- mine_target(target_med)
low <- mine_target(target_low)

cat("\nCounts (same thresholds as High):\n")
cat("High: original =", length(rs), " | filtered =", length(rs3), "\n")
cat("Med : original =", length(med$raw), " | filtered =", length(med$filtered), "\n")
cat("Low : original =", length(low$raw), " | filtered =", length(low$filtered), "\n")

# Show top rules by lift for each (up to 10)
show_top <- function(rset, title) {
  cat("\nTop rules by LIFT —", title, ":\n")
  if (length(rset) == 0) { cat("(none)\n"); return(invisible(NULL)) }
  inspect(head(sort(rset, by="lift"), 10))
}

show_top(rs3,  "Popularity = High")
show_top(med$filtered, "Popularity = Medium")
show_top(low$filtered, "Popularity = Low")

# Optional: quick summary table to compare average quality
to_df <- function(rset) {
  if (length(rset)==0) return(data.frame(support=numeric(), confidence=numeric(), lift=numeric()))
  as(rset, "data.frame")[, c("support","confidence","lift")]
}

cmp <- rbind(
  cbind(target="High",   to_df(rs3)),
  cbind(target="Medium", to_df(med$filtered)),
  cbind(target="Low",    to_df(low$filtered))
)

if (nrow(cmp) > 0) {
  agg <- aggregate(. ~ target, data=cmp, FUN=function(x) round(mean(x), 4))
  cat("\nAverage quality metrics by target (filtered sets):\n")
  print(agg)
} else {
  cat("\nNo filtered rules to summarize.\n")
}

# --- STEP 9: Export tidy CSVs for High / Medium / Low ---

tidy_rules_clean <- function(rset) {
  if (length(rset) == 0) return(data.frame())
  df <- data.frame(
    lhs = sapply(LIST(lhs(rset)), paste, collapse = " & "),
    rhs = sapply(LIST(rhs(rset)), paste, collapse = " & "),
    support = quality(rset)$support,
    confidence = quality(rset)$confidence,
    lift = quality(rset)$lift,
    count = quality(rset)$count
  )
  df$lhs_len <- size(lhs(rset))
  df$rhs_len <- size(rhs(rset))
  df <- df[order(-df$lift, -df$confidence), ]
  df
}

# Ricrea e salva i file in formato chiaro
write.csv(tidy_rules_clean(rs3), "clean_rules_high.csv", row.names = FALSE)
write.csv(tidy_rules_clean(med$filtered), "clean_rules_medium.csv", row.names = FALSE)
write.csv(tidy_rules_clean(low$filtered), "clean_rules_low.csv", row.names = FALSE)

cat("\nSaved:\n  - rules_popularity_high.csv",
    "\n  - rules_popularity_medium.csv",
    "\n  - rules_popularity_low.csv\n")

# Optional: show quick head() to confirm
cat("\nPreview HIGH (top 5):\n"); print(head(df_high, 5))
cat("\nPreview MEDIUM (top 5):\n"); print(head(df_medium, 5))
cat("\nPreview LOW (top 5):\n"); print(head(df_low, 5))



# --- Feature importance based on rule frequency for High popularity ---

# 1. Extract all LHS items from the final rules predicting High popularity
lhs_items <- unlist(LIST(lhs(rs3)))

# 2. Count how often each feature appears across rules
lhs_freq <- sort(table(lhs_items), decreasing = TRUE)

# 3. Transform into a data frame for visualization
feature_importance <- data.frame(
  Feature = names(lhs_freq),
  Frequency = as.numeric(lhs_freq)
)

# 4. Optional: Clean up feature names for readability
feature_importance$Feature <- gsub("=.*", "", feature_importance$Feature)

# 5. Aggregate counts by variable (e.g., “danceability” appears in both High/Medium/Low bins)
feature_summary <- aggregate(Frequency ~ Feature, data = feature_importance, sum)
feature_summary <- feature_summary[order(-feature_summary$Frequency), ]

# 6. Display top related features
cat("\nTop features most related to High popularity (by occurrence in rules):\n")
print(head(feature_summary, 10))

# 7. Optional: Barplot for quick visualization
barplot(
  feature_summary$Frequency[1:10],
  names.arg = feature_summary$Feature[1:10],
  las = 2, col = "steelblue",
  main = "Top Features Related to High Popularity",
  ylab = "Frequency in Rules (LHS)"
)

