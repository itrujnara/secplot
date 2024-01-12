ss.summary <- function(ss) {
  feat_counts <- ss$features %>% group_by(Type) %>% summarize(count = n())
  feat_count_report <- feat_counts %>%
    apply(1, function(x) paste0("\t", x[1], ": ", x[2], "\n"))

  feat_coverage <- ss$features %>% group_by(Type) %>% summarize(coverage = sum(Length))
  feat_coverage_report <- feat_coverage %>%
    apply(1, function(x) paste0("\t", x[1], ": ", x[2], "\n"))

  feat_lengths <- ss$features %>% group_by(Type) %>%
    summarize(min = min(Length), mean = mean(Length),
              median = median(Length), max = max(Length))
  feat_length_report <- feat_lengths %>%
    apply(1, function(x) paste0(x[1], "\t", x[2], "\t", x[3], "\t", x[4], "\t", x[5], "\n"))

  cat(paste("Secondary structure with ID", ss$id, "\n"))
  cat(paste("Sequence extent:", min(ss$features$Start), "to",
            max(ss$features$End), "\n"))
  cat(paste("Number of features:", nrow(ss$features), "\n"))
  for(line in feat_count_report) {
    cat(line)
  }
  cat("Feature coverage:\n")
  for(line in feat_coverage_report) {
    cat(line)
  }
  cat("Feature length:\n")
  cat("Type\tMin\tMean\tMedian\tMax\n")
  for(line in feat_length_report) {
    cat(line)
  }
}
