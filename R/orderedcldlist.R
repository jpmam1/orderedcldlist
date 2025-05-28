#' @title: Ordered cldList
#'
#' @description: This function takes the output of TukeyHSD post-hoc test, modified to include 
#' a Comparison column
#'
#' @examples
#' set.seed(12345)
#' df <- data.frame(program = rep(c("AA", "BB", "CC", "DD"), each = 10),
#'                  weight_loss = c(runif(10, 0, 10),
#'                                  runif(10, 0, 4),
#'                                  runif(10, 1, 7),
#'                                  runif(10, 0, 3)),
#'                  group = factor(sample(c(0,1), replace = TRUE, size = 10)))
#' model = aov(weight_loss ~ program * group, data = df)
#' summary(model)
#' TUK = TukeyHSD(model, ordered = TRUE)
#' TUK = as.data.frame(TUK[[1]])
#' HSD = data.frame(Comparison=row.names(TUK),
#'                  diff=TUK$diff, lwr=TUK$lwr, upr=TUK$upr, p.adj=TUK$`p adj`)
#' HSD
#' desired_order = c("BB", "AA", "CC", "DD")
#' orderedcldlist(data = HSD,
#'                comparison = "Comparison",
#'                p.value = "p.adj",
#'                threshold = 0.05,
#'                desired_order = desired_order)
#' 
#' desired_order2 = c("DD", "BB", "CC", "AA")
#' orderedcldlist(data = HSD,
#'                comparison = "Comparison",
#'                p.value = "p.adj",
#'                threshold = 0.05,
#'                desired_order = desired_order2)
#'
#' @param data Tukey HSD results, formatted as shown in the example below
#' @param comparison A vector of text describing comparisons, with each element in the form "Treat.A-Treat.B"
#' @param p.value A vector of p-values corresponding to the comparisons in the comparison argument
#' @param threshold The alpha value: the p-value below which the comparison will be considered statistically significant
#' @param print.comp If TRUE, prints out a data frame of the comparisons. Useful for debugging
#' @param desired_order A vector of one or more comparisons in the desired order, e.g. c("Treat.B", "Treat.A")
#' @return A list of variables with a letter denoting statistical significance
#' @export
orderedcldlist <- function(data = NULL, comparison = NULL, p.value = NULL, 
                           threshold = 0.05, print.comp = FALSE, desired_order = NULL) 
{
  
  if (is.null(desired_order)) {
    stop("desired_order not specified; please see ?orderedcldlist")
  }
  
  FLAG = 0
  
  if (sum(data[data[[p.value]] <= threshold,][[p.value]], na.rm = TRUE) == 0) {
    FLAG = 1
  }
  
  df = data[data[[p.value]] <= threshold,]
  combinations <- expand.grid(x = desired_order, y = desired_order)
  combinations$Comparison <- paste(combinations$y, combinations$x, sep = "-")
  combinations$score <- rownames(combinations)
  combinations$x <- NULL
  combinations$y <- NULL
  combinations
  
  df$switched <- paste(gsub(".*-", "", df[[comparison]]),
                       gsub("-.*", "", df[[comparison]]),
                       sep = "-")
  
  for (i in 1:nrow(df)) {
    for(j in seq_along(desired_order)) {
      if (desired_order[j] == gsub(".*-", "", df[[comparison]][i])) {
        df[[comparison]][i] <- df$switched[i]
        break
      } else if (desired_order[j] == gsub("-.*", "", df[[comparison]][i])) {
        break
      }
    }
  }
  df
  df <- merge(df, combinations, by = "Comparison", all.x = TRUE)
  
  ordered_df <- df[order(as.integer(df[["score"]])),]
  Comparison = ordered_df[[p.value]]
  names(Comparison) <- ordered_df[[comparison]]
  
  if (print.comp == TRUE) {
    Y = data.frame(Comparisons = Comparison, p.value = p.value, 
                   Value = Comparison, Threshold = threshold)
    cat("\n", "\n")
    print(Y)
    cat("\n", "\n")
  }
  
  MCL = multcompView::multcompLetters(Comparison)
  Group = names(MCL$Letters)
  Letter = as.character(MCL$Letters)
  if (FLAG == 0) {
    MonoLetter = as.character(MCL$monospacedLetters)
  }
  if (FLAG == 1) {
    MonoLetter = Letter
  }
  Z = data.frame(Group, Letter, MonoLetter)
  return(Z)
}
