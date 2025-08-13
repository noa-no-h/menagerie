factor <- c("Included", "Included", "Excluded", "Excluded")
affected <- c("Affected", "Not Affected", "Affected", "Not Affected")
mean_rating <- c(76.00000, 39.55556, 49.00000, 41.62069)
n <- c(23, 2, 17, 19)

df1 <- data.frame(factor, affected, mean_rating, n)

ggplot(df1, aes(x = factor, y = mean_rating, fill = affected, group = affected)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), y = mean_rating), 
            hjust = 1, vjust=-0.35, position = position_dodge(0.9)) +
  guides(fill = FALSE, color = FALSE)
