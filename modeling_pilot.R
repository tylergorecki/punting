# Using this as a script to mess around with yard modeling
downed_punts <- read.csv("downed_ep_wp.csv")
returned_punts <- read.csv("returned-epwp.csv")

downed_punts$punt_type <- "downed"
returned_punts$punt_type <- "returned"

violin_table_1 <- downed_punts[, c("punt_type", "actual_ep")]
violin_table_2 <- returned_punts[, c("punt_type", "EPasplayed")]

colnames(violin_table_1)[2] <- "expected_points"
colnames(violin_table_2)[2] <- "expected_points"

violin_table <- rbind(violin_table_1, violin_table_2)

ggplot(violin_table, aes(x = punt_type, y = expected_points)) +
  geom_jitter(aes(color = expected_points)) +
  scale_color_gradient2(low = "red", high = "green", mid = "red", midpoint = 0)

violin_table$frequency_dist <- ifelse(violin_table$punt_type == "downed" & violin_table$expected_points < 0,
                                 "downed_bp", ifelse(violin_table$punt_type == "returned" & violin_table$expected_points < 0,
                                 "returned_bp", ifelse(violin_table$punt_type == "downed" & violin_table$expected_points >= 0 & violin_table$expected_points < 1.925,
                                 "downed_np", ifelse(violin_table$punt_type == "returned" & violin_table$expected_points >= 0 & violin_table$expected_points < 1.925,
                                 "returned_np", ifelse(violin_table$punt_type == "downed" & violin_table$expected_points >= 1.925,
                                 "downed_gp", ifelse(violin_table$punt_type == "returned" & violin_table$expected_points >= 1.925,
                                 "returned_gp", NA))))))

downed_bp <- as.numeric(sum(violin_table$frequency_dist == "downed_bp"))
returned_bp <- as.numeric(sum(violin_table$frequency_dist == "returned_bp"))
downed_np <- as.numeric(sum(violin_table$frequency_dist == "downed_np"))
returned_np <- as.numeric(sum(violin_table$frequency_dist == "returned_np"))
downed_gp <- as.numeric(sum(violin_table$frequency_dist == "downed_gp"))                        
returned_gp <- as.numeric(sum(violin_table$frequency_dist == "returned_gp"))

# Calculate P(Punt Return Quality | Punt Type Occurred)
# Multiply that Probability 