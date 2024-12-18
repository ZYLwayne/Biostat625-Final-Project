# 1. Distribution of Demographics in terms of Length of Stay
p1 <- ggplot(data, aes(x = Hospital.Service.Area, y = log(Length.of.Stay), fill = Hospital.Service.Area)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Hospital.Service.Area", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(data, aes(x = Age.Group, y = log(Length.of.Stay), fill = Age.Group)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) + # Calculate p-value
  labs(x = "Age.Group", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p3 <- ggplot(data, aes(x = Gender, y = log(Length.of.Stay), fill = Gender)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Gender", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- ggplot(data, aes(x = Race, y = log(Length.of.Stay), fill = Race)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Race", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p5 <- ggplot(data, aes(x = Ethnicity, y = log(Length.of.Stay), fill = Ethnicity)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Ethnicity", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

ggarrange(p1, ggarrange(p2, p3, ncol = 2), ggarrange(p4, p5, ncol = 2), ncol = 1)

# 2. Distribution of Admission Information in terms of Length of Stay
q1 <- ggplot(data, aes(x = Type.of.Admission, y = log(Length.of.Stay), fill = Type.of.Admission)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Type.of.Admission", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

q2 <- ggplot(data, aes(x = Emergency.Department.Indicator, y = log(Length.of.Stay), fill = Emergency.Department.Indicator)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Emergency.Department.Indicator", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

ggarrange(q1, q2, ncol = 2)


# 3. Linear relationship between Charges and Costs
model_nonlog <- lm(Total.Charges ~ Total.Costs, data = data)
model_nonlog_summary <- tidy(model_nonlog)
intercept <- round(model_nonlog_summary$estimate[1], 2)  # intercept
slope <- round(model_nonlog_summary$estimate[2], 2)      # slope
p_value <- round(model_nonlog_summary$p.value[2], 2)     # p-value
adjusted_r_squared <- round(summary(model_nonlog)$adj.r.squared,4) # R squared

r1 <- ggplot(data = data, aes(x = Total.Costs, y = Total.Charges)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  annotate("text", x = min(data$Total.Costs), y = max(data$Total.Charges) * 0.8, 
           label = paste0("y = ", slope, "x + ", intercept), 
           hjust = 0, size = 4, color = "black") +
  annotate("text", x = min(data$Total.Costs), y = max(data$Total.Charges) * 0.75, 
           label = paste0("P value = ", p_value,";","R squared = ", adjusted_r_squared),
           hjust = 0, size = 3, color = "red") + 
  labs(title = "Scatter plot",
       x = "Total Costs",
       y = "Total Charges") +
  theme_minimal() + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) 

model_log <- lm(log(Total.Charges) ~ log(Total.Costs), data = data)
model_log_summary <- tidy(model_log)
intercept <- round(model_log_summary$estimate[1], 2)  # intercept
slope <- round(model_log_summary$estimate[2], 2)      # slope
p_value <- round(model_log_summary$p.value[2], 2)     # p-value
adjusted_r_squared <- round(summary(model_log)$adj.r.squared,4) # R squared

r2 <- ggplot(data = data, aes(x = log(Total.Costs), y = log(Total.Charges))) +
  geom_point(col = 'blue') + 
  geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  annotate("text", x = min(log(data$Total.Costs)), y = max(log(data$Total.Charges)) * 0.78, 
           label = paste0("y = ", slope, "x + ", intercept), 
           hjust = 0, size = 4, color = "black") +
  annotate("text", x = min(log(data$Total.Costs)), y = max(log(data$Total.Charges)) * 0.755, 
           label = paste0("P value = ", p_value,";","R squared = ", adjusted_r_squared),
           hjust = 0, size = 3, color = "red") + 
  labs(title = "Scatter plot-log transformation",
       x = "Total Costs",
       y = "Total Charges") +
  theme_minimal() 


log_charges <- log(data$Total.Charges)
r3 <- ggplot(data, aes(x = log_charges)) +
  geom_density(fill = "blue", alpha = 0.5) +
  stat_density(aes(y = ..density..), geom = "line") +
  # Use the maximum y-value of the density plot to label the skewness value.
  annotate("text", x = mean(log_charges), 
           y = max(density(log_charges)$y),  
           label = paste("Skewness:", round(skewness(log_charges), 3)), 
           size = 3, color = "red") +
  labs(title = "Density Plot of Log(Total Charges) ",
       x = "Log(Total Charges)",
       y = "Density") +
  theme_minimal()

log_costs <- log(data$Total.Costs)
r4 <- ggplot(data, aes(x = log_costs)) +
  geom_density(fill = "blue", alpha = 0.5) +
  stat_density(aes(y = ..density..), geom = "line") +
  annotate("text", x = mean(log_costs), 
           y = max(density(log_costs)$y),  
           label = paste("Skewness:", round(skewness(log_costs), 3)), 
           size = 3, color = "red") +
  labs(title = "Density Plot of Log(Total Costs) ",
       x = "Log(Total Charges)",
       y = "Density") +
  theme_minimal()

grid.arrange(r1,r2,r3,r4, ncol = 2)
