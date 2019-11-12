cinnamon <- q2 %>% 
  group_by(id) %>% 
  summarise(w1= mean(wage, na.rm=TRUE))


View(cinnamon)

fixef(m1)
confint(m1)
dataPlot <- data.frame(cbind( fixef(m1), confint(m1)[ 3:6, ]))
dataPlot

rownames(dataPlot)[1] <- "Intercept"
dataPlot
colnames(dataPlot) <- c("mean", "l95", "u95")
dataPlot
dataPlot$parameter <- rownames(dataPlot)

# Plot the results using ggplot2
ggplot(dataPlot, aes(x = parameter, 
                     y = mean,
                     ymin = l95, ymax = u95)) +
  geom_hline( yintercept = 0, color = 'red' ) +
  geom_linerange() + 
  geom_point() + 
  coord_flip() + 
  theme_minimal() +
  labs(title='Something',
       subtitle='Something else',
       caption='Plot by @privlko')
############





