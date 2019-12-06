library(patchwork)


p1 <- ggplot(mpg) + 
  geom_point(aes(hwy, displ))
p2 <- ggplot(mpg) + 
  geom_bar(aes(manufacturer, fill = stat(count))) + 
  coord_flip()


p1

p2
# patchwork allows you to add plots together

p1/ p2

(p1 + p2)/(p2+p1)
