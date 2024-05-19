(2/3)/ (5/6)

log10(.8)

six <- seq(0,10, by = 1)
others <- seq(0, 10, by  = 1)

options <- expand.grid(six = six, others = others)

options$weight  <- round(10 *( log10(.1) + options$six * log10(2) + options$others * log10(.8)),1)


library(ggplot2)
library(ggthemes)

ggplot(options, aes(x = six, y = others, fill = weight)) +
  geom_tile(color = "white", lwd = .1,
            linetype = 1)+
  geom_text(aes(label = round(weight,2)), color = "white", size = 5)+
  scale_fill_gradient(low = "black", high = "orangered")+
  theme_tufte()+
  ggtitle("Good's weights for up to 20 coin tosses (db)")+
  labs(subtitle = "One  loaded die (1/3 for six) among 10 otherwise fair dice, random die selection")+scale_x_continuous(breaks = seq(0,10, by = 1))+
  scale_y_continuous(breaks = seq(0,10, by = 1))


log_{10}(2)