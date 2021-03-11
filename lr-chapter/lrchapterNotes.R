library(ggplot2)
library(ggthemes)
library(rethinking)


samples <- rbinom(choose(65000,2),choose(65000,2), p = 1/1.1e6)

HPDI(samples, p = .89)



hdi = function(x, x.density, coverage){
  best = 0
for (ai in 1 : (length(x) - 1))
{for (bi in (ai + 1) : length(x))
{mass = sum(diff(x[ai : bi]) * x.density[(ai + 1) : bi])
if (mass >= coverage && mass / (x[bi] - x[ai]) > best)
{best = mass / (x[bi] - x[ai])
ai.best = ai
bi.best = bi}}}
c(x[ai.best], x[bi.best])
}

x <- 0:3000
x.density <- dbinom(x,size = choose(65000,2), p = 1/1.1e6)
interval <- hdi(x, x.density, .89)

p <- 1/1.1e6
d = seq(1500,2500, by = 1)
probs <- dbinom(d,choose(65000,2),1/1.1e6)
df <- data.frame(d,probs)
density <- ggplot(df, aes(x = d, y = probs))+geom_line()+theme_tufte()+labs(x="number of matches", y="probability of at least x  matches")
d <- ggplot_build(density)$data[[1]]
density+ geom_area(data = subset(d, x >= interval[1] & x <=interval[2]),aes(x=x, y = y), fill="skyblue", alpha = 0.4)

+ geom_area(data = subset(d, x >= interval[1] & x <=interval[2]),aes(x=x, y = y), fill="skyblue", alpha = 0.4)


library(ggplot2)
library(ggthemes)

density89 <- density + geom_area(data = subset(d, x > 0.704 & x < 0.93),
                                   aes(x=x, y = y), fill="skyblue", alpha = 0.4)+ggtitle("50% PI")



