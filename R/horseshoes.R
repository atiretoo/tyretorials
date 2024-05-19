# fit a linear model and check the assumptions
# use Agresti's horseshoe crab data from
# https://users.stat.ufl.edu/~aa/cda/data.html
library("tidyverse")
hsc <- read_csv("data/horseshoes_agresti.csv")

ggplot(data = hsc,
       mapping = aes(x = width, y = weight)) +
  geom_point(mapping = aes(color = satell)) +
  geom_smooth(method = "lm")

# check data for normality
hist(hsc$weight, breaks = 30)
hist(hsc$width, breaks = 30)

# fit a model and extract residuals
hsc_lm <- lm(weight ~ width, data = hsc)
summary(hsc_lm)
hsc$resid <- resid(hsc_lm)
hist(hsc$resid, breaks = 30)
shapiro.test(hsc$resid)

par(mfrow = c(2,2))
plot(hsc_lm)
