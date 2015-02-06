# Hi there! It looks like you want to create an Rmd presentation. Would you like some help?
# Okay! First, you will go the upper left hand corner of RStudio and click the new file
# button. It will give you a pop-down menu. Choose "R Markdown". This will open a new window.
# Choose "presentation", then "HTML - ioslides", then click okay!

# You should see a preformatted Rmd file appear, with your name and the date already filled
# in for you. Now, see if you can create an Rmd presentation using the R scripts below.
# Feel free to create your own headings, or modify the code in any way you like.
# If you get stuck, try looking at cheat_slides_Rmd, or presentation.Rmd

### Script follows
# Loading the required packages

library(ggplot2)

# Creating the data that we will use for this workshop

dat <- data.frame(variable.1 = c(seq(-5,5,1) - rnorm(n=11, mean=0, sd=1.5), 5, -5),
                  variable.2 = c(seq(-5,5,1) - rnorm(n=11, mean=0, sd=1.5), -5, 5),
                  variable.3 = rbinom(n = 13,size = 1, prob = 0.5))
dat$variable.4 <- dat$variable.3 + rnorm(13,5,1)
dat$variable.3[dat$variable.3 == 0] <- 'Giant'
dat$variable.3[dat$variable.3 == 1] <- 'Sandwich'
dat$variable.3 <- as.factor(dat$variable.3)
save(dat, file = "dat.Rda")

# A little bit of data exploration, just for fun

str(dat)
summary(dat)
qplot(x = variable.1, y = variable.2, data = dat)
qplot(x = variable.3, y = variable.4, data = dat)

# Creating our first analysis

ggplot(data = dat, aes(x=variable.3,y=variable.4)) + 
  geom_jitter(position = position_jitter(width = .1, height = 0))
t.test(x=dat$variable.4[dat$variable.3 == 'Giant'], y=dat$variable.4[dat$variable.3 == 'Sandwich'])

# Creating our second analysis

ggplot(data = dat, aes(x=variable.1,y=variable.2)) + geom_point() + 
  stat_smooth(method = loess)
cor.test(x=dat$variable.1,y=dat$variable.2)
summary(lm(variable.2 ~ variable.1, data = dat))

