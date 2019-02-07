###############################################################
#  Example code to perform Poisson regression
# 
# Adapted from:
#   https://stats.idre.ucla.edu/r/dae/poisson-regression/
#
###############################################################

############################
# Load libraries
############################
library(ggplot2)
library(sqldf)


############################
# Load data
############################

# num_awards is the outcome variable and indicates 
# the number of awards earned by students at a high school in a year, 
# math is a continuous predictor variable and represents students' scores
# on their math final exam, and prog is a categorical predictor variable 
# with three levels indicating the type of program in which the students were enrolled. 
# It is coded as 1 = General, 2 = Academic and 3 = Vocational
df_counts_data <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
df_counts_data$prog <- factor(df_counts_data$prog, 
                              levels = c(1,2,3),
                              labels = c("General", "Academic", "Vocational")
                              ) 

############################
# Visualize data
############################
summary(df_counts_data)
#table(df_counts_data)

hist(df_counts_data$num_awards)

head(df_counts_data)

theme_set(theme_gray())
gp2 <- ggplot(df_counts_data, aes(x=prog,fill=math))
gp2 <- gp2 + geom_histogram(stat = "count")#(alpha=0.5)
gp2 <- gp2 + xlab("Number of awards") + ylab("Counts")
#gp <- gp + facet_wrap(histology~.)
#gp2 <- gp2 + facet_grid(math~.)
gp2


theme_set(theme_gray())
gp <- ggplot(df_counts_data, aes(x=math))#,color=prog)) # fill=source
gp <- gp + geom_point(aes(y=num_awards))
# gp <- gp + facet_wrap(~ math, ncol=2, scales = "free_y")
gp2 <- gp2 + xlab("Number of awards") + ylab("Counts")
gp


#ggplot(df_counts_data, aes(x = math, colour = prog)) +
#  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
#  # geom_line(y = num_awards) +
#  labs(x = "Math Score", y = "Expected number of awards")

theme_set(theme_gray())
gp2 <- ggplot(df_counts_data, aes(x=prog))
gp2 <- gp2 + geom_histogram(stat = "count", aes(x=prog))#(alpha=0.5)
gp2 <- gp2 + xlab("Number of awards") + ylab("Counts")
#gp <- gp + facet_wrap(histology~.)
#gp2 <- gp2 + facet_grid(math~.)
gp2


############################
# Perform Poisson regression
############################
glm_poisson_regression <- glm(formula = num_awards ~ prog + math, 
                              family = "poisson",
                              data = df_counts_data)

summary(glm_poisson_regression)


############################
# Perform predictions

############################
glm_predictions <- predict(glm_poisson_regression, type = "response")

hist(glm_predictions)

