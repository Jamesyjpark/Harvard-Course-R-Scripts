# Rmd file of this project is avaialble in the same repository
install.packages(c("tidyverse", "dplyr", "ggplot2", "usmaps"))
lib <- c("tidyverse", "dplyr", "ggplot2", "usmaps")
lapply(lib, require, character.only = TRUE)
load("elections_polls.RData")

# United States presidential election was held on November 8, 2016
polls <- 
  polls %>% mutate(endmonth = strftime(polls$enddate, "%m")) %>% filter(endmonth %in% c(10,11))

polls$spread <- (polls$republican_poll - polls$democrat_poll)/100
polls$spread_act <- (polls$republican_result - polls$democrat_result)/100

reduced_polls <-
  polls %>% group_by(race_state) %>%
  summarize(spread.mean = mean(spread), spread.sd = sd(spread), spread.act.mean = mean(spread_act), n = n(), year = unique(year), type = unique(type), state=unique(state))

reduced_polls <- 
  reduced_polls %>% 
  filter(!grepl("House-G", type) & n > 2)

reduced_polls$upper <- 
  reduced_polls$spread.mean + 1.96 * (reduced_polls$spread.sd/sqrt(reduced_polls$n))

reduced_polls$lower <- 
  reduced_polls$spread.mean - 1.96 * (reduced_polls$spread.sd/sqrt(reduced_polls$n))

reduced_polls$CI <- 
  ifelse(reduced_polls$spread.act.mean >= reduced_polls$lower & reduced_polls$spread.act.mean <= reduced_polls$upper, 0, 1)

reduced_polls %>% 
  group_by(year, type) %>% 
  summarise(Deviation = sum(CI)/length(CI))

reduced_polls %>% 
  filter(year == 2016, type == "Pres") %>%
  ggplot(aes(reorder(state, spread.mean), spread.mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) + 
  geom_point(aes(state, spread.act.mean), col = "lightblue") +
  geom_hline(yintercept=0, col = "grey") + 
  theme_classic() + 
  ylab("Estimates") +
  xlab("State") + 
  ggtitle("2016 Presidential Election Prediction vs. Estimate") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

reduced_polls %>%
  filter(year == 2016, type == "Pres", lower < 0 & upper < 0, spread.act.mean > 0)

reduced_polls %>%
  ggplot(aes(as.factor(year), biasterm, color = type)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x= "Year", y = "Bias term") + 
  ggtitle("Distribution of bias terms for races in each year") +
  geom_hline(yintercept=0, col = "grey") + 
  theme(plot.title = element_text(hjust = 0.5))

election2008 <- reduced_polls[reduced_polls$year == 2008,]
plot_usmap(data = election2008, values = "biasterm", labels = TRUE) + 
  scale_fill_continuous(low = "white", high = "purple", name = "Bias term") + 
  theme(legend.position = "right") + 
  labs(title = "2008 Presidential election - distribution of bias term across the states")
