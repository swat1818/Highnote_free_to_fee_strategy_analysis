# In this project, we perform PSM (propensity score matching) to test whether having "subscriber firneds" influences people to adopt / take up premium subscriptions
# use a logistic regression approach to test which variables (including subscriber friends) are significant for explaining the likelihood of becoming an adopter
# Discuss some key takeaways from your analysis. Specifically, how do your results inform a “free-to-fee” strategy for High Note?


library(MatchIt)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(purrr)
library(psych)

# read the dataset 
highnote<-read.csv("HighNote_Data_Midterm.csv",stringsAsFactors=T)

# Summary stats by adopter type
highnote %>% split(.$adopter) %>% map(describe)

#Analyze the differences in the mean values of the variables
lapply(highnote[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
                   'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
                   'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country', 'subscriber_friend_cnt')], function(i) t.test(i ~ highnote$adopter))

print("Box Plot for Peer Influence understanding to help visualize how adopters and non-adopters perform")
abc2 <- ggplot(data_desc, aes(x=paid, y=friend_cnt)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
abc2 <- abc2 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$friend_cnt, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
abc3 <- ggplot(data_desc, aes(x=paid, y=avg_friend_age)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
abc3 <- abc3 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$avg_friend_age, c(0, 1)))+theme(axis.title.x=element_blank())
abc4 <- ggplot(data_desc, aes(x=paid, y=avg_friend_male)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
abc4 <- abc4 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$avg_friend_male, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
abc5 <- ggplot(data_desc, aes(x=paid, y=friend_country_cnt)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
abc5 <- abc5 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$friend_country_cnt, c(0, 0.9)))+theme(axis.title.x=element_blank())
abc6 <- ggplot(data_desc, aes(x=paid, y=subscriber_friend_cnt)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
abc6 <- abc6 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$subscriber_friend_cnt, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
grid.arrange(abc2,abc3,abc4,abc5,abc6, ncol=3)

print("Box Plot for Engagement to help visualize how adopters and non-adopters")
a2 <- ggplot(data_desc, aes(x=paid, y=songsListened)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
a2 <- a2 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$friend_cnt, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
a3 <- ggplot(data_desc, aes(x=paid, y=lovedTracks)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
a3 <- a3 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$avg_friend_age, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
a5 <- ggplot(data_desc, aes(x=paid, y=playlists)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
a5 <- a5 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+ylim(0,5)+theme(legend.position="none")+theme(axis.title.x=element_blank())
a6 <- ggplot(data_desc, aes(x=paid, y=shouts)) + geom_boxplot(aes(fill=paid),outlier.colour="red", outlier.shape=8,outlier.size=1)
a6 <- a6 + scale_fill_manual(values=c("#FF4500", "#32CD32"))+theme(legend.position="none")+scale_y_continuous(limits = quantile(data_desc$avg_friend_age, c(0.1, 0.9)))+theme(axis.title.x=element_blank())
grid.arrange(a2,a3,a5,a6, ncol=2)

#Create control and treatment group based on subscriber friends
# 1 if friend is > 0 (treatment group)  0 if 0 friends (control group)
highnote$treatment<- ifelse(highnote$subscriber_friend_cnt >0,1,0)

#calculating mean of all covariates after grouping with subscriber friend count
hn_cov <- c('age', 'male', 'good_country', 'friend_cnt', 'avg_friend_age', 'avg_friend_male', 'friend_country_cnt', 'songsListened', 'lovedTracks', 'posts', 'playlists', 'shouts', 'tenure' )
initial_mean<-as.data.frame(highnote %>%
                              group_by(treatment) %>%
                              select(one_of(hn_cov)) %>%
                              summarise_all(funs(mean(., na.rm = T))))
initial_mean

# LM to understand significance of independent variables wrt "Subscriber frnd count"
m_ps <- glm(treatment ~ log(age+1) + male + good_country + 
              log(friend_cnt+1) + log(avg_friend_age+1) + log(avg_friend_male+1) + log(friend_country_cnt+1) + 
              log(songsListened+1) + log(lovedTracks+1) + log(posts+1) + log(playlists+1) + log(shouts+1) + log(tenure+1),
            family = binomial(), data = highnote)
summary(m_ps)
exp(m_ps$coefficients)

# use model above to calculate propensity scores for each observation
psm_df <- data.frame(psm_score = predict(m_ps, type = "response"),
                     treatment = m_ps$model$treatment)
head(psm_df)

#examining region of common support
labs <- paste("Type of User:", c("Premium", "Non-Premium"))
psm_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psm_score)) +
  geom_histogram(color="navyblue", fill="lightblue") +
  facet_wrap(~treatment) +
  xlab("Probability of Treatment (Having >1 Subscriber friend)") +
  theme_classic() 

# match the treatment and control groups usint matchit
mod_match <- matchit(treatment ~ log(age) + male + good_country + 
                       log(friend_cnt+1) + log(avg_friend_age+1) + log(avg_friend_male+1)
                     + log(friend_country_cnt+1) + log(songsListened+1) + log(lovedTracks+1) + 
                       log(posts+1) + log(playlists+1) + log(shouts+1) + log(tenure+1),
                     method = "nearest", data = highnote, caliper=0.01)
summary(mod_match)
plot(mod_match)


#create dataframe of matched observations
matched_data <- match.data(mod_match)
dim(matched_data)

#original data
originaldata_mean  <- highnote%>%
  group_by(treatment) %>%
  select(one_of(hn_cov)) %>%
  summarise_all(funs(mean))

#matched data
matcheddata_mean  <- matched_data%>%
  group_by(treatment) %>%
  select(one_of(hn_cov)) %>%
  summarise_all(funs(mean))

# Difference of means
matched_data%>%
  group_by(adopter_frnd) %>%
  select(one_of(hn_cov)) %>%
  summarise_all(funs(mean))

print(hn_cov)
lapply(hn_cov, function(v) {
  t.test(matched_data[, v] ~ matched_data$treatment)
})
# most t-tests are not significant
# there are no significant differences between treatment and test groups

# Estimating the treatment effect using a t-test:
with(matched_data, t.test(adopter ~ treatment))
# the t-test is highly significant. P value is <5% (alomost 0)
# All the t-tests (adopter to each variable) have extremely significant p-values, indicating that the difference in means is significant for free and premium users.
# From the results, treatment (having >=1 subscriber friend) has a positive and significant impact on the dependent variable. Having 1 or more subscriber friends, increases the odds of becoming adopter by 5.8%.

dta<-matched_data
ggplot(dta, aes(x = distance, y = variable, color = subscriber_friend_cnt)) +
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$subscriber_friend_cnt <- as.factor(dta$subscriber_friend_cnt)
  support <- c(min(dta$variable), max(dta$variable))
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

library(gridExtra)
grid.arrange(
  fn_bal(matched_data, "age"),
  fn_bal(matched_data, "male") + theme(legend.position = "none"),
  fn_bal(matched_data, "good_country"),
  fn_bal(matched_data, "friend_cnt") + theme(legend.position = "none"),
  fn_bal(matched_data, "avg_friend_age"),
  fn_bal(matched_data, "avg_friend_male") + theme(legend.position = "none"),
  fn_bal(matched_data, "friend_country_cnt"),
  fn_bal(matched_data, "songsListened") + theme(legend.position = "none"),
  fn_bal(matched_data, "lovedTracks"),
  fn_bal(matched_data, "posts") + theme(legend.position = "none"),
  fn_bal(matched_data, "playlists"),
  fn_bal(matched_data, "shouts") + theme(legend.position = "none"),
  fn_bal(matched_data, "tenure"),
  nrow = 7, widths = c(1, 0.8)
)

# treatment effect using regression
lm_treat1 <- lm(adopter ~ treatment, data = matched_data)
summary(lm_treat1)

lm_treat2 <- lm(adopter ~ treatment + songsListened + lovedTracks + playlists + posts + shouts +
                  friend_cnt + age + male + tenure + good_country ,
                data = matched_data, family = 'binomial')
summary(lm_treat2)

# Q4) Logistic regression to predict probability of becoming a premium subscriber
final_m1 <- glm(adopter ~ age + male + good_country + 
                  friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + 
                  songsListened + subscriber_friend_cnt + lovedTracks + posts + playlists + shouts + tenure ,
                data = matched_data, family = 'binomial')
summary(final_m1)

final_m2 <- glm(adopter ~ age + male + good_country + subscriber_friend_cnt+
                  avg_friend_age + songsListened + lovedTracks + posts + playlists + shouts + tenure ,
                data = matched_data, family = 'binomial')
summary(final_m2)

exp(final_m2$coefficients)
# Using the significant variables, the results indicate that subscriber friend count, male, avg friend male are positive significant indicators of the likelihood of becoming an adopter, whereas good_country and tenure are significant negative indicators of the odds of becoming an adopter (premium user).

# Interpretation: 
# Demographics: • Being a male user (1) , increases odds of becoming an adopter by a factor of 1.58 • Every unit increase in age, increases the odds of becoming an adopter by a factor of 1.01 • For every unit change in tenure, the odds of becoming an adopter decrease by a factor of 0.99 • For a change in country (to US,UK), odds of becoming an adopter decrease by a factor of 0.66 
# Engagement: • Songs listened and loved tracks do not impact / increase the likelihood of becoming an adopter significantly. • Whereas creating playlists, increases the odds of becoming an adopter by a factor of 1.13 
# Social: • Every unit increase in subscriber friends increases the odds of becoming an adopter by a factor of 1.43. • Every unit increase in avg friend age, increases the odds of becoming an adopter by a factor of 1.03.

# TakeAways
# • With the matched data, the number of songs listened to is significant, but the impact is almost nil, which is quite intuitive as the mean time spent on the website before becoming a premium member is higher than 3 years. 
# • The Community Participation data (posts and shouts) are also not significant to predict a customer to become a premium member. As per the “ladder of participation” theory, community involvement plays a significant role in subscription decisions, but for Highnote this is not the case. # On the contrary, the number of subscriber friends is still associated with a strong positive effect on the user’s likelihood to become a premium member, it has a significant association with the subscription decision and the with odd ratio is 1.443. Therefore, I can conclude that there is a causal relationship between having subscriber friends and becoming a subscriber. Highnote can focus on “virality” to increase subscriber count. 
# Male users have a higher likelihood of becoming premium users when compared to women. This is clear demographic differential, which can be utilized to develop target marketing strategies.
#  Also, users from US, UK and Germany seem to have very low likelihood of becoming premium users. This can probably be due to competition in the streaming industry in these countries or subscription price compared to competition.
