require(tidyverse)
require(expss)
require(pROC)
require(usdm)
require(gdata)

data.df <- read_csv("SpeedDating.csv")


cro(data.df$DecisionM, data.df$DecisionF)

data.df <- data.df %>% mutate(second.date = ifelse(data.df$DecisionM + data.df$DecisionF == 2, 1, 0))

attach(data.df)
par(mfrow= c(2,5))
plot(jitter(DecisionM), jitter(DecisionF), main = "M & F Decisions", pch = second.date)
plot(jitter(LikeM), jitter(LikeF), main = "M & F Like level", pch = second.date)
plot(jitter(PartnerYesM), jitter(PartnerYesF), main = "M & F Partner Material", pch = second.date)
plot(jitter(AgeM), jitter(AgeF), main = "M & F Ages", pch = second.date)
plot(jitter(AttractiveM), jitter(AttractiveF), main = "M & F Attractive Ratings", pch = second.date)
plot(jitter(SincereM), jitter(SincereF), main = "M & F Sincerity Ratings", pch = second.date)
plot(jitter(IntelligentM), jitter(IntelligentF), main = "M & F Intel. Ratings", pch = second.date)
plot(jitter(FunM), jitter(FunF), main = "M & F Fun Ratings", pch = second.date)
plot(jitter(AmbitiousM), jitter(AmbitiousF), main = "M & F Ambition Ratings", pch = second.date)
plot(jitter(SharedInterestsM), jitter(SharedInterestsF), main = "M & F Shared Interests", pch = second.date)

heads <- colnames(data.df)
heads.num <- heads[- c(9,10)]


summary(complete.cases(data.df))

summary(is.na(RaceM))
summary(is.na(RaceF))
summary(is.na(data.df[ , heads]))

detach(data.df)

data.trim <- data.df[complete.cases(data.df), ]
data.trim$RaceM <- as.character.factor(data.trim$RaceM)

attach(data.trim)

table(RaceF)
table(RaceM)
as.factor(RaceF)
as.factor(data.trim$RaceM)

mosaicplot(table(RaceM, RaceF))
detach(data.trim)

data.num <- data.trim[ , -c(9,10)]
pairs(data.num)


clean <- as.data.frame(read.csv("clean.csv"))
clean.num <- as.data.frame(read.csv("cleancut.csv"))

pairs(clean.num[ ,-1])
round(cor(clean.num), digits = 3)


glm.1 <- glm(second.date ~ Like + Age + Partner, data=clean, family=binomial(link="logit"))
summary(glm.1)


cro(data.trim$DecisionM, data.trim$DecisionF)


vif(as.data.frame(clean[,c("Like", "Age", "Partner")]))

##log-likeliohood
pchisq(summary(glm.1)$null.deviance - summary(glm.1)$deviance, 
       df=summary(glm.1)$df.null - summary(glm.1)$df.residual,
       lower.tail=FALSE)

# plot ROC curve

roc.figure1 <- roc(response=clean$second.date, predictor=glm.1$fitted.values, plot=TRUE, las=TRUE, 	legacy.axes=TRUE, lwd=5,
    main="ROC for Speed Dating Analysis, 0.5 threshold", cex.main=1.6, cex.axis=1.3, cex.lab=1.3)
#dev.off()

# sensitivity and specificity for a specific threshold of 0.5
coords(roc.info, x=0.5, input="threshold", ret=c("threshold", "sensitivity", "specificity"))
coords(roc.info, x="best", input="threshold", ret=c("threshold", "sensitivity", "specificity"))
# get AUC
auc(response= clean$second.date, predictor=glm.1$fitted.values)



theme.info <- theme(plot.title = element_text(size=16, hjust=0.5),
                    axis.title = element_text(size=14),
                    axis.text = element_text(size=14))

kudzu <- read.csv("kudzu.csv")


kudzu%>% 
  group_by(Treatment) %>%
  count()

# compute mean for each treatment group
aggregate(kudzu$BMD, by=list(kudzu$Treatment), mean)

# compute standard deviation for each treatment group
aggregate(kudzu$BMD, by=list(kudzu$Treatment), sd)

kudzu %>% 
  ggplot(aes(Treatment, BMD)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="line", aes(group=1), lwd=2, col="cadetblue") +
  stat_summary(fun.y=mean, geom="point", pch=19, size=2, col="firebrick") +
  ggtitle("Boxplots of BMD by Treatment Group\nWith Connected Means") +
  labs(x="treatment group", 
       y="BMD(g/ sq. cent.)") +
  theme.info 


result <-aov(BMD ~ Treatment, data=kudzu)

summary(result)



# looking at residuals
temp <- kudzu %>%
  group_by(Treatment) %>%
  summarize(mean(BMD))
left_join(kudzu, temp) %>%
  mutate(residuals = BMD - `mean(BMD)`) %>%
  ggplot(aes(sample=residuals)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normal Quantile Plot of the\nOne-Way ANOVA Model Residuals") +
  theme.info



# looking at response by treatment group
kudzu%>%
  ggplot(aes(sample=BMD)) +
  facet_grid(~ Treatment) +
  stat_qq() + 
  stat_qq_line() +
  ggtitle("Normal Quantile Plots by Treatment Group") +
  theme.info

TukeyHSD(result, conf.level=0.99)
