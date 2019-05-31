require(tidyverse)
require(gridExtra)
require(broom)
require(usdm)

# for plots
theme.info <- theme(plot.title = element_text(size=16, hjust=0.5),
                    axis.title = element_text(size=14),
                    axis.text = element_text(size=14))

countries <- read_csv("countries.csv")

literacy <- read_csv("literacy.csv")
assistance <- read_csv("assrec.csv")
GDP <- read_csv("GDPpc.csv")
workforce <- read_csv("laborers.csv")
life <- read_csv("lifeex.csv")
MilpcGDP <- read_csv("military.csv")
StudRat <- read_csv("ptratio.csv")
trained <- read_csv("trained.csv")
unemp <- read_csv("unemployment.csv")
urbanpct <- read.csv("urban.csv")
access <- read_csv("elic.csv")


data.df <- data.frame(countries)
lit <- filter(literacy,literacy$`Country Name` %in% data.df$Country.Name)
lit <- lit[ , "2011"]
sum(is.na(lit))
assist <- filter(assistance, assistance$`Country Name` %in% data.df$Country.Name)
assist <- assist[ ,"2011"]
sum(is.na(assist))
GDPpc <- filter(GDP,GDP$`Country Name` %in% data.df$Country.Name)
GDPpc <-GDPpc[ , "2011"]
sum(is.na(GDPpc))
workers <- filter(workforce,workforce$`Country Name` %in% data.df$Country.Name)
workers <-workers[ , "2011"]
sum(is.na(workers))
lifeex <- filter(life,life$`Country Name` %in% data.df$Country.Name)
lifeex <-lifeex[ , "2011"]
sum(is.na(lifeex))
Milpct <- filter(MilpcGDP,MilpcGDP$`Country Name` %in% data.df$Country.Name)
Milpct <-Milpct[ , "2011"]
sum(is.na(Milpct))
ptratio <- filter(StudRat,StudRat$`Country Name` %in% data.df$Country.Name)
ptratio <-ptratio[ , "2011"]
sum(is.na(ptratio))
trnteach <- filter(trained,trained$`Country Name` %in% data.df$Country.Name)
trnteach <-trnteach[ , "2011"]
sum(is.na(trnteach))
unemploy <- filter(unemp,unemp$`Country Name` %in% data.df$Country.Name)
unemploy <-unemploy[ , "2011"]
sum(is.na(unemploy))
pctcity <- filter(urbanpct,urbanpct$Country.Name %in% data.df$Country.Name)
pctcity <-pctcity[ ,55]
sum(is.na(pctcity))
electric <- filter(access,access$`Country Name` %in% data.df$Country.Name)
electric <-electric[ , "2011"]
sum(is.na(electric))

data.df <- data.frame(data.df, c(lit, GDPpc, workers, lifeex, unemploy, pctcity, electric ))
colnames(data.df) <- c(lit, GDPpc, workers, lifeex, unemploy, pctcity, electric )
variables <- cbind(c(lit, GDPpc, workers, lifeex, unemploy, pctcity, electric ))


data.good <- data.frame(read_csv("features.csv"))

df <- filter(data.good, data.good$Country_Name %in% data.df$Country.Name)

NAs <- is.na.data.frame(df)

df <- df[-c(1,4,9,10,21,29,31,46,47,49,52,53), -4]

NAs <- is.na.data.frame(df)
NAs


pairs(df[, c("Literacy", "GDPpc", "Lifeex", "Unemployment", "Urbanpct", "Elictricity")], 
      main="Pairwise Scatterplots of Quantitative Variables", las=TRUE, col="#0080ff70", pch=19)
df$GDPpc <- sqrt(df$GDPpc)

pairs(df[, c("Literacy", "GDPpc", "Lifeex", "Unemployment", "Urbanpct", "Elictricity")], 
      main="Pairwise Scatterplots of Quantitative Variables", las=TRUE, col="#0080ff70", pch=19)
round(cor(df[, c("Literacy", "Lifeex", "Unemployment", "Urbanpct", "Elictricity")]), digits = 3)


df <- df[ ,-3]

pairs(df[, c("Literacy","Lifeex", "Unemployment", "Urbanpct", "Elictricity")], 
      main="Pairwise Scatterplots of Quantitative Variables", las=TRUE, col="#0080ff70", pch=19)

round(cor(df[, c("Literacy", "Lifeex", "Unemployment", "Urbanpct")]), digits = 3)


pairs(df[, c("Literacy","Lifeex", "Unemployment", "Urbanpct")], 
      main="Pairwise Scatterplots of Quantitative Variables", las=TRUE, col="#0080ff70", pch=19)

df$Urbanpct <- log(df$Urbanpct)
pairs(df[, c("Literacy","Lifeex", "Unemployment", "Urbanpct")], 
      main="Pairwise Scatterplots of Quantitative Variables", las=TRUE, col="#0080ff70", pch=19)

lm.literacy <- lm(Literacy~ .-Country_Name, data = df)
summary(lm.literacy)




lm.augment <- augment(lm.literacy)
lm.augment

h1 <- lm.augment %>% 
  ggplot(aes(x=.resid)) +
  geom_histogram(bins=25, col="gray50", fill="cadetblue") +
  labs(x=expression(paste("residuals, ", e[i], " (Literacy Rate)", sep=""))) +
  ggtitle(expression(paste("Histogram of Residuals, ", e[i], sep=""))) +
  theme.info

h2 <- lm.augment %>% 
  ggplot(aes(x=.std.resid)) +
  geom_histogram(bins=25, col="gray50", fill="cadetblue") +
  labs(x=expression(paste("standardized residuals, ", r[i], sep=""))) +
  ggtitle(expression(paste("Histogram of Standardized Residuals, ", r[i], sep=""))) +
  theme.info

h1
h2


par(mfrow=c(2,2), mar=c(5, 5, 4, 2)+0.1)
plot(lm.literacy, las=TRUE, cex.main=1.4, cex.axis=1.4, cex.lab=1.4)



full_join(lm.augment, df) %>% 
  ggplot(aes(x=.fitted, y=.std.resid)) +
  geom_point(size=2) +
  labs(x="fitted values (Literacy Rate)", y="standardized residuals") +
  ggtitle("Std. Residuals vs. Fitted Values") +
  theme.info
  