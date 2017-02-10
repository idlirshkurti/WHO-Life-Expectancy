# full <- full data frame (data unavailable)

# > names(full)
# [1] "Country" "Year" "Region" "Life" "GNI" "Infant" "Adult"
# [8] "Diptheria" "Measles" "Polio" "Maternal" "Water" "Sanitation"

full$Country <- as.factor(full$Country)
full$Region <- as.factor(full$Region)

# ----------------- Exploratory Analysis ----------------- #
#Histogram

pg <- ggplot(full, aes(Life))
pg <- pg + geom_histogram(binwidth = 2,aes(fill = ..count..))
pg <- pg + facet_wrap(~Year, ncol=1)
pg <- pg + theme(text = element_text(size=14))
pg <- pg + labs(title = "Life expectancy at birth in different years")
pg <- pg + xlab("Life Expectancy")
pg <- pg + theme(plot.title = element_text(size = rel(2)))
pg <- pg + theme(axis.title.y = element_text(size = rel(1.7), angle = 90))
pg <- pg + theme(axis.title.x = element_text(size = rel(1.7)))
print(pg)

pg1 <- ggplot(full, aes(Life))
pg1 <- pg1 + geom_histogram(binwidth = 2,aes(fill = ..count..))
pg1 <- pg1 + facet_wrap(~Region, ncol=1)
pg1 <- pg1 + theme(text = element_text(size=14))
pg1 <- pg1 + labs(title = "Overall Life Expectancy Within Different Regions")
pg1 <- pg1 + xlab("Life Expectancy")
pg1 <- pg1 + theme(plot.title = element_text(size = rel(2)))
pg1 <- pg1 + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
pg1 <- pg1 + theme(axis.title.x = element_text(size = rel(1.5)))
print(pg1)

pg2 <- ggplot(full, aes(Life)) + stat_density(geom = "path", position = "identity", aes(colour = Year))
print(pg2)

### Correlgram ###
M <- cor(full1)
corrplot(M, method="number")

####BOXPLOTS####
ggplot(aes(y = Life, x = Year), data = full) + geom_boxplot()
ggplot(aes(y = LifeW, x = Year, fill = LifeM), data = full) + geom_boxplot()
