#naam: Mohamed Darkaoui
#rolnummer: s0185350

data = read.csv(file="drugs_2021.csv", header=TRUE, sep=";")

i = 3
j = 5
k = 0

data = data[-c(k+1, j+1, i+1, j*k+1, i*j+1, i*k+1, i*j*k+1, i+j+k+1),]




###################################################
# vraag 1: verdeling en normaliteit va variabele age

age =  data$age


mean_age <- mean(age,na.rm = TRUE)
median_age <- median(age, na.rm = TRUE)
matrix_age <- t(age)
n_age <- NROW(!is.na(age))

sprintf("mean age: %s", mean_age)
sprintf("median age: %s", median_age)
sprintf("age sample size: %s", n_age)


png(file = "age/age_table.png")
plot(table(age), main="age table plot") 

png(file = "age/age_hist.png")
hist(age,breaks=16, main="age histogram")

#normaliteit
png("age/age_qq.png")
qqnorm(age, main="age QQ-plot")
qqline(age)


png("age/age_boxplot.png")
boxplot(age, main="age boxplot")

# testen of data normaal verdeeld is 
shapiro.test(age) #Shapiro-Wilk normality test
### data is niet normaal verdeeld


# transofmatie pogingen
shapiro.test(log(age))
shapiro.test(age^2)
shapiro.test(age^(-1))
shapiro.test(sqrt(age))
shapiro.test(age^(1/3))
shapiro.test(age^(1/4))






###################################################
# vraag 2
# print(data)


# subsets
long_treatment_data<-data[data$treat=='1',]
short_treatment_data<-data[data$treat=='0',]

time = data$time
treat = data$treat

time0 = short_treatment_data$time
time1 = long_treatment_data$time

png("oef2/boxplot.png")
boxplot(time ~ treat, main="boxplots voor groep 1 en groep 2")

png("oef2/qqplot_groep1.png")
qqnorm(time0, main="QQ-plot groep 1")
qqline(time0)

png("oef2/qqplot_groep2.png")
qqnorm(time1, main="QQ-plot groep 2")
qqline(time1)

wilcox.test(data$time ~ data$treat, alt="less", correct=FALSE)

# gemiddelde
X0 = mean(short_treatment_data$time)
X1 = mean(long_treatment_data$time)

# variantie
S0 = sqrt(var(short_treatment_data$time))
S1 = sqrt(var(long_treatment_data$time))

# steekproefgrootte
N0 = nrow(short_treatment_data)
N1 = nrow(long_treatment_data)





###################################################
# vraag 3

ivhx = data$ivhx

table (treat, ivhx)
Xsq <- chisq.test(treat,ivhx)
Xsq$expected
Xsq





###################################################
# vraag4
png("oef4.png")
los <- data$los
time <- data$time

# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(los, time, main = "scatter plot",
     xlab = "los", ylab = "time",
     pch = 19, frame = FALSE)
# Add regression line
plot(los, time, main = "scatter plot with regression line",
     xlab = "los", ylab = "time",
     pch = 19, frame = FALSE)
abline(lm(time ~ los, data = mtcars), col = "blue")

lm = lm(los~time)
summary(lm)

cor.test(los, time, method="spearman", exact = FALSE)
