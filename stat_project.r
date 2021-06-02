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
png(file = "age/age_table")
plot(table(age), main="age table plot") 

png(file = "age/age_hist")
hist(age,breaks=16, main="age histogram")
##### 2 duidelijke uitschieters, ziet er rechts scheef uit #######

#normaliteit

png("age/age_qq.png")
qqnorm(age, main="age QQ-plot")
qqline(age)
#### korte staart langs lings
#### opnieuw 2 uitschieters waargenomen

png("age/age_boxplot.png")
boxplot(age, main="age boxplot")
### de +1.5 IQR is groter dan de -1.5 IQR (whiskers)
### mediaan ligt in het midden van IQR

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

# subsets
long_treatment_data<-data[data$treat=='1',]
short_treatment_data<-data[data$treat=='0',]


# gemiddelde
X0 = mean(short_treatment_data$time)
X1 = mean(long_treatment_data$time)

# variantie
S0 = sqrt(var(short_treatment_data$time))
S1 = sqrt(var(long_treatment_data$time))

# steekproefgrootte
N0 = nrow(short_treatment_data)
N1 = nrow(long_treatment_data)

df0 = N0-1
df1 = N1-1

print(paste0("X0: ", X0))
print(paste0("X1: ", X1))
print(paste0("S0: ", S0))
print(paste0("S1: ", S1))

time = data$time
treat = data$treat

# F-test voor varianties
var.test(time  ~ treat)

# AG voor varianties
qf(c(0.025,0.995),df0,df1)

png("oef2/boxplot.png")
boxplot(time ~ treat, main="boxplots voor groep 1 en groep 2")


data$treat <- as.character(data$treat)
# t-test: H1: µ0 - µ1 < 0
t.test(data$time ~ data$treat,mu=0, alternative = "less", var.equal = TRUE)

# AG voor test H1: µ1 > µ0
qt(1-0.025, 604)

###################################################
# vraag 3

hercoc = data$hercoc

table (treat, hercoc)
Xsq <- chisq.test(treat,hercoc)
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
plot(los, time, main = "regression line",
     xlab = "los", ylab = "time",
     pch = 19, frame = FALSE)
abline(lm(time ~ los, data = mtcars), col = "blue")

cor.test(los, time, method="pearson")