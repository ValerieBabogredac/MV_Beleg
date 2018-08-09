setwd("/Users/babo/ownCloud/Studium/8_Semester/MV umweltstatistik /Belegarbeit/Beleg _Valerie")

library(carData)
library(car)
library(moments)

Guelpe <- read.table("Daten_Guelpe_12_13_17.txt", header=TRUE)
Daten <- na.omit(Guelpe[c(151:209, 215:244), c(3:4, 8:9, 15)])
write.table(Daten,file="export.txt")

# wir wollen schauen ob Daten sich statistisch signifikant unterscheiden
# dazu 
# Daten 2013 mit Strato 2 und 3
G13 <- read.table("Guelpe13.txt", header=TRUE)
attach(G13)

#Werte von Stratum 2 Jahr 2013 in eigene Tabelle
Str2_13 <- G13[c(1:35),]

summary(Str2_13$Bodenfeuchte_Prozent)
Stabw_Str_2_13 = sd(Str2_13$Bodenfeuchte_Prozent)
Interquartilbereich_Str_2_13 = IQR(Str2_13$Bodenfeuchte_Prozent)

#Tabelle_Str2_13 = data.frame(mit Ausreisser = c("MW", "Median","Sd", "QD"), )

#Schiefe Stratum 2 Jahr 2013:

Schiefe_Str_2_13 = skewness(Str2_13$Bodenfeuchte_Prozent)
# Ungleich 0, also Schief und größer als 1 = Schiefe ausgeprägt ; d.h Daten NICHT Normalverteilt; 


#Werte von Stratum 3 Jahr 2013 in eigene Tabelle
Str3_13 <- G13[c(36:56),]

summary(Str3_13$Bodenfeuchte_Prozent)
Stabw_Str_3_13 = sd(Str3_13$Bodenfeuchte_Prozent)
Interquartilbereich_Str_3_13 = IQR(Str3_13$Bodenfeuchte_Prozent)

#Schiefe Stratum 3 Jahr 2013:
Schiefe_Str_3_13 = skewness(Str3_13$Bodenfeuchte_Prozent)
# ungleich 0, also Schief aber kleiner als -1 = Schiefe nur gering ausgeprägt; daher kann man Normalverteilung annehmen


#Boxplot für Stratum 2 & 3 2013 - um zu schauen wie die Daten Verteilt sind und ob veile Außreiser 
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ursprungsdaten 2013", xlab="Stratum", ylab="Bodenfeuchte", col=c(4,2), horizontal = F)
abline(h=mean(Str2_13$Bodenfeuchte_Prozent), col=4)
abline(h=mean(Str3_13$Bodenfeuchte_Prozent), col=2)

#Ergebnis:
# Box:
#   Str2: 50% der Daten liegen in diesem Bereich -> Die meisten Bodenproben haben eine Feuchte zwischen ~31% - ~46%
#   Str3: 50% der Daten liegen in diesem Bereich -> Die meisten Bodenproben haben eine Feuchte zwischen ~62% - ~77%
# Median:
#   Str2: 39,4%  Str3: 70,88%
#Whisker:
#   Str2: min ~19% ; max: ~61%
#   Str3: min ~54% ; max: ~78%
#Quartile:
#   Str2: Q1 = ~31% ; Q2 = ~46%
#   Str3: Q1 = ~62% ; Q2 = ~77%
# leichte linksschiefe/rechtssteil, bei Str 2 mehr als bei Str3; evt aufgrund der Ausreißer; evt nicht Normalverteilt? 

#Histogramm Jahr 2013 - Stratum 2 - hier mit Ausreißern - um zu sehen wie Daten Verteilt sind 
hist(Str2_13$Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 Jahr 2013", xlab="Bodenfeuchte in %", col=4)
lines(density(Bodenfeuchte_Prozent), col=2, lwd=3)

#Quantil-Quantil Plot Jahr 2013 - Stratum 2 - um zu sehen wie Daten Verteilt sind 
qqnorm(Str2_13$Bodenfeuchte_Prozent, col=4, main="Normal Q-Q Plot Stratum 2 Jahr 2013")
qqline(Str2_13$Bodenfeuchte_Prozent, col=2)

#Shapiro-Wilk-Test auf Normalverteilung für Jahr 2013 - Strato 2
# Znächst Nullhypothese & Alternativhypothese aufstellen: 
#   H0: die Zufallsvariable „Bodenfeuchte“ folgt einer Normalverteilung
#   H1: die Zufallsvariable „Bodenfeuchte“ folgt keiner Normalverteilung
shapiro.test(Str2_13$Bodenfeuchte_Prozent) #nullhypothese wird verworfen weil p kleiner 0.05
#es wird hierbei eine Warscheinlichkeit von 95% genommen, also ist alpha automatisch 0,05
#nullhypothese wird verworfen weil p kleiner 0.05
#   -> p-Value kleiner als das festgelegte Signifikanzniveau 0,05 -> daher wird alternativhypothese angenommen; 
#       -> Daten sind NICHT Normalverteilt
# WAS NUN? -> Ausreißer ebtfernen und danach nochmal schauen ob Normalverteilt

#Histogramm Jahr 2013 - Stratum 3 - hier mit Ausreißern - um zu sehen wie Daten Verteilt sind 
hist(Str3_13$Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 3 Jahr 2013", xlab="Bodenfeuchte in %", col=2 )
lines(density(Bodenfeuchte_Prozent), col=4, lwd=3)

#Quantil-Quantil Plot Jahr 2013 - Stratum 3 - um zu sehen wie Daten Verteilt sind 
qqnorm(Str3_13$Bodenfeuchte_Prozent, col=2, main="Normal Q-Q Plot Stratum 3 Jahr 2013")
qqline(Str3_13$Bodenfeuchte_Prozent, col=4)

#Shapiro-Wilk-Test auf Normalverteilung für Jahr 2013 - Strato 3
# Znächst Nullhypothese & Alternativhypothese aufstellen: 
#   H0: die Zufallsvariable „Bodenfeuchte“ folgt einer Normalverteilung
#   H1: die Zufallsvariable „Bodenfeuchte“ folgt keiner Normalverteilung
shapiro.test(Str3_13$Bodenfeuchte_Prozent)
#Ergebnis:nullhypothese wird verworfen weil p kleiner 0.05
#   -> p-Value größer als das festgelegte Signifikanzniveau 0,05 -> daher wird nullhypothese angenommen; 
#       -> Daten sind Normalverteilt

#Ausreisserbehandlung 2013 (nur fur Stratum 2 erforderlich):
#Discarding Outliers 2013 <- Reihen mit Daten von Ausreisser werden geloescht
write.table(Str2_13, file="Str2_13.txt")
S2_13 <- read.table("Str2_13.txt", header=TRUE)

detach(G13)
attach(S2_13) #FEHLERMELDUNG

#Boxplot mit ausreißer Strato 2 Jahr 2013
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ursprungsdaten 2013", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(4))
abline(h=mean(Bodenfeuchte_Prozent), col=4)
summary(Bodenfeuchte_Prozent)

#Entfernen der Ausreißer; alles was über 44,95 ist (3.Quartil), soll raus genommen werden.
#   die 1,5 stehen für: 
bench1 <- 44.95+1.5*IQR(Bodenfeuchte_Prozent)
bench1
data1 <- S2_13[S2_13$Bodenfeuchte_Prozent<bench1,] #auschmeissen der aussreiser

detach(S2_13)

attach(data1)
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ergebnis nach dem ersten Verfahren Jahr 2013", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(4))
shapiro.test(data1$Bodenfeuchte_Prozent)
hist(Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 nach dem ersten Verfahren Jahr 2013", xlab="Bodenfeuchte in %", col=4 )
lines(density(Bodenfeuchte_Prozent), col=2, lwd=3)
qqnorm(Bodenfeuchte_Prozent, col=4, main="Normal Q-Q Plot S2 nach dem ersten Verfahren 2013")
qqline(Bodenfeuchte_Prozent, col=2)
write.table(data1, file="data1.txt")
detach(data1)

#Winsorizing 2013 <- Daten werden mit dem Wert der Bench-Mark getauscht
data2 <- S2_13
data2$Bodenfeuchte_Prozent[which(data2$Bodenfeuchte_Prozent>bench1)] <- bench1
attach(data2)
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ergebnis nach dem zweiten Verfahren Jahr 2013", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(4))
shapiro.test(data2$Bodenfeuchte_Prozent)
hist(Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 nach dem zweiten Verfahren Jahr 2013", xlab="Bodenfeuchte in %", col=4 )
lines(density(Bodenfeuchte_Prozent), col=2, lwd=3)
qqnorm(Bodenfeuchte_Prozent, col=4, main="Normal Q-Q Plot S2 nach dem zweiten Verfahren 2013")
qqline(Bodenfeuchte_Prozent, col=2)
write.table(data2, file="data2.txt")
detach(data2)

#Variable Transformation 2013 <- eine andere Interpretation ist erforderlich
#wegen der Log-Funktion
data3 <- S2_13
data3$Bodenfeuchte_Prozent <- log(data3$Bodenfeuchte_Prozent)
attach(data3)
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ergebnis nach dem dritten Verfahren Jahr 2013", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(4))
shapiro.test(data3$Bodenfeuchte_Prozent)
hist(Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 nach dem dritten Verfahren Jahr 2013", xlab="Log-Funktion von Bodenfeuchte in %", col=4 )
lines(density(Bodenfeuchte_Prozent), col=2, lwd=3)
qqnorm(Bodenfeuchte_Prozent, col=4, main="Normal Q-Q Plot S2 nach dem dritten Verfahren 2013")
qqline(Bodenfeuchte_Prozent, col=2)
detach(data3)

# ------------

#Varianzen Jahr 2013 Stratum 2 und 3 anschauen
#   Um festzustellen, ob eine Differenz zwischen Streubreiten (Varianzen) statistisch signifikant ist, 
#   führen Sie einen der folgenden Vorgänge aus:
#   - Führen Sie einen Test auf Varianzen bei zwei Stichproben aus, wenn nur zwei Gruppen vorhanden sind.
# ODER MIT Levene Test:
leveneTest(Str2_13$Bodenfeuchte_Prozent, Str2_13$Bodenfeuchte_Prozent)

#Varianz ders Jahres 2013 Stratum 2 
var(Bodenfeuchte_Prozent[Stratum=="2"])

#Varianz ders Jahres 2013 Stratum 3
var(Bodenfeuchte_Prozent[Stratum=="3"])

#------------

#SOLLTE DOCH ERST NACH AUSREISSERBEHANDLUNG GEMACHT WERDEN!!!
#Wir vermuten, dass wir nach der Entfernung von Ausreissern eine Normalverteilung
#fur Stratum 2 bekommen werden, deswegen verwenden wir den t-Test:
t.test(Bodenfeuchte_Prozent~Stratum, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
detach(G13)


#Da die Daten normalverteilt sind, wenn wir die Aussreisser begegnet haben,
#verwenden wir den zwei-seitigen t-Test:
#2013:
#Datensatz nach "Discarding Ourliers"
G13_1 <- read.table("G13_neu1.txt", header=TRUE)
attach(G13_1)
S2_13_1 <- G13_1[c(1:33),]
S3_13_1 <- G13_1[c(34:54),]
boxplot(Bodenfeuchte_Prozent~Stratum, main="Datensatz nach dem ersten Verfahren Jahr 2013", xlab="Stratum", ylab="Bodenfeuchte", col=c(4,2))
abline(h=mean(S2_13_1$Bodenfeuchte_Prozent), col=4)
abline(h=mean(S3_13_1$Bodenfeuchte_Prozent), col=2)

var(Bodenfeuchte_Prozent[Stratum=="2"])
var(Bodenfeuchte_Prozent[Stratum=="3"])
t.test(Bodenfeuchte_Prozent~Stratum, mu=0, alt="two.sided", conf=0.95, var.eq=TRUE, paired=F)
detach(G13_1)

#Datensatz nach "Winsorizing"
G13_2 <- read.table("G13_neu2.txt", header=TRUE)
attach(G13_2)
S2_13_2 <- G13_2[c(1:35),]
S3_13_2 <- G13_2[c(36:56),]
boxplot(Bodenfeuchte_Prozent~Stratum, main="Datensatz nach dem zweiten Verfahren Jahr 2013", xlab="Stratum", ylab="Bodenfeuchte", col=c(4,2))
abline(h=mean(S2_13_2$Bodenfeuchte_Prozent), col=4)
abline(h=mean(S3_13_2$Bodenfeuchte_Prozent), col=2)

var(Bodenfeuchte_Prozent[Stratum=="2"])
var(Bodenfeuchte_Prozent[Stratum=="3"])
t.test(Bodenfeuchte_Prozent~Stratum, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
detach(G13_2)


#2017:
G17 <- read.table("Guelpe17.txt", header=TRUE)
attach(G17)
Str2_17 <- G17[c(1:18),]
Str3_17 <- G17[c(19:30),]
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ursprungsdaten 2017", xlab="Stratum", ylab="Bodenfeuchte", col=c(3,6))
abline(h=mean(Str2_17$Bodenfeuchte_Prozent), col=3)
abline(h=mean(Str3_17$Bodenfeuchte_Prozent), col=6)

shapiro.test(Str2_17$Bodenfeuchte_Prozent)
hist(Str2_17$Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 Jahr 2017", xlab="Bodenfeuchte in %", col=3 )
lines(density(Bodenfeuchte_Prozent), col=6, lwd=3)
qqnorm(Str2_17$Bodenfeuchte_Prozent, col=3, main="Normal Q-Q Plot Stratum 2 Jahr 2017")
qqline(Str2_13$Bodenfeuchte_Prozent, col=6)

shapiro.test(Str3_17$Bodenfeuchte_Prozent)
hist(Str3_17$Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 3 Jahr 2017", xlab="Bodenfeuchte in %", col=6 )
lines(density(Bodenfeuchte_Prozent), col=3, lwd=3)
qqnorm(Str3_17$Bodenfeuchte_Prozent, col=6, main="Normal Q-Q Plot Stratum 3 Jahr 2017")
qqline(Str3_17$Bodenfeuchte_Prozent, col=3)

var(Bodenfeuchte_Prozent[Stratum=="2"])
var(Bodenfeuchte_Prozent[Stratum=="3"])

#Wir vermuten, dass wir nach der Entfernung von Ausreissern eine Normalverteilung
#fur Stratum 2 bekommen werden, deswegen verwenden wir den t-Test:
t.test(Bodenfeuchte_Prozent~Stratum, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
detach(G17)

#Ausreisserbehandlung 2017 (nur fur Stratum 2 erforderlich):
#Discarding Outliers 2017 <- Reihen mit Daten von Ausreisser werden geloescht
write.table(Str2_17, file="Str2_17.txt")
S2_17 <- read.table("Str2_17.txt", header=TRUE)
attach(S2_17)
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ursprungsdaten 2017", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(3))
summary(Bodenfeuchte_Prozent)
bench2 <- 43.05+1.5*IQR(Bodenfeuchte_Prozent)
bench2
data4 <- S2_17[S2_17$Bodenfeuchte_Prozent<bench2,]
detach(S2_17)

attach(data4)
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ergebnis nach dem ersten Verfahren Jahr 2017", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(3))
shapiro.test(data4$Bodenfeuchte_Prozent)
hist(Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 nach dem ersten Verfahren Jahr 2017", xlab="Bodenfeuchte in %", col=3 )
lines(density(Bodenfeuchte_Prozent), col=6, lwd=3)
qqnorm(Bodenfeuchte_Prozent, col=3, main="Normal Q-Q Plot S2 nach dem ersten Verfahren 2017")
qqline(Bodenfeuchte_Prozent, col=6)
write.table(data4, file="data4.txt")
detach(data4)

#Winsorizing 2017 <- Daten werden mit dem Wert der Bench-Mark getauscht
data5 <- S2_17
data5$Bodenfeuchte_Prozent[which(data5$Bodenfeuchte_Prozent>bench2)] <- bench2
attach(data5)
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ergebnis nach dem zweiten Verfahren Jahr 2017", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(3))
shapiro.test(data5$Bodenfeuchte_Prozent)
hist(Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 nach dem zweiten Verfahren Jahr 2017", xlab="Bodenfeuchte in %", col=3 )
lines(density(Bodenfeuchte_Prozent), col=6, lwd=3)
qqnorm(Bodenfeuchte_Prozent, col=3, main="Normal Q-Q Plot S2 nach dem zweiten Verfahren 2017")
qqline(Bodenfeuchte_Prozent, col=6)
write.table(data5, file="data5.txt")
detach(data5)

#Variable Transformation 2017 <- eine andere Interpretation ist erforderlich
#wegen der Log-Funktion
data6 <- S2_17
data6$Bodenfeuchte_Prozent <- log(data6$Bodenfeuchte_Prozent)
attach(data6)
boxplot(Bodenfeuchte_Prozent~Stratum, main="Ergebnis nach dem dritten Verfahren Jahr 2017", xlab="Stratum 2", ylab="Bodenfeuchte", col=c(3))
shapiro.test(data6$Bodenfeuchte_Prozent)
hist(Bodenfeuchte_Prozent, freq=F, main="Histogram Stratum 2 nach dem dritten Verfahren Jahr 2017", xlab="Log-Funktion von Bodenfeuchte in %", col=3 )
lines(density(Bodenfeuchte_Prozent), col=6, lwd=3)
qqnorm(Bodenfeuchte_Prozent, col=3, main="Normal Q-Q Plot S2 nach dem dritten Verfahren 2017")
qqline(Bodenfeuchte_Prozent, col=6)
detach(data6)

#Da die Daten normalverteilt sind, wenn wir die Aussreisser begegnet haben,
#verwenden wir den zwei-seitigen t-Test:
#2017:
#Datensatz nach "Discarding Ourliers"
G17_1 <- read.table("G17_neu1.txt", header=TRUE)
attach(G17_1)
S2_17_1 <- G17_1[c(1:15),]
S3_17_1 <- G17_1[c(16:27),]
boxplot(Bodenfeuchte_Prozent~Stratum, main="Datensatz nach dem ersten Verfahren Jahr 2017", xlab="Stratum", ylab="Bodenfeuchte", col=c(3,6))
abline(h=mean(S2_17_1$Bodenfeuchte_Prozent), col=3)
abline(h=mean(S3_17_1$Bodenfeuchte_Prozent), col=6)

var(Bodenfeuchte_Prozent[Stratum=="2"])
var(Bodenfeuchte_Prozent[Stratum=="3"])
t.test(Bodenfeuchte_Prozent~Stratum, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
detach(G17_1)

#Datensatz nach "Winsorizing"
G17_2 <- read.table("G17_neu2.txt", header=TRUE)
attach(G17_2)
S2_17_2 <- G17_2[c(1:18),]
S3_17_2 <- G17_2[c(19:30),]
boxplot(Bodenfeuchte_Prozent~Stratum, main="Datensatz nach dem zweiten Verfahren Jahr 2017", xlab="Stratum", ylab="Bodenfeuchte", col=c(3,6))
abline(h=mean(S2_17_2$Bodenfeuchte_Prozent), col=3)
abline(h=mean(S3_17_2$Bodenfeuchte_Prozent), col=6)

var(Bodenfeuchte_Prozent[Stratum=="2"])
var(Bodenfeuchte_Prozent[Stratum=="3"])
t.test(Bodenfeuchte_Prozent~Stratum, mu=0, alt="two.sided", conf=0.95, var.eq=FALSE, paired=F)
detach(G17_2)

