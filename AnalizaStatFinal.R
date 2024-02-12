setwd('C:/Users/karol/Pulpit/MwT/MwT sem7/Inzynierka/BPMStatAnalysis') 
pomiary <- read.csv("pomiary.csv",header=TRUE,sep =";") 

print(pomiary) 
n <- dim(pomiary)[1] 
boxplot(pomiary)
summary(pomiary$A15)

shapiro.test(pomiary$D0) 
shapiro.test(pomiary$D5)
shapiro.test(pomiary$D15)
shapiro.test(pomiary$A0)
shapiro.test(pomiary$A5)
shapiro.test(pomiary$A15)

czujnik <- c(rep('D0',n), rep('D5',n), rep('D15',n), 
             rep('A0',n), rep('A5',n), rep('A15',n))  
BPM <- c(pomiary$D0, pomiary$D5, pomiary$D15, 
         pomiary$A0, pomiary$A5, pomiary$A15) 
listaPomiary <- data.frame(czujnik,BPM) 

bartlett.test(BPM ~ czujnik) 

listaPomiaryD <- listaPomiary[c(1:(3*n)),] 
listaPomiaryA <- listaPomiary[c((3*n+1):(6*n)),]

digitalSensor.aov <- aov(BPM ~ czujnik, data = listaPomiaryD)
summary(digitalSensor.aov)

TukeyHSD(digitalSensor.aov) 
plot(TukeyHSD(digitalSensor.aov))

analogSensor.aov <- aov(BPM ~ czujnik, data = listaPomiaryA)
summary(analogSensor.aov)

TukeyHSD(analogSensor.aov) 
plot(TukeyHSD(analogSensor.aov))

listaPomiary0 <- listaPomiary[c(1:n,(3*n+1):(4*n)),] 
listaPomiary5 <- listaPomiary[c((n+1):(2*n),(4*n+1):(5*n)),]
listaPomiary15 <- listaPomiary[c((2*n+1):(3*n),(5*n+1):(6*n)),]

t.test(BPM ~ czujnik, data = listaPomiary0)
t.test(BPM ~ czujnik, data = listaPomiary5)
t.test(BPM ~ czujnik, data = listaPomiary15)

