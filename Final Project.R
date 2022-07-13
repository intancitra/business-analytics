setwd('E:/Bisnis Analitik')
df0 <- read.csv('E:/Bisnis Analitik/Dataset_3.csv',header=T,sep=',')
data <- df0[-9673,] #Remove Corrupt Data
data[8349,"Tipe"] <- "Free" #Harga Nol, Free

library(ggplot2)
library(stringr)
library(dplyr)
library(e1071)
library(catools)
library(ggcorrplot)
library(caret)
library(corrgram)

#Density Plot
ggplot(data,aes(x=Penilaian))+
  geom_histogram(aes(y=..density..),colour="#E2B659", fill="#FFEECA")+
  geom_density(alpha=0.1,fill="#00D4B0",trim=F, colour = "#005B4C") + 
  ggtitle("Persebaran Penilaian") + theme_classic() + 
  theme(plot.title = element_text(face = "bold", size = 15, color = "#3a3e3f"))

data$Penilaian[is.na(data$Penilaian)] <- median(data$Penilaian, na.rm=T)
sum(is.na(data))

data$Kategori=as.factor(data$Kategori)
data$Tipe=as.factor(data$Tipe)
data$Penilaian_Konten=as.factor(data$Penilaian_Konten)
summary(data)

data$Installs <- str_replace_all(data$Installs, "[+]","")
data$Installs <- str_replace_all(data$Installs, ",","")
data$Installs = as.numeric(data$Installs)

data$Harga <- str_replace_all(data$Harga, "[$]","")
data$Harga = as.numeric(data$Harga)

data$Ukuran <- str_replace_all(data$Ukuran, "0", "0k")
which(data$Ukuran == "0k")
measure <- substr(data$Ukuran,nchar(data$Ukuran),nchar(data$Ukuran))
measure = as.data.frame(measure)
data$Ukuran <- str_replace_all(data$Ukuran, "Varies with device", "0k")

data$Ukuran <- str_replace_all(data$Ukuran, "[M]","")
data$Ukuran <- str_replace_all(data$Ukuran, "[K]","")
data$Ukuran <- str_replace_all(data$Ukuran, "[k]","")
data$Ukuran = as.numeric(data$Ukuran)

data$Ukuran[measure == 'k' | measure == 'K'] = data$Ukuran[measure == 'k' | measure == 'K'] * 1000
data$Ukuran[measure == 'M'] = data$Ukuran[measure == 'M'] * 1000000

data$Ukuran[measure == "e"] = median(data$Ukuran[measure == 'k' | measure == 'K' | measure == 'M'])

data$Kategori = as.factor(data$Kategori)
levels(data$Kategori)
table(data$Kategori)

data$Kategori <- str_replace_all(data$Kategori, "_"," ")

library(tidyverse)
data = data[order(-data$Ulasan),]
data = data[!duplicated(data$App),]


forlabel <- data %>%
  group_by(Kategori) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#JUMLAH APK PER KAT
#install.packages("ggthemes")
library(ggthemes)
ggplot(forlabel, aes(x = reorder(Kategori, count),y = count, fill = Kategori)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ggtitle("Jumlah Aplikasi Berdasarkan Kategori",)  + 
  geom_text(aes(label = count), vjust = 0.4, size = 2.5, hjust = -0.2) + 
  labs(x = "Kategori", y = "Jumlah Aplikasi") + 
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", size = 15, color = "#3a3e3f"),
        legend.position = "none") + 
  scale_fill_manual(values = c("#ff6f91", "#ff808c", "#ff928b", "#ffa28e",
                               "#ffb295", "#ffbb94", "#ffc594", "#ffcf95", 
                               "#ffd88b", "#ffe181", "#ffec78", "#f9f871",
                               "#ff6f91", "#ff808c", "#ff928b", "#ffa28e",
                               "#ffb295", "#ffbb94", "#ffc594", "#ffcf95", 
                               "#ffd88b", "#ffe181", "#ffec78", "#f9f871",
                               "#ff6f91", "#ff808c", "#ff928b", "#ffa28e",
                               "#ffb295", "#ffbb94", "#ffc594", "#ffcf95", 
                               "#ffd88b", "#ffe181", "#ffec78", "#f9f871")) 


#PERSEBARAN 
bayar <- data %>%
  group_by(Tipe) %>%
  summarise(count = n()) 

ggplot(bayar, aes(x = "", y = count, fill = Tipe)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1, label = paste0(round(count/sum(count)*100,1),"%")),
            position = position_stack(vjust = 0.5)) + 
  ggtitle("Persentase Aplikasi Berbayar") + 
  labs(fill = "Tipe Aplikasi", 
       x = NULL, 
       y = NULL) + 
  scale_fill_manual(values = c("#33EDC6","#F9F871")) + theme_void() +
  theme(plot.title = element_text(face = "bold", size = 15, color = "#3a3e3f"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#Konten

library(ggpubr)
kontenlabel <- data %>%
  group_by(Penilaian_Konten) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

ggdotchart(kontenlabel, x = "Penilaian_Konten", y = "count",
           rotate = TRUE, ggtheme = theme_pubclean(), sorting = "descending",
           add = "segments", label = kontenlabel$count,
           dot.size = 9, font.label = list(color = "black",size = 8,vjust = 0.5,
                                            hjust = 0.5),
           add.params = list(color = "#A87565", size = 3),
           color = "Penilaian_Konten",
           palette = c("#c6d7b9","#f6b9ad","#ee6f68","#f68f3c","#5e8d5a","#FFBFC3"),
           xlab = "Penilaian Konten", ylab = "Jumlah Aplikasi", 
           main = "Persebaran Penilaian Konten",
           legend = "none")

class(data$Harga)

#PALING POPULER 
pop <- data %>% 
  filter(Installs == 1e+09) 

#Menambahkan Selisih Hari
data$Terakhir_Diperbarui <- as.Date(data$Terakhir_Diperbarui,
                                    format = "%d-%b-%y")

data = mutate(data, Selisih_Hari = Sys.Date()-Terakhir_Diperbarui)

data_used <- data %>%
  select(Penilaian, Ulasan, Installs, Selisih_Hari, Harga, Ukuran)

data_used$Selisih_Hari <- as.numeric(data_used$Selisih_Hari)

data_used_naive <- data

data_used_naive$Selisih_Hari <- as.numeric(data_used_naive$Selisih_Hari)

#Matriks Korelasi
corrgram(data_used_naive %>% select(where(is.numeric)), order=TRUE,
         upper.panel=panel.cor, main="Google Play Store App Classification")

##Naive Bayes##
#Partisi Data
set.seed(120)
index <- createDataPartition(data_used_naive$Penilaian, p=0.80, list=FALSE)
# select 80% of the data for Training
training <- data_used_naive[index,]
dim(training)
# use the remaining 80% of data to testing the models
testing <- data_used_naive[-index,]
dim(testing)

set.seed(120) # Setting Seed
train = training %>%
  mutate(Popular = "Popularity")

train$Popular[which(train$Penilaian >= 3.5)] = "Popular"
train$Popular[which(train$Penilaian < 3.5 & train$Penilaian >= 2.5)] = "Moderate" 
train$Popular[which(train$Penilaian < 2.5)] = "Unpopular" 

classifier_cl <- naiveBayes(Popular ~ ., data = train)
classifier_cl

# Predicting on training data'
y_pred <- predict(classifier_cl, newdata = train)
# Confusion Matrix
cm <- table(train$Popular, y_pred)
cm
# Model Evaluation
confusionMatrix(cm)

test = testing %>%
  mutate(Popular = "Popularity")

test$Popular[which(test$Penilaian >= 3.5)] = "Popular"
test$Popular[which(test$Penilaian < 3.5 & test$Penilaian >= 2.5)] = "Moderate" 
test$Popular[which(test$Penilaian < 2.5)] = "Unpopular" 

# Predicting on test data'
y_pred2 <- predict(classifier_cl, newdata = test)

# Confusion Matrix
cm2 <- table(test$Popular, y_pred2)
cm2

# Model Evaluation
confusionMatrix(cm2)

##Decision Tree##
data_dt = data %>%
  filter(Kategori == c("FINANCE", "BUSINESS", "SHOPPING"))
#Partisi Data
set.seed(120)
index <- createDataPartition(data_dt$Installs, p=0.80, list=FALSE)
# select 80% of the data for Training
training_dt <- data_dt[index,]
dim(training_dt)
# use the remaining 80% of data to testing the models
testing_dt <- data_dt[-index,]
dim(testing_dt)

train_dt = training_dt %>%
  select(Installs, Penilaian, Kategori)

##Decision Tree##
library(rpart.plot)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(120)
dtree_fit <- train(Kategori ~., data = train_dt, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

test_dt = testing_dt %>%
  select(Installs, Penilaian, Kategori)

y_pred_dt <- predict(dtree_fit, newdata = test_dt)

cm <- table(test_dt$Kategori, y_pred_dt)

confusionMatrix(cm)
