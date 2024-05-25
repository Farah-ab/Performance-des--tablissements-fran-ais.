library(readxl)
library(ggplot2)
library(tidyr)
library(plot3D)
library(dplyr)

###################################
#Partie 1:Statis descriptives
###################################


file_path <- "/Users/farahaboucha/Desktop/Base de données.xlsx"
data <- read_excel(file_path)
colnames <- colnames(data)
print(colnames)

drop <- c(3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
subdata <- data[, -drop]

default_value <- "0"
subsubdata <- subdata %>%
  mutate_all(~coalesce(as.character(.), default_value)) %>%
  mutate_all(~ifelse(. %in% c("eff trop petit", "eff. trop petit"), 0, .)) %>%
  mutate_at(vars(-c(1, 2, 3)), as.numeric)

print(subsubdata)

subset_data <- data %>%
  slice(40)

colnames <- colnames(subsubdata)
print(colnames)

summary <- summary(subdata)
summary


###########################
#Partie 2: Graphiques
###########################

file_path <- "/Users/farahaboucha/Desktop/BDD Farah.xlsx"
datafarah <- read_excel(file_path)
colnames <- colnames(datafarah)
print(colnames)

#(X)Budget par étudiant - (Y) Taux d'insertion

budginsertion <- datafarah[complete.cases(datafarah[, c(1, 50, 22)]), c(1, 50, 22)]

UJM <- budginsertion[budginsertion$libellé == "Université Jean Monnet", ]
ggplot(budginsertion, aes(x = `Budg_etu19-20`, y = `tx_insertion_2020`)) +
  geom_point(shape = 16, 
             size = ifelse(budginsertion$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(budginsertion$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Budget par étudiant ", y = "Taux d'insertion ", title = " Relation entre le budget par étudiant \net le taux d'insertion à 18 mois") +
  theme_minimal()+
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),)


#(X)Budget par étudiant - (Y) Taux d’emploi

budgemploi <- datafarah[complete.cases(datafarah[, c(1, 3, 50)]), c(1, 3, 50)]

UJM <- budgemploi[budgemploi$libellé == "Université Jean Monnet", ]
ggplot(budgemploi, aes(x = `Budg_etu19-20`, y = `tx_emploi_18mois_2020`)) +
  geom_point(shape = 16, 
             size = ifelse(budgemploi$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(budgemploi$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Budget par étudiant ", y = "Taux d'emploi en 2020", title = "Relation entre le budget par étudiant\n et le taux d'emploi en 18 mois") +
  theme_minimal()+
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),)


#(X)Budget par étudiant - (Y) Taux de réussite license

budgreul <- datafarah[complete.cases(datafarah[, c(1, 50, 28)]), c(1, 50, 28)]

UJM <- budgreul[budgreul$libellé == "Université Jean Monnet", ]
ggplot(budgreul, aes(x = `Budg_etu19-20`, y = `reussite_licence_3ans`)) +
  geom_point(shape = 16, 
             size = ifelse(budgreul$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(budgreul$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Budget par étudiant ", y = "Taux de réussite en license ", title = " Budget par étudiant par rapport au taux de réussite en license en 2020") +
  theme_minimal()+
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),)


#(X)Budget par étudiant - (Y) Taux de réussite master

budgreum <- datafarah[complete.cases(datafarah[, c(1, 50, 41)]), c(1, 50, 41)]

UJM <- budgreum[budgreum$libellé == "Université Jean Monnet", ]
ggplot(budgreum, aes(x = `Budg_etu19-20`, y = `reussite_master`)) +
  geom_point(shape = 16, 
             size = ifelse(budgreum$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(budgreum$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Budget par étudiant ", y = "Taux de réussite en master", title = " Budget par étudiant par rapport au taux de réussite en master en 2020") +
  theme_minimal()+
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),)


#(X)Nombre d’enseignants titulaires - (Y) Taux d'insertion

nbenseiginsertion <- datafarah[complete.cases(datafarah[, c(1, 54, 22)]), c(1, 54, 22)]

UJM <- nbenseiginsertion[nbenseiginsertion$libellé == "Université Jean Monnet", ]
ggplot(nbenseiginsertion , aes(x = `Nb_ens_tit19-20`, y = `tx_insertion_2020`)) +
  geom_point(shape = 16, 
             size = ifelse(nbenseiginsertion$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(nbenseiginsertion$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Nombre d'enseignants titulaires", y = "Taux d'insertion ", title = "Nombre d'enseignants titulaires par rapport au taux d'insertion en 2020") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),)


#(X)Nombre d’enseignants titulaires - (Y) Taux d'emploi


nbenseignemploi <- datafarah[complete.cases(datafarah[, c(1, 3, 54)]), c(1, 3, 54)]

UJM <- nbenseignemploi[nbenseignemploi$libellé == "Université Jean Monnet", ]
ggplot(nbenseignemploi, aes(x = `Nb_ens_tit19-20`, y = `tx_emploi_18mois_2020`)) +
  geom_point(shape = 16, 
             size = ifelse(nbenseignemploi$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(nbenseignemploi$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Nombre d'enseignants titulaires", y = "Taux d'emploi ", title = "Nombre d'enseignants titulaires par rapport au taux d'emploi en 2020") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5), 
  )
  

#(X)Nombre d’enseignants titulaires - (Y) Taux de réussite license

nbenseignréu <- datafarah[complete.cases(datafarah[, c(1, 28, 54)]), c(1, 28, 54)]

UJM <- nbenseignréu[nbenseignréu$libellé == "Université Jean Monnet", ]
ggplot(nbenseignréu, aes(x = `Nb_ens_tit19-20`, y = `reussite_licence_3ans`)) +
  geom_point(shape = 16, 
             size = ifelse(nbenseignréu$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(nbenseignréu$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Nombre d'enseignants titulaires", y = "Taux de réussite en license ", title = "Nombre d'enseignants titulaires par rapport au taux de réussite en license en 2020") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5), 
  )


#Nombre d’étudiants en licence - Nombre de licences proposées

nbetulicense <- datafarah[complete.cases(datafarah[, c(1, 63, 58)]), c(1, 63, 58)]

UJM <- nbetulicense[nbetulicense$libellé == "Université Jean Monnet", ]
ggplot(nbetulicense, aes(x = `nb_licences`, y = `Nb_etu_lic19-20`)) +
  geom_point(shape = 16, 
             size = ifelse(nbetulicense$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(nbetulicense$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Nombre de licences proposées ", y = "Nombre d’étudiants en licence", title = "Relation entre le nombre d’étudiants en licence \net le nombre de licences proposées en 2020") +
  theme_minimal()+
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),)


#Nombre d’étudiants en master - Nombre de masters proposés

nbetumast <- datafarah[complete.cases(datafarah[, c(1, 64, 62)]), c(1, 64, 62)]

UJM <- nbetumast[nbetumast$libellé == "Université Jean Monnet", ]
ggplot(nbetumast, aes(x = `nb_masters`, y = `Nb_etu_mas19-20`)) +
  geom_point(shape = 16, 
             size = ifelse(nbetumast$libellé == "Université Jean Monnet", 3, 1), 
             color = ifelse(nbetumast$libellé == "Université Jean Monnet", "red", "black")) + 
  geom_text(data = UJM, aes(label = "UJM"), vjust = -3, size = 4, color = "red", fontface = "bold") +   
  geom_smooth(method = "lm", se = FALSE, col = "palegreen3", lwd = 1, linetype = "dashed") +  
  labs(x = "Nombre d’étudiants en master", y = "Nombre de masters proposés", title = "Relation entre le nombre d’étudiants en master \net le nombre de masters proposés en 2020") +
  theme_minimal()+
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),)