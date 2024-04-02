# Installation des packages

install.packages('dplyr')
install.packages('VIM')
install.packages('corrplot')
install.packages('ggplot2')
install.packages('MASS')
install.packages('cluster')
install.packages('factoextra')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('FactoMineR')
install.packages('caret')
install.packages('randomforest')
install.packages('microbenchmark')
install.packages('ipred')

# IMPORTATION DES BASES 

# Changer le directory 
setwd("~/MoSEF 2022/Data Mining/Projet_Ferhat_Sefrioui_Yevniyeva")

champs = read.csv("champs.csv")
head(champs)

matches = read.csv("matches.csv")
head(matches)

participants = read.csv("participants.csv")
head(participants)

nrow(participants)

stats1 = read.csv("stats1.csv")
head(stats1)

stats2 = read.csv("stats2.csv")
head(stats2)

######################### JOINTURES ##########################################

nrow(stats1)
nrow(stats2)

stats = rbind(stats1, stats2)
nrow(stats)

df <- merge(x=participants, y=stats, by="id", all.x=TRUE)

head(df)

nrow(df)
ncol(df)

df <- merge(x=df, y=champs, by.x="championid", by.y="id", all.x=TRUE)

nrow(df)
ncol(df)

df <- merge(x=df, y=matches, by.x="matchid", by.y="id", all.x=TRUE)

nrow(df)
ncol(df)

head(df)

colnames(df)

########################################################################

final_position <- function(row){
  if((row['role']=='DUO_SUPPORT')|(row['role']=='DUO_CARRY')){
    return(row['role'])
  }
  else{
    return(row['position'])
  }
}

team_player_fun <- function(x){
  if(x<=5){
    return('1')
  }
  else{
    return('2')
  }
}

df$adjposition = apply(X = df, MARGIN = 1, FUN = final_position)
df$team = lapply(df['player'], team_player_fun)
df$team_role = paste(df$team , ' - ' , df$adjposition)

library(dplyr)

df <- select(df, c('id', 'matchid', 'player', 'name', 'adjposition', 'team_role', 
             'win', 'kills', 'deaths', 'assists', 'turretkills','totdmgtochamp', 'totheal', 
             'totminionskilled', 'goldspent', 'totdmgtaken', 'inhibkills', 'pinksbought', 'wardsplaced', 
             'duration', 'platformid', 'seasonid', 'version'))

head(df, 10)

df_v <-data.frame(df)

# On crÈe un boucle pour supprimmer les variables qui ne contiennennt qu'une seule modalitÈ (aucun pouvoir pred)

# Et on affiche les modalitÈs des variables restantes

for (i in colnames(df_v)){
  if (length(df[i]) == 1) {
    select(df_v, -c(i))
  }
  print(paste("La variable", colnames(df[i]), 
              'a', length(unique(df[[i]])), 'modalitÈs'))
}

# VALEURS MANQUANTES

print(paste("Il y a", sum(is.na(df_v)), "valeurs manquantes"))


# Visualisation des valeurs manquantes et structure

library(VIM)
res<-summary(aggr(df_v, sortVar=TRUE))$combinations

head(res[rev(order(res[,2])),])

df_v <- na.omit(df_v)

# CrÈation de VALEURS SEUILS (on a vu que les wards placÈes on beaucoup trop de modalitÈs)

ward_upperlimit <- function(x) {
  if(x<30){
    return(x)
  }
  else{
    return('30')
  }
}

ward_lowerlimit <- function(x) {
  if(x>0){
    return(x)
  }
  else{
    return('0')
  }
}

df_v$wardsplaced <- lapply(X = df_v$wardsplaced, FUN = ward_upperlimit)
df_v$wardsplaced <- lapply(X = df_v$wardsplaced, FUN = ward_lowerlimit)

# DurÈe moyenne d'une partie

mean(df_v$duration)

# Etude des corrÈlations pour des games entiËres

df_corr <- select_if(df_v, is.numeric)
df_corr <- select(df_corr, -c('id', 'matchid', 'player', 'seasonid'))

library(corrplot)
m<-cor(df_corr)
corrplot(m,method='number')

# Pour des games de moins de 25min (rapide)

df_corr_u25 <- select_if(df_v, is.numeric)
df_corr_u25 <- df_corr_u25[df_corr_u25$duration<=1500,]
df_corr_u25 <- select(df_corr_u25, -c('id', 'matchid', 'player', 'seasonid'))

library(corrplot)
m_u25<-cor(df_corr_u25)
corrplot(m_u25,method='number')

# Pour des games de plus de 40min (longues)

df_corr_u40 <- select_if(df_v, is.numeric)
df_corr_u40 <- df_corr_u40[df_corr_u40$duration>2400,]
df_corr_u40 <- select(df_corr_u40, -c('id', 'matchid', 'player', 'seasonid'))

library(corrplot)
m_u40<-cor(df_corr_u40)
corrplot(m_u40,method='number')

### Taux de victoire par champion

df_win_rate <- df_v

library(dplyr)
df_win_rate <- df_win_rate %>% 
  group_by(name) %>% 
  summarise(win = sum(win, na.rm = TRUE),
            matches = n(),
            kills = mean(kills, na.rm = TRUE),
            deaths = mean(deaths, na.rm = TRUE),
            assists = mean(assists, na.rm = TRUE)) %>%
  ungroup()

# CrÈation de la variable win_rate

df_win_rate$win_rate = df_win_rate$win /  df_win_rate$matches * 100 

# CrÈation de la variable KDA

df_win_rate$KDA = df_win_rate$kills + df_win_rate$assists / df_win_rate$deaths

# Et on trie par win_rate dÈcroissant pour effectuer un classement

df_win_rate <- df_win_rate[order(df_win_rate$win_rate, decreasing = TRUE),]


winratehead <- head(df_win_rate, 10)
winratehead

winratetail <- tail(df_win_rate, 10)
winratetail

#Sortie des tables en latex
#library(xtable)
#print(xtable(winratehead, type = "latex"), file = "winratehead.tex")
#print(xtable(winratetail, type = "latex"), file = "winratetail.tex")


# Scatterplot des champions par win_rate et KDA

library(ggplot2)

mid <- mean(df_win_rate$KDA)
ggplot(df_win_rate, aes(x=matches, y=win_rate, color=KDA)) + 
  geom_point() + geom_text(aes(label=name)) + scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                                                                    high = "red", space = "Lab" )


# On peut dÈcomposer par poste

df_win_rate <- df_v

library(dplyr)
df_win_rate <- df_win_rate %>% 
  group_by(name, adjposition) %>% 
  summarise(win = sum(win, na.rm = TRUE),
            matches = n(),
            kills = mean(kills, na.rm = TRUE),
            deaths = mean(deaths, na.rm = TRUE),
            assists = mean(assists, na.rm = TRUE)) %>%
  ungroup()

df_win_rate$win_rate = df_win_rate$win /  df_win_rate$matches * 100

df_win_rate$KDA = df_win_rate$kills + df_win_rate$assists / df_win_rate$deaths

df_win_rate <- df_win_rate[order(df_win_rate$win_rate, decreasing = TRUE),]

winrateadjhead <- head(df_win_rate, 10)
winrateadjhead

winrateadjtail <- tail(df_win_rate, 10)
winrateadjtail

#Sortie des tables en latex
#library(xtable)
#print(xtable(winrateadjhead, type = "latex"), file = "winrateadjhead.tex")
#print(xtable(winrateadjtail, type = "latex"), file = "winrateadjtail.tex")

# C'est bien mais c'est biaisÈ par le nombre de matches dont on va imposer un minimum de 100

df_win_rate <- df_v

library(dplyr)
df_win_rate <- df_win_rate %>% 
  group_by(name, adjposition) %>% 
  summarise(win = sum(win, na.rm = TRUE),
            matches = n(),
            kills = mean(kills, na.rm = TRUE),
            deaths = mean(deaths, na.rm = TRUE),
            assists = mean(assists, na.rm = TRUE)) %>%
  ungroup()


df_win_rate <- df_win_rate[df_win_rate$matches >= 100,]

df_win_rate$win_rate = df_win_rate$win /  df_win_rate$matches * 100

df_win_rate$KDA = df_win_rate$kills + df_win_rate$assists / df_win_rate$deaths

df_win_rate <- df_win_rate[order(df_win_rate$win_rate, decreasing = TRUE),]

winrateadj100head <- head(df_win_rate, 10)
winrateadj100head

winrateadj100tail <- tail(df_win_rate, 10)
winrateadj100tail

#Sortie des tables en latex
#library(xtable)
#print(xtable(winrateadj100head, type = "latex"), file = "winrateadj100head.tex")
#print(xtable(winrateadj100tail, type = "latex"), file = "winrateadj100tail.tex")

# PARTIE NON SUPERVISE

library(MASS)
library(cluster)
library(factoextra)
library(rpart)
library(rpart.plot)
library(FactoMineR)
library(dplyr)
library(caret)

# nous divisons notre jeu de donn√©es en train et testons des √©chantillons

set.seed(42)
trainDT = sample(1:nrow(df_win_rate),0.6*nrow(df_win_rate))
train=df_win_rate[trainDT,]
test=df_win_rate[-trainDT,]

# nous utilisons rpart pour construire des arbres de classification 

tree=rpart(win_rate~adjposition + kills + deaths + assists + matches,data=train)
rpart.plot(tree)
prp(tree,extra=1)
print(tree$cptable[which.min(tree$cptable[,4]),1])
plotcp(tree)

# nous s√©parons les variables quantitatives et qualitatives

quali <- df_win_rate[sapply(df_win_rate,is.numeric)==F]
num <- df_win_rate %>%
  dplyr::select(where(is.numeric))

# Pour les variables cat√©gorielles, nous calculons MCA puis appliquons 
# la fonction HCPC() sur les r√©sultats comme d√©crit ci-dessus. 
# Nous commen√ßons par effectuer une MCA sur les individus.

res.mca <-MCA(quali, graph=FALSE)
ind <- get_mca_ind (res.mca)
indiv=data.frame(ind$coord) #conserver les coordonnees des ind sur les axes
data2=data.frame(num,indiv)

# Nous proc√©dons au calcul √† nouveau de l'analyse en composantes principales (ACP). 
# Ensuite, le HCPC est appliqu√© sur le r√©sultat de l'ACP.
res.pca <- PCA(data2, ncp=Inf,graph = FALSE)
res.hcpc <- HCPC(res.pca, nb.clust = -1) 

# Pour visualiser le dendrogramme g√©n√©r√© par 
#le clustering hi√©rarchique, nous allons utiliser la fonction fviz_dend()

fviz_dend(res.hcpc, 
          cex = 0.7,                     # taille de label
          palette = "jco",               # couleur
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco",           # couleur des rectangles
          labels_track_height = 0.8      
)

# La fonction fviz_cluster() peut √™tre utilis√©e pour visualiser des clusters individuels.

fviz_cluster(res.hcpc,
             repel = TRUE,            # √âviter le chevauchement des √©tiquettes
             show.clust.cent = TRUE, # Afficher les centres de cluster
             palette = "jco",         # Couleur
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# Pour afficher les donn√©es d'origine avec les affectations de cluster
head(res.hcpc$data.clust, 10)
# Pour afficher les variables quantitatives qui d√©crivent le mieux chaque cluster
res.hcpc$desc.var$quanti
# pour montrer les dimensions principales les plus associ√©es aux clusters
res.hcpc$desc.axes$quanti
# les individus repr√©sentatifs de chaque cluster peuvent √™tre extraits
res.hcpc$desc.ind$para

# nous lions les informations des clusters aux donn√©es initiales
final <- cbind(res.hcpc$data.clust$clust,df_win_rate)

# nous reconstruisons l'arbre de d√©cision avec des informations de cluster
tree_final=rpart(win_rate~res.hcpc$data.clust$clust + adjposition + kills + deaths + assists + matches,data=final)
rpart.plot(tree_final)

################################ SUPERVISE

#on rÈcupËre le dataframe sur lequel on va travailler

df_sup <- subset(df_win_rate, select=-c(name, win, KDA))

head(df_sup)

df_sup$adjposition <- as.factor(df_sup$adjposition)

#All covariables
x = dplyr::select(df_sup, adjposition, matches, kills, deaths, assists)
#Target variable
target = df_sup$win_rate

head(x)
target

split1<- sample(c(rep(0, 0.7 * nrow(df_sup)), rep(1, 0.3 * nrow(df_sup))))
train <- df_sup[split1 == 0, ]   
test <- df_sup[split1== 1, ] 

#Random forest pour prÈdire la position d'un champion
library(randomForest)
model.rf <- randomForest(adjposition ~ ., data = train, ntree = 20, na.action = na.omit,mtry = 6, importance = TRUE)
model.rf
hist(model.rf$oob.times)

# Liste des variables importantes
varImpPlot(model.rf)

# visualisation des votes dans le RF
model.rf$votes[1:10,]

#prÈdiction model RF
p.rf <- predict(model.rf, test)
table(p.rf, test$adjposition)
caret::confusionMatrix(p.rf, test$adjposition) # matrice de confusion

# Dans la suite on fait trois Benchmark en changeant les parmaËtres, 
#ou en utilisant un autre package de fonctions pour mesurer l'Èvolution de performance du RF

library(microbenchmark)
mb <- microbenchmark(times = 100, 
                     un = randomForest(adjposition ~ ., data = train, ntree = 500, na.action = na.omit, mtry = 1), 
                     deux = randomForest(adjposition ~ ., data = train, ntree = 500, na.action = na.omit, mtry = 2), 
                     trois = randomForest(adjposition ~ ., data = train, ntree = 500, na.action = na.omit, mtry = 3), 
                     quatre = randomForest(adjposition ~ ., data = train, ntree = 500, na.action = na.omit, mtry = 4))
autoplot(mb)



mb_2 <- microbenchmark(times = 100, 
                       avec = randomForest(adjposition ~ ., data = train, ntree = 500, na.action = na.omit, importance = TRUE), 
                       sans = randomForest(adjposition ~ ., data = train, ntree = 500, na.action = na.omit, importance = FALSE))

autoplot(mb_2)




mb_3 <- microbenchmark(times = 10, 
                       caret = train(adjposition ~ ., data = train,na.action = na.omit),
                       randomForest = randomForest(adjposition ~ ., data = train, na.action = na.omit))

autoplot(mb_3)


#bagging

library(ipred)
model.bagging <- bagging(adjposition ~ ., data = train)
bag1 = predict(model.bagging, newdata = test)
m_bag1 = table(bag1, test$adjposition)
caret::confusionMatrix(m_bag1)

