# Companion file for the Seminar ON PCA given by Martin Talbot to SE group Jan 2016


###########
# Slide 2 #
###########
rm(list = ls())
#install.packages("reshape")
#install.packages("multcomp")
#install.packages("vcd")
#install.packages("Rcpp")
#install.packages("FactoMineR")

library(reshape)
library(multcomp)
library(vcd)
library(Rcpp)
library(FactoMineR)

# setting path to access my csv file
setwd("C:/Users/mtalbot/Documents/RStudioFiles/Work-SEM")
mydata<- read.csv(file="lcgo_CrossData8.csv",head=TRUE,sep=",")


###########
# Slide 2 #
###########
# Rename
names(mydata) <-
  c(
    "EMAIL"
    ,"FEEDBACK TAG"
    ,"LCGO VERSION"
    ,"s__UID"
    ,"lastestPuzzleID"
    ,"nbOfDistinctPuzzlesPlayed"
    ,"totalTimePlayed"
    ,"nbSuccesses"
    ,"nbFailures"
    ,"nbBrokenVases"
    ,"purchasedHints"
    ,"purchasedSuits"
    ,"BomberSuit"
    ,"Area51Suit"
    ,"AntarcticaSuit"
    ,"WetSuit"
    ,"CatSuit"
    ,"MidasSuit"
    ,"LANGUAGE"
    ,"TOKEN"
    ,"COMPLETE"
    ,"TS"
    ,"PROGRESS"                                                                                                                     
    ,"Hitman.GO.Influence"                                                                    
    ,"Tomb.Raider.Influence"                                                     
    ,"Graphics.Influence"                                                                
    ,"Turn.based.puzzle.Influence"                                               
    ,"Satisfaction"                                                                                                       
    ,"NPS"                                                     
    ,"HavePlayedLC.TR.B4"                       
    ,"HavePlayed.TR.2013.PC.Console"                                                                      
    ,"HavePlayed.TR.Mobile"                         
    ,"HavePlayed.Hitman.GO"                                                                           
    ,"Watched.LC.Movie"                                                      
    ,"WhereFirstHearOfLC"                                                                            
    ,"Combat.City.Builder"                                                                  
    ,"Brain.Puzzle"                                                                   
    ,"Physic.Puzzle"                                          
    ,"Card.Battle"                                                   
    ,"ActionFighting"                                       
    ,"Shooter"
    ,"General.Strategy"                                   
    ,"Tower.Defense"                                                     
    ,"Matching.Puzzle"                                    
    ,"SkillChance"                                                        
    ,"Premium.gamesWeekPlaytime"                                                               
    ,"F2P.gamesWeekPlaytime"                                                                               
    ,"PC.or.ConsoleWeekPlaytime"                                                                                                      
    ,"Premium.games.WeekSpending"                                                               
    ,"F2P.WeekSpending"                                                                               
    ,"P1"                                                                                                               
    ,"P2"                                                                                                               
    ,"P3"                                                                                                               
    ,"P4"                                                                                                               
    ,"P5"                                                                                                               
    ,"Gender"                                                                                                     
    ,"Age"    
    ,"Education"                       
    ,"HouseholdIncome"                                          
  )

# remove undesirable dimensions from mydata and cast dimensions as numerical or categorical (factor)
mydata.sub <- data.frame(
  as.numeric(mydata$lastestPuzzleID),
  as.numeric(mydata$nbOfDistinctPuzzlesPlayed),
  as.numeric(mydata$totalTimePlayed),
  as.numeric(mydata$nbSuccesses),
  as.numeric(mydata$nbFailures),
  as.numeric(mydata$nbBrokenVases),
  as.numeric(mydata$Hitman.GO.Influence) / 0.4,   # scaling/10
  as.numeric(mydata$Tomb.Raider.Influence) / 0.4,                                                     
  as.numeric(mydata$Graphics.Influence) / 0.4,                                                                
  as.numeric(mydata$Turn.based.puzzle.Influence) / 0.4,                                               
  as.numeric(mydata$Satisfaction) / 0.6,                                                                                                      
  as.numeric(mydata$NPS),   
  as.numeric(mydata$Combat.City.Builder) / 0.4,                                                                  
  as.numeric(mydata$Brain.Puzzle) / 0.4,                                                                   
  as.numeric(mydata$Physic.Puzzle)/ 0.4,                                          
  as.numeric(mydata$Card.Battle) / 0.4,                                                   
  as.numeric(mydata$ActionFighting) / 0.4,                                       
  as.numeric(mydata$Shooter) / 0.4,
  as.numeric(mydata$General.Strategy) / 0.4,                                   
  as.numeric(mydata$Tower.Defense) / 0.4,                                                     
  as.numeric(mydata$Matching.Puzzle) / 0.4,                                   
  as.numeric(mydata$SkillChance) / 0.4,   
  as.factor(mydata$purchasedHints),
  as.factor(mydata$purchasedSuits),
  as.factor(mydata$BomberSuit),
  as.factor(mydata$Area51Suit),
  as.factor(mydata$AntarcticaSuit),
  as.factor(mydata$WetSuit),
  as.factor(mydata$CatSuit),
  as.factor(mydata$MidasSuit),                                             
  as.factor(mydata$HavePlayedLC.TR.B4),                       
  as.factor(mydata$HavePlayed.TR.2013.PC.Console),                                                                      
  as.factor(mydata$HavePlayed.TR.Mobile),                         
  as.factor(mydata$HavePlayed.Hitman.GO),                                                                           
  as.factor(mydata$Watched.LC.Movie),                                                     
  as.factor(mydata$WhereFirstHearOfLC),                                                                                                                     as.factor(mydata$Premium.gamesWeekPlaytime),                                                              
  as.factor(mydata$F2P.gamesWeekPlaytime),                                                                              
  as.factor(mydata$PC.or.ConsoleWeekPlaytime),                                                                                                      
  as.factor(mydata$Premium.games.WeekSpending),                                                               
  as.factor(mydata$F2P.WeekSpending),                                                                               
  as.factor(mydata$P1),                                                                                                              
  as.factor(mydata$P2),                                                                                                              
  as.factor(mydata$P3),                                                                                                               
  as.factor(mydata$P4),                                                                                                               
  as.factor(mydata$P5),                                                                                                               
  as.factor(mydata$Gender),                                                                                                    
  as.factor(mydata$Age),    
  as.factor(mydata$Education),                       
  as.factor(mydata$HouseholdIncome) 
)

# Rename (again) + variable are numbered to refer to them in PCA
names(mydata.sub) <-
  c(
    ##### quantitative ##### 123
    "lastestPuzzleID"                 	# 1 
    ,"nbOfDistinctPuzzlesPlayed"      	# 2
    ,"totalTimePlayed"                	# 3 
    ,"nbSuccesses"                    	# 4
    ,"nbFailures"                     	# 5
    ,"nbBrokenVases"                  	# 6
    ,"Hitman.GO.Influence"            	# 7                                                        
    ,"Tomb.Raider.Influence"          	# 8                                           
    ,"Graphics.Influence"             	# 9                                                   
    ,"Turn.based.puzzle.Influence"      #10                                           
    ,"Satisfaction"                   	#11                                                                                    
    ,"NPS"                            	#12
    ,"Combat.City.Builder"            	#13                                                      
    ,"Brain.Puzzle"                   	#14                                                
    ,"Physic.Puzzle"                  	#15                        
    ,"Card.Battle"                    	#16                               
    ,"ActionFighting"                 	#17                      
    ,"Shooter"                        	#18
    ,"General.Strategy"               	#19                    
    ,"Tower.Defense"                  	#20                                   
    ,"Matching.Puzzle"                	#21                    
    ,"SkillChance"                    	#22
    
    ##### Qualitative #### ABC
    ,"purchasedHints"                 	#23
    ,"purchasedSuits"                 	#24
    ,"BomberSuit"                     	#25
    ,"Area51Suit"                     	#26
    ,"AntarcticaSuit"                 	#27
    ,"WetSuit"                        	#28
    ,"CatSuit"                        	#29 
    ,"MidasSuit"                      	#30                      
    ,"HavePlayedLC.TR.B4"             	#31          
    ,"HavePlayed.TR.2013.PC.Console"    #32                                                                    
    ,"HavePlayed.TR.Mobile"           	#33              
    ,"HavePlayed.Hitman.GO"           	#34                                                                
    ,"Watched.LC.Movie"               	#35                                       
    ,"WhereFirstHearOfLC"             	#36                                                                                                  
    ,"Premium.gamesWeekPlaytime"        #37                                                         
    ,"F2P.gamesWeekPlaytime"            #38                                                                     
    ,"PC.or.ConsoleWeekPlaytime"        #39                                                                                                
    ,"Premium.games.WeekSpending"       #40                                                          
    ,"F2P.WeekSpending"              	  #41                                                                
    ,"P1"                             	#42                                                                                              
    ,"P2"                             	#43                                                                                           
    ,"P3"                             	#44                                                                                              
    ,"P4"                             	#45                                                                                              
    ,"P5"                             	#46                                                                                              
    ,"Gender"                         	#47                                                                            
    ,"Age"                           	  #48
    ,"Education"                      	#49 
    ,"HouseholdIncome"                	#50                          
  )



###########
# Slide 4 #
###########
summary(mydata.sub)


###########
# Slide 7 #
###########
mydata.sub.pca <- PCA(mydata.sub, quali.sup = 23:50, graph=FALSE )
summary(mydata.sub.pca)
?PCA


############
# Slide 12 #
############
summary(mydata.sub.pca, nbelement=50, ncp=7)


############
# Slide 14 #
############
plot(mydata.sub.pca, choix='var', cex=.7, axes=c(1,2), new.plot = TRUE , xlim=range(-1:1), ylim=range(-1:1))
plot(mydata.sub.pca, choix='var', cex=.7, axes=c(3,4), new.plot = TRUE , xlim=range(-1:1), ylim=range(-1:1))


############
# Slide 15 #
############
?PCA


############
# Slide 16 #
############
plot(mydata.sub.pca, choix='var', cex=.7, axes=c(3,4),  new.plot = TRUE , xlim=range(-1:1), ylim=range(-1:1), select='cos2 10')
plot(mydata.sub.pca, choix='var', cex=.7, axes=c(3,4),  new.plot = TRUE , xlim=range(-1:1), ylim=range(-1:1), select='cos2 0.3')
plot(mydata.sub.pca, choix='var', cex=.7, axes=c(3,4),  new.plot = TRUE , xlim=range(-1:1), ylim=range(-1:1), select='contrib 10')  ## try it!
     

############
# Slide 18 #
############
mydata.sub.pca <- PCA(mydata.sub, quali.sup = 23:50, quanti.sup = 7:22, ncp=10, graph=FALSE )
mydata.sub.pca.hcpc <- HCPC(mydata.sub.pca) # WARNING: RStudio won't let you interact with the crossbow

mydata.sub.famd <- FAMD(mydata.sub, sup.var = c(0:6, 23:30), ncp=10, graph=FALSE )
?FAMD
mydata.sub.famd.hcpc <- HCPC(mydata.sub.famd)

names(mydata.sub.pca.hcpc) # same results for FAMD 


############
# Slide 21 #
############
mydata.sub.pca.hcpc$desc.var  # for everything or..

# easier to analyze when Category and quanti of the same dimensions are grouped together.
mydata.sub.pca.hcpc$desc.var$quanti$`1`
mydata.sub.pca.hcpc$desc.var$category$`1`


############
# Slide 22 #
############
# Dim1
mydata.sub.pca.hcpc$desc.var$quanti$`1`
mydata.sub.pca.hcpc$desc.var$category$`1`

# Dim2
mydata.sub.pca.hcpc$desc.var$quanti$`2`
mydata.sub.pca.hcpc$desc.var$category$`2`

# Dim 3
mydata.sub.pca.hcpc$desc.var$quanti$`3`
mydata.sub.pca.hcpc$desc.var$category$`3`

# Dim 4
mydata.sub.pca.hcpc$desc.var$quanti$`4`
mydata.sub.pca.hcpc$desc.var$category$`4`

# Dim 5
mydata.sub.pca.hcpc$desc.var$quanti$`5`
mydata.sub.pca.hcpc$desc.var$category$`5`


# Hint: use this to sort categories and quanti varaiables by v.test in absolute decreasing order
mydata.sub.pca.hcpc$desc.var$category$`2`[order(-abs(mydata.sub.pca.hcpc$desc.var$category$`2`[,5])),] 
mydata.sub.pca.hcpc$desc.var$quanti$`2`[order(-abs(mydata.sub.pca.hcpc$desc.var$quanti$`2`[,1])),] 


############
# Slide 23 #
############
mydata.sub.pca.hcpc$desc.ind


############
# Slide 24 #
############
data.frame(t(mydata.sub[1,]), t(mydata.sub[11,]), t(mydata.sub[4,]), t(mydata.sub[10,]), t(mydata.sub[8,]))


############
# Slide 25 #
############
# Dim1
mydata.sub.famd.hcpc$desc.var$quanti$`1`
mydata.sub.famd.hcpc$desc.var$category$`1`

# Dim2
mydata.sub.famd.hcpc$desc.var$quanti$`2`
mydata.sub.famd.hcpc$desc.var$category$`2`

# Dim 3
mydata.sub.famd.hcpc$desc.var$quanti$`3`
mydata.sub.famd.hcpc$desc.var$category$`3`


############
# Slide 26 #
############
mydata.sub.famd.hcpc$desc.ind


############
# Slide 27 #
############
data.frame(t(mydata.sub[199,]), t(mydata.sub[16,]), t(mydata.sub[67,]), t(mydata.sub[24,]), t(mydata.sub[196,]))


##################### EOF #####################.