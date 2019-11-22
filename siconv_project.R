##################################################
#SICONV_PROJECT
##################################################
#
#The objective of this project is to develop an algorithm
#capable of predicting whether or not a proposal of agreement with federal resources
#as those registered in SICONV, from brazilian federal 
#government, will be approved.
#
#
#
#SICONV data is publicly available at http://dados.gov.br/dataset/siconv
#
#
#
#
#
#
#
###################################################
#1 ORGANIZING THE DATA
###################################################
#
#Siconv data is available through 23 files, but I will only need 4 of them. In order to make
#my analysis, I have to organize the data I need in one single file.
#
#
##FIRST STEP: INSTALLING REQUIRED PACKAGES

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(partykit)) install.packages("partykit", repos = "http://cran.us.r-project.org")

##SECOND STEP:IMPORTING FILES

dl1 <- tempfile()
download.file("http://plataformamaisbrasil.gov.br/images/docs/CGSIS/csv/siconv_proposta.csv.zip", dl1)

siconv_proposta <- read_delim(unzip(dl1),";", escape_double = FALSE, trim_ws = TRUE)

dl2 <- tempfile()
download.file("http://plataformamaisbrasil.gov.br/images/docs/CGSIS/csv/siconv_programa_proposta.csv.zip", dl2)

siconv_programa_proposta <- read_delim(unzip(dl2),";", escape_double = FALSE, trim_ws = TRUE)

dl3 <- tempfile()
download.file("http://plataformamaisbrasil.gov.br/images/docs/CGSIS/csv/siconv_emenda.csv.zip", dl3)

siconv_emenda <- read_delim(unzip(dl3),";", escape_double = FALSE, trim_ws = TRUE)

dl4 <- tempfile()
download.file("http://plataformamaisbrasil.gov.br/images/docs/CGSIS/csv/siconv_justificativas_proposta.csv.zip", dl4)

siconv_justificativas_proposta <- read_delim(unzip(dl4),";", escape_double = FALSE, trim_ws = TRUE)

##THIRD STEP:TIDYING FILES

#First file I need to organize is siconv_proposta.csv. It's a huge file, as shown by the code below:

dim(siconv_proposta)



#I only need 7 of those 31 variables. The column SIT_PROPOSTA has information I want to predict (whether a proposal was approved). 
#However, there are intermediary status not useful for my algorithm. For my purpose, I want only approved ("Proposta/Plano de Trabalho Aprovados") 
#and not approved proposals ("Proposta/Plano de Trabalho Rejeitados" and "Proposta/Plano de Trabalho Rejeitados por Impedimento técnico"). 
#I will then filter this situation from the SIT_PROPOSTA column.

#The following code do all this at once:
  
tab_proposal<-siconv_proposta%>%
  filter(SIT_PROPOSTA== "Proposta/Plano de Trabalho Aprovados" | SIT_PROPOSTA== "Proposta/Plano de Trabalho Rejeitados" | SIT_PROPOSTA=="Proposta/Plano de Trabalho Rejeitados por Impedimento técnico")%>%
  select(ID_PROPOSTA,SIT_PROPOSTA, UF_PROPONENTE,COD_ORGAO_SUP,NATUREZA_JURIDICA,ANO_PROP,VL_GLOBAL_PROP)
head(tab_proposal)

#Since I only want to know if a proposal was approved or no, I will convert the SIT_PROPOSTA column to binary values, 
#being *1 = approved* and *0 = not approved*:
  
tab_proposal$SIT_PROPOSTA<-ifelse(tab_proposal$SIT_PROPOSTA=="Proposta/Plano de Trabalho Aprovados",1,0)
dim(tab_proposal)

#I will also add information about filling the justification fields of the proposal, 
#because there are many cases of NAs and I believe filling correctly these fields can improve approval chances.

#First I will add columns that I want from *siconv_justificativas_proposta* to *tab_justificativa*:
  
tab_justificativa<-siconv_justificativas_proposta%>%
  select(ID_PROPOSTA,CARACTERIZACAO_INTERESSES_RECI,CAPACIDADE_TECNICA) 

#Now I join this columns to tab_proposal:
  
tab_proposal<-left_join(tab_proposal,tab_justificativa,by="ID_PROPOSTA")
head(tab_proposal)

#I will check the proportion of NAs for both columns:
  
prop_caracterizacao<-sum(!is.na(tab_proposal$CARACTERIZACAO_INTERESSES_RECI))/nrow(tab_proposal)
prop_caracterizacao

prop_capacidade<-sum(!is.na(tab_proposal$CAPACIDADE_TECNICA))/nrow(tab_proposal)
prop_capacidade

#Only 1.8% of the proposals fill justification fields, while 68% fill techical capacity field. 
#I will check if both fields can work as predictors later, within my models.
#The final step is to replace *NA* with *0* and any other result with *1*.

tab_proposal$CARACTERIZACAO_INTERESSES_RECI<-ifelse(is.na(tab_proposal$CARACTERIZACAO_INTERESSES_RECI),0,1)
tab_proposal$CAPACIDADE_TECNICA<-ifelse(is.na(tab_proposal$CAPACIDADE_TECNICA),0,1)

#Possibly an important predictor is whether the proposal comes from a parliamentary amendment or not. 
#This information can be found at *siconv_emenda.csv* file.
#I will create a table named *tab_emendas* grouping information I need.

tab_emenda<-siconv_emenda%>%
  select(ID_PROPOSTA,NR_EMENDA)

#Now I add the columns to *tab_proposal*:
  
tab_proposal<-left_join(tab_proposal,tab_emenda,by="ID_PROPOSTA")

#I replace *NA* with *0* and any other result (meaning proposal comes from parliamentary amendment) with *1*.

tab_proposal$NR_EMENDA<-ifelse(is.na(tab_proposal$NR_EMENDA),0,1)
head(tab_proposal)

#I will also include another predictor: *budget program*. 

tab_proposal<-left_join(tab_proposal,siconv_programa_proposta,by="ID_PROPOSTA")

#Other important predictor is proposal year. More specifically, my guess is election years increase the approval chances of a proposal. 
#In Brazil, election occurs in even years, so it is easy to use this as a predictor:
  
tab_proposal<-tab_proposal%>%
  mutate(ELECTION_YEAR=ifelse(ANO_PROP %% 2 == 0, 1,0))%>%
  select(-ANO_PROP)
head(tab_proposal)

#Final steps to tidy *tab_proposal* dataset includes changing some variables to *factor* and changing *SIT_PROPOSTA* 
#from binary values to *factor* ("YES" and "NO"), because some models requires this. Besides this, I will remove *ID_PROPOSTA* column.

tab_proposal<-tab_proposal%>%
  mutate(SIT_PROP_YN=ifelse(SIT_PROPOSTA == 1, "YES","NO"))%>%
  select(-ID_PROPOSTA, -SIT_PROPOSTA) 

tab_proposal$UF_PROPONENTE=as.factor(tab_proposal$UF_PROPONENTE)
tab_proposal$COD_ORGAO_SUP=as.factor(tab_proposal$COD_ORGAO_SUP)
tab_proposal$NATUREZA_JURIDICA=as.factor(tab_proposal$NATUREZA_JURIDICA)
tab_proposal$CARACTERIZACAO_INTERESSES_RECI=as.factor(tab_proposal$CARACTERIZACAO_INTERESSES_RECI)
tab_proposal$CAPACIDADE_TECNICA=as.factor(tab_proposal$CAPACIDADE_TECNICA)
tab_proposal$NR_EMENDA=as.factor(tab_proposal$NR_EMENDA)
tab_proposal$ID_PROGRAMA=as.factor(tab_proposal$ID_PROGRAMA)
tab_proposal$ELECTION_YEAR=as.factor(tab_proposal$ELECTION_YEAR)
tab_proposal$SIT_PROP_YN=as.factor(tab_proposal$SIT_PROP_YN)
str(tab_proposal)


#################################################################################################################
##2 DATA ANALYSIS
#################################################################################################################

#It is time to analyze *tab_proposal* dataset to better understand what challenges I will face in order to predict results.
#First of all, let's check the final dimmensions of my dataset:

dim(tab_proposal)

#Now, let's see what proportion of proposal were accepted:
  
mean(tab_proposal$SIT_PROP_YN=="YES")

#Now, let's group data by our variables. First, by UF (state). The following code groups data by state and plots the results:

tab_group_by_UF<-tab_proposal%>%
  group_by(UF_PROPONENTE)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))

tab_group_by_UF

tab_group_by_UF%>%
  mutate(UF_PROPONENTE = reorder(UF_PROPONENTE, Approved)) %>%
  ggplot(aes(UF_PROPONENTE,Approved)) + 
  geom_bar(stat = "identity", fill = "blue", color = "black")+
  xlab("States") +
  ylab("Rate of Approved Proposals")+
  coord_cartesian(ylim = c(0.4, 0.8)) 

#We can see the proportion of approved proposals varys from one state to other. While Maranhão (MA) has 52% of approved proposals, 
#Acre (AC) gets 78% of its proposals approved.

#Time to check another variable: *COD_ORGAO_SUP*, which discriminates government agency responsible for the transfer of resources.

tab_group_by_COD<-tab_proposal%>%
  group_by(COD_ORGAO_SUP)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))%>%
  arrange(desc(n))

tab_group_by_COD

#We can see there are 30 government agencies, with different number of proposals (n) and rates of approval. Analyzing *n*, 
#8 agencies have more then 10,000 proposals, while 10 have 100 or less. Difference in *n* may lead to a bias in the predictor. 
#A simple way to minimize this is to establish a threshold. In this case, we can see *n* drops from 925 (COD_ORGAO_SUP = 58000) 
#to 100 (COD_ORGAO_SUP = 32000). Therefore, I will keep only agencies with number of proposals greater than 100:

tab_proposal<-tab_proposal%>%
       group_by(COD_ORGAO_SUP)%>%
       mutate(n_ORGAO=sum(n()))%>%
       filter(n_ORGAO>100)%>%
       ungroup()

#Now, let's group again and plot the results:
  
tab_group_ORGAO<-tab_proposal%>%
  group_by(COD_ORGAO_SUP)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))%>%
  arrange(desc(n))

tab_group_ORGAO%>%
  mutate(COD_ORGAO_SUP = reorder(COD_ORGAO_SUP, Approved)) %>%
  ggplot(aes(COD_ORGAO_SUP, Approved)) + 
  geom_bar(stat = "identity", fill = "red", color = "black")+
  coord_flip()+
  xlab("Rate of Approved Proposals")+
  ylab("COD_ORGAO_SUP")


#It's possible to see that rate of approval enormously varies among *COD_ORGAO_SUP*, meaning this variable will possibly work as a predictor.

#Now I will check the variable *NATUREZA_JURIDICA*, which shows who is proposing the agreement: municipalities, states or non-governmental entities.

tab_group_NATUREZA<-tab_proposal%>%
  group_by(NATUREZA_JURIDICA)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))

tab_group_NATUREZA%>%
  mutate(NATUREZA_JURIDICA = reorder(NATUREZA_JURIDICA, Approved)) %>%
  ggplot(aes(NATUREZA_JURIDICA, Approved)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black", width =  0.5)+
  coord_flip()+
  ylab("Rate of Approved Proposals")+
  xlab("PROPOSER'S LEGAL NATURE")

#In this case, we can see the variability is smaller, being the smallest rate of approval municipalities 
#(Administração Pública Municipal), with 61%, and the highest, non-governmental entities, with 80%.

#To check approval rate by value, I have to define some intervals, since variance is huge. There are proposals from *0* to almost *$50 bilions*.

tab_proposal<-tab_proposal%>%
  mutate(GROUP_VALUE=ifelse(VL_GLOBAL_PROP<=200000,">200K",                         
              ifelse(VL_GLOBAL_PROP<=500000,">200K<=500K",
              ifelse(VL_GLOBAL_PROP<=2000000,">500K<=2M",
              ifelse(VL_GLOBAL_PROP<=10000000,">2M<=10M",
              ifelse(VL_GLOBAL_PROP<=50000000,">10M<=50M",
              ifelse(VL_GLOBAL_PROP<=250000000,">50M<=250M",                        
              ifelse(VL_GLOBAL_PROP<=2000000000,">250M<=2B",">2B"))))))))%>%
              select(-n_ORGAO,-VL_GLOBAL_PROP)

#The following code plots the results:

tab_group_VALUE<-tab_proposal%>%
  group_by(GROUP_VALUE)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))

tab_group_VALUE%>%
  mutate(GROUP_VALUE = reorder(GROUP_VALUE, Approved)) %>%
  ggplot(aes(GROUP_VALUE, Approved)) + 
  geom_bar(stat = "identity", fill = "brown", color = "black")+
  coord_flip()+
  ylab("Rate of Approved Proposals")+
  xlab("GROUP VALUE")

#Generally speaking, as proposal value increases, rate of approval increases too.

#Two related variables are *CARACTERIZACAO_INTERESSES_RECI* and *CAPACIDADE_TECNICA*, both related to filling justification fields.

tab_group_CARACT<-tab_proposal%>%
  group_by(CARACTERIZACAO_INTERESSES_RECI)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))

tab_group_CARACT%>%
  ggplot(aes(CARACTERIZACAO_INTERESSES_RECI, Approved)) + 
  geom_bar(stat = "identity", fill = "purple", color = "black", width = 0.75)+
  ylab("Rate of Approved Proposals")+
  xlab("JUSTIFICATION FIELD")

tab_group_TECH<-tab_proposal%>%
  group_by(CAPACIDADE_TECNICA)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))

tab_group_TECH%>%
  ggplot(aes(CAPACIDADE_TECNICA, Approved)) + 
  geom_bar(stat = "identity", fill = "dark green", color = "black", width = 0.75)+
  ylab("Rate of Approved Proposals")+
  xlab("TECHNICAL CAPACITY")

#As observed, filling justification field *CARACTERIZACAO_INTERESSES_RECI* increases rate of approval more than filling *CAPACIDADE_TECNICA* does. 

#Now let's check how rate of approval changes if proposal comes from parliamentary ammendment:

tab_group_AMMENDMENT<-tab_proposal%>%
  group_by(NR_EMENDA)%>%
  summarise(Approved=mean(SIT_PROP_YN=="YES"))

tab_group_AMMENDMENT%>%
  ggplot(aes(NR_EMENDA, Approved)) + 
  geom_bar(stat = "identity", fill = "dark blue", color = "black")+
  ylab("Rate of Approved Proposals")+
  xlab("AMMENDMENT ORIGIN?")

#90% of proposals origined from parliamentary ammendment are approved, whilst only 51% of "independent proposals" are approved.

#The following plot shows if election years increase approval rate:

tab_group_election<-tab_proposal%>%
  group_by(ELECTION_YEAR)%>%
  summarise(Approved=mean(SIT_PROP_YN=="YES"))

tab_group_election%>%
  ggplot(aes(ELECTION_YEAR, Approved)) + 
  geom_bar(stat = "identity", fill = "dark grey", color = "black")+
  ylab("Rate of Approved Proposals")+
  xlab("ELECTION YEAR?")

#As plot shows, in election years, 73% of proposals are approved, against 58% in non-election years.

#The last variable is ID_PROGRAMA. 

tab_group_PROGRAM<-tab_proposal%>%
  group_by(ID_PROGRAMA)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))%>%
  arrange(desc(n))
head(tab_group_PROGRAM)

tab_group_PROGRAM<-tab_proposal%>%
  group_by(ID_PROGRAMA)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))%>%
  arrange(n)
head(tab_group_PROGRAM)
dim(tab_group_PROGRAM)


#We can see we have a problem. There are 21,770 unique values for ID_PROGRAMA, meaning there are a lot of programs with small value of *n*.
#In fact, 15,435 programs have only one proposal (n = 1). This situation represents two problems: 
#first, small values of *n* generates unintended bias. Second, a very large number of levels in a variable can't be 
#handled by most of models I want to use.

#Again, a simple way to minimize this problem is to establish a threshold. In this case, I will keep only programs with *n > 100*:

tab_proposal<-tab_proposal%>%
  group_by(ID_PROGRAMA)%>%
  mutate(n_PROG=sum(n()))%>%
  filter(n_PROG>100)%>%
  ungroup()
n_distinct(tab_proposal$n_PROG)

#Now I have 271 unique values for ID_PROGRAM. This number is still too large to be handled by random forest models, for example. 
#Because of this, I will group ID_PROGRAM according with approval rate. To do that, first I will add a column with this information:

tab_proposal<-tab_proposal%>%
  group_by(ID_PROGRAMA)%>%
  mutate(PROP_PROG=sum(SIT_PROP_YN=="YES")/sum(n()))%>%
  ungroup()

tab_proposal$INTERVAL<-findInterval(tab_proposal$PROP_PROG, seq(0,1,0.04), rightmost.closed = TRUE)

tab_proposal<-select(tab_proposal,-ID_PROGRAMA,-n_PROG,-PROP_PROG)
tab_proposal$INTERVAL<-as.factor(tab_proposal$INTERVAL)
tab_proposal$GROUP_VALUE<-as.factor(tab_proposal$GROUP_VALUE)
tab_proposal$COD_ORGAO_SUP<-factor(tab_proposal$COD_ORGAO_SUP)

str(tab_proposal)



#Now I can see how approval rates vary according to program:

tab_group_INTERVAL<-tab_proposal%>%
  group_by(INTERVAL)%>%
  summarise(n=n(), Approved=mean(SIT_PROP_YN=="YES"))

scaleFUN <- function(x) sprintf("%.2f", x)

tab_group_INTERVAL%>%
  mutate(INTERVAL = reorder(INTERVAL, Approved)) %>%
  ggplot(aes(Approved, n)) + 
  geom_bar(stat = "identity", fill = "dark red", color = "black")+
  coord_flip()+
  scale_y_continuous(labels=scaleFUN)+
  ylab("Rate of Approved Proposals")+
  xlab("INTERVAL")

#More than 30,000 programs had 0% approval rate, whilst 80,000 were 100% approved. 
#As we can see, the approval rate distribution among programs is not normal.

#After this extensive analysis, finally I have my dataset ready to begin the development process.


##################################################################################################
##BEGINNING TO DEVELOP THE MODEL
##################################################################################################


##Creating train and test sets

#In order to test the predictors, I've decided to split the *tab_proposal* set into *train_set* and *test_set*. 
#First, I must load *caret package*, which includes tools for data splitting. 

library(caret) 

#I set the seed to 1: 

set.seed(1, sample.kind="Rounding") 

#Finally, I split the tab_proposal set, with 80% of the data to train_set and 20% for test_set: 

test_index <- createDataPartition(y = tab_proposal$SIT_PROP_YN, times = 1, p = 0.2, list = FALSE) 
train_set <- tab_proposal[-test_index,] 
test_set <- tab_proposal[test_index,]

#While I tested the different models, I had huge problems with calculation time. 
#Some models, without tune, took 12+ hours to run each test. This proved impractical. 
#I decided then to create smaller train and test sets to run the first versions of my models.

set.seed(1, sample.kind="Rounding") 

train_small<-sample_n(tab_proposal, 10000)
test_small<-sample_n(tab_proposal, 10000)


#######################################################################################################
## Testing models
#######################################################################################################

### First model - GLM

#The first model I will test will be *glm*, or *generalized linear model*. In project description, 
#students are required to "go beyond standard linear regression". I will apply this to this project in a way 
#that my starting point, my reference will be results obtained with this model. 

set.seed(1, sample.kind="Rounding") 

glm_model = glm(SIT_PROP_YN ~ ., family = binomial, 
                data = train_small)

p_glm<-predict(glm_model,newdata = test_small)

y_glm<-ifelse(p_glm>0.5,"YES","NO")
y_glm<-factor(y_glm)


confMatrix.glm <- confusionMatrix(table(as.factor(y_glm),as.factor(test_small$SIT_PROP_YN)))

confMatrix.glm$overall["Accuracy"]

confMatrix.glm$byClass["F1"]

#My start is already good, with *Accuracy* of 0.9114 and *F1* of 0.8934.

#I will try to improve this numbers using other models.

###Second model - Random Forest

#This is a very simple and "untuned" randomForest test. I will start with *ntree = 500*.

set.seed(1, sample.kind="Rounding") 

rf = randomForest(SIT_PROP_YN ~ .,data=train_small,ntree= 500, importance=TRUE)

plot(rf)

p_rf<-predict(rf,newdata = test_small)

confMatrix.rf<-confusionMatrix(table(p_rf,test_small$SIT_PROP_YN))
confMatrix.rf$overall["Accuracy"]

confMatrix.rf$byClass["F1"]

###Third model - Random Forest with "Rborist" method

set.seed(1, sample.kind="Rounding") 

rf_rborist <- train(SIT_PROP_YN ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = 1),
                    data = train_small,
                    nTree = 300,
                    classWeight = "balance",
                    minInfo=0.01)


p_rf_rborist<-predict(rf_rborist,newdata = test_small)

confMatrixRF_rborist<-confusionMatrix(table(p_rf_rborist, test_small$SIT_PROP_YN))
confMatrixRF_rborist$overall["Accuracy"]
confMatrixRF_rborist$byClass["F1"]

#I tuned parameters for this method, but didn't get good results. No combination could improve Accuracy up to 0.90.

###2.3.4 Fourth model - Classification (Decision) Trees**


set.seed(1, sample.kind="Rounding") 


train_rpart <- train(SIT_PROP_YN ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_small)

ggplot(train_rpart, highlight = TRUE)

p_rpart<-predict(train_rpart,newdata = test_small)

confMatrix.rpart<-confusionMatrix(table(p_rpart, test_small$SIT_PROP_YN))

confMatrix.rpart$overall["Accuracy"]
confMatrix.rpart$byClass["F1"]


###Fifth model - KNN

set.seed(1, sample.kind="Rounding") 

train_knn <- train(SIT_PROP_YN ~ ., 
                   method = "knn", 
                   data = train_small)

ggplot(train_knn, highlight = TRUE)

p_knn<-predict(train_knn,newdata = test_small)

confMatrix.knn<-confusionMatrix(table(p_knn, test_small$SIT_PROP_YN))

confMatrix.knn$overall["Accuracy"]
confMatrix.knn$byClass["F1"]

###Sixth model - Conditional Tree

set.seed(1, sample.kind="Rounding") 

fitControl <- trainControl(method = "cv", number = 10)

ctree = ctree(SIT_PROP_YN ~ ., 
              data=train_small)

p_ctree<-predict(ctree,newdata = test_small)

confMatrix.ctree<-confusionMatrix(table(p_ctree, test_small$SIT_PROP_YN))

confMatrix.ctree$overall["Accuracy"]
confMatrix.ctree$byClass["F1"]

###Seventh model - GBM

#This method is called Generalized Boosted Regression Model. I'll set it up to a 10-fold cross-validation with 10 times repetition.

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(1, sample.kind = "Rounding")

gbmFit1 <- train(SIT_PROP_YN ~ ., 
                 data = train_small, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)

p_gbmFit1.sm<-predict(gbmFit1,newdata = test_small)

confMatrix.gbmfit1.sm<-confusionMatrix(table(p_gbmFit1.sm, test_small$SIT_PROP_YN))

plot(gbmFit1)

confMatrix.gbmfit1.sm$overall["Accuracy"]

confMatrix.gbmfit1.sm$byClass["F1"]

#Tuning some parameters of the model:

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

set.seed(1, sample.kind = "Rounding")


gbmFit3 <- train(SIT_PROP_YN ~ ., 
                 data = train_small, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")

p_gbmFit3<-predict(gbmFit3,newdata = test_small)

confMatrix.gbmfit3<-confusionMatrix(table(p_gbmFit3, test_small$SIT_PROP_YN))

ggplot(gbmFit3, highlight = TRUE)

confMatrix.gbmfit3$overall["Accuracy"]

confMatrix.gbmfit3$byClass["F1"]

########################################################
## Comparing model performance on small train set
########################################################

#After testing seven models (and several tuning parameters for each one of them not described here), 
#it's time to compare them and choose  which one gets better performance.

model.summary <- data.frame(Model = c("GLM", "randomForest", "RF_Rborist", "Rpart", "kNN", "ctree", "GBM"), 
                            Accuracy = c(confMatrix.glm$overall["Accuracy"],
                                         confMatrix.rf$overall["Accuracy"],
                                         confMatrixRF_rborist$overall["Accuracy"],
                                         confMatrix.rpart$overall["Accuracy"],
                                         confMatrix.knn$overall["Accuracy"],
                                         confMatrix.ctree$overall["Accuracy"],
                                         confMatrix.gbmfit3$overall["Accuracy"]), 
                            Sensitivity = c(confMatrix.glm$byClass["Sensitivity"],
                                            confMatrix.rf$byClass["Sensitivity"],
                                            confMatrixRF_rborist$byClass["Sensitivity"],
                                            confMatrix.rpart$byClass["Sensitivity"],
                                            confMatrix.knn$byClass["Sensitivity"],
                                            confMatrix.ctree$byClass["Sensitivity"],
                                            confMatrix.gbmfit3$byClass["Sensitivity"]),
                            Specificity = c(confMatrix.glm$byClass["Specificity"],
                                            confMatrix.rf$byClass["Specificity"],
                                            confMatrixRF_rborist$byClass["Specificity"],
                                            confMatrix.rpart$byClass["Specificity"],
                                            confMatrix.knn$byClass["Specificity"],
                                            confMatrix.ctree$byClass["Specificity"],
                                            confMatrix.gbmfit3$byClass["Specificity"]),
                            Precision = c(confMatrix.glm$byClass["Precision"],
                                          confMatrix.rf$byClass["Precision"],
                                          confMatrixRF_rborist$byClass["Precision"],
                                          confMatrix.rpart$byClass["Precision"],
                                          confMatrix.knn$byClass["Precision"],
                                          confMatrix.ctree$byClass["Precision"],
                                          confMatrix.gbmfit3$byClass["Precision"]),
                            Recall = c(confMatrix.glm$byClass["Recall"],
                                       confMatrix.rf$byClass["Recall"],
                                       confMatrixRF_rborist$byClass["Recall"],
                                       confMatrix.rpart$byClass["Recall"],
                                       confMatrix.knn$byClass["Recall"],
                                       confMatrix.ctree$byClass["Recall"],
                                       confMatrix.gbmfit3$byClass["Recall"]),
                            F1 = c(confMatrix.glm$byClass["F1"],
                                   confMatrix.rf$byClass["F1"],
                                   confMatrixRF_rborist$byClass["F1"],
                                   confMatrix.rpart$byClass["F1"],
                                   confMatrix.knn$byClass["F1"],
                                   confMatrix.ctree$byClass["F1"],
                                   confMatrix.gbmfit3$byClass["F1"]))



model.summary[order(-model.summary$Accuracy),]

#####################################################
## Trying to improve randomForest model
#####################################################

#I will try to improve randomForest results through train function, from caret package, tuning some parameters. 
#I tried to run this tests on the train set this time, but each run was taking 15-20 hours, 
#so I decided to keep the test on train_small and test_small.

### First try: testing different values for mtry

#First try I will use ten fold cross validation and repeat it 3 times. I will test mtry values from 1 to 30. 

numFolds <- trainControl(method = "cv", number = 10)

cpGrid <- expand.grid(.mtry = seq(1, 30, 1))
set.seed(1, sample.kind="Rounding") 

rf_cvt<-train(SIT_PROP_YN~., data = train_small, method = "rf", trControl = numFolds, tuneGrid = cpGrid)

print(rf_cvt)
plot(rf_cvt)
p_rf_cvt<-predict(rf_cvt,newdata = test_small)

confMatrix.rf_cvt<-confusionMatrix(table(p_rf_cvt,test_small$SIT_PROP_YN))
confMatrix.rf_cvt$overall["Accuracy"]

confMatrix.rf_cvt$byClass["F1"]

### **2.5.2 Second try: Random Search

#In this second attempt, I will add random mode to search paramater within trainControl. 
#It will take longer, but *train* will test some random values of *mtry* and point the one with best results.

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")

set.seed(1, sample.kind = "Rounding")

mtry <- sqrt(ncol(train_small))

rf_random <- train(SIT_PROP_YN~., data=train_small, method="rf", metric="Accuracy", tuneLength=15, trControl=control)

print(rf_random)
plot(rf_random)

p_rf_random<-predict(rf_random,newdata = test_small)

confMatrix.rf_random<-confusionMatrix(table(p_rf_random, test_small$SIT_PROP_YN))

confMatrix.rf_random$overall["Accuracy"]
confMatrix.rf_random$byClass["F1"]

### 2.5.3 Third try: different ntree with best mtry

#We got *14* and *17* as best value for *mtry* in the two previous methods. 
#Now I will test if increasing *ntree* I improve results for both *mtry* values.

set.seed(1, sample.kind = "Rounding")
tunegrid <- expand.grid(.mtry=14)
rf_mtry_14 <- train(SIT_PROP_YN~., data=train_small, method="rf", metric="Accuracy", tuneGrid=tunegrid, ntree = 1000)

print(rf_mtry_14)

p_rf_mtry_14<-predict(rf_mtry_14,newdata = test_small)

confMatrix.rf_mtry_14<-confusionMatrix(table(p_rf_mtry_14, test_small$SIT_PROP_YN))

confMatrix.rf_mtry_14$overall["Accuracy"]

confMatrix.rf_mtry_14$byClass["F1"]

set.seed(1, sample.kind = "Rounding")
tunegrid <- expand.grid(.mtry=17)
rf_mtry_17 <- train(SIT_PROP_YN~., data=train_small, method="rf", metric="Accuracy", tuneGrid=tunegrid, ntree = 1000)

print(rf_mtry_17)

p_rf_mtry_17<-predict(rf_mtry_17,newdata = test_small)

confMatrix.rf_mtry_17<-confusionMatrix(table(p_rf_mtry_17, test_small$SIT_PROP_YN))

confMatrix.rf_mtry_17$overall["Accuracy"]

confMatrix.rf_mtry_17$byClass["F1"]

### Random Forest summary

#The following data frame shows how each *random forest* try performed:
  
model.summary <- data.frame(Model = c("RF", "RF_CV", "RF_random_search", "RF_mtry_14", "RF_mtry_17"), 
                            Accuracy = c(confMatrix.rf$overall["Accuracy"],
                                         confMatrix.rf_cvt$overall["Accuracy"],
                                         confMatrix.rf_random$overall["Accuracy"],
                                         confMatrix.rf_mtry_14$overall["Accuracy"],
                                         confMatrix.rf_mtry_17$overall["Accuracy"]), 
                            Sensitivity = c(confMatrix.rf$byClass["Sensitivity"],
                                            confMatrix.rf_cvt$byClass["Sensitivity"],
                                            confMatrix.rf_random$byClass["Sensitivity"],
                                            confMatrix.rf_mtry_14$byClass["Sensitivity"],
                                            confMatrix.rf_mtry_17$byClass["Sensitivity"]),
                            Specificity = c(confMatrix.rf$byClass["Specificity"],
                                            confMatrix.rf_cvt$byClass["Specificity"],
                                            confMatrix.rf_random$byClass["Specificity"],
                                            confMatrix.rf_mtry_14$byClass["Specificity"],
                                            confMatrix.rf_mtry_17$byClass["Specificity"]),
                            Precision = c(confMatrix.rf$byClass["Precision"],
                                          confMatrix.rf_cvt$byClass["Precision"],
                                          confMatrix.rf_random$byClass["Precision"],
                                          confMatrix.rf_mtry_14$byClass["Precision"],
                                          confMatrix.rf_mtry_17$byClass["Precision"]),
                            Recall = c(confMatrix.rf$byClass["Recall"],
                                       confMatrix.rf_cvt$byClass["Recall"],
                                       confMatrix.rf_random$byClass["Recall"],
                                       confMatrix.rf_mtry_14$byClass["Recall"],
                                       confMatrix.rf_mtry_17$byClass["Recall"]),
                            F1 = c(confMatrix.rf$byClass["F1"],
                                   confMatrix.rf_cvt$byClass["F1"],
                                   confMatrix.rf_random$byClass["F1"],
                                   confMatrix.rf_mtry_14$byClass["F1"],
                                   confMatrix.rf_mtry_17$byClass["F1"]))



model.summary[order(-model.summary$Accuracy),]

#As we can see, all Random Forest models performed very similarly. In the following section, **Results**,  I will present the best model.

###################################################################################################
# RESULTS
###################################################################################################

## Applying chosen model to train_set and test_set

#The model with best result in the train_small set was *randomForest without tuning*. I ran this model in train and test sets:
  
  
set.seed(1, sample.kind="Rounding") 

rf_final = randomForest(SIT_PROP_YN ~ .,data=train_set,ntree= 500, importance=TRUE)

plot(rf_final)
p_rf_final<-predict(rf_final,newdata = test_set)

confMatrix.rf_final<-confusionMatrix(table(p_rf_final,test_set$SIT_PROP_YN))

confMatrix.rf_final$overall["Accuracy"] 
confMatrix.rf_final$byClass["Sensitivity"]
confMatrix.rf_final$byClass["Specificity"]
confMatrix.rf_final$byClass["Precision"]
confMatrix.rf_final$byClass["Recall"]
confMatrix.rf_final$byClass["F1"]

## Accuracy 
## 0.9288876
## Sensitivity 
## 0.9320843
## Specificity 
## 0.9267862
## Precision 
## 0.8932603
## Recall 
## 0.9320843
## F1 
## 0.9122594
