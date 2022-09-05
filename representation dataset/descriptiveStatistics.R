library("writexl")
library("openxlsx")
#library("stringr")

teacher2020 <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - 2020 Teacher Projects.csv")
intern2020 <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - 2020 Intern Projects.csv")
intern2021 <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - 2021 Intern Projects.csv")
allTeachers <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - All teachers.csv")
allInterns <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - All interns.csv")
all <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - All.csv")

df1 <- data.frame(Variables = c("hasAvatar", "noAvatar", "playAvatar", "npcAvatar", "guideAvatar", "humanAvatar", "femaleAvatar", "maleAvatar", "nbAvatar", "whiteAvatar", "pocAvatar", "historicAvatar", "youngAvatar", "multipleAvatars", "preloadedAvatar", "importedAvatar", "womenOfColor", "humanPlayAvatar", "femalePlayAvatar", "malePlayAvatar", "nbPlayAvatar", "whitePlayAvatar", "pocPlayAvatar", "historicPlayAvatar", "youngPlayAvatar", "preloadPlayAvatar", "importPlayAvatar", "humanNPCAvatar", "femaleNPCAvatar", "maleNPCAvatar", "nbNPCAvatar", "whiteNPCAvatar", "pocNPCAvatar", "historicNPCAvatar", "youngNPCAvatar", "preloadNPCAvatar", "importNPCAvatar", "humanGuideAvatar", "femaleGuideAvatar","maleGuideAvatar", "nbGuideAvatar", "whiteGuideAvatar", "pocGuideAvatar", "historicGuideAvatar", "youngGuideAvatar", "preloadGuideAvatar", "importGuideAvatar"))
avatarDS <- "C:/Users/nalin/Downloads/NC State/Datasets/Descriptive/avatarDS.xlsx"
creatorDS <- "C:/Users/nalin/Downloads/NC State/Datasets/Descriptive/creatorDS.xlsx"

##############functions##########################
checkCondition <- function(row, column, value, storeList, data){
  if (data[row, column] == value) {
    return (data[row, 1])
  }
}

getDS <- function(list, denom){
  return (length(list)/denom)
}

cleanData <- function(data, x){
  colnames(data)[1] <- "project"
  colnames(data)[2] <- "description"
  colnames(data)[3] <- "subjects"
  colnames(data)[4] <- "grades"
  colnames(data)[5] <- "standards"
  colnames(data)[10] <- "avatar"
  colnames(data)[11] <- "type_of_avatar"
  colnames(data)[12] <- "human_avatar"
  colnames(data)[13] <- "avatar_gender"
  colnames(data)[14] <- "avatar_race"
  colnames(data)[15] <- "historic_lit_avatar"
  colnames(data)[16] <- "youth"
  colnames(data)[17] <- "multiple"
  colnames(data)[18] <- "preloaded_imported"
  
  ###################### cleaning specific data #####################
  if (x == "teacher2019"){
    for (i in 2:12) {
      data[i, 1] <- "A Midsummer Night's Dream Character Confessional and Scene Selection"
    }
    
    for (i in 2:12) {
      data[i, 2] <- "How do characters in A Midsummer Night's Dream try to control each other, and what happens when they do?"
    }
    
    data[98, 1] <- "Let's Tessellate!"
    
    data <- data[-c(30, 62, 67, 75, 123, 134, 144, 145),]
  }
  
  if (x == "teacher2020"){
    data <- data[-c(47, 55, 59),]
  }
  
  if (x == "intern2020"){
    data <- data[-c(91),]
    
    data <- data[-c(10)]
  }
  
  # cleaning white space
  for (i in 10:ncol(data)) {
    for (j in 1:nrow(data)) {
      data[j, i] <- toupper(data[j, i])
      data[j, i] <- gsub("[[:blank:]]", "", data[j, i])
      #data[j, i] <- str_replace_all(data[j, i], ' ', '')
    }
  }
  
  return(data)
}

combineCategories <- function(row, columnList, valueList, data){
  counter <- 0
  
  #if(!data[row, 1] %in% storeList){
    for (j in 1:length(columnList)){
      if (data[row, columnList[j]] == valueList[j]){
        counter <- counter + 1
      }
    }
    
    if (counter == length(columnList)){
      #print(valueList)
      #print(data[row, 1])
      return(data[row, 1])
    }
  #}
}

enterMatrix <- function(location, sheet, row, column, value){
  dir <- paste("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/", location, ".xlsx", sep="")
  wb <- loadWorkbook(dir)
  
  writeData(wb, sheet=sheet, data.frame(value=value), startRow= row, startCol= column, colNames=FALSE)
  saveWorkbook(wb, dir, overwrite=TRUE)
}

mainFunction <- function(dataset, name){
  dataset <- cleanData(dataset, name)
  
  # initialization
  ######### general vars ###########
  allLessons <- list()
  hasAvatar <- list()
  noAvatar <- list()
  playAvatar <- list()
  npcAvatar <- list()
  guideAvatar <- list()
  humanAvatar <- list()
  femaleAvatar <- list()
  maleAvatar <- list()
  nbAvatar <- list()
  whiteAvatar <- list()
  pocAvatar <- list()
  historicAvatar <- list()
  youngAvatar <- list()
  multipleAvatars <- list()
  preloadedAvatar <- list()
  importedAvatar <- list()

  ########## specific combinations ########
  wocAvatar <- list()
  
  ############# combinations ########
  humanPlayAvatar <- list()
  NhumanPlayAvatar <- list()
  femalePlayAvatar <- list()
  malePlayAvatar <- list()
  nbPlayAvatar <- list()
  whitePlayAvatar <- list()
  pocPlayAvatar <- list()
  historicPlayAvatar <- list()
  NhistoricPlayAvatar <- list()
  youngPlayAvatar <- list()
  NyoungPlayAvatar <- list()
  preloadPlayAvatar <- list()
  importPlayAvatar <- list()
  
  humanNPCAvatar <- list()
  NhumanNPCAvatar <- list()
  femaleNPCAvatar <- list()
  maleNPCAvatar <- list()
  nbNPCAvatar <- list()
  whiteNPCAvatar <- list()
  pocNPCAvatar <- list()
  historicNPCAvatar <- list()
  NhistoricNPCAvatar <- list()
  youngNPCAvatar <- list()
  NyoungNPCAvatar <- list()
  preloadNPCAvatar <- list()
  importNPCAvatar <- list()
  
  humanGuideAvatar <- list()
  NhumanGuideAvatar <- list()
  femaleGuideAvatar <- list()
  maleGuideAvatar <- list()
  nbGuideAvatar <- list()
  whiteGuideAvatar <- list()
  pocGuideAvatar <- list()
  historicGuideAvatar <- list()
  NhistoricGuideAvatar <- list()
  youngGuideAvatar <- list()
  NyoungGuideAvatar <- list()
  preloadGuideAvatar <- list()
  importGuideAvatar <- list()

  
  # creating vars
  for (i in 1:nrow(dataset)) {
    
    if (!dataset[i,1] %in% allLessons){
      allLessons <- append(allLessons, dataset[i,1])
    }
    
    numLessons <- length(allLessons)
    
    ################## general variables #########
    hasAvatar <- append(hasAvatar, checkCondition(i, 10, "Y", hasAvatar, dataset))
    noAvatar <- append(noAvatar, checkCondition(i, 10, "N", noAvatar, dataset))
    playAvatar <- append(playAvatar, checkCondition(i, 11, "PC", playAvatar, dataset))
    npcAvatar <- append(npcAvatar, checkCondition(i, 11, "NPC", npcAvatar, dataset))
    guideAvatar <- append(guideAvatar, checkCondition(i, 11, "G", guideAvatar, dataset))
    humanAvatar <- append(humanAvatar, checkCondition(i, 12, "Y", humanAvatar, dataset))
    femaleAvatar <- append(femaleAvatar, checkCondition(i, 13, "F", femaleAvatar, dataset))
    maleAvatar <- append(maleAvatar, checkCondition(i, 13, "M", maleAvatar, dataset))
    nbAvatar <- append(nbAvatar, checkCondition(i, 13, "NB", nbAvatar, dataset))
    whiteAvatar <- append(whiteAvatar, checkCondition(i, 14, "W", whiteAvatar, dataset))
    pocAvatar <- append(pocAvatar, checkCondition(i, 14, "POC", pocAvatar, dataset))
    historicAvatar <- append(historicAvatar, checkCondition(i, 15, "Y", historicAvatar, dataset))
    youngAvatar <- append(youngAvatar, checkCondition(i, 16, "Y", youngAvatar, dataset))
    multipleAvatars <- append(multipleAvatars, checkCondition(i, 17, "Y", multipleAvatars, dataset))
    preloadedAvatar <- append(preloadedAvatar, checkCondition(i, 18, "P", preloadedAvatar, dataset))
    importedAvatar <- append(importedAvatar, checkCondition(i, 18, "I", importedAvatar, dataset))
    
    ########################## specific combinations #################
    wocAvatar <- append(wocAvatar, combineCategories(i, c(13, 14), c("F", "POC"), dataset))
    
    ########################### combinations #################
    humanPlayAvatar <- append(humanPlayAvatar, combineCategories(i, c(11, 12), c("PC", "Y"), dataset))
    NhumanPlayAvatar <- append(NhumanPlayAvatar, combineCategories(i, c(11, 12), c("PC", "N"), dataset))
    femalePlayAvatar <- append(femalePlayAvatar, combineCategories(i, c(11, 13), c("PC", "F"), dataset))
    malePlayAvatar <- append(malePlayAvatar, combineCategories(i, c(11, 13), c("PC", "M"), dataset))
    nbPlayAvatar <- append(nbPlayAvatar, combineCategories(i, c(11, 13), c("PC", "NB"), dataset))
    whitePlayAvatar <- append(whitePlayAvatar, combineCategories(i, c(11, 14), c("PC", "W"), dataset))
    pocPlayAvatar <- append(pocPlayAvatar, combineCategories(i, c(11, 14), c("PC", "POC"), dataset))
    historicPlayAvatar <- append(historicPlayAvatar, combineCategories(i, c(11, 15), c("PC", "Y"), dataset))
    NhistoricPlayAvatar <- append(NhistoricPlayAvatar, combineCategories(i, c(11, 15), c("PC", "N"), dataset))
    youngPlayAvatar <- append(youngPlayAvatar, combineCategories(i, c(11, 16), c("PC", "Y"), dataset))
    NyoungPlayAvatar <- append(NyoungPlayAvatar, combineCategories(i, c(11, 16), c("PC", "N"), dataset))
    preloadPlayAvatar <- append(preloadPlayAvatar, combineCategories(i, c(11, 18), c("PC", "P"), dataset))
    importPlayAvatar <- append(importPlayAvatar, combineCategories(i, c(11, 18), c("PC", "I"), dataset))
    
    humanNPCAvatar <- append(humanNPCAvatar, combineCategories(i, c(11, 12), c("NPC", "Y"), dataset))
    NhumanNPCAvatar <- append(NhumanNPCAvatar, combineCategories(i, c(11, 12), c("NPC", "N"), dataset))
    femaleNPCAvatar <- append(femaleNPCAvatar, combineCategories(i, c(11, 13), c("NPC", "F"), dataset))
    maleNPCAvatar <- append(maleNPCAvatar, combineCategories(i, c(11, 13), c("NPC", "M"), dataset))
    nbNPCAvatar <- append(nbNPCAvatar, combineCategories(i, c(11, 13), c("NPC", "NB"), dataset))
    whiteNPCAvatar <- append(whiteNPCAvatar, combineCategories(i, c(11, 14), c("NPC", "W"), dataset))
    pocNPCAvatar <- append(pocNPCAvatar, combineCategories(i, c(11, 14), c("NPC", "POC"), dataset))
    historicNPCAvatar <- append(historicNPCAvatar, combineCategories(i, c(11, 15), c("NPC", "Y"), dataset))
    NhistoricNPCAvatar <- append(NhistoricNPCAvatar, combineCategories(i, c(11, 15), c("NPC", "N"), dataset))
    youngNPCAvatar <- append(youngNPCAvatar, combineCategories(i, c(11, 16), c("NPC", "Y"), dataset))
    NyoungNPCAvatar <- append(NyoungNPCAvatar, combineCategories(i, c(11, 16), c("NPC", "N"), dataset))
    preloadNPCAvatar <- append(preloadNPCAvatar, combineCategories(i, c(11, 18), c("NPC", "P"), dataset))
    importNPCAvatar <- append(importNPCAvatar, combineCategories(i, c(11, 18), c("NPC", "I"), dataset))
    
    humanGuideAvatar <- append(humanGuideAvatar, combineCategories(i, c(11, 12), c("G", "Y"), dataset))
    NhumanGuideAvatar <- append(NhumanGuideAvatar, combineCategories(i, c(11, 12), c("G", "N"), dataset))
    femaleGuideAvatar <- append(femaleGuideAvatar, combineCategories(i, c(11, 13), c("G", "F"), dataset))
    maleGuideAvatar <- append(maleGuideAvatar, combineCategories(i, c(11, 13), c("G", "M"), dataset))
    nbGuideAvatar <- append(nbGuideAvatar, combineCategories(i, c(11, 13), c("G", "NB"), dataset))
    whiteGuideAvatar <- append(whiteGuideAvatar, combineCategories(i, c(11, 14), c("G", "W"), dataset))
    pocGuideAvatar <- append(pocGuideAvatar, combineCategories(i, c(11, 14), c("G", "POC"), dataset))
    historicGuideAvatar <- append(historicGuideAvatar, combineCategories(i, c(11, 15), c("G", "Y"), dataset))
    NhistoricGuideAvatar <- append(NhistoricGuideAvatar, combineCategories(i, c(11, 15), c("G", "N"), dataset))
    youngGuideAvatar <- append(youngGuideAvatar, combineCategories(i, c(11, 16), c("G", "Y"), dataset))
    NyoungGuideAvatar <- append(NyoungGuideAvatar, combineCategories(i, c(11, 16), c("G", "N"), dataset))
    preloadGuideAvatar <- append(preloadGuideAvatar, combineCategories(i, c(11, 18), c("G", "P"), dataset))
    importGuideAvatar <- append(importGuideAvatar, combineCategories(i, c(11, 18), c("G", "I"), dataset))
  }
  
  numAvatars <- length(hasAvatar)
  numHumans <- length(humanAvatar)
  numPlay <- length(playAvatar)
  numNPC <- length(npcAvatar)
  numGuide <- length(guideAvatar)
  
  DShasAvatar <- getDS(unique(hasAvatar), numLessons)
  DSnoAvatar <- getDS(unique(noAvatar), numLessons)
  DSplayAvatar <- getDS(playAvatar, numAvatars)
  DSnpcAvatar <- getDS(npcAvatar, numAvatars)
  DSguideAvatar <- getDS(guideAvatar, numAvatars)
  DShumanAvatar <- getDS(humanAvatar, numAvatars)
  DSfemaleAvatar <- getDS(femaleAvatar, numHumans)
  DSmaleAvatar <- getDS(maleAvatar, numHumans)
  DSnbAvatar <- getDS(nbAvatar, numHumans)
  DSwhiteAvatar <- getDS(whiteAvatar, numHumans)
  DSpocAvatar <- getDS(pocAvatar, numHumans)
  DShistoricAvatar <- getDS(historicAvatar, numHumans)
  DSyoungAvatar <- getDS(youngAvatar, numHumans)
  DSmultipleAvatars <- getDS(unique(multipleAvatars), numLessons)
  DSpreloadedAvatar <- getDS(preloadedAvatar, numAvatars)
  DSimportedAvatar <- getDS(importedAvatar, numAvatars)
  
  DSwocAvatar <- getDS(wocAvatar, numAvatars)
  
  DShumanPlayAvatar <- getDS(humanPlayAvatar, numPlay)
  enterMatrix(name, "HUMAN", 2, "B", length(humanPlayAvatar))
  enterMatrix(name, "HUMAN", 2, "C", length(NhumanPlayAvatar))
  DSfemalePlayAvatar <- getDS(femalePlayAvatar, numPlay)
  enterMatrix(name, "GENDER", 2, "B", length(femalePlayAvatar))
  DSmalePlayAvatar <- getDS(malePlayAvatar, numPlay)
  enterMatrix(name, "GENDER", 2, "C", length(malePlayAvatar))
  DSnbPlayAvatar <- getDS(nbPlayAvatar, numPlay)
  enterMatrix(name, "GENDER", 2, "D", length(nbPlayAvatar))
  DSwhitePlayAvatar <- getDS(whitePlayAvatar, numPlay)
  enterMatrix(name, "RACE", 2, "B", length(whitePlayAvatar))
  DSpocPlayAvatar <- getDS(pocPlayAvatar, numPlay)
  enterMatrix(name, "RACE", 2, "C", length(pocPlayAvatar))
  DShistoricPlayAvatar <- getDS(historicPlayAvatar, numPlay)
  enterMatrix(name, "HISTORIC", 2, "B", length(historicPlayAvatar))
  enterMatrix(name, "HISTORIC", 2, "C", length(NhistoricPlayAvatar))
  DSyoungPlayAvatar <- getDS(youngPlayAvatar, numPlay)
  enterMatrix(name, "YOUTH", 2, "B", length(youngPlayAvatar))
  enterMatrix(name, "YOUTH", 2, "C", length(NyoungPlayAvatar))
  DSpreloadPlayAvatar <- getDS(preloadPlayAvatar, numPlay)
  enterMatrix(name, "COSTUME", 2, "B", length(preloadPlayAvatar))
  DSimportPlayAvatar <- getDS(importPlayAvatar, numPlay)
  enterMatrix(name, "COSTUME", 2, "C", length(importPlayAvatar))
  
  DShumanNPCAvatar <- getDS(humanNPCAvatar, numNPC)
  enterMatrix(name, "HUMAN", 3, "B", length(humanNPCAvatar))
  enterMatrix(name, "HUMAN", 3, "C", length(NhumanNPCAvatar))
  DSfemaleNPCAvatar <- getDS(femaleNPCAvatar, numNPC)
  enterMatrix(name, "GENDER", 3, "B", length(femaleNPCAvatar))
  DSmaleNPCAvatar <- getDS(maleNPCAvatar, numNPC)
  enterMatrix(name, "GENDER", 3, "C", length(maleNPCAvatar))
  DSnbNPCAvatar <- getDS(nbNPCAvatar, numNPC)
  enterMatrix(name, "GENDER", 3, "D", length(nbNPCAvatar))
  DSwhiteNPCAvatar <- getDS(whiteNPCAvatar, numNPC)
  enterMatrix(name, "RACE", 3, "B", length(whiteNPCAvatar))
  DSpocNPCAvatar <- getDS(pocNPCAvatar, numNPC)
  enterMatrix(name, "RACE", 3, "C", length(pocNPCAvatar))
  DShistoricNPCAvatar <- getDS(historicNPCAvatar, numNPC)
  enterMatrix(name, "HISTORIC", 3, "B", length(historicNPCAvatar))
  enterMatrix(name, "HISTORIC", 3, "C", length(NhistoricNPCAvatar))
  DSyoungNPCAvatar <- getDS(youngNPCAvatar, numNPC)
  enterMatrix(name, "YOUTH", 3, "B", length(youngNPCAvatar))
  enterMatrix(name, "YOUTH", 3, "C", length(NyoungNPCAvatar))
  DSpreloadNPCAvatar <- getDS(preloadNPCAvatar, numNPC)
  enterMatrix(name, "COSTUME", 3, "B", length(preloadNPCAvatar))
  DSimportNPCAvatar <- getDS(importNPCAvatar, numNPC)
  enterMatrix(name, "COSTUME", 3, "C", length(importNPCAvatar))
  
  DShumanGuideAvatar <- getDS(humanGuideAvatar, numGuide)
  enterMatrix(name, "HUMAN", 4, "B", length(humanGuideAvatar))
  enterMatrix(name, "HUMAN", 4, "C", length(NhumanGuideAvatar))
  DSfemaleGuideAvatar <- getDS(femaleGuideAvatar, numGuide)
  enterMatrix(name, "GENDER", 4, "B", length(femaleGuideAvatar))
  DSmaleGuideAvatar <- getDS(maleGuideAvatar, numGuide)
  enterMatrix(name, "GENDER", 4, "C", length(maleGuideAvatar))
  DSnbGuideAvatar <- getDS(nbGuideAvatar, numGuide)
  enterMatrix(name, "GENDER", 4, "D", length(nbGuideAvatar))
  DSwhiteGuideAvatar <- getDS(whiteGuideAvatar, numGuide)
  enterMatrix(name, "RACE", 4, "B", length(whiteGuideAvatar))
  DSpocGuideAvatar <- getDS(pocGuideAvatar, numGuide)
  enterMatrix(name, "RACE", 4, "C", length(pocGuideAvatar))
  DShistoricGuideAvatar <- getDS(historicGuideAvatar, numGuide)
  enterMatrix(name, "HISTORIC", 4, "B", length(historicGuideAvatar))
  enterMatrix(name, "HISTORIC", 4, "C", length(NhistoricGuideAvatar))
  DSyoungGuideAvatar <- getDS(youngGuideAvatar, numGuide)
  enterMatrix(name, "YOUTH", 4, "B", length(youngGuideAvatar))
  enterMatrix(name, "YOUTH", 4, "C", length(NyoungGuideAvatar))
  DSpreloadGuideAvatar <- getDS(preloadGuideAvatar, numGuide)
  enterMatrix(name, "COSTUME", 4, "B", length(preloadGuideAvatar))
  DSimportGuideAvatar <- getDS(importGuideAvatar, numGuide)
  enterMatrix(name, "COSTUME", 4, "C", length(importGuideAvatar))
  
  df[name] <- c(DShasAvatar, DSnoAvatar, DSplayAvatar, DSnpcAvatar, DSguideAvatar, DShumanAvatar, DSfemaleAvatar, DSmaleAvatar, DSnbAvatar, DSwhiteAvatar, DSpocAvatar, DShistoricAvatar, DSyoungAvatar, DSmultipleAvatars, DSpreloadedAvatar, DSimportedAvatar, DSwocAvatar, DShumanPlayAvatar, DSfemalePlayAvatar, DSmalePlayAvatar, DSnbPlayAvatar, DSwhitePlayAvatar, DSpocPlayAvatar, DShistoricPlayAvatar, DSyoungPlayAvatar, DSpreloadPlayAvatar, DSimportPlayAvatar, DShumanNPCAvatar, DSfemaleNPCAvatar, DSmaleNPCAvatar, DSnbNPCAvatar, DSwhiteNPCAvatar, DSpocNPCAvatar, DShistoricNPCAvatar, DSyoungNPCAvatar, DSpreloadNPCAvatar, DSimportNPCAvatar, DShumanGuideAvatar, DSfemaleGuideAvatar,DSmaleGuideAvatar, DSnbGuideAvatar, DSwhiteGuideAvatar, DSpocGuideAvatar, DShistoricGuideAvatar, DSyoungGuideAvatar, DSpreloadGuideAvatar, DSimportGuideAvatar)
  
  return(df)
}

mainFunctionC_gender <- function(dataset, name){
  dataset <- cleanData(dataset, name)
  
  # initialization
  ######### general vars ###########
  if (name == "intern2020") {
    allCreators <- list('2020A', '2020B', '2020C', '2020D', '2020E', '2020F', '2020G', '2020H', '2020J', '2020K', '2020L', '2020M', '2020N', '2020P', '2020Q', '2020R', '2020S', '2020T', '2020U', '2020V', '2020W', '2020X', '2020Y', '2020Z', '2020AA', '2020BB', '2020CC', '2020DD', '2020EE', '2020FF')
  }
  
  if (name == "intern2021") {
    allCreators <- list('2021A', '2021B', '2021C', '2021D', '2021E', '2021F', '2021G', '2021H', '2021J', '2021K', '2021L', '2021M', '2021N', '2021P', '2021Q', '2021R', '2021S', '2021T', '2021U', '2021V', '2021W', '2021X', '2021Y', '2021Z', '2021AA', '2021BB', '2021CC', '2021DD', '2021EE', '2021FF', '2021GG', '2021HH', '2021JJ', '2021KK', '2021LL')
  }
  
  if (name == "allInterns") {
    allCreators <- list('2020A', '2020B', '2020C', '2020D', '2020E', '2020F', '2020G', '2020H', '2020J', '2020K', '2020L', '2020M', '2020N', '2020P', '2020Q', '2020R', '2020S', '2020T', '2020U', '2020V', '2020W', '2020X', '2020Y', '2020Z', '2020AA', '2020BB', '2020CC', '2020DD', '2020EE', '2020FF', '2021A', '2021B', '2021C', '2021D', '2021E', '2021F', '2021G', '2021H', '2021J', '2021K', '2021L', '2021M', '2021N', '2021P', '2021Q', '2021R', '2021S', '2021T', '2021U', '2021V', '2021W', '2021X', '2021Y', '2021Z', '2021AA', '2021BB', '2021CC', '2021DD', '2021EE', '2021FF', '2021GG', '2021HH', '2021JJ', '2021KK', '2021LL')
  }
  
  humanFAvatar <- list()
  NhumanFAvatar <- list()
  femaleFAvatar <- list()
  maleFAvatar <- list()
  nbFAvatar <- list()
  whiteFAvatar <- list()
  pocFAvatar <- list()
  historicFAvatar <- list()
  NhistoricFAvatar <- list()
  youngFAvatar <- list()
  NyoungFAvatar <- list()
  preloadFAvatar <- list()
  importFAvatar <- list()
  
  humanMAvatar <- list()
  NhumanMAvatar <- list()
  femaleMAvatar <- list()
  maleMAvatar <- list()
  nbMAvatar <- list()
  whiteMAvatar <- list()
  pocMAvatar <- list()
  historicMAvatar <- list()
  NhistoricMAvatar <- list()
  youngMAvatar <- list()
  NyoungMAvatar <- list()
  preloadMAvatar <- list()
  importMAvatar <- list()
  
  humanMXAvatar <- list()
  NhumanMXAvatar <- list()
  femaleMXAvatar <- list()
  maleMXAvatar <- list()
  nbMXAvatar <- list()
  whiteMXAvatar <- list()
  pocMXAvatar <- list()
  historicMXAvatar <- list()
  NhistoricMXAvatar <- list()
  youngMXAvatar <- list()
  NyoungMXAvatar <- list()
  preloadMXAvatar <- list()
  importMXAvatar <- list()
  
  # creating vars
  for (i in 1:nrow(dataset)) {
    humanFAvatar <- append(humanFAvatar, combineCategories(i, c(21, 12), c("F", "Y"), dataset))
    NhumanFAvatar <- append(NhumanFAvatar, combineCategories(i, c(21, 12), c("F", "N"), dataset))
    femaleFAvatar <- append(femaleFAvatar, combineCategories(i, c(21, 13), c("F", "F"), dataset))
    maleFAvatar <- append(maleFAvatar, combineCategories(i, c(21, 13), c("F", "M"), dataset))
    nbFAvatar <- append(nbFAvatar, combineCategories(i, c(21, 13), c("F", "NB"), dataset))
    whiteFAvatar <- append(whiteFAvatar, combineCategories(i, c(21, 14), c("F", "W"), dataset))
    pocFAvatar <- append(pocFAvatar, combineCategories(i, c(21, 14), c("F", "POC"), dataset))
    historicFAvatar <- append(historicFAvatar, combineCategories(i, c(21, 15), c("F", "Y"), dataset))
    NhistoricFAvatar <- append(NhistoricFAvatar, combineCategories(i, c(21, 15), c("F", "N"), dataset))
    youngFAvatar <- append(youngFAvatar, combineCategories(i, c(21, 16), c("F", "Y"), dataset))
    NyoungFAvatar <- append(NyoungFAvatar, combineCategories(i, c(21, 16), c("F", "N"), dataset))
    preloadFAvatar <- append(preloadFAvatar, combineCategories(i, c(21, 18), c("F", "P"), dataset))
    importFAvatar <- append(importFAvatar, combineCategories(i, c(21, 18), c("F", "I"), dataset))
    
    humanMAvatar <- append(humanMAvatar, combineCategories(i, c(21, 12), c("M", "Y"), dataset))
    NhumanMAvatar <- append(NhumanMAvatar, combineCategories(i, c(21, 12), c("M", "N"), dataset))
    femaleMAvatar <- append(femaleMAvatar, combineCategories(i, c(21, 13), c("M", "F"), dataset))
    maleMAvatar <- append(maleMAvatar, combineCategories(i, c(21, 13), c("M", "M"), dataset))
    nbMAvatar <- append(nbMAvatar, combineCategories(i, c(21, 13), c("M", "NB"), dataset))
    whiteMAvatar <- append(whiteMAvatar, combineCategories(i, c(21, 14), c("M", "W"), dataset))
    pocMAvatar <- append(pocMAvatar, combineCategories(i, c(21, 14), c("M", "POC"), dataset))
    historicMAvatar <- append(historicMAvatar, combineCategories(i, c(21, 15), c("M", "Y"), dataset))
    NhistoricMAvatar <- append(NhistoricMAvatar, combineCategories(i, c(21, 15), c("M", "N"), dataset))
    youngMAvatar <- append(youngMAvatar, combineCategories(i, c(21, 16), c("M", "Y"), dataset))
    NyoungMAvatar <- append(NyoungMAvatar, combineCategories(i, c(21, 16), c("M", "N"), dataset))
    preloadMAvatar <- append(preloadMAvatar, combineCategories(i, c(21, 18), c("M", "P"), dataset))
    importMAvatar <- append(importMAvatar, combineCategories(i, c(21, 18), c("M", "I"), dataset))
    
    humanMXAvatar <- append(humanMXAvatar, combineCategories(i, c(21, 12), c("MX", "Y"), dataset))
    NhumanMXAvatar <- append(NhumanMXAvatar, combineCategories(i, c(21, 12), c("MX", "N"), dataset))
    femaleMXAvatar <- append(femaleMXAvatar, combineCategories(i, c(21, 13), c("MX", "F"), dataset))
    maleMXAvatar <- append(maleMXAvatar, combineCategories(i, c(21, 13), c("MX", "M"), dataset))
    nbMXAvatar <- append(nbMXAvatar, combineCategories(i, c(21, 13), c("MX", "NB"), dataset))
    whiteMXAvatar <- append(whiteMXAvatar, combineCategories(i, c(21, 14), c("MX", "W"), dataset))
    pocMXAvatar <- append(pocMXAvatar, combineCategories(i, c(21, 14), c("MX", "POC"), dataset))
    historicMXAvatar <- append(historicMXAvatar, combineCategories(i, c(21, 15), c("MX", "Y"), dataset))
    NhistoricMXAvatar <- append(NhistoricMXAvatar, combineCategories(i, c(21, 15), c("MX", "N"), dataset))
    youngMXAvatar <- append(youngMXAvatar, combineCategories(i, c(21, 16), c("MX", "Y"), dataset))
    NyoungMXAvatar <- append(NyoungMXAvatar, combineCategories(i, c(21, 16), c("MX", "N"), dataset))
    preloadMXAvatar <- append(preloadMXAvatar, combineCategories(i, c(21, 18), c("MX", "P"), dataset))
    importMXAvatar <- append(importMXAvatar, combineCategories(i, c(21, 18), c("MX", "I"), dataset))
  }
  
  numCreators <- length(allCreators)
  numAvatars <- length(hasAvatar)
  numHumans <- length(humanAvatar)
  numPlay <- length(playAvatar)
  numNPC <- length(npcAvatar)
  numGuide <- length(guideAvatar)
  
  # DShasAvatar <- getDS(unique(hasAvatar), numLessons)
  # DSnoAvatar <- getDS(unique(noAvatar), numLessons)
  # DSplayAvatar <- getDS(playAvatar, numAvatars)
  # DSnpcAvatar <- getDS(npcAvatar, numAvatars)
  # DSguideAvatar <- getDS(guideAvatar, numAvatars)
  # DShumanAvatar <- getDS(humanAvatar, numAvatars)
  # DSfemaleAvatar <- getDS(femaleAvatar, numHumans)
  # DSmaleAvatar <- getDS(maleAvatar, numHumans)
  # DSnbAvatar <- getDS(nbAvatar, numHumans)
  # DSwhiteAvatar <- getDS(whiteAvatar, numHumans)
  # DSpocAvatar <- getDS(pocAvatar, numHumans)
  # DShistoricAvatar <- getDS(historicAvatar, numHumans)
  # DSyoungAvatar <- getDS(youngAvatar, numHumans)
  # DSmultipleAvatars <- getDS(unique(multipleAvatars), numLessons)
  # DSpreloadedAvatar <- getDS(preloadedAvatar, numAvatars)
  # DSimportedAvatar <- getDS(importedAvatar, numAvatars)
  # 
  # DSwocAvatar <- getDS(wocAvatar, numAvatars)
  
  # DShumanPlayAvatar <- getDS(humanPlayAvatar, numPlay)
  enterMatrix(paste(name, "C_gender", sep=""), "HUMAN", 2, "B", length(humanFAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "HUMAN", 2, "C", length(NhumanFAvatar))
  # DSfemaleFAvatar <- getDS(femaleFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 2, "B", length(femaleFAvatar))
  # DSmaleFAvatar <- getDS(maleFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 2, "C", length(maleFAvatar))
  # DSnbFAvatar <- get# DS(nbFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 2, "D", length(nbFAvatar))
  # DSwhiteFAvatar <- getDS(whiteFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "RACE", 2, "B", length(whiteFAvatar))
  # DSpocFAvatar <- getDS(pocFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "RACE", 2, "C", length(pocFAvatar))
  # DShistoricFAvatar <- getDS(historicFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "HISTORIC", 2, "B", length(historicFAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "HISTORIC", 2, "C", length(NhistoricFAvatar))
  # DSyoungFAvatar <- getDS(youngFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "YOUTH", 2, "B", length(youngFAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "YOUTH", 2, "C", length(NyoungFAvatar))
  # DSpreloadFAvatar <- getDS(preloadFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "COSTUME", 2, "B", length(preloadFAvatar))
  # DSimportFAvatar <- getDS(importFAvatar, numF)
  enterMatrix(paste(name, "C_gender", sep=""), "COSTUME", 2, "C", length(importFAvatar))
  
  # DShumanNPCAvatar <- getDS(humanNPCAvatar, numNPC)
  enterMatrix(paste(name, "C_gender", sep=""), "HUMAN", 3, "B", length(humanMAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "HUMAN", 3, "C", length(NhumanMAvatar))
  # DSfemaleMAvatar <- getDS(femaleMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 3, "B", length(femaleMAvatar))
  # DSmaleMAvatar <- getDS(maleMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 3, "C", length(maleMAvatar))
  # DSnbMAvatar <- getDS(nbMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 3, "D", length(nbMAvatar))
  # DSwhiteMAvatar <- getDS(whiteMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "RACE", 3, "B", length(whiteMAvatar))
  # DSpocMAvatar <- getDS(pocMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "RACE", 3, "C", length(pocMAvatar))
  # DShistoricMAvatar <- getDS(historicMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "HISTORIC", 3, "B", length(historicMAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "HISTORIC", 3, "C", length(NhistoricMAvatar))
  # DSyoungMAvatar <- getDS(youngMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "YOUTH", 3, "B", length(youngMAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "YOUTH", 3, "C", length(NyoungMAvatar))
  # DSpreloadMAvatar <- getDS(preloadMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "COSTUME", 3, "B", length(preloadMAvatar))
  # DSimportMAvatar <- getDS(importMAvatar, numM)
  enterMatrix(paste(name, "C_gender", sep=""), "COSTUME", 3, "C", length(importMAvatar))
  
  # DShumanMXAvatar <- getDS(humanMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "HUMAN", 4, "B", length(humanMXAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "HUMAN", 4, "C", length(NhumanMXAvatar))
  # DSfemaleMXAvatar <- getDS(femaleMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 4, "B", length(femaleMXAvatar))
  # DSmaleMXAvatar <- getDS(maleMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 4, "C", length(maleMXAvatar))
  # DSnbMXAvatar <- getDS(nbMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "GENDER", 4, "D", length(nbMXAvatar))
  # DSwhiteMXAvatar <- getDS(whiteMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "RACE", 4, "B", length(whiteMXAvatar))
  # DSpocMXAvatar <- getDS(pocMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "RACE", 4, "C", length(pocMXAvatar))
  # DShistoricMXAvatar <- getDS(historicMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "HISTORIC", 4, "B", length(historicMXAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "HISTORIC", 4, "C", length(NhistoricMXAvatar))
  # DSyoungMXAvatar <- getDS(youngMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "YOUTH", 4, "B", length(youngMXAvatar))
  enterMatrix(paste(name, "C_gender", sep=""), "YOUTH", 4, "C", length(NyoungMXAvatar))
  # DSpreloadMXAvatar <- getDS(preloadMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "COSTUME", 4, "B", length(preloadMXAvatar))
  # DSimportMXAvatar <- getDS(importMXAvatar, numMX)
  enterMatrix(paste(name, "C_gender", sep=""), "COSTUME", 4, "C", length(importMXAvatar))
  
  # df[name] <- c(DShasAvatar, DSnoAvatar, DSplayAvatar, DSnpcAvatar, DSguideAvatar, DShumanAvatar, DSfemaleAvatar, DSmaleAvatar, DSnbAvatar, DSwhiteAvatar, DSpocAvatar, DShistoricAvatar, DSyoungAvatar, DSmultipleAvatars, DSpreloadedAvatar, DSimportedAvatar, DSwocAvatar, DShumanPlayAvatar, DSfemalePlayAvatar, DSmalePlayAvatar, DSnbPlayAvatar, DSwhitePlayAvatar, DSpocPlayAvatar, DShistoricPlayAvatar, DSyoungPlayAvatar, DSpreloadPlayAvatar, DSimportPlayAvatar, DShumanNPCAvatar, DSfemaleNPCAvatar, DSmaleNPCAvatar, DSnbNPCAvatar, DSwhiteNPCAvatar, DSpocNPCAvatar, DShistoricNPCAvatar, DSyoungNPCAvatar, DSpreloadNPCAvatar, DSimportNPCAvatar, DShumanGuideAvatar, DSfemaleGuideAvatar,DSmaleGuideAvatar, DSnbGuideAvatar, DSwhiteGuideAvatar, DSpocGuideAvatar, DShistoricGuideAvatar, DSyoungGuideAvatar, DSpreloadGuideAvatar, DSimportGuideAvatar)
  
  #return(df)
}

mainFunctionC_poc <- function(dataset, name){
  dataset <- cleanData(dataset, name)
  
  # initialization
  ######### general vars ###########
  if (name == "intern2020") {
    allCreators <- list('2020A', '2020B', '2020C', '2020D', '2020E', '2020F', '2020G', '2020H', '2020J', '2020K', '2020L', '2020M', '2020N', '2020P', '2020Q', '2020R', '2020S', '2020T', '2020U', '2020V', '2020W', '2020X', '2020Y', '2020Z', '2020AA', '2020BB', '2020CC', '2020DD', '2020EE', '2020FF')
  }
  
  if (name == "intern2021") {
    allCreators <- list('2021A', '2021B', '2021C', '2021D', '2021E', '2021F', '2021G', '2021H', '2021J', '2021K', '2021L', '2021M', '2021N', '2021P', '2021Q', '2021R', '2021S', '2021T', '2021U', '2021V', '2021W', '2021X', '2021Y', '2021Z', '2021AA', '2021BB', '2021CC', '2021DD', '2021EE', '2021FF', '2021GG', '2021HH', '2021JJ', '2021KK', '2021LL')
  }
  
  if (name == "allInterns") {
    allCreators <- list('2020A', '2020B', '2020C', '2020D', '2020E', '2020F', '2020G', '2020H', '2020J', '2020K', '2020L', '2020M', '2020N', '2020P', '2020Q', '2020R', '2020S', '2020T', '2020U', '2020V', '2020W', '2020X', '2020Y', '2020Z', '2020AA', '2020BB', '2020CC', '2020DD', '2020EE', '2020FF', '2021A', '2021B', '2021C', '2021D', '2021E', '2021F', '2021G', '2021H', '2021J', '2021K', '2021L', '2021M', '2021N', '2021P', '2021Q', '2021R', '2021S', '2021T', '2021U', '2021V', '2021W', '2021X', '2021Y', '2021Z', '2021AA', '2021BB', '2021CC', '2021DD', '2021EE', '2021FF', '2021GG', '2021HH', '2021JJ', '2021KK', '2021LL')
  }
  
  humanPOCAvatar <- list()
  NhumanPOCAvatar <- list()
  femalePOCAvatar <- list()
  malePOCAvatar <- list()
  nbPOCAvatar <- list()
  whitePOCAvatar <- list()
  pocPOCAvatar <- list()
  historicPOCAvatar <- list()
  NhistoricPOCAvatar <- list()
  youngPOCAvatar <- list()
  NyoungPOCAvatar <- list()
  preloadPOCAvatar <- list()
  importPOCAvatar <- list()
  
  humanWAvatar <- list()
  NhumanWAvatar <- list()
  femaleWAvatar <- list()
  maleWAvatar <- list()
  nbWAvatar <- list()
  whiteWAvatar <- list()
  pocWAvatar <- list()
  historicWAvatar <- list()
  NhistoricWAvatar <- list()
  youngWAvatar <- list()
  NyoungWAvatar <- list()
  preloadWAvatar <- list()
  importWAvatar <- list()
  
  humanMXAvatar <- list()
  NhumanMXAvatar <- list()
  femaleMXAvatar <- list()
  maleMXAvatar <- list()
  nbMXAvatar <- list()
  whiteMXAvatar <- list()
  pocMXAvatar <- list()
  historicMXAvatar <- list()
  NhistoricMXAvatar <- list()
  youngMXAvatar <- list()
  NyoungMXAvatar <- list()
  preloadMXAvatar <- list()
  importMXAvatar <- list()
  
  # creating vars
  for (i in 1:nrow(dataset)) {
    humanPOCAvatar <- append(humanPOCAvatar, combineCategories(i, c(22, 12), c("Y", "Y"), dataset))
    NhumanPOCAvatar <- append(NhumanPOCAvatar, combineCategories(i, c(22, 12), c("Y", "N"), dataset))
    femalePOCAvatar <- append(femalePOCAvatar, combineCategories(i, c(22, 13), c("Y", "Y"), dataset))
    malePOCAvatar <- append(malePOCAvatar, combineCategories(i, c(22, 13), c("Y", "M"), dataset))
    nbPOCAvatar <- append(nbPOCAvatar, combineCategories(i, c(22, 13), c("Y", "NB"), dataset))
    whitePOCAvatar <- append(whitePOCAvatar, combineCategories(i, c(22, 14), c("Y", "W"), dataset))
    pocPOCAvatar <- append(pocPOCAvatar, combineCategories(i, c(22, 14), c("Y", "POC"), dataset))
    historicPOCAvatar <- append(historicPOCAvatar, combineCategories(i, c(22, 15), c("Y", "Y"), dataset))
    NhistoricPOCAvatar <- append(NhistoricPOCAvatar, combineCategories(i, c(22, 15), c("Y", "N"), dataset))
    youngPOCAvatar <- append(youngPOCAvatar, combineCategories(i, c(22, 16), c("Y", "Y"), dataset))
    NyoungPOCAvatar <- append(NyoungPOCAvatar, combineCategories(i, c(22, 16), c("Y", "N"), dataset))
    preloadPOCAvatar <- append(preloadPOCAvatar, combineCategories(i, c(22, 18), c("Y", "P"), dataset))
    importPOCAvatar <- append(importPOCAvatar, combineCategories(i, c(22, 18), c("Y", "I"), dataset))
    
    humanWAvatar <- append(humanWAvatar, combineCategories(i, c(22, 12), c("N", "Y"), dataset))
    NhumanWAvatar <- append(NhumanWAvatar, combineCategories(i, c(22, 12), c("N", "N"), dataset))
    femaleWAvatar <- append(femaleWAvatar, combineCategories(i, c(22, 13), c("N", "F"), dataset))
    maleWAvatar <- append(maleWAvatar, combineCategories(i, c(22, 13), c("N", "N"), dataset))
    nbWAvatar <- append(nbWAvatar, combineCategories(i, c(22, 13), c("N", "NB"), dataset))
    whiteWAvatar <- append(whiteWAvatar, combineCategories(i, c(22, 14), c("N", "W"), dataset))
    pocWAvatar <- append(pocWAvatar, combineCategories(i, c(22, 14), c("N", "POC"), dataset))
    historicWAvatar <- append(historicWAvatar, combineCategories(i, c(22, 15), c("N", "Y"), dataset))
    NhistoricWAvatar <- append(NhistoricWAvatar, combineCategories(i, c(22, 15), c("N", "N"), dataset))
    youngWAvatar <- append(youngWAvatar, combineCategories(i, c(22, 16), c("N", "Y"), dataset))
    NyoungWAvatar <- append(NyoungWAvatar, combineCategories(i, c(22, 16), c("N", "N"), dataset))
    preloadWAvatar <- append(preloadWAvatar, combineCategories(i, c(22, 18), c("N", "P"), dataset))
    importWAvatar <- append(importWAvatar, combineCategories(i, c(22, 18), c("N", "I"), dataset))
    
    humanMXAvatar <- append(humanMXAvatar, combineCategories(i, c(22, 12), c("M", "Y"), dataset))
    NhumanMXAvatar <- append(NhumanMXAvatar, combineCategories(i, c(22, 12), c("M", "N"), dataset))
    femaleMXAvatar <- append(femaleMXAvatar, combineCategories(i, c(22, 13), c("M", "F"), dataset))
    maleMXAvatar <- append(maleMXAvatar, combineCategories(i, c(22, 13), c("M", "M"), dataset))
    nbMXAvatar <- append(nbMXAvatar, combineCategories(i, c(22, 13), c("M", "NB"), dataset))
    whiteMXAvatar <- append(whiteMXAvatar, combineCategories(i, c(22, 14), c("M", "W"), dataset))
    pocMXAvatar <- append(pocMXAvatar, combineCategories(i, c(22, 14), c("M", "POC"), dataset))
    historicMXAvatar <- append(historicMXAvatar, combineCategories(i, c(22, 15), c("M", "Y"), dataset))
    NhistoricMXAvatar <- append(NhistoricMXAvatar, combineCategories(i, c(22, 15), c("M", "N"), dataset))
    youngMXAvatar <- append(youngMXAvatar, combineCategories(i, c(22, 16), c("M", "Y"), dataset))
    NyoungMXAvatar <- append(NyoungMXAvatar, combineCategories(i, c(22, 16), c("M", "N"), dataset))
    preloadMXAvatar <- append(preloadMXAvatar, combineCategories(i, c(22, 18), c("M", "P"), dataset))
    importMXAvatar <- append(importMXAvatar, combineCategories(i, c(22, 18), c("M", "I"), dataset))
  }
  
  numCreators <- length(allCreators)
  numAvatars <- length(hasAvatar)
  numHumans <- length(humanAvatar)
  numPlay <- length(playAvatar)
  numNPC <- length(npcAvatar)
  numGuide <- length(guideAvatar)
  
  # DShasAvatar <- getDS(unique(hasAvatar), numLessons)
  # DSnoAvatar <- getDS(unique(noAvatar), numLessons)
  # DSplayAvatar <- getDS(playAvatar, numAvatars)
  # DSnpcAvatar <- getDS(npcAvatar, numAvatars)
  # DSguideAvatar <- getDS(guideAvatar, numAvatars)
  # DShumanAvatar <- getDS(humanAvatar, numAvatars)
  # DSfemaleAvatar <- getDS(femaleAvatar, numHumans)
  # DSmaleAvatar <- getDS(maleAvatar, numHumans)
  # DSnbAvatar <- getDS(nbAvatar, numHumans)
  # DSwhiteAvatar <- getDS(whiteAvatar, numHumans)
  # DSpocAvatar <- getDS(pocAvatar, numHumans)
  # DShistoricAvatar <- getDS(historicAvatar, numHumans)
  # DSyoungAvatar <- getDS(youngAvatar, numHumans)
  # DSmultipleAvatars <- getDS(unique(multipleAvatars), numLessons)
  # DSpreloadedAvatar <- getDS(preloadedAvatar, numAvatars)
  # DSimportedAvatar <- getDS(importedAvatar, numAvatars)
  # 
  # DSwocAvatar <- getDS(wocAvatar, numAvatars)
  
  # DShumanPlayAvatar <- getDS(humanPlayAvatar, numPlay)
  enterMatrix(paste(name, "C_poc", sep=""), "HUMAN", 2, "B", length(humanPOCAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "HUMAN", 2, "C", length(NhumanPOCAvatar))
  # DSfemalePOCAvatar <- getDS(femalePOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 2, "B", length(femalePOCAvatar))
  # DSmalePOCAvatar <- getDS(malePOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 2, "C", length(malePOCAvatar))
  # DSnbPOCAvatar <- get# DS(nbPOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 2, "D", length(nbPOCAvatar))
  # DSwhitePOCAvatar <- getDS(whitePOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "RACE", 2, "B", length(whitePOCAvatar))
  # DSpocPOCAvatar <- getDS(pocPOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "RACE", 2, "C", length(pocPOCAvatar))
  # DShistoricPOCAvatar <- getDS(historicPOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "HISTORIC", 2, "B", length(historicPOCAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "HISTORIC", 2, "C", length(NhistoricPOCAvatar))
  # DSyoungPOCAvatar <- getDS(youngPOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "YOUTH", 2, "B", length(youngPOCAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "YOUTH", 2, "C", length(NyoungPOCAvatar))
  # DSpreloadPOCAvatar <- getDS(preloadPOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "COSTUME", 2, "B", length(preloadPOCAvatar))
  # DSimportPOCAvatar <- getDS(importPOCAvatar, numPOC)
  enterMatrix(paste(name, "C_poc", sep=""), "COSTUME", 2, "C", length(importPOCAvatar))
  
  # DShumanNPCAvatar <- getDS(humanNPCAvatar, numNPC)
  enterMatrix(paste(name, "C_poc", sep=""), "HUMAN", 3, "B", length(humanWAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "HUMAN", 3, "C", length(NhumanWAvatar))
  # DSfemaleWAvatar <- getDS(femaleWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 3, "B", length(femaleWAvatar))
  # DSmaleWAvatar <- getDS(maleWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 3, "C", length(maleWAvatar))
  # DSnbWAvatar <- getDS(nbWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 3, "D", length(nbWAvatar))
  # DSwhiteWAvatar <- getDS(whiteWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "RACE", 3, "B", length(whiteWAvatar))
  # DSpocWAvatar <- getDS(pocWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "RACE", 3, "C", length(pocWAvatar))
  # DShistoricWAvatar <- getDS(historicWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "HISTORIC", 3, "B", length(historicWAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "HISTORIC", 3, "C", length(NhistoricWAvatar))
  # DSyoungWAvatar <- getDS(youngWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "YOUTH", 3, "B", length(youngWAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "YOUTH", 3, "C", length(NyoungWAvatar))
  # DSpreloadWAvatar <- getDS(preloadWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "COSTUME", 3, "B", length(preloadWAvatar))
  # DSimportWAvatar <- getDS(importWAvatar, numM)
  enterMatrix(paste(name, "C_poc", sep=""), "COSTUME", 3, "C", length(importWAvatar))
  
  # DShumanMXAvatar <- getDS(humanMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "HUMAN", 4, "B", length(humanMXAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "HUMAN", 4, "C", length(NhumanMXAvatar))
  # DSfemaleMXAvatar <- getDS(femaleMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 4, "B", length(femaleMXAvatar))
  # DSmaleMXAvatar <- getDS(maleMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 4, "C", length(maleMXAvatar))
  # DSnbMXAvatar <- getDS(nbMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "GENDER", 4, "D", length(nbMXAvatar))
  # DSwhiteMXAvatar <- getDS(whiteMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "RACE", 4, "B", length(whiteMXAvatar))
  # DSpocMXAvatar <- getDS(pocMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "RACE", 4, "C", length(pocMXAvatar))
  # DShistoricMXAvatar <- getDS(historicMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "HISTORIC", 4, "B", length(historicMXAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "HISTORIC", 4, "C", length(NhistoricMXAvatar))
  # DSyoungMXAvatar <- getDS(youngMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "YOUTH", 4, "B", length(youngMXAvatar))
  enterMatrix(paste(name, "C_poc", sep=""), "YOUTH", 4, "C", length(NyoungMXAvatar))
  # DSpreloadMXAvatar <- getDS(preloadMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "COSTUME", 4, "B", length(preloadMXAvatar))
  # DSimportMXAvatar <- getDS(importMXAvatar, numMX)
  enterMatrix(paste(name, "C_poc", sep=""), "COSTUME", 4, "C", length(importMXAvatar))
  
  # df[name] <- c(DShasAvatar, DSnoAvatar, DSplayAvatar, DSnpcAvatar, DSguideAvatar, DShumanAvatar, DSfemaleAvatar, DSmaleAvatar, DSnbAvatar, DSwhiteAvatar, DSpocAvatar, DShistoricAvatar, DSyoungAvatar, DSmultipleAvatars, DSpreloadedAvatar, DSimportedAvatar, DSwocAvatar, DShumanPlayAvatar, DSfemalePlayAvatar, DSmalePlayAvatar, DSnbPlayAvatar, DSwhitePlayAvatar, DSpocPlayAvatar, DShistoricPlayAvatar, DSyoungPlayAvatar, DSpreloadPlayAvatar, DSimportPlayAvatar, DShumanNPCAvatar, DSfemaleNPCAvatar, DSmaleNPCAvatar, DSnbNPCAvatar, DSwhiteNPCAvatar, DSpocNPCAvatar, DShistoricNPCAvatar, DSyoungNPCAvatar, DSpreloadNPCAvatar, DSimportNPCAvatar, DShumanGuideAvatar, DSfemaleGuideAvatar,DSmaleGuideAvatar, DSnbGuideAvatar, DSwhiteGuideAvatar, DSpocGuideAvatar, DShistoricGuideAvatar, DSyoungGuideAvatar, DSpreloadGuideAvatar, DSimportGuideAvatar)
  
  #return(df)
}

###############main functions#############
df1 <- mainFunction(teacher2019, "teacher2019")
df1 <- mainFunction(teacher2020, "teacher2020")
df1 <- mainFunction(intern2020, "intern2020")
df1 <- mainFunction(intern2021, "intern2021")
df1 <- mainFunction(allTeachers, "allTeachers")
df1 <- mainFunction(allInterns, "allInterns")
df1 <- mainFunction(all, "all")

mainFunctionC_gender(intern2020, "intern2020")
mainFunctionC_gender(intern2021, "intern2021")
mainFunctionC_gender(allInterns, "allInterns")

mainFunctionC_poc(intern2020, "intern2020")
mainFunctionC_poc(intern2021, "intern2021")
mainFunctionC_poc(allInterns, "allInterns")

write_xlsx(df, finalDS)
