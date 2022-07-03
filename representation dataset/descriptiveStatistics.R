library("writexl")

teacher2019 <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - 2019 Teacher Projects.csv")
teacher2020 <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - 2020 Teacher Projects.csv")
intern2020 <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - 2020 Intern Projects.csv")
intern2021 <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - 2021 Intern Projects.csv")
allTeachers <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - All teachers.csv")
allInterns <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - All interns.csv")
all <- read.csv("C:/Users/nalin/Downloads/NC State/Datasets/Representation Tagging - All.csv")

df <- data.frame(Variables = c("hasAvatar", "noAvatar", "playAvatar", "npcAvatar", "guideAvatar", "humanAvatar", "femaleAvatar", "maleAvatar", "nbAvatar", "whiteAvatar", "pocAvatar", "historicAvatar", "youngAvatar", "multipleAvatars", "preloadedAvatar", "importedAvatar", "womenOfColor"))
finalDS <- "C:/Users/nalin/Downloads/NC State/Datasets/FinalDS.xlsx"

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
  
  for (i in 10:18) {
    for (j in 1:nrow(data)) {
      data[j, i] <- toupper(data[j, i])
      data[j, i] <- gsub(" ", "", data[j, i])
    }
  }
  
  ######################cleaning specific data#####################
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
  
  return(data)
}

combineCategories <- function(row, columnList, valueList, storeList, data){
  counter <- 0
  
  if(!data[row, 1] %in% storeList){
    for (i in length(columnList)){
      if (data[row, columnList[i]] == valueList[i]){
        counter <- counter + 1
      }
    }
    
    if (counter == length(columnList)){
      return(data[row, 1])
    }
  }
}

mainFunction <- function(dataset, name){
  dataset <- cleanData(dataset, name)

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
  
  wocAvatar <- list()
  
  humanPlayAvatar <- list()
  femalePlayAvatar <- list()
  malePlayAvatar <- list()
  nbPlayAvatar <- list()
  whitePlayAvatar <- list()
  pocPlayAvatar <- list()
  historicPlayAvatar <- list()
  youngPlayAvatar <- list()
  preloadPlayAvatar <- list()
  importPlayAvatar <- list()
  
  humanNPCAvatar <- list()
  femaleNPCAvatar <- list()
  maleNPCAvatar <- list()
  nbNPCAvatar <- list()
  whiteNPCAvatar <- list()
  pocNPCAvatar <- list()
  historicNPCAvatar <- list()
  youngNPCAvatar <- list()
  preloadNPCAvatar <- list()
  importNPCAvatar <- list()
  
  humanGuideAvatar <- list()
  femaleGuideAvatar <- list()
  maleGuideAvatar <- list()
  nbGuideAvatar <- list()
  whiteGuideAvatar <- list()
  pocGuideAvatar <- list()
  historicGuideAvatar <- list()
  youngGuideAvatar <- list()
  preloadGuideAvatar <- list()
  importGuideAvatar <- list()

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
    wocAvatar <- combineCategories(i, list(13, 14), list("F", "POC"), wocAvatar, dataset)
    
    ########################### combinations #################
    humanPlayAvatar <- combineCategories(i, list(11, 12), list("PC", "Y"), humanPlayAvatar, dataset)
    femalePlayAvatar <- combineCategories(i, list(11, 13), list("PC", "F"), femalePlayAvatar, dataset)
    malePlayAvatar <- combineCategories(i, list(11, 13), list("PC", "M"), malePlayAvatar, dataset)
    nbPlayAvatar <- combineCategories(i, list(11, 13), list("PC", "NB"), nbPlayAvatar, dataset)
    whitePlayAvatar <- combineCategories(i, list(11, 14), list("PC", "W"), whitePlayAvatar, dataset)
    pocPlayAvatar <- combineCategories(i, list(11, 14), list("PC", "POC"), pocPlayAvatar, dataset)
    historicPlayAvatar <- combineCategories(i, list(11, 15), list("PC", "Y"), historicPlayAvatar, dataset)
    youngPlayAvatar <- combineCategories(i, list(11, 16), list("PC", "Y"), youngPlayAvatar, dataset)
    preloadPlayAvatar <- combineCategories(i, list(11, 18), list("PC", "P"), preloadPlayAvatar, dataset)
    importPlayAvatar <- combineCategories(i, list(11, 18), list("PC", "I"), importPlayAvatar, dataset)
    
    humanNPCAvatar <- combineCategories(i, list(11, 12), list("NPC", "Y"), humanNPCAvatar, dataset)
    femaleNPCAvatar <- combineCategories(i, list(11, 13), list("NPC", "F"), femaleNPCAvatar, dataset)
    maleNPCAvatar <- combineCategories(i, list(11, 13), list("NPC", "M"), maleNPCAvatar, dataset)
    nbNPCAvatar <- combineCategories(i, list(11, 13), list("NPC", "NB"), nbNPCAvatar, dataset)
    whiteNPCAvatar <- combineCategories(i, list(11, 14), list("NPC", "W"), whiteNPCAvatar, dataset)
    pocNPCAvatar <- combineCategories(i, list(11, 14), list("NPC", "POC"), pocNPCAvatar, dataset)
    historicNPCAvatar <- combineCategories(i, list(11, 15), list("NPC", "Y"), historicNPCAvatar, dataset)
    youngNPCAvatar <- combineCategories(i, list(11, 16), list("NPC", "Y"), youngNPCAvatar, dataset)
    preloadNPCAvatar <- combineCategories(i, list(11, 18), list("NPC", "P"), preloadNPCAvatar, dataset)
    importNPCAvatar <- combineCategories(i, list(11, 18), list("NPC", "I"), importNPCAvatar, dataset)
    
    humanGuideAvatar <- combineCategories(i, list(11, 12), list("G", "Y"), humanGuideAvatar, dataset)
    femaleGuideAvatar <- combineCategories(i, list(11, 13), list("G", "F"), femaleGuideAvatar, dataset)
    maleGuideAvatar <- combineCategories(i, list(11, 13), list("G", "M"), maleGuideAvatar, dataset)
    nbGuideAvatar <- combineCategories(i, list(11, 13), list("G", "NB"), nbGuideAvatar, dataset)
    whiteGuideAvatar <- combineCategories(i, list(11, 14), list("G", "W"), whiteGuideAvatar, dataset)
    pocGuideAvatar <- combineCategories(i, list(11, 14), list("G", "POC"), pocGuideAvatar, dataset)
    historicGuideAvatar <- combineCategories(i, list(11, 15), list("G", "Y"), historicGuideAvatar, dataset)
    youngGuideAvatar <- combineCategories(i, list(11, 16), list("G", "Y"), youngGuideAvatar, dataset)
    preloadGuideAvatar <- combineCategories(i, list(11, 18), list("G", "P"), preloadGuideAvatar, dataset)
    importGuideAvatar <- combineCategories(i, list(11, 18), list("G", "I"), importGuideAvatar, dataset)
  }
  
  numAvatars <- length(hasAvatar)
  numHumans <- length(humanAvatar)
  
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
  
  
  
  df[name] <- c(DShasAvatar, DSnoAvatar, DSplayAvatar, DSnpcAvatar, DSguideAvatar, DShumanAvatar, DSfemaleAvatar, DSmaleAvatar, DSnbAvatar, DSwhiteAvatar, DSpocAvatar, DShistoricAvatar, DSyoungAvatar, DSmultipleAvatars, DSpreloadedAvatar, DSimportedAvatar, DSwocAvatar)
  
  return(df)
}

###############main functions#############
df <- mainFunction(teacher2019, "teacher2019")
df <- mainFunction(teacher2020, "teacher2020")
df <- mainFunction(intern2020, "intern2020")
df <- mainFunction(intern2021, "intern2021")
df <- mainFunction(allTeachers, "allTeachers")
df <- mainFunction(allInterns, "allInterns")
df <- mainFunction(all, "all")

write_xlsx(df, finalDS)
