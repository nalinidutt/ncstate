library(readxl)

#teacher2019 <- read_excel("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/teacher2019.xlsx")
#teacher2020 <- read_excel("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/teacher2020.xlsx")
#intern2020 <- read_excel("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/intern2020.xlsx")
#intern2021 <- read_excel("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/intern2021.xlsx")
#allTeachers <- read_excel("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/allTeachers.xlsx")
#allInterns <- read_excel("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/allInterns.xlsx")
#all <- read_excel("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/all.xlsx")

datasets <- c("teacher2019", "teacher2020", "intern2020", "intern2021", "allTeachers", "allInterns", "all", "intern2020C_gender", "intern2020C_poc", "intern2021C_gender", "intern2021C_poc", "allInternsC_gender", "allInternsC_poc")
sheetNames <- c("HUMAN", "GENDER", "RACE", "HISTORIC", "YOUTH", "COSTUME")

chiSquareTest <- function(dataName){
  for(sheet in sheetNames){
    temp_data <- read_excel(paste("C:/Users/nalin/Downloads/NC State/Datasets/Correlative/", dataName, ".xlsx", sep=""), sheet=sheet)
    temp_df <- data.frame(temp_data)
    
    # deleting index column
    rownames(temp_df) <- temp_df[,1]
    temp_df[,1] <- NULL
    
    print(paste("Data: ", dataName, sep=""))
    print(paste("Sheet: ", sheet, sep=""))
    print(chisq.test(temp_df, correct=FALSE))
    
  }
}

for (data in datasets){
  chiSquareTest(data) 
}
