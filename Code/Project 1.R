library(dplyr)
library(data.table)

############################################# Zadanie 1 #############################################

#################### 1.1 ####################

df_sql_1 <- function(Tags){
  sqldf::sqldf('SELECT TagName, Count 
        FROM Tags
        ORDER BY Count DESC 
        LIMIT 10')
}

df_sql_1(Tags)

#################### 1.2 ####################

df_base_1 <- function(df1){
  
  newTags <- df1[c("TagName", "Count")]                      # tworzymy ramkę danych newTags z kolumnami TagName i Count
  newTags <- newTags[order(df1["Count"], decreasing = T), ]  # sortujemy nową ramkę danych względem kolumny Count malejąco
  row.names(newTags) <- NULL                                 # ustawiamy kolejność kolejność wierszy 1:n
  head(newTags, 10)                                          # zwracamy pierwsze 10 rzędów
  
}

df_base_1(Tags)

#################### 1.3 ####################

df_dplyr_1 <- function(df1){
  
  # tworzymy ramkę danych newtags z kolumnami TagName i Count
  newTags <- df1 %>% select(TagName, Count) %>% arrange(desc(Tags, Count))  # i sortujemy malejąco względem kolumny Count
  row.names(newTags) <- NULL                         # ustawiamy kolejność kolejność wierszy 1:n              
  head(newTags, 10)                                  # zwracamy pierwsze 10 rzędów
}

df_dplyr_1(Tags)

#################### 1.4 ####################

df_table_1 <- function(df1){
  newTags <- setorder(df1, -Count) # tworzymy ramkę danych newTags jako ramka danuch df posortowana odnośnie Count malejąco
  row.names(newTags) <- NULL       # ustawiamy kolejność kolejność wierszy 1:n
  head(newTags[, c("TagName", "Count")], 10)# zwracamy pierwsze 10 rzędów kolumn TagName i Count
}

df_table_1(Tags)


############################################# Zadanie 2 #############################################

#################### 2.1 ####################

df_sql_2 <- function(df1, df2){
  sqldf::sqldf("SELECT Users.DisplayName, Users.Age, Users.Location,
                       AVG(Posts.Score) as PostsMeanScore,
                       MAX(Posts.CreationDate) AS LastPostCreationDate
               FROM Posts
               JOIN Users ON Users.AccountId=Posts.OwnerUserId
               WHERE OwnerUserId != -1
               GROUP BY OwnerUserId    
               ORDER BY PostsMeanScore DESC
               LIMIT 10


")
}
df_sql_2(Posts, Users)


#################### 2.2 ####################

# niestety nie udało mi się pogrupować w jednej ramce i za pomocą jednego wywoływania funkcji aggregate() :(

df_base_2 <- function(df1, df2){
  
  # tworzymy ramkę newTable1, która jest pogrupowaniem mean(Score) by OwnerUserId
  newTable1 <- aggregate(x = df1[, c("Score")],                        
                         by = df1["OwnerUserId"],                     
                         FUN = function(z) mean(z))
  
  # tworzymy ramkę newTable2, która jest pogrupowaniem max(CreationDate) by OwnerUserId
  newTable2 <- aggregate(x = df1[, "CreationDate"],                      
                         by = df1["OwnerUserId"],                     
                         FUN = function(z) max(z))
  # do ramki newTable1 dopisujemy kolumne x (to jest max(CreationDate)) z newTable2 
  newTable1$LastPostCreationDate <- newTable2$x
  # zmieniamy nazwę kolumny x na PostsMeanScore
  names(newTable1)[2] <- "PostsMeanScore"        
  
  # tworzymy ramkę newUsers z potrzebnymi kolumnami z Users
  newUsers <- df2[, c("DisplayName", "Age", "Location", "AccountId")]  
  # łączymy newTable2 i newUsers
  Table <- merge(x = newTable1, y = newUsers, by.x = "OwnerUserId", by.y = "AccountId") 
  
  # Wybieramy wszystkie rzędy, w których wartość kolumny OwnerUserId nie równa się -1
  Table <- Table[Table$OwnerUserId != -1, ] 
  # sortujemy malejąco odnośnie kolumny PostsMeanScore
  Table <- Table[order(Table["PostsMeanScore"], decreasing = T), ] 
  
  Table$OwnerUserId <- NULL                 # usuwamy niepotrzebną kolumne OwnerUserId
  row.names(Table) <- NULL                  # ustawiamy kolejność kolejność wierszy 1:n 
  head(Table, 10)                           # zwracamy pierwsze 10 rzędów
}

df_base_2(Posts, Users)

#################### 2.3 ####################

df_dplyr_2 <- function(df1, df2){
  
  # tworzymy ramkę newTable1, która jest pogrupowaniem mean(Score) by OwnerUserId
  newTable1 <- df1 %>% group_by(OwnerUserId) %>%
    summarise(PostsMeanScore = mean(Score))
  
  # tworzymy ramkę newTable2, która jest pogrupowaniem max(CreationDate) by OwnerUserId
  newTable2 <- df1 %>% group_by(OwnerUserId) %>%
    summarise(LastPostCreationDate = max(CreationDate))
  
  # do ramki newTable1 dopisujemy kolumne LastPostCreationDate z newTable2
  newTable <- mutate(newTable1, newTable2 %>% select(LastPostCreationDate)) 
  
  # tworzymy ramkę newUsers z potrzebnymi kolumnami z Users
  newUsers <- df2 %>% select(DisplayName, Age, Location, AccountId) 
  
  Table <- inner_join(newTable,
                      newUsers, 
                      by = c("OwnerUserId" = "AccountId"))    # łączymy newTable2 i newUsers
  
  # Wybieramy wszystkie rzędy, w których wartość kolumny OwnerUserId nie równa się -1
  Table <- Table[Table$OwnerUserId != -1, ]
  
  # formatowanie kosmetyczne :) (nie wiem dlaczego, ale stworzyły się 3 puste wiersze na końcu tabeli po join)
  Table <- Table[1:795, ]
  
  
  Table <- select(Table, -OwnerUserId)                     # usuwamy niepotrzebną kolumne OwnerUserId
  Table <- Table %>% arrange(desc(Table, PostsMeanScore))  # sortujemy malejąco odnośnie kolumny PostsMeanScore
  head(Table, 10)                                          # zwracamy pierwsze 10 rzędów
}

df_dplyr_2(Posts, Users)

#################### 2.4 ####################

df_table_2 <- function(df1, df2){
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts
  newPosts <- data.table(OwnerUserId = df1$OwnerUserId,
                         Score = df1$Score,
                         CreationDate = df1$CreationDate)
  
  # grupujemy odnośnie kolumny OwnerUserId 
  newTable <- newPosts[, .(PostsMeanScore = mean(Score), 
                           LastPostCreationDate = max(CreationDate)), 
                       by = OwnerUserId]
  
  # tworzymy ramkę newUsers z potrzebnymi kolumnami z Users i klucz key = "AccountId", który przyspiesza merge w data.table
  newUsers <- data.table(DisplayName = df2$DisplayName,
                         Age = df2$Age,
                         Location = df2$Location,
                         AccountId = df2$AccountId,
                         key = "AccountId")
  # łączymy newTable2 i newUsers
  Table <- merge(x = newTable, y = newUsers, by.x = "OwnerUserId", by.y = "AccountId")
  # Wybieramy wszystkie rzędy, w których wartość kolumny OwnerUserId nie równa się -1
  Table <- Table[Table$OwnerUserId != -1, ]
 
  Table <- setorder(Table, -PostsMeanScore)      # sortujemy malejąco odnośnie kolumny PostsMeanScore
  Table$OwnerUserId <- NULL                      # usuwamy niepotrzebną kolumne OwnerUserId
  row.names(Table) <- NULL                       # ustawiamy kolejność kolejność wierszy 1:n   
  head(Table, 10)                                # zwracamy pierwsze 10 rzędów
}

df_table_2(Posts, Users)


############################################# Zadanie 3 #############################################

#################### 3.1 ####################

df_sql_3 <- function(Users, Posts){
  sqldf::sqldf("SELECT DisplayName, QuestionsNumber, AnswersNumber
       FROM
           (
              SELECT COUNT(*) as AnswersNumber, Users.DisplayName, Users.Id
              FROM Users 
              JOIN Posts ON Users.Id = Posts.OwnerUserId
              WHERE Posts.PostTypeId = 1
              GROUP BY Users.Id
           )  AS Tab1
       JOIN
           (
              SELECT COUNT(*) as QuestionsNumber, Users.Id
              FROM Users 
              JOIN Posts ON Users.Id = Posts.OwnerUserId
              WHERE Posts.PostTypeId = 2
              GROUP BY Users.Id
           ) AS Tab2
       ON Tab1.Id = Tab2.Id
       WHERE QuestionsNumber < AnswersNumber
       ORDER BY AnswersNumber DESC")
}

df_sql_3(Users, Posts)



#################### 3.2 ####################

df_base_3 <- function(df1, df2){
  
  # na wszelki wypadek, bo czasam występuje błąd przy wczytywaniu funkcji 
  class(df1) <- "data.frame"
  
  # Tab1
  # tworzymy ramkę newTable z kolumną Id z Posts i tworzymy kolumnę AnswersNumber, 
  # która implementuje COUNT(*)
  newTable <- aggregate(x = df1[, "Id"],
                        by = df1["Id"], 
                        FUN = length)
  
  # zmieniamy nazwę drugiej kolumny na AnswersNumber
  names(newTable)[2] <- "AnswersNumber" 
  # dodawamy tabele DisplayName z ramki Users
  newTable$DisplayName <- df1$DisplayName
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts 
  newPosts <- df2[, c("OwnerUserId", "PostTypeId")]
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 1
  newPosts <- newPosts[newPosts$PostTypeId == 1, ]
  
  # łączymy newTable i newPosts
  newJoin <- merge(x = newTable, y = newPosts, by.x = "Id", by.y = "OwnerUserId")
  
  # tworzymy Tab1, grupujemy ramkę odnośnie kolumn Id, DisplayName i sumujemy "odpowiedzi"
  Tab1 <- aggregate(x = newJoin[, "AnswersNumber"],
                    by = newJoin[c("DisplayName", "Id")],
                    FUN = sum)
  # zmieniamy nazwę trzeciej kolumny na AnswersNumber
  names(Tab1)[3] <- "AnswersNumber"
  
  
  # Tab2 
  # nie ma potrzeby tworzyć newTable, bo już mamy ją 
  # zmieniamy nazwę drugiej kolumny na QuestionsNumber
  names(newTable)[2] <- "QuestionsNumber" 
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts 
  newPosts <- df2[, c("OwnerUserId", "PostTypeId")]
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 2
  newPosts <- newPosts[newPosts$PostTypeId == 2, ]
  
  # łączymy newTable i newPosts
  newJoin <- merge(newTable, newPosts, by.x = "Id", by.y = "OwnerUserId")
  
  # tworzymy Tab2, grupujemy ramkę odnośnie kolumny Id i sumujemy "numery pytań"
  Tab2 <- aggregate(x = newJoin[, "QuestionsNumber"],
                    by = newJoin["Id"],
                    FUN = sum)
  
  # zmieniamy nazwę drugiej kolumny na AnswersNumber
  names(Tab2)[2] <- "QuestionsNumber"
  
  
  # łączymy Tab1 i Tab2
  Table <- merge(x = Tab1, y = Tab2, by = "Id")
  # usuwamy niepotrzebną kolumnę Id
  Table$Id <- NULL
  # wybieramy tylko te wiersze, w których wartość QuestionsNumber jest mniejsza od AnswersNumber
  Table <- Table[Table$QuestionsNumber < Table$AnswersNumber, ]
  # sortujemy malejąco odnośnie kolumny AnswersNumber
  Table <- Table[order(Table$AnswersNumber, decreasing = T), ]
  # ustawiamy kolejność kolejność wierszy 1:n  
  row.names(Table) <- NULL
  # zwracamy pierwsze 10 wierszy
  Table
  
}

df_base_3(Users, Posts)

#################### 3.3 ####################

df_dplyr_3 <- function(df1, df2){
  
  # Tab1
  # tworzymy ramkę newTable z kolumną Id z Posts i tworzymy kolumnę AnswersNumber, 
  # która implementuje COUNT(*)
  # funkcja n() in dplyr jest równoważna funkcji length() 
  newTable <- df1 %>% group_by(Id) %>% summarise(AnswersNumber = n())
  # dodawamy tabele DisplayName z ramki Users
  newTable$DisplayName <- df1$DisplayName
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts 
  newPosts <- df2 %>% select(OwnerUserId, PostTypeId)
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 1
  newPosts <- newPosts[newPosts$PostTypeId == 1, ]
  
  # łączymy newTable i newPosts
  newJoin <- inner_join(newTable, 
                        newPosts, 
                        by = c("Id" = "OwnerUserId"))
  
  # tworzymy Tab1, grupujemy ramkę odnośnie kolumn Id, DisplayName i sumujemy "odpowiedzi"
  # dodatkowo: za pomocą .groups = 'drop' usuniemy group attributes 
  # i nie będziemy dostawać za każdym wywołaniem funkcji friendly warning message:
  # `summarise()` has grouped output by 'Id'. You can override using the `.groups` argument.
  Tab1 <- newJoin %>% group_by(Id, DisplayName) %>% summarise(AnswersNumber = sum(AnswersNumber), .groups = 'drop')
  
  
  # Tab2 
  # tworzymy ramkę newTable z kolumną Id z Posts i tworzymy kolumnę QuestionsNumber,
  # która implementuje COUNT(*)
  newTable <- df1 %>% group_by(Id) %>% summarise(QuestionsNumber = n())
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts 
  newPosts <- df2 %>% select(OwnerUserId, PostTypeId)
  # i wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 2
  newPosts <- newPosts[newPosts$PostTypeId == 2, ]
  
  # łączymy newTable i newPosts
  newJoin <- inner_join(newTable, 
                        newPosts, 
                        by = c("Id" = "OwnerUserId"))
  
  # tworzymy Tab2, grupujemy ramkę odnośnie kolumny Id i sumujemy "numery pytań"
  Tab2 <- newJoin %>% group_by(Id) %>% summarise(QuestionsNumber = sum(QuestionsNumber))
  
  
  
  # łączymy Tab1 i Tab2
  Table <- inner_join(Tab1, 
                      Tab2,
                      by = "Id")
  
  # usuwamy niepotrzebną kolumnę Id
  Table$Id <- NULL
  
  # wybieramy tylko te wiersze, w których wartość QuestionsNumber jest mniejsza od AnswersNumber
  Table <- Table[Table$QuestionsNumber < Table$AnswersNumber, ]
  
  # sortujemy malejąco odnośnie kolumny AnswersNumber
  Table <- Table[order(Table$AnswersNumber, decreasing = T), ] 
  # zwracamy ramkę danych
  Table       
}

df_dplyr_3(Users, Posts)


#################### 3.4 ####################

df_table_3 <- function(df1, df2){
  
  # Tab1
  # tworzymy ramkę newTable z kolumną Id z Posts i tworzymy kolumnę AnswersNumber, 
  # która implementuje COUNT(*)
  # funkcja .N in data.table jest równoważna funkcji length() 
  newTable <- setDT(df1)[, .(AnswersNumber = .N), Id]
  # dodawamy tabele DisplayName z ramki Users
  newTable$DisplayName <- df1$DisplayName
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts  
  newPosts <- data.table(OwnerUserId = df2$OwnerUserId, 
                         PostTypeId = df2$PostTypeId)
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 1
  newPosts <- newPosts[newPosts$PostTypeId == 1, ]
  
  # łączymy newTable i newPosts
  newJoin <- merge(x = newTable, y = newPosts, by.x = "Id", by.y = "OwnerUserId")
  
  # tworzymy Tab1, grupujemy ramkę odnośnie kolumn Id, DisplayName i sumujemy "odpowiedzi"
  Tab1 <- newJoin[, .(AnswersNumber = sum(AnswersNumber)),
                  by = .(Id, DisplayName)]
  
  
  # Tab2 
  # tworzymy ramkę newTable z kolumną Id z Posts i tworzymy kolumnę QuestionsNumber,
  # która implementuje COUNT(*) 
  newTable <- setDT(df1)[, .(QuestionsNumber = .N), Id]
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts 
  newPosts <- data.table(OwnerUserId = df2$OwnerUserId, 
                         PostTypeId = df2$PostTypeId) 
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 2
  newPosts <- newPosts[newPosts$PostTypeId == 2, ]
  
  # łączymy newTable i newPosts
  newJoin <- merge(x = newTable, y = newPosts, by.x = "Id", by.y = "OwnerUserId")
  
  # tworzymy Tab2, grupujemy ramkę odnośnie kolumny Id i sumujemy "numery pytań"
  Tab2 <- newJoin[, .(QuestionsNumber = sum(QuestionsNumber)),
                  by = Id]
  
  
  # lączymy tab1 i Tab2
  Table <- merge(x = Tab1, y = Tab2, by = "Id")
  # usuwamy niepotrzebną kolumnę Id
  Table$Id <- NULL
  # wybieramy tylko te wiersze, w których wartość QuestionsNumber jest mniejsza od AnswersNumber
  Table <- Table[Table$QuestionsNumber < Table$AnswersNumber, ]
  # sortujemy malejąco odnośnie kolumny AnswersNumber
  Table <- setorder(Table, -AnswersNumber)
  # zwracamy ramkę danych
  Table
  
}

df_table_3(Users, Posts)

############################################# Zadanie 4 #############################################

#################### 4.1 ####################

df_sql_4 <- function(df1, df2){
  sqldf::sqldf("SELECT
            Posts.Title, Posts.CommentCount,
            CmtTotScr.CommentsTotalScore,
            Posts.ViewCount
       FROM (
               SELECT
                    PostID,
                    UserID,
                    SUM(Score) AS CommentsTotalScore
               FROM Comments
               GROUP BY PostID, UserID
            ) AS CmtTotScr
       JOIN Posts ON Posts.ID=CmtTotScr.PostID
       WHERE Posts.PostTypeId=1
       ORDER BY CmtTotScr.CommentsTotalScore DESC
       LIMIT 10
       ")
}

df_sql_4(Comments, Posts)


#################### 4.2 ####################


df_base_4 <- function(df1, df2){
  
  # tworzymy pomocniczą ramkę newComments z potrzebnymi kolumnami z Comments  
  newComments <- df1[, c("Score", "UserId", "PostId")]
  
  # tworzymy ramkę CmtTotScr
  # tranformujemy, bo w kolumnie UserId występują wartości NA i musimy 
  # "zrobić" z tych NA str, aby aggregate() mogła grupować po UserId
  CmtTotScr <- transform(aggregate(Score ~ PostId + UserId, transform(newComments, UserId = paste(UserId)), sum),
                         UserId = type.convert(UserId))
  
  # nadajemy nazwę CommentsTotalScore trzeciej kolumnie
  names(CmtTotScr)[3] <- "CommentsTotalScore"
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts  
  newPosts <- df2[, c("Title", "CommentCount", "ViewCount", "Id", "PostTypeId")]
  
  # łączymy CmtTotScr i newPosts
  Table <- merge(x = CmtTotScr, y = newPosts, by.x = "PostId", by.y = "Id")
  
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 1
  Table <- Table[Table$PostTypeId == 1, ]
  # sortujemy malejąco odnośnie kolumny CommentsTotalScore 
  Table <- Table[order(Table["CommentsTotalScore"], decreasing = T), ]
  
  Table$PostId <- NULL      # usuwamy niepotrzebną kolumnę PostId 
  Table$UserId <- NULL      # usuwamy niepotrzebną kolumnę UserId
  Table$PostTypeId <- NULL  # usuwamy niepotrzebną kolumnę PostTypeId
  row.names(Table) <- NULL  # ustawiamy kolejność kolejność wierszy 1:n 
  head(Table, 10)           # zwracamy pierwsze 10 wierszy
}

df_base_4(Comments, Posts)

#################### 4.3 ####################

df_dplyr_4 <- function(df1, df2){
  
  # tworzymy ramkę CmtTotScr
  
  # tutaj nie ma potrzeby tranformować, bo funkcja group_by() grupuje z NA
  # dodatkowo: za pomocą .groups = 'drop' usuniemy group attributes 
  # i nie będziemy dostawać za każdym wywołaniem funkcji friendly warning message:
  # `summarise()` has grouped output by 'Id'. You can override using the `.groups` argument.
  CmtTotScr <- df1 %>% group_by(PostId, UserId) %>% summarise(CommentsTotalScore = sum(Score), .groups = 'drop')
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts 
  newPosts <- df2 %>% select(Title, CommentCount, ViewCount, Id, PostTypeId)
  
  # łączymy CmtTotScr i newPosts
  Table <- inner_join(CmtTotScr, 
                      newPosts,
                      by = c("PostId" = "Id"))
  
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 1
  Table <- Table[Table$PostTypeId == 1, ]
  
  Table$PostId <- NULL      # usuwamy niepotrzebną kolumnę PostId 
  Table$UserId <- NULL      # usuwamy niepotrzebną kolumnę UserId 
  Table$PostTypeId <- NULL  # usuwamy niepotrzebną kolumnę PostTypeId 
  
  # sortujemy malejąco odnośnie kolumny CommentsTotalScore 
  Table <- Table %>% arrange(desc(Table, CommentsTotalScore))
  
  # zwracamy pierwsze 10 wierszy
  head(Table, 10)
}

df_dplyr_4(Comments, Posts)


#################### 4.4 ####################

df_table_4 <- function(df1, df2){
  
  # musimy stworzyć pomocnicza tabelkę newComments typu data.table, bo 
  # taki sposób grupowania pracuje tylko z objektami data.table
  # oraz tworzymy klucz aby przyspeszyć wykonanie funkcji
  newComments <- data.table(Score = df1$Score,  
                            PostId = df1$PostId,
                            UserId = df1$UserId, 
                            key = "PostId")
  
  # tworzymy ramkę danych CmtTotScr, grupujemy odnośnie kolumn UserId, PostId
  CmtTotScr <- newComments[, .(CommentsTotalScore = sum(Score)),
                           by = .(UserId, PostId)]
  
  # tworzymy ramkę newPosts z potrzebnymi kolumnami z Posts 
  # oraz tworzymy klucz aby przyspeszyć wykonanie funkcji 
  newPosts <- data.table(Title = df2$Title,
                         CommentCount = df2$CommentCount,
                         ViewCount = df2$ViewCount,
                         Id = df2$Id,
                         PostTypeId = df2$PostTypeId,
                         key = "Id")
  
  # łączymy CmtTotScr i newPosts
  Table <- merge(x = CmtTotScr, y = newPosts, by.x = "PostId", by.y = "Id")
  
  # wybieramy tylko te wiersze, w których wartość PostTypeId jest równa się 1
  Table <- Table[Table$PostTypeId == 1, ]
  # sortujemy malejąco odnośnie kolumny CommentsTotalScore 
  Table <- setorder(Table, -CommentsTotalScore)
  
  Table$PostId <- NULL      # usuwamy niepotrzebną kolumnę PostId 
  Table$UserId <- NULL      # usuwamy niepotrzebną kolumnę UserId 
  Table$PostTypeId <- NULL  # usuwamy niepotrzebną kolumnę PostTypeId 
  
  head(Table, 10)           # zwracamy pierwsze 10 wierszy  
}

df_table_4(Comments, Posts)


