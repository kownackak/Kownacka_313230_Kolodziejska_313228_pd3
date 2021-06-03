library(dplyr)
library(XML)
library(plyr)
BadgesCH <- xmlParse("chrzescijanizm/Badges.xml",useInternalNodes = TRUE)
BadgesCH <- xmlToList(BadgesCH)
BadgesCH <- ldply(BadgesCH, rbind)
head(BadgesCH)
View(BadgesCH)
# Chrzeœcijanizm
PostHistoryCH <- xmlParse("chrzescijanizm/PostHistory.xml",useInternalNodes = TRUE)
PostHistoryCH <- xmlToList(PostHistoryCH)
PostHistoryCH <- ldply(PostHistoryCH, rbind)
View(PostHistoryCH)

# wybieramy wiersze, w których PostHistoryTypeId = 12 i PostHistoryTypeId = 14
wynik <- PostHistoryCH %>%
    filter(PostHistoryTypeId == c(12, 14)) %>%
    select(UserId, PostId, PostHistoryTypeId) %>%
    group_by(UserId) %>%
    count(UserId) #%>%
    #rename(PostsNumber = n)

# wykres <- wynik %>%
#    count(PostHistoryTypeId == 12) %>%
#    count(PostHistoryTypeId == 14)
View(wynik)
View(wykres)

# Judaizm
PostHistoryJ <- xmlParse("judaizm/PostHistory.xml",useInternalNodes = TRUE)
PostHistoryJ <- xmlToList(PostHistoryJ)
PostHistoryJ <- ldply(PostHistoryJ, rbind)
View(PostHistoryJ)

# Islam
PostHistoryI <- xmlParse("islam/PostHistory.xml",useInternalNodes = TRUE)
PostHistoryI <- xmlToList(PostHistoryI)
PostHistoryI <- ldply(PostHistoryI, rbind)
View(PostHistoryI)
