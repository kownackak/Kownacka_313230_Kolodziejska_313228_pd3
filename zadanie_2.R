library(dplyr)
library(XML)
library(plyr)

rep <- function(PostHistory, Users){
    wynik_1 <- PostHistory %>%
        filter(PostHistoryTypeId == c(12, 14)) %>%
        filter(UserId != -1) %>% # bo UserId = -1 to Id osób, które nie by³y zalogowane
        select(UserId, PostId, PostHistoryTypeId)
    wynik <- aggregate(x = wynik_1$UserId, by = wynik_1["UserId"], FUN = length)
    colnames(wynik)[2] <- "BlockedPostsNumber"
    wynik <- wynik[order(wynik$BlockedPostsNumber, decreasing = TRUE), ]
    rep <- merge(wynik, Users[c("Id", "Reputation")], by.x = "UserId", by.y = "Id")
    rep <- rep[order(rep$BlockedPostsNumber, decreasing = TRUE), ]
    rownames(rep) <- NULL
    rep
}

# Chrzeœcijanizm

PostHistoryCH <- xmlParse("chrzescijanizm/PostHistory.xml",useInternalNodes = TRUE)
PostHistoryCH <- xmlToList(PostHistoryCH)
PostHistoryCH <- ldply(PostHistoryCH, rbind)
UsersCH <- xmlParse("chrzescijanizm/Users.xml",useInternalNodes = TRUE)
UsersCH <- xmlToList(UsersCH)
UsersCH <- ldply(UsersCH, rbind)

reputation_CH <- rep(PostHistoryCH, UsersCH)

# Judaizm

PostHistoryJ <- xmlParse("judaizm/PostHistory.xml",useInternalNodes = TRUE)
PostHistoryJ <- xmlToList(PostHistoryJ)
PostHistoryJ <- ldply(PostHistoryJ, rbind)
UsersJ <- xmlParse("judaizm/Users.xml",useInternalNodes = TRUE)
UsersJ <- xmlToList(UsersJ)
UsersJ <- ldply(UsersJ, rbind)

reputation_J <- rep(PostHistoryJ, UsersJ)

# Islam

PostHistoryI <- xmlParse("islam/PostHistory.xml",useInternalNodes = TRUE)
PostHistoryI <- xmlToList(PostHistoryI)
PostHistoryI <- ldply(PostHistoryI, rbind)
UsersI <- xmlParse("islam/Users.xml",useInternalNodes = TRUE)
UsersI <- xmlToList(UsersI)
UsersI <- ldply(UsersI, rbind)

reputation_I <- rep(PostHistoryI, UsersI)

View(reputation_CH)
View(reputation_I)
View(reputation_J)


# tworzenie tabelki porównuj¹cej iloœæ u¿ytkowników z zablokowanymi postami w ka¿dym z forum

tab <- data.frame("Religion" = c("CH", "J", "I"), 
                  "UserNumber" = c(nrow(reputation_CH), nrow(reputation_J), nrow(reputation_I)), 
                  "BlockedPostNumber" = c(sum(sapply(reputation_CH$BlockedPostsNumber, sum)),
                                          sum(sapply(reputation_J$BlockedPostsNumber, sum)),
                                          sum(sapply(reputation_I$BlockedPostsNumber, sum))))


View(tab)

# tworzenie wykresu

wykres <- function(counts, title){
    
    t <- seq(0, 2*pi, length.out = 100) 
    x <- cos(t)
    y <- sin(t)
    
    frac <- prop.table(counts)
    xy <- c(1, round(cumsum(frac * 100)))
    
    plot.new()
    plot.window(c(-1, 1), c(-1, 1), asp = 1)
    kolory <- topo.colors(length(counts))
    for(i in 2:length(xy)){
        polygon(c(0, x[ xy[i-1]:xy[i] ]), 
                c(0, y[ xy[i-1]:xy[i] ]), col = kolory[i-1])
        tx <- mean(x[ xy[i-1]:xy[i] ])
        ty <- mean(y[ xy[i-1]:xy[i] ])
        shift <- (par("usr")[2]-par("usr")[1])
    }
    title(title)
    legend("bottom", col = kolory, pch = 16, 
           legend = sprintf("%s: %.2f%%", names(counts), 100*frac))
    polygon(x <- 0.5 * cos(t),
            y <- 0.5 * sin(t), col = 'white')
    invisible(NULL)
}

wykres_post <- wykres(c("Chrzeœcijañstwo" = 208, "Judaizm" = 408, "Islam" = 104), "Liczba zablokowanych i usuniêtych postów")
wykres_user <- wykres(c("Chrzeœcijañstwo" = 65, "Judaizm" = 93, "Islam" = 58), "Liczba u¿ytkowników z zablokowanymi i usuniêtymi postami")