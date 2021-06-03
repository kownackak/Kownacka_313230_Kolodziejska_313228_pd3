library(XML)
library(methods)
library(plyr)
library(ggplot2)
library(gganimate)
library(dplyr)

xmltotable<-function(file)
{
  file<-xmlToList(file)
  file<-ldply(file,rbind)
}
Postsj<-xmlParse('judaizm\\Posts.xml')
Postsj<-xmltotable(Postsj)


Commentsj<-xmlParse('judaizm\\Comments.xml')
Commentsj<-xmltotable(Commentsj)


Postsc<-xmlParse('chrzescijanizm\\Posts.xml')
Postsc<-xmltotable(Postsc) %>% filter(OwnerUserId!=-1)

Commentsc<-xmlParse('chrzescijanizm\\Comments.xml')
Commentsc<-xmltotable(Commentsc)

Postsi<-xmlParse('islam\\Posts.xml')
Postsi<-xmltotable(Postsi)

Commentsi<-xmlParse('islam\\Comments.xml')
Commentsi<-xmltotable(Commentsi)

#zadanie 1

 fun1<-function(P,C)
{
  Comments<-as.data.frame(C)
  Posts<-as.data.frame(P)
  xy<-merge(x=Comments,y=Posts,by.x = "PostId",by.y = "Id")
 z<- xy %>% filter(PostTypeId=="1") %>%
    group_by (PostId) %>%
    count(PostId)%>%
    rename(CommentsNumber=n)
  xy<-merge(x=xy,y=z,by= "PostId")
  xy<-xy %>% distinct(PostId, Title, CommentsNumber)
  xy<-arrange(xy, desc(CommentsNumber))

}


jud1<-fun1(Postsj,Commentsj)
chr1<-fun1(Postsc,Commentsc)
isl1<-fun1(Postsi,Commentsi)


fun11<-function(A,B,C)
{
  A<-A[1,3]
  B<-B[1,3]
  C<-C[1,3]
  Tab<-c(A,B,C)
  Tab<-ldply(Tab,rbind)
  Tab<- Tab %>% mutate(NazwaForum=0) %>% rename(CommentsNumber=1)
  Tab[1,2]<-"Judaizm"
  Tab[2,2]<-"Chrzeœcijañswo"
  Tab[3,2]<-"Islam"
  Tab


}
Tab<-fun11(jud1,chr1,isl1)
View(Tab)

ggplot(Tab,aes(x=NazwaForum,y=CommentsNumber,fill=NazwaForum))+geom_bar(stat='identity',color="red")

fun12<-function(Tab)
{
  Tab<-Tab %>% distinct(UserId)%>%mutate(UserId=n())
  Tab<-head(Tab,1)
}

fun13<-function(A,B,C)
{
  A<-fun12(A)
  B<-fun12(B)
  C<-fun12(C)
  Tab<-c(A,B,C)
  Tab<-ldply(Tab,rbind)
  Tab<- Tab %>% rename(NazwaForum=.id)
  Tab[1,1]<-"Judaizm"
  Tab[2,1]<-"Chrzeœcijañswo"
  Tab[3,1]<-"Islam"
  Tab

}

xyz<-fun13(Commentsj,Commentsc,Commentsi)
View(xyz)
#zadanie 2

Badgesj<-xmlParse('judaizm\\Badges.xml')
Badgesj<-xmltotable(Badgesj)

Badgesi<-xmlParse('islam\\Badges.xml')
Badgesi<-xmltotable(Badgesi)

Badgesc<-xmlParse('chrzescijanizm\\Badges.xml')
Badgesc<-xmltotable(Badgesc)

fun2<-function(B)
{
  Badges<-as.data.frame(B)
    
    
  Tab1 <- (Badges%>% filter(Class=="1"))
  Tab2 <- (Badges%>% filter(Class=="2"))
  Tab3 <- (Badges%>% filter(Class=="3"))
  
  Tab1<- Tab1 %>%
    group_by (UserId) %>%
    count(UserId) %>%
    rename(GoldBadgesNumber=n)
  
  Tab2<- Tab2 %>%
    group_by (UserId) %>%
    count(UserId) %>%
    rename(SilverBadgesNumber=n)
  
  Tab3<- Tab3 %>%
    group_by (UserId) %>%
    count(UserId) %>%
    rename(BronzeBadgesNumber=n)
  
  
  Tab<-full_join(Badges, Tab1, by="UserId")
  Tab<-full_join(Tab,Tab2, by="UserId")
  Tab<-full_join(Tab,Tab3, by="UserId")
  Tab[is.na(Tab)] <- 0
  Tab<-Tab %>% distinct(UserId,GoldBadgesNumber,SilverBadgesNumber, BronzeBadgesNumber)
  Tab<-arrange(Tab, desc(GoldBadgesNumber))
 
}

 jud2<-fun2(Badgesj)
 chr2<-fun2(Badgesc)
 isl2<-fun2(Badgesi)
 View(jud2)

fun22<-function(B,P)
{
  Tab1 <- (B%>% filter(Class=="1"))
  Tab1<- Tab1 %>%
    group_by (UserId) %>%
    count(UserId) %>%
    rename(BadgesNumber=n)


    Tab2<-P %>%  select(Id,OwnerUserId) %>% group_by(OwnerUserId)%>% summarize(Id=n(),.groups = "drop") 
    
    Tab2<-full_join(Tab1,Tab2, by=c("UserId"="OwnerUserId")) 
    Tab2[is.na(Tab2)] <- 0
    Tab2<-Tab2[c("BadgesNumber","Id")]
    Tab2<- Tab2 %>% group_by(BadgesNumber) %>% summarize(PostNumber=as.integer(mean(Id)),.groups = "drop")
    
    Tab3 <- (B%>% filter(Class=="2"))
    
    
    Tab3<- Tab3 %>%
      group_by (UserId) %>%
      count(UserId) %>%
      rename(BadgesNumber=n)
    
    Tab4<-P %>%  select(Id,OwnerUserId) %>% group_by(OwnerUserId)%>% summarize(Id=n(),.groups = "drop") 
    
    Tab4<-full_join(Tab3,Tab4, by=c("UserId"="OwnerUserId")) 
    Tab4[is.na(Tab4)] <- 0
    Tab4<-Tab4[c("BadgesNumber","Id")]
    Tab4<- Tab4 %>% group_by(BadgesNumber) %>% summarize(PostNumber=as.integer(mean(Id)),.groups = "drop")
    
    Tab5 <- (B%>% filter(Class=="3"))
    
    
    Tab5<- Tab5 %>%
      group_by (UserId) %>%
      count(UserId) %>%
      rename(BadgesNumber=n)
    
    Tab6<-P %>%  select(Id,OwnerUserId) %>% group_by(OwnerUserId)%>% summarize(Id=n(),.groups = "drop") 
    
    Tab6<-full_join(Tab5,Tab6, by=c("UserId"="OwnerUserId")) 
    Tab6[is.na(Tab6)] <- 0
    Tab6<-Tab6[c("BadgesNumber","Id")]
    Tab6<- Tab6 %>% group_by(BadgesNumber) %>% summarize(PostNumber=as.integer(mean(Id)),.groups = "drop")
    
    Tab2<-Tab2 %>% mutate(BadgeType="G")
    Tab4<-Tab4 %>% mutate(BadgeType="S")
    Tab6<-Tab6 %>% mutate(BadgeType="B")
    
    wynik<-rbind(Tab2,Tab4,Tab6)
     
      wykres<-ggplot(wynik, aes(x=BadgesNumber, y=PostNumber, group=BadgeType, color=BadgeType)) +
      geom_line()  +
        scale_color_manual(values = c("#592708","#a8a032","#77c4c7")) +
      ggtitle("Stosunek iloœci Badges do iloœci postów")+
        transition_reveal(BadgesNumber)
      print(wykres)
      animate(wykres, renderer = gifski_renderer())
      anim_save(filename="wykres.gif", animation=wykres)
}

fun22(Badgesc,Postsc)

