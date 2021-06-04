library(XML)
library(methods)
library(plyr)
library(ggplot2)
library(gganimate)
library(dplyr)
library(scales)

xmltotable<-function(file)
{
  file<-xmlToList(file)
  file<-ldply(file,rbind)
}
Postsj<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\judaism\\Posts.xml')
Postsj<-xmltotable(Postsj)


Commentsj<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\judaism\\Comments.xml')
Commentsj<-xmltotable(Commentsj)


Postsc<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\christ\\Posts.xml')
Postsc<-xmltotable(Postsc)

Commentsc<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\christ\\Comments.xml')
Commentsc<-xmltotable(Commentsc)

Postsi<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\islam\\Posts.xml')
Postsi<-xmltotable(Postsi)

Commentsi<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\islam\\Comments.xml')
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

fun12<-function(Tab1)
{
  Tab1 %>% select(CommentsNumber) %>% mutate (AllComments=sum(CommentsNumber)) %>% slice_head(n=1) %>% select(AllComments)
  
}
jud11<-fun12(jud1)
chr11<-fun12(chr1)
isl11<-fun12(isl1)
Tab11<-rbind(jud11,chr11,isl11)
  Tab11<- Tab11 %>% mutate(NazwaForum=0) 
  Tab11[1,2]<-"Judaizm"
  Tab11[2,2]<-"Chrzeœcijañswo"
  Tab11[3,2]<-"Islam"

fun13<-function(plik)
{
  plik1<-plik %>% mutate(AllComments=0) %>% mutate(frame=rep("a",3))
  plik2<-plik %>% mutate(frame=rep("b",3))
  plik3<-rbind(plik1,plik2)
  my.name <- readline(prompt="Enter name: ")
  Chart<-ggplot(plik3, aes(x=NazwaForum, y=AllComments, fill=NazwaForum)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.9))+
  theme_bw()+
  theme(axis.text = element_text(size = 8))+
  theme(axis.text.y = element_text(size=5))+
  scale_y_continuous(labels = comma) +
  ggtitle("Iloœæ komentarzy na forach")

  ggsave(my.name)

  my.name2 <- readline(prompt="Enter name: ")

  Gif<-ggplot(plik3, aes(x=NazwaForum, y=AllComments, fill=NazwaForum)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.9))+
  theme_bw()+
  theme(axis.text = element_text(size = 8))+
  theme(axis.text.y = element_text(size=5))+
    scale_y_continuous(labels = comma) +
  ggtitle("Iloœæ komentarzy na forach")+
  transition_states(frame, transition_length = 2, state_length = 1) +
  ease_aes('sine-in-out')

animate(Gif, renderer = gifski_renderer())
anim_save(filename=my.name2, animation=Gif)
}
fun13(Tab11)

fun14<-function(A,B,C)
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
Tab<-fun14(jud1,chr1,isl1)
View(Tab)
View(Commentsc)
fun15<-function(C)
{
  C<-C %>% distinct(UserId) %>% mutate(CommentsAmount=1) %>% mutate(CommentsAmount=sum(CommentsAmount)) %>% select(CommentsAmount) 
  head(C,1)
  
}
fun16<-function(A,B,C)
{
  A<-fun15(A)
  B<-fun15(B)
  C<-fun15(C)
  Tab<-c(A,B,C)
  Tab<-ldply(Tab,rbind)
  Tab<- Tab %>% rename(NazwaForum=.id)%>% rename(AllUsers="1")
  Tab[1,1]<-"Judaizm"
  Tab[2,1]<-"Chrzeœcijañswo"
  Tab[3,1]<-"Islam"
  Tab

}

xyz<-fun16(Commentsj,Commentsc,Commentsi)
View(xyz)

fun17<-function(plik)
{
  plik1<-plik %>% mutate(AllUsers=0) %>% mutate(frame=rep("a",3))
  plik2<-plik %>% mutate(frame=rep("b",3))

  plik3<-rbind(plik1,plik2)
  my.name <- readline(prompt="Enter name: ")
  Chart<-ggplot(plik3, aes(x=NazwaForum, y=AllUsers, fill=NazwaForum)) +
    geom_col(width = 0.5, position = position_dodge(width = 0.9))+
    theme_bw()+
    theme(axis.text = element_text(size = 8))+
    theme(axis.text.y = element_text(size=5))+
    scale_y_continuous(labels = comma) +
    ggtitle("Iloœæ aktywnych u¿ytkowników na forach")

  ggsave(my.name)

  my.name2 <- readline(prompt="Enter name: ")

  Gif<-ggplot(plik3, aes(x=NazwaForum, y=AllUsers, fill=NazwaForum)) +
    geom_col(width = 0.5, position = position_dodge(width = 0.9))+
    theme_bw()+
    theme(axis.text = element_text(size = 8))+
    theme(axis.text.y = element_text(size=5))+
    scale_y_continuous(labels = comma) +
    ggtitle("Iloœæ aktywnych u¿ytkowników na forach")+
    transition_states(frame, transition_length = 2, state_length = 1) +
    ease_aes('sine-in-out')

  animate(Gif, renderer = gifski_renderer())
  anim_save(filename=my.name2, animation=Gif)
}
fun17(xyz)
#zadanie 2

Badgesj<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\judaism\\Badges.xml')
Badgesj<-xmltotable(Badgesj)
 
Badgesi<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\islam\\Badges.xml')
Badgesi<-xmltotable(Badgesi)

Badgesc<-xmlParse('C:\\Users\\sophi\\OneDrive\\Pulpit\\pdu3\\christ\\Badges.xml')
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
 View(Badgesc)
 View(Badgesi)
 View(Postsc)
 View(Postsi)

fun22<-function(B,P)
{
  Tab1 <- (B%>% filter(Class=="1"))
  Tab1<- Tab1 %>%
    group_by (UserId) %>%
    count(UserId) %>%
    rename(BadgesNumber=n)


    Tab2<-P %>%  select(Id,OwnerUserId) %>% group_by(OwnerUserId)%>% summarize(Id=n(),.groups = "drop") 
    
    Tab2<-full_join(Tab1,Tab2, by=c("UserId"="OwnerUserId")) 
    Tab2<-Tab2[c("BadgesNumber","Id")]%>%mutate(BadgesNumber=coalesce(BadgesNumber, 0), Id=coalesce(Id,0))
    Tab2<- Tab2 %>% group_by(BadgesNumber) %>% summarize(PostNumber=as.integer(mean(Id)),.groups = "drop")
    
    Tab3 <- (B%>% filter(Class=="2"))
    
    
    Tab3<- Tab3 %>%
      group_by (UserId) %>%
      count(UserId) %>%
      rename(BadgesNumber=n)
    
    Tab4<-P %>%  select(Id,OwnerUserId) %>% group_by(OwnerUserId)%>% summarize(Id=n(),.groups = "drop") 
    
    Tab4<-full_join(Tab3,Tab4, by=c("UserId"="OwnerUserId")) 
    Tab4<-Tab4[c("BadgesNumber","Id")]%>%mutate(BadgesNumber=coalesce(BadgesNumber, 0), Id=coalesce(Id,0))
    Tab4<- Tab4 %>% group_by(BadgesNumber) %>% summarize(PostNumber=as.integer(mean(Id)),.groups = "drop")
    
    Tab5 <- (B%>% filter(Class=="3"))
    
    
    Tab5<- Tab5 %>%
      group_by (UserId) %>%
      count(UserId) %>%
      rename(BadgesNumber=n)
    
    Tab6<-P %>%  select(Id,OwnerUserId) %>% group_by(OwnerUserId)%>% summarize(Id=n(),.groups = "drop") 
    
    Tab6<-full_join(Tab5,Tab6, by=c("UserId"="OwnerUserId"))
    Tab6<-Tab6[c("BadgesNumber","Id")]%>%mutate(BadgesNumber=coalesce(BadgesNumber, 0), Id=coalesce(Id,0))
    Tab6<- Tab6 %>% group_by(BadgesNumber) %>% summarize(PostNumber=as.integer(mean(Id)),.groups = "drop")
    
    Tab2<-Tab2 %>% mutate(BadgeType="GoldBadges")
    Tab4<-Tab4 %>% mutate(BadgeType="SilverBadges")
    Tab6<-Tab6 %>% mutate(BadgeType="BronzeBadges")
    
    wynik<-rbind(Tab2,Tab4,Tab6)
}

wynik<-fun22(Badgesi,Postsi)
View(wynik)

fun23<-function(wynik)
{

  my.name <- readline(prompt="Enter name: ")

  wykres<-ggplot(wynik, aes(x=BadgesNumber, y=PostNumber, group=BadgeType, color=BadgeType)) +
    geom_line()  +
    scale_color_manual(values = c("#592708","#a8a032","#77c4c7")) +
    ggtitle("Stosunek iloœci Badges do iloœci postów")+
    transition_reveal(BadgesNumber)
  print(wykres)
  animate(wykres, renderer = gifski_renderer())
  anim_save(filename=my.name, animation=wykres)

  my.name2 <- readline(prompt="Enter name: ")

  Chart<-ggplot(wynik, aes(x=BadgesNumber, y=PostNumber, group=BadgeType, color=BadgeType)) +
    geom_line()  +
    scale_color_manual(values = c("#592708","#a8a032","#77c4c7")) +
    ggtitle("Stosunek iloœci Badges do iloœci postów")+
    theme_bw()+
    theme(axis.text = element_text(size = 8))+
    ggsave(my.name2)
}
fun23(wynik)

