

data<-readxl::read_xlsx("data.xlsx")%>%
  select(-1)

item_labels=data.frame(label=c("Setbacks don’t discourage me",
                               "I am very studious and put a lot of effort into my schoolwork.",
                               "I always finish what I start",
                               "I am careful and dedicated in what I do",
                               "I have been obsessed with a certain idea or project for a short time but later lost interest.",
                               "I often set a goal but later choose to pursue a different one",
                               "I have difficulty in maintaining my focus on projects that take more than a few months to complete.",
                               "I am grateful for new opportunities that come into my life",
                               "Changing plans or strategies is important to achieve my long-term goals in life",
                               "Changes in life motivate me to work harder",
                               "I am able to cope with the changing circumstances in life.",
                               "I am always motivated to do my best" ,
                               "I always keep working for what I want even when I don’t do as well as I would like to",
                               "Sometimes studying doesn't matter so much to me"),
                       Item=paste("Item",c(1:14)))




names(data)=paste("Item",c(1:14))

polychor_matrix<-polychoric(data)

as.data.frame(polychor_matrix$rho)%>%
  kable("pipe",digits=2)



k<-KMO(polychor_matrix$rho)

psych::cortest.bartlett(polychor_matrix$rho)
data.frame(KMO=round(k$MSA,digits=2))%>%
  kable("pipe")


as.data.frame(polychor_matrix$rho)%>%
  mutate(y=paste("Item ",row_number(),sep=""))%>%
  pivot_longer(starts_with("Item"))%>%
  filter(value^2>=.09,
         value!=1)%>%
  group_by(y)%>%
  count()%>%
  filter()%>%
  rename(Item=y,
         #`Número de correlaciones <.3`=n
  )%>%
  arrange(n)%>%
  kable("pipe",captio="Número de correlaciones >.3^2")



library(performance)

item_intercor(data,polychor_matrix$rho,
              method = "pearson")



data%>%
  #select(-c(`Item 1`,`Item 5`,`Item 6`,`Item 7`))%>%
  nfactors(rotate = "oblimin",cor="poly")



f<-data%>%
  #select(-c(`Item 1`,`Item 5`,`Item 6`,`Item 7`))%>%
  fa(nfactors=2,fm="ml", rotate = "oblimin",cor="poly")

#print(f,sort=TRUE,cut=.3)

fa.lookup(f,f)%>%
  as.data.frame()%>%
  mutate(ML2=case_when(ML2^2>.09~ML2,
                       ML2^2<.09~NA),
         ML1=case_when(ML1^2>.09~ML1,
                       ML1^2<.09~NA),
         Item=row.names(.))%>%
  left_join(item_labels,by="Item")%>%
  select(-c(y))%>%
  relocate(label,.before=1)%>%
  relocate(Item,.before=1)%>%
  kable("pipe")

f$Vaccounted%>%
  kable("pipe",digits=2)


data%>%
  select(-c(`Item 9`,
            `Item 1`,
            `Item 5`,
            `Item 7`,
            `Item 6`,
            `Item 14`))%>%
  nfactors(rotate = "oblimin",cor="poly")



f<-data%>%
  select(-c(`Item 9`,
            `Item 1`,
            `Item 5`,
            `Item 7`,
            `Item 6`,
            `Item 14`))%>%
  fa(nfactors=2,fm="ml", rotate = "oblimin",cor="poly")

#print(f,sort=TRUE,cut=.3)

fa.lookup(f,f)%>%
  as.data.frame()%>%
  mutate(ML2=case_when(ML2^2>.09~ML2,
                       ML2^2<.09~NA),
         ML1=case_when(ML1^2>.09~ML1,
                       ML1^2<.09~NA),
         Item=row.names(.))%>%
  left_join(item_labels,by="Item")%>%
  select(-c(y))%>%
  relocate(label,.before=1)%>%
  relocate(Item,.before=1)%>%
  kable("pipe")


f$Vaccounted%>%
  as.data.frame()%>%
  kable("pipe",digits=2)

as.data.frame(f$score.cor)%>%
  mutate(x=c("ML1","ML2"))%>%
  relocate(x,.before = 1)%>%
  kable("pipe",digits=2,col.names = c("","ML1","ML2"))
