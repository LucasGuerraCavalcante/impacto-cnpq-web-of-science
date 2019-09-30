
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

df <- read.csv('data/todos.csv', sep=',', header=FALSE)

grafico1 <- ggplot(df, aes(x = V1, y = V3)) + 
  geom_line(aes(color = V2), size = 1) +
  scale_color_manual(values = c("#3E2CD6","#2177B0","#B616D9"))   +
  scale_x_continuous(breaks = seq(min(df$V1), max(df$V1))) +
  geom_point(aes(size = V2 == "CNPq"), show.legend = FALSE)  +
  geom_text(aes(label=ifelse(V2 == "CNPq", V3, "")), nudge_y = 1000) + ylab("Quantidade de Artigos Publicados") +
  scale_size_manual(values=c(-1,1)) +  theme_minimal() + theme(legend.title = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.text.x = element_text(color="black"))

grafico1 

df2 <- read.csv('data/todosbrasil.csv', sep=',', header=FALSE)

grafico2 <- ggplot(df2, aes(x = V1, y = V3)) + 
  geom_line(aes(color = V2), size = 1) +
  scale_color_manual(values = c("#56FA4B" ,"#3E2CD6","#2177B0","#B616D9")) +
  geom_point(aes(size = V2 == "Brasil"), show.legend = FALSE)  +
  geom_text(aes(label=ifelse(V2 == "Brasil", V3, "")), nudge_y = 4000) +
  scale_x_continuous(breaks = seq(min(df$V1), max(df$V1))) +
  ylab("Quantidade de Artigos Publicados") +
  scale_size_manual(values=c(-1,1)) + theme_minimal() + theme(legend.title = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.text.x = element_text(color="black"))

grafico2


brasilxcnpq <- read.csv('data/brasilxcnpq.csv', sep=',', header=FALSE)

ct = 1
listabr <- c()
listacnpq <- c()

while (ct != (nrow(brasilxcnpq)/2)+1) {
  
  pcnpq <- as.integer((((brasilxcnpq[ct+12,3])*100)/brasilxcnpq[ct,3]))
  
  pbr <- 100 - pcnpq
  
  pcnpq <- paste(str_extract(as.character(pcnpq),"^.{2}"), "%", sep = "")
  
  pbr <- paste(str_extract(as.character(pbr),"^.{2}"), "%", sep = "")
  
  listabr <- c(listabr, pbr)
  
  listacnpq <- c(listacnpq, pcnpq)
  
  ct = ct + 1
}

brasilxcnpq <- cbind(brasilxcnpq, porcent = c(listabr, listacnpq))

rm(listabr)
rm(listacnpq)
rm(ct)
rm(pbr)
rm(pcnpq)


grafico3 <- ggplot(data=brasilxcnpq, aes(x=V1, y=V3, fill=V2)) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(min(df$V1), max(df$V1))) +
  ylab("Quantidade de Artigos Publicados") +
  scale_fill_manual(values=c("#56FA4B", "#2177B0")) +
  theme_minimal() + theme(legend.title = element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(), axis.text.y = element_text(color="black")) +
  geom_text(aes(label = ifelse(brasilxcnpq$V2 == "CNPq", as.character(brasilxcnpq$porcent), "")), 
            hjust = -0.1, size = 4,
            position = position_dodge(width = 1),
            inherit.aes = TRUE, vjust=1) + coord_flip()

grafico3

universidades <- read.csv('data/Organizações.csv', sep=';', header=FALSE)


grafico4 <- ggplot(data=universidades, aes(x=reorder(V1,-V2), y=V2, fill = V2)) +
  geom_bar(width=0.8, stat="identity") + coord_flip() +
  ylab("Quantidade de Artigos Publicados") +
  theme_minimal() + theme(legend.title = element_blank(),axis.title.y=element_blank(),axis.text.x=element_text(color="black"), axis.text.y = element_text(color="black")) + geom_text(aes(label = universidades$V2), size = 3.5, vjust=0.4, hjust=1, color="black")


grafico4


paises <- read.csv('data/Países.csv', sep=',', header=FALSE)

grafico5 <- ggplot(data=paises, aes(x=reorder(V1,-V2), y=V2, fill = V2)) +
  geom_bar(width=0.8, stat="identity") + coord_flip() +
  ylab("Quantidade de Artigos Publicados") +
  theme_minimal() + theme(legend.title = element_blank(),axis.title.y=element_blank(),axis.text.x=element_text(color="black"), axis.text.y = element_text(color="black")) + geom_text(aes(label = paises$V2), size = 3.5, vjust=0.4, hjust=1, color="black")


grafico5
