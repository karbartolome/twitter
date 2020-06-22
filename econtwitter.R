library(dplyr)
library(ggplot2)
library(extrafont)
library(lubridate)
library(ggpubr)
library(packcircles)
library(ggplot2)

df=read.csv('df_econtwitter.csv',stringsAsFactors = FALSE )
df$tweet=tolower(df$tweet)

# Tweets que contienen algún lenguaje ------------------------------------------------------------

df_tweets=df %>% mutate(date=as.Date(date)) %>% 
       group_by(date) %>% 
       summarise(n=n())          

r=df %>% 
  mutate(date = as.Date(date),
         r = ifelse(str_detect(tweet,'rstats|#r\\s|[:space:]r\\s'),'si','no')) %>% 
  group_by(date,r) %>% 
  summarise(n = n()) %>% 
  spread(key='r', value='n') %>% 
  mutate(n=ifelse(is.na(si),0,si), 
         prop=n*100/(no+si), 
         lenguaje='r') 

python=df %>% 
  mutate(date = as.Date(date),
         r = ifelse(str_detect(tweet,'python'),'si','no')) %>% 
  group_by(date,r) %>% 
  summarise(n = n()) %>% 
  spread(key='r', value='n') %>% 
  mutate(n=ifelse(is.na(si),0,si), 
         prop=n*100/(no+si), 
         lenguaje='python') 

stata=df %>% 
  mutate(date = as.Date(date),
         r = ifelse(str_detect(tweet,'stata'),'si','no')) %>% 
  group_by(date,r) %>% 
  summarise(n = n()) %>% 
  spread(key='r', value='n') %>% 
  mutate(n=ifelse(is.na(si),0,si), 
         prop=n*100/(no+si), 
         lenguaje='stata') 

df_lang=rbind(r,python,stata) %>% 
  mutate(index=row_number(),
         prop=round(prop,2))


p1=ggplot()+
  geom_smooth(data=df_tweets, aes(x=date,y=n), color='#80ffc8', fill='#80ffc8', alpha=0.2)+
  geom_line(data=df_tweets,aes(x=date,y=n), color='black',alpha=0.7)+
  labs(x="",
       y='cantidad',
       title='Tweets con el hashtag #econtwitter')+
  theme_minimal()+
  theme(text=element_text(size=15,  family="Courier New"), 
        title=element_text(size=20),
        panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption.position = 'plot',
        strip.background = element_rect(color="black", fill="#f5f5f5", size=0, linetype="solid")
  )

p2=ggplot()+
  geom_col(data=df_lang,aes(x=date,y=n,fill=lenguaje), position='stack')+
  scale_fill_manual(values = c("#F2CB05","#4197d9", "#69c779"))+
  labs(x="", 
       y='cantidad',
       fill='',
       title='Tweets de #econtwitter que mencionan algún lenguaje', 
       caption='@karbartolome')+
  theme_minimal()+
  theme(text=element_text(size=15,  family="Courier New"), 
        title=element_text(size=20),
        legend.position = 'top',
        panel.grid.major = element_blank(), 
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption.position = 'plot',
        strip.background = element_rect(color="black", fill="#f5f5f5", size=0, linetype="solid")
  )

ggarrange(p1, p2, 
          nrow = 2)


# Con 2 ejes:
ggplot()+
  geom_smooth(data=df_tweets, aes(x=date,y=n), color='#80ffc8', fill='#80ffc8', alpha=0.2)+
  geom_line(data=df_tweets,aes(x=date,y=n), color='#9da69f')+
  geom_point(data=df_tweets,aes(x=date,y=n), color='#9da69f', alpha=0.2)+
  geom_col(data=df_lang, aes(x=date,y=n*4, fill=lenguaje), position='stack')+
  #geom_text(data=df_lang, aes(x=date,y=n, label=n), position = position_stack(vjust = .5))+
  scale_y_continuous(name='tweets',
                     sec.axis = sec_axis(~./4, name = "tweets que mencionan lenguajes"))+
  scale_fill_manual(values = c("#F2CB05","#415CF2", "#69c779"))+
  labs(x="", fill='lenguaje',
       title='Evolución del hashtag #econtwitter y lenguajes', 
       caption='@karbartolome')+
  theme_minimal()+
  theme(text=element_text(size=15),
        #family="Courier New"), 
        title=element_text(size=20),
        panel.grid.major = element_blank(),
        legend.position = 'bottom',
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        plot.caption.position = 'plot',
        strip.background = element_rect(color="black", fill="#f5f5f5", size=0, linetype="solid")
  )


# Bubble chart ------------------------------------------------------------

df_users = df %>% group_by(username) %>% 
  summarise(likes = sum(likes_count),n=n()) %>% 
  top_n(200) %>% 
  arrange(desc(n))

packing <- circleProgressiveLayout(df_users$n, sizetype='area')

data <- cbind(df_users, packing) %>% arrange(n) %>% mutate(texto=paste(username,"\n ",n))

bubbles_df <- circleLayoutVertices(packing, npoints=50) %>% 
  mutate(color=ifelse(id==18,'stata','nostata'))

ggplot() +
  geom_polygon(
    data = bubbles_df,
    aes(x,y,group = id,fill = as.numeric(id), color=color),alpha = 1) +
  geom_text(data = data, 
            aes(x, y, label = texto, size=n)) +
  scale_fill_gradient2(low='white',high='#80ffc8')+
  scale_size_continuous(range = c(1, 5)) +
  scale_color_manual(values = c("black","red"))+
  labs(title='Usuarios que más tweetean en #econtwitter', 
       subtitle='Ene 2020 - Jun 2020',
       caption="@karbartolome")+
  coord_equal()+
  theme(legend.position = "none", 
        text=element_text(family="Courier New"),
        panel.background = element_rect(fill='white'), 
        plot.background =  element_rect(fill='white'),
        plot.title = element_text(hjust = 0.5, color='black', size=20, vjust=-2), 
        plot.subtitle = element_text(hjust = 0.5, color='black', size=12, vjust=-2), 
        plot.caption = element_text(color='black', size=12), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank())
