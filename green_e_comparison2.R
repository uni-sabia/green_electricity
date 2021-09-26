# Are market structure and green power market size correlated? 

Looking at the maps, it is difficult to tell where there is a correlation between market structures and green power market size. 


```{r}
# Summarize the data and prepare it for graphing
gp_mkt <- left_join(gp, markets, by="abb") %>% drop_na(electric) %>%
  mutate(tot_gen_gwh = tot_gen/1000)
mkt_sum <- gp_mkt %>% 
  group_by(electric) %>%
  summarize(tot_gen_gwh = sum(tot_gen_gwh),
            tot_cus = sum(tot_cus)) 

gen <- ggplot(mkt_sum, aes(x=electric, y=tot_gen_gwh)) + 
  geom_bar(stat="identity", fill="aquamarine2") +
  theme_classic() + 
  theme(axis.title.x=element_blank()) + 
  labs(y="Generation (GWh)")

cus <- ggplot(mkt_sum, aes(x=electric, y=tot_cus/100)) + 
  geom_bar(stat="identity", fill="aquamarine4") + 
  theme_classic() +
  theme(axis.title.x=element_blank()) + 
  labs(y="# Customers (x100)")

ggarrange(gen, cus, nrol=1, nrow=2)
```