#http://gastonsanchez.com/how-to/2012/10/13/MCA-in-R/

lotto.2=lotto %>% select(ORGANISATION_TYPE,CHARITABLE_STATUS,Council.or.School,Funding.per.head)
lotto.2$Funding.per.head=cut(lotto.2$Funding.per.head,100)

cats = apply(lotto.2, 2, function(x) nlevels(as.factor(x)))

mca1=MCA(lotto.2)
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) 

  xlim(c(-1,0))+
  ylim(c(-1,1))+
  ggtitle("MCA plot of variables using R package FactoMineR")


summary(mca1)
