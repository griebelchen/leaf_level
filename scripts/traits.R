library("dplyr")
library("ggplot2")
library("gridExtra")
source("scripts/abbreviations.R")

df <- read.csv("raw_data/water_potentials_170517.csv", na.strings=c("","NA"),header=TRUE)
df$h2o<-(1-(df$dry_weight/df$fresh_weight))
df$lma<-df$dry_weight/df$leaf_area
df$phi<-df$water_potential*-1


df_euc<-subset(df, type == 'eucalypt')
df_mis<-subset(df, type == 'mistletoe')
fib<-subset(df, species == 'fibrosa')
mol<-subset(df, species == 'moluccana')
mel<-subset(df, species == 'melaleuca')

boxplot(water_potential~species,data=df)

boxplot(water_potential~infestation,data=df)
boxplot(water_potential~infestation,data=fib)
boxplot(water_potential~infestation,data=mol)


par(mfrow=c(3,1))
phi <- ggplot(data = df, aes(x=infestation, y=phi)) + ylim(-3.2,-1.4) +
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none")
 

h2o <- ggplot(data = df, aes(x=infestation, y=h2o)) + ylim(0.45,0.75) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none")

lma <- ggplot(data = df, aes(x=infestation, y=lma)) + ylim(0.015,0.03) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none")


traits<-grid.arrange(phi, h2o , lma, nrow=3, ncol=1)
ggsave("output/traits.png", plot = traits,  width = 20, height = 20, units = "cm", dpi = 300)

phi_mis <- ggplot(data = df_mis, aes(x=infestation, y=phi)) + ylim(-3.2,-1.4) +
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none") + labs(x="Mistletoe leaves")


h2o_mis <- ggplot(data = df_mis, aes(x=infestation, y=h2o)) + ylim(0.45,0.75) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none") + labs(x="Mistletoe leaves")

lma_mis <- ggplot(data = df_mis, aes(x=infestation, y=lma)) + ylim(0.015,0.03) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none") + labs(x="Mistletoe leaves")


traits_mis<-grid.arrange(phi_mis, h2o_mis , lma_mis, nrow=3, ncol=1)
ggsave("output/traits_mis.png", plot = traits_mis,  width = 20, height = 20, units = "cm", dpi = 300)

phi_euc <- ggplot(data = df_euc, aes(x=infestation, y=phi)) + ylim(-3.2,-1.4) +
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none") + labs(x="Eucalypt leaves")


h2o_euc <- ggplot(data = df_euc, aes(x=infestation, y=h2o)) + ylim(0.45,0.75) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none") + labs(x="Eucalypt leaves")

lma_euc <- ggplot(data = df_euc, aes(x=infestation, y=lma)) + ylim(0.015,0.03) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free") + theme(legend.position="none") + labs(x="Eucalypt leaves")


traits_euc<-grid.arrange(phi_euc, h2o_euc , lma_euc, nrow=3, ncol=1)
ggsave("output/traits_euc.png", plot = traits_euc,  width = 20, height = 20, units = "cm", dpi = 300)


phi_mel <- ggplot(data = mel, aes(x=infestation, y=phi)) + ylim(-3.2,-1.4) +
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none") + labs(x="melaleuca leaves")


h2o_mel <- ggplot(data = mel, aes(x=infestation, y=h2o)) + ylim(0.45,0.75) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free")+ theme(legend.position="none") + labs(x="melaleuca leaves")

lma_mel <- ggplot(data = mel, aes(x=infestation, y=lma)) + ylim(0.015,0.03) + 
  geom_boxplot(aes(fill=infestation)) + facet_wrap( ~ species, scales="free") + theme(legend.position="none") + labs(x="melaleuca leaves")


traits_mel<-grid.arrange(phi_mel, h2o_mel , lma_mel, nrow=3, ncol=1)
ggsave("output/traits_mel.png", plot = traits_mel,  width = 20, height = 20, units = "cm", dpi = 300)



traits_all<-grid.arrange(phi_euc, phi_mis , phi_mel, h2o_euc , h2o_mis, h2o_mel, lma_euc,lma_mis, lma_mel, nrow=3, ncol=3)
ggsave("output/traits_all.png", plot = traits_all,  width = 35, height = 20, units = "cm", dpi = 600)
