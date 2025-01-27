# Simulation study
# Is the distribution of drawdowns more influenced by the serial dependence structure than by 
# the variability in the distribution?

# Varying variability -----------------------------------------------------
# Compare g and h for increasing volatility (Sample from N(0,d) with d =1,2,3,...)

source("gh_index.R")
source("DD.R")

P0 <- 1
mu <- c(-0.001,0,0.001)
sds <- seq(from = 0.05, to = 0.15, length.out = 10)
seed <- 1:100
hp <- expand.grid(sd=sds, seed=seed, mu=mu,
               g1=NA, h1=NA,
               g2=NA, h2=NA,
               g3=NA, h3=NA)

for(i in 1:nrow(hp)){
  set.seed(hp$seed[i])
  # Convert to log scale parameters
  x <- rnorm(1000,hp$mu[i], hp$sd[i])
  x <- P0*cumprod(exp(x))  
  dd <- DD(x, type=1, out.plot=F)
  gh <- ghindex(-dd$dd, out.plot=F)
  hp$g1[i] <- gh$g
  hp$h1[i] <- gh$h
  dd <- DD(x, type=2, out.plot=F)
  gh <- ghindex(-dd$dd, out.plot=F)
  hp$g2[i] <- gh$g
  hp$h2[i] <- gh$h
  dd <- DD(x, type=3, out.plot=F)
  gh <- ghindex(-dd$dd, out.plot=F)
  hp$g3[i] <- gh$g
  hp$h3[i] <- gh$h
}

library(dplyr)
hp_summ_mu_0 <- hp[hp$mu==mu[1],] %>%
  group_by(sd) %>%
  summarise(across(everything(), mean))
# hp_summ_mu_0_scaled=hp_summ_mu_0
# hp_summ_mu_0_scaled[,4:9]=apply(hp_summ_mu_0[,4:9],2,scale)

hp_summ_mu_0.5 <- hp[hp$mu==mu[2],] %>%
  group_by(sd) %>%
  summarise(across(everything(), mean))
# hp_summ_mu_0.5_scaled=hp_summ_mu_0.5
# hp_summ_mu_0.5_scaled[,4:9]=apply(hp_summ_mu_0.5[,4:9],2,scale)

hp_summ_mu_1 <- hp[hp$mu==mu[3],] %>%
  group_by(sd) %>%
  summarise(across(everything(), mean))
# hp_summ_mu_1_scaled=hp_summ_mu_1
# hp_summ_mu_1_scaled[,4:9]=apply(hp_summ_mu_1[,4:9],2,scale)

library(ggplot2)
library(ggplot2)
sz <- 0.9
sz_ax <- 15

Pmu0 <- ggplot(hp_summ_mu_0, aes(x = sd)) +
  geom_line(aes(y = g1, color = "g1"),size=sz) +
  geom_line(aes(y = h1, color = "h1"),size=sz) +
  geom_line(aes(y = g2, color = "g2"),size=sz) +
  geom_line(aes(y = h2, color = "h2"),size=sz) +
  geom_line(aes(y = g3, color = "g3"),size=sz) +
  geom_line(aes(y = h3, color = "h3"),size=sz) +
  scale_color_manual(
    values = c("g1" = "red3", "h1" = "red", "g3" = "green4", "h3" = "green2", "g2" = "blue", "h2" = "cyan3"),
    name = "Index",
    breaks = c("g1", "h1", "g2", "h2", "g3", "h3"),
    labels = c(expression(g(d[1])), expression(h(d[1])),
               expression(g(d[2])), expression(h(d[2])),
               expression(g(d[3])), expression(h(d[3])))
  ) + scale_x_continuous(
    breaks = seq(min(sds), max(sds), length.out=length(sds)-4), 
    labels = scales::label_number(accuracy = 0.01) 
  ) +
  labs(
    x = expression(sigma),
    y = "Value",
    title = bquote(mu == .(mu[1]))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = sz_ax),
    axis.title = element_text(size = sz_ax),
    plot.title = element_text(size = sz_ax + 2, face = "bold"), # Titolo in grassetto
    legend.key.size = unit(1.5, "lines"), 
    legend.text = element_text(size = sz_ax - 1), 
    legend.title = element_text(size = sz_ax)
  )

# Repeat for Pmu0.5 e Pmu1
Pmu0.5 <- ggplot(hp_summ_mu_0.5, aes(x = sd)) +
  geom_line(aes(y = g1, color = "g1"),size=sz) +
  geom_line(aes(y = h1, color = "h1"),size=sz) +
  geom_line(aes(y = g2, color = "g2"),size=sz) +
  geom_line(aes(y = h2, color = "h2"),size=sz) +
  geom_line(aes(y = g3, color = "g3"),size=sz) +
  geom_line(aes(y = h3, color = "h3"),size=sz) +
  scale_color_manual(
    values = c("g1" = "red3", "h1" = "red", "g3" = "green4", "h3" = "green2", "g2" = "blue", "h2" = "cyan3"),
    name = "Index",
    breaks = c("g1", "h1", "g2", "h2", "g3", "h3"),
    labels = c(expression(g(d[1])), expression(h(d[1])),
               expression(g(d[2])), expression(h(d[2])),
               expression(g(d[3])), expression(h(d[3])))
  ) + scale_x_continuous(
    breaks = seq(min(sds), max(sds), length.out=length(sds)-4), 
    labels = scales::label_number(accuracy = 0.01) 
  ) +
  labs(
    x = expression(sigma),
    y = " ",
    title = bquote(mu == .(mu[2]))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = sz_ax),
    axis.title = element_text(size = sz_ax),
    plot.title = element_text(size = sz_ax + 2, face = "bold"), 
    legend.key.size = unit(1.5, "lines"), 
    legend.text = element_text(size = sz_ax - 1), 
    legend.title = element_text(size = sz_ax)
  )

Pmu1 <- ggplot(hp_summ_mu_1, aes(x = sd)) +
  geom_line(aes(y = g1, color = "g1"),size=sz) +
  geom_line(aes(y = h1, color = "h1"),size=sz) +
  geom_line(aes(y = g2, color = "g2"),size=sz) +
  geom_line(aes(y = h2, color = "h2"),size=sz) +
  geom_line(aes(y = g3, color = "g3"),size=sz) +
  geom_line(aes(y = h3, color = "h3"),size=sz) +
  scale_color_manual(
    values = c("g1" = "red4", "h1" = "red", "g3" = "green4", "h3" = "green2", "g2" = "blue", "h2" = "cyan3"),
    name = "Index",
    breaks = c("g1", "h1", "g2", "h2", "g3", "h3"),
    labels = c(expression(g(d[1])), expression(h(d[1])),
               expression(g(d[2])), expression(h(d[2])),
               expression(g(d[3])), expression(h(d[3])))
  ) + scale_x_continuous(
    breaks = seq(min(sds), max(sds), length.out=length(sds)-4), 
    labels = scales::label_number(accuracy = 0.01) 
  ) +
  labs(
    x = expression(sigma),
    y = " ",
    title = bquote(mu == .(mu[3]))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = sz_ax),
    axis.title = element_text(size = sz_ax),
    plot.title = element_text(size = sz_ax + 2, face = "bold"), 
    legend.key.size = unit(1.5, "lines"), 
    legend.text = element_text(size = sz_ax - 1), 
    legend.title = element_text(size = sz_ax)
  )


library(ggpubr)

# save pdf A4
#pdf("gh_index_variability_2.pdf",
    #height=8.27,width=11.69)
ggarrange(Pmu0,Pmu0.5,Pmu1,
          #Pmu0_scaled,Pmu0.5_scaled,Pmu1_scaled,
          ncol=3,nrow=1,common.legend = TRUE,
          legend="right")
#dev.off()

# Varying serial dependence -----------------------------------------------
# Compare g and h for increasing serial dependence (Sample from AR(1) with rho = 0.01, 0.1, 0.2,...)

P0 <- 1
rhos <- seq(-0.9,0.9, length.out=10)
sd <- 0.01
seed <- 1:100
mu <- seq(-0.001,0.001,length.out=3)

hp <- expand.grid(rho=rhos, seed=seed, mu=mu,
               g1=NA,h1=NA,
               g2=NA,h2=NA,
               g3=NA,h3=NA)

for(i in 1:nrow(hp)){
  set.seed(hp$seed[i])
  x <- arima.sim(n=1000,list(ar=hp$rho[i]),sd=sd)+hp$mu[i]
  x <- P0*cumprod(exp(x))  
  dd <- DD(x, type=1, out.plot=F)
  gh <- ghindex(-dd$dd,out.plot=F)
  hp$g1[i] <- gh$g
  hp$h1[i] <- gh$h
  dd <- DD(x, type=2, out.plot=F)
  gh <- ghindex(-dd$dd, out.plot=F)
  hp$g2[i] <- gh$g
  hp$h2[i] <- gh$h
  dd <- DD(x, type=3, out.plot=F)
  gh <- ghindex(-dd$dd, out.plot=F)
  hp$g3[i] <- gh$g
  hp$h3[i] <- gh$h
}

hp_ser_mu_0 <- hp[hp$mu==mu[1],] %>%
  group_by(rho) %>%
  summarise(across(everything(), mean))
# hp_ser_mu_0_scaled=hp_ser_mu_0
# hp_ser_mu_0_scaled[,4:9]=apply(hp_ser_mu_0[,4:9],2,scale)

hp_ser_mu_0.5 <- hp[hp$mu==mu[2],] %>%
  group_by(rho) %>%
  summarise(across(everything(), mean))
# hp_ser_mu_0.5_scaled=hp_ser_mu_0.5
# hp_ser_mu_0.5_scaled[,4:9]=apply(hp_ser_mu_0.5[,4:9],2,scale)


hp_ser_mu_1 <- hp[hp$mu==mu[3],] %>%
  group_by(rho) %>%
  summarise(across(everything(), mean))
# hp_ser_mu_1_scaled=hp_ser_mu_1
# hp_ser_mu_1_scaled[,4:9]=apply(hp_ser_mu_1[,4:9],2,scale)


# sz=.9
# sz_ax=15

Pser_mu0 <- ggplot(hp_ser_mu_0, aes(x = rho)) +
  geom_line(aes(y = g1, color = "g1"),size=sz) +
  geom_line(aes(y = h1, color = "h1"),size=sz) +
  geom_line(aes(y = g2, color = "g2"),size=sz) +
  geom_line(aes(y = h2, color = "h2"),size=sz) +
  geom_line(aes(y = g3, color = "g3"),size=sz) +
  geom_line(aes(y = h3, color = "h3"),size=sz) +
  scale_color_manual(
    values = c("g1" = "red3", "h1" = "red", "g3" = "green4", "h3" = "green2", "g2" = "blue", "h2" = "cyan3"),
    name = "Index",
    breaks = c("g1", "h1", "g2", "h2", "g3", "h3"),
    labels = c(expression(g(d[1])), expression(h(d[1])),
               expression(g(d[2])), expression(h(d[2])),
               expression(g(d[3])), expression(h(d[3])))
  ) +
  scale_x_continuous(
    breaks = seq(min(rhos), max(rhos), length.out=length(rhos)-4),  
    labels = scales::label_number(accuracy = 0.01) 
  ) +
  labs(
    x = expression(rho),
    y = "Value",
    title = bquote(mu == .(mu[1]))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = sz_ax),
    axis.title = element_text(size = sz_ax),
    plot.title = element_text(size = sz_ax + 2, face = "bold"), 
    legend.key.size = unit(1.5, "lines"), 
    legend.text = element_text(size = sz_ax - 1), 
    legend.title = element_text(size = sz_ax)
  )

Pser_mu0.5 <- ggplot(hp_ser_mu_0.5, aes(x = rho)) +
  geom_line(aes(y = g1, color = "g1"),size=sz) +
  geom_line(aes(y = h1, color = "h1"),size=sz) +
  geom_line(aes(y = g2, color = "g2"),size=sz) +
  geom_line(aes(y = h2, color = "h2"),size=sz) +
  geom_line(aes(y = g3, color = "g3"),size=sz) +
  geom_line(aes(y = h3, color = "h3"),size=sz) +
  scale_color_manual(
    values = c("g1" = "red3", "h1" = "red", "g3" = "green4", "h3" = "green2", "g2" = "blue", "h2" = "cyan3"),
    name = "Index",
    breaks = c("g1", "h1", "g2", "h2", "g3", "h3"),
    labels = c(expression(g(d[1])), expression(h(d[1])),
               expression(g(d[2])), expression(h(d[2])),
               expression(g(d[3])), expression(h(d[3])))
  ) +
  scale_x_continuous(
    breaks = seq(min(rhos), max(rhos), length.out=length(rhos)-4),  
    labels = scales::label_number(accuracy = 0.01) 
  ) +
  labs(
    x = expression(rho),
    y = " ",
    title = bquote(mu == .(mu[2]))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = sz_ax),
    axis.title = element_text(size = sz_ax),
    plot.title = element_text(size = sz_ax + 2, face = "bold"), 
    legend.key.size = unit(1.5, "lines"), 
    legend.text = element_text(size = sz_ax - 1), 
    legend.title = element_text(size = sz_ax)
  )

Pser_mu1 <- ggplot(hp_ser_mu_1, aes(x = rho)) +
  geom_line(aes(y = g1, color = "g1"), size=sz) +
  geom_line(aes(y = h1, color = "h1"), size=sz) +
  geom_line(aes(y = g2, color = "g2"), size=sz) +
  geom_line(aes(y = h2, color = "h2"), size=sz) +
  geom_line(aes(y = g3, color = "g3"), size=sz) +
  geom_line(aes(y = h3, color = "h3"), size=sz) +
  scale_color_manual(
    values = c("g1" = "red3", "h1" = "red", "g3" = "green4", "h3" = "green2", "g2" = "blue", "h2" = "cyan3"),
    name = "Index",
    breaks = c("g1", "h1", "g2", "h2", "g3", "h3"),
    labels = c(expression(g(d[1])), expression(h(d[1])),
               expression(g(d[2])), expression(h(d[2])),
               expression(g(d[3])), expression(h(d[3])))
  ) +
  scale_x_continuous(
    breaks = seq(min(rhos), max(rhos), length.out=length(rhos)-4),  
    labels = scales::label_number(accuracy = 0.01) 
  ) +
  labs(
    x = expression(rho),
    y = " ",
    title = bquote(mu == .(mu[3]))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = sz_ax),
    axis.title = element_text(size = sz_ax),
    plot.title = element_text(size = sz_ax + 2, face = "bold"), 
    legend.key.size = unit(1.5, "lines"), 
    legend.text = element_text(size = sz_ax - 1), 
    legend.title = element_text(size = sz_ax)
  )


#pdf("gh_index_serialdep.pdf",
    #height=8.27,width=11.69)
ggarrange(Pser_mu0,Pser_mu0.5,Pser_mu1,
          #Pmu0_scaled,Pmu0.5_scaled,Pmu1_scaled,
          ncol=3,nrow=1,common.legend = TRUE,
          legend="right")
#dev.off()

