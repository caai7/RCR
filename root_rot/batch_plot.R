library(readxl)
library(dplyr)
library(ggplot2)
library(ggtext)
library(forcats) 
library(patchwork)

b1 <- read_excel("D:/LAB/RCR/rootrot/2026_b1.xlsx")
b1$DI <- as.numeric(b1$DI)
b1$Line <- as.factor(b1$Line)
fit1=lm(b1$DI ~ Line,data=b1)
names(fit1)
r1=fit1$residuals
r1

###TEST###
shapiro.test(r1)
qqnorm(r1,pch=1,ylab="Normal % probability", xlab="Residual")
qqline(r1)

##2 independent assumption -->scatter plot
plot(fit1$fitted.values,r1,pch=19,main="Scatter Plot",
     ylab="residuals",xlab="fitted.values")
abline(0,0)

##3 equal variance assumption
#H0:變方同質 vs. H1:變方異質
bartlett.test(b1$DI ~ Line,data=b1)
library(car)
leveneTest(b1$DI ~ Line,data=b1)
###########

b1 <- b1 %>%
  mutate(highlight = ifelse(Line %in% c("TN5", "KH9", "W82", "C4"), "highlight", "normal"))

batch1 <- ggplot(b1, aes(x = fct_reorder(Line, -DI, .fun = mean), y = DI ,fill = highlight)) +
  geom_boxplot(aes(fill = highlight)) +
  labs(y = "Line", x = "Disease Index") +
  scale_fill_manual(values = c("highlight" = "#F2DD78FF", "normal" = "#D0D3A2FF")) +
  geom_dotplot(aes(color = highlight), binaxis='y', stackdir='center', dotsize=.5, stroke = 2.5) +
  scale_color_manual(values = c("highlight" = "#B88244FF", "normal" = "#225F2FFF")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

batch1

ggsave("D:/LAB/RCR/rootrot/2026_b1.png", batch1,
       width = 15, height = 6, dpi = 1500)