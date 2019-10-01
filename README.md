# My-First-Repository

Hello my name is Shaila Jamal and this is my first repository for the course **Reproducible Research Workflow**. 

---
title: "Shaila Jamal Activity 1"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Research Interest

\begin{flushleft}
\hspace*{10mm}
My research area focuses on transportation and its association with different factors such as demography, built-environment, health, and technology. In my PhD, I will be studying the inter-generational differences in travel behavior and its impact in the long term, using Hamilton, Ontario as a case study. The study will focus on baby boomers (individuals born between 1945 and 1964), millennials (born approximately 1980- 1995) and generation Z (born approximately 1996 - 2005). 

\hspace*{10mm}
Each generation’s travel behavior, as well as transportation needs, are somewhat different than the other generations (McDonald 2015). Moreover, not all individuals within the same generation act homogenously (Lavieri et al. 2017). For example, some of the millennials marry early, live in suburban single-family houses, and drive alone for their commute, similar to the life courses observed amongst boomers (Circella et al. 2017). Heterogeneity in individuals’ travel behavior could exist because of socio-demographic status, economic conditions, place of residence, living arrangments, lifestyle choice, and attitudes towards transportation services and quality of life. Keeping these circumstances in mind, my PhD study will address the following research questions:

\hspace*{10mm}
- What are the differences in travel behavior within and between different generations (e.g. baby boomers, millennials and generation Z)? What are the influencing factors?

\hspace*{10mm}
- How is the travel behavior of each generation evolving? Will there be any significant transformation in future travel demand?

\end{flushleft}

# Favorites

## Favorite Music

I always listen to Bengali songs. Here is the list of my favorites:

1. _Amar ekla akash thomke geche_ by Shreya Ghosal

2. _Pori_ by Bappa Mazumdar.

3. _Kobita porar prohor eseche_ by Samina Chowdhury

4. _Valo achi valo theko_ by Somlata

5. _Tumi boruna holey hobo ami sunil_ by Mahadi.


## Favorite Equation


$$log\frac{\pi^{(r)}_i}{\pi^{(t)}_i} = (x_i\beta)^{(r)}, r = 1, ......, t-1      (1)$$



## Favorite Artists

\begin{center} Table 1: Famous Bangladeshi artists and their achievements \end{center}

**Name** | **Achievement**
-|-
1. Zainul Abedin | 1. Earned title _Shilpacharjo_ (great teacher of the arts)
2. Quamrul Hassan| 2. Independence Day Award
3. Shahabuddin Ahmed| 3. _Chevalier De L'ordre Des Arts Et Des Lettres_ (Knight in the Order of Fine Arts and Humanities) by the Ministry of Cultural Affair and Communication of France
4. S.M. Sultan| 4. Independence Day Award
5. Safiuddin Ahmed| 5. Independence Day Award


**<br><br>**

## A Chunk of Code 

**<br> <br>**

```{r, include=FALSE}

library(readxl)
library(car)
library(plyr)
library(MASS)

Pedestrian_Injury <- read_excel("Accident Data.xlsx")

```

```{r, echo=FALSE, message=FALSE, fig.align="center"}

DotPlot <- count(Pedestrian_Injury$INJURY)
DotPlot$percentage <- (DotPlot$freq/sum(DotPlot$freq))*100


dotchart(DotPlot$percentage,labels=DotPlot$x,pch = 19, cex= 0.8, cex.sub= 1.0,
         main="Pedestrian Injury Type", 
         xlab="Percentage", xlim = c(0,100))
```

\begin{center} Figure 1: Pedestrian Injury Type between 2007 - 2017 in Toronto \end{center}

**<br><br>**


```{r, echo=FALSE, message=FALSE, fig.align="center"}


Major <- Pedestrian_Injury[which(Pedestrian_Injury$INJURY == 'Major'), ]

Major_1 <- count(Major$INVAGE)

revalue(Major_1$x, c("0 to 4" = "0 to 19")) -> Major_1$x
revalue(Major_1$x, c("5 to 9" = "0 to 19")) -> Major_1$x
revalue(Major_1$x, c("10 to 14" = "0 to 19")) -> Major_1$x
revalue(Major_1$x, c("15 to 19" = "0 to 19")) -> Major_1$x
revalue(Major_1$x, c("20 to 24" = "20 to 44")) -> Major_1$x
revalue(Major_1$x, c("25 to 29" = "20 to 44")) -> Major_1$x
revalue(Major_1$x, c("30 to 34" = "20 to 44")) -> Major_1$x
revalue(Major_1$x, c("35 to 39" = "20 to 44")) -> Major_1$x
revalue(Major_1$x, c("40 to 44" = "20 to 44")) -> Major_1$x
revalue(Major_1$x, c("45 to 49" = "45 to 64")) -> Major_1$x
revalue(Major_1$x, c("50 to 54" = "45 to 64")) -> Major_1$x
revalue(Major_1$x, c("55 to 59" = "45 to 64")) -> Major_1$x
revalue(Major_1$x, c("60 to 64" = "45 to 64")) -> Major_1$x
revalue(Major_1$x, c("65 to 69" = "over 65")) -> Major_1$x
revalue(Major_1$x, c("70 to 74" = "over 65")) -> Major_1$x
revalue(Major_1$x, c("75 to 79" = "over 65")) -> Major_1$x
revalue(Major_1$x, c("80 to 84" = "over 65")) -> Major_1$x
revalue(Major_1$x, c("85 to 89" = "over 65")) -> Major_1$x
revalue(Major_1$x, c("90 to 94" = "over 65")) -> Major_1$x
revalue(Major_1$x, c("Over 95" = "over 65")) -> Major_1$x

Major_plot <- aggregate(Major_1$freq, by=list(Major_1$x), FUN= sum)


par(mar = c(5, 5, 5, 5))
barplot(Major_plot$x, names.arg = Major_plot$Group.1, 
        main= "Number of Major Injuries",mgp = c(2,0.5,0),
        ylab="Number", xlab = "Age Group", ylim = c(0,600),
        cex.lab = 1.0, cex.axis = 0.70,las = 1, cex.names = 0.80, cex.main = 0.8, cex.sub = 1.0)
```

\begin{center} Figure 2: Number of Major Injuries between 2007 - 2017 in Toronto by Age Groups \end{center}