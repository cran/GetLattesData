## ------------------------------------------------------------------------
library(GetLattesData)

# ids from EA-UFRGS
my.ids <- c('K4713546D3', 'K4440252H7', 
            'K4783858A0', 'K4723925J2')

# qualis for the field of management
field.qualis = 'ADMINISTRAÇÃO PÚBLICA E DE EMPRESAS, CIÊNCIAS CONTÁBEIS E TURISMO'

l.out <- gld_get_lattes_data(id.vec = my.ids, field.qualis = field.qualis)

## ------------------------------------------------------------------------
names(l.out)

## ------------------------------------------------------------------------
tpesq <- l.out$tpesq
str(tpesq)

## ------------------------------------------------------------------------
tpublic <- l.out$tpublic
str(tpublic)

## ------------------------------------------------------------------------
tsupervisions <- l.out$tsupervisions
str(tsupervisions)

## ------------------------------------------------------------------------
library(ggplot2)

p <- ggplot(tpublic, aes(x = qualis)) +
  geom_bar(position = 'identity') + facet_wrap(~name) +
  labs(x = paste0('Qualis: ', field.qualis))
print(p)

## ------------------------------------------------------------------------
library(dplyr)

my.tab <- tpublic %>%
  group_by(name) %>%
  summarise(n.papers = n(),
            max.SJR = max(SJR, na.rm = T),
            mean.SJR = mean(SJR, na.rm = T),
            n.A1.qualis = sum(qualis == 'A1', na.rm = T),
            n.A2.qualis = sum(qualis == 'A2', na.rm = T),
            median.authorship = median(as.numeric(order.aut), na.rm = T ))

knitr::kable(my.tab)

