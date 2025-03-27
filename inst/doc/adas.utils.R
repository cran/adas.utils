## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim=c(8, 4),
  out.width="100%"
)
library(adas.utils)
library(tidyverse)

## -----------------------------------------------------------------------------
(dm <- fp_design_matrix(2, rep=2) %>% 
  mutate(Y=rnorm(n())))

## -----------------------------------------------------------------------------
fp_design_matrix(~Speed*Weight)

## -----------------------------------------------------------------------------
fp_design_matrix(2) %>% 
  fp_add_names(A="Temperature", B="Pressure") %>%
  fp_add_scale(A=c(20, 25), B=c(75, 125), suffix=".scaled")

## -----------------------------------------------------------------------------
fp_design_matrix(2, levels=-1:1)

## -----------------------------------------------------------------------------
fp_design_matrix(3) %>%
  fp_augment_center(rep=4)

## -----------------------------------------------------------------------------
fp_design_matrix(3) %>% 
  fp_augment_center(rep=3) %>% 
  fp_augment_axial(rep=2)

## -----------------------------------------------------------------------------
fp <- fp_design_matrix(2, rep=3)

## -----------------------------------------------------------------------------
fp$Y <- ccd_experiment_yield$base

## -----------------------------------------------------------------------------
fp %>% 
  lm(Y ~ A*B, data=.) %>% 
  anova()

## -----------------------------------------------------------------------------
fpc <- fp %>% 
  fp_augment_center(rep=4)

fpc$Y[fpc$.treat == "center"] <- ccd_experiment_yield$center

## -----------------------------------------------------------------------------
fpc %>% 
  lm(Y ~ A*B+I(A^2), data=.) %>% 
  anova()

## -----------------------------------------------------------------------------
fpccd <- fpc %>% 
  fp_augment_axial(rep=2)

fpccd$Y[fpccd$.treat == "axial"] <- ccd_experiment_yield$axial

fpccd %>% 
  lm(Y ~ A*B*I(A^2)*I(B^2), data=.) %>% 
  anova()

## -----------------------------------------------------------------------------
fpccd %>% 
  lm(Y ~ A*B+I(A^2), data=.) %>% 
  summary()

## ----eval=FALSE---------------------------------------------------------------
# dm <-  fp_design_matrix(2) %>%
#   fp_add_names(A="Temperature", B="Pressure") %>%
#   fp_add_scale(A=c(2, 12), B=c(40, 60), suffix="_s") %>%
#   fp_write_csv("design_matrix.csv")

## ----eval=FALSE---------------------------------------------------------------
# dm <- dm %>%
#   fp_read_csv("design_matrix.csv")

## -----------------------------------------------------------------------------
fp_design_matrix(5) %>% 
  fp_fraction(~A*B*C*D) %>% 
  fp_fraction(~B*C*D*E)

## -----------------------------------------------------------------------------
fp_design_matrix(3) %>% 
  fp_fraction(~A*B*C, remove=FALSE)

## -----------------------------------------------------------------------------
(am <- fp_alias_matrix(~A*B*C, ~B*C*D))

## -----------------------------------------------------------------------------
am %>% plot()

## -----------------------------------------------------------------------------
am %>% as_tibble()

## ----warning=FALSE------------------------------------------------------------
df <- tibble(
  xn = rnorm(100, mean=20, sd=5),
  xu = runif(100, min=0, max=40)
)

df %>% normplot(xn)
df %>% normplot(xu)

## -----------------------------------------------------------------------------
set.seed(1)
tibble(
  val=rnorm(10, sd=5),
  cat=LETTERS[1:length(val)]
  ) %>%
  pareto_chart(labels=cat, values=val)

## -----------------------------------------------------------------------------
filtration %>% 
  lm(Y~A*B*C*D, data=.) %>%
  pareto_chart()

## -----------------------------------------------------------------------------
daniel_plot_qq(lm(Y~A*B*C*D, data=filtration))

## -----------------------------------------------------------------------------
filtration %>% 
  lm(Y~A*B*C*D, data=.) %>%
  daniel_plot_hn(nlab=6, repel=TRUE)


## -----------------------------------------------------------------------------
filtration %>% 
  lm(Y~A*C*D, data=.) %>%
  anova()

## -----------------------------------------------------------------------------
data <- examples_url("battery.dat") %>%
  read_table() %>%
  mutate(across(c(Temperature, Material), factor)) %>%
  mutate(Material = LETTERS[Material])

data.t <- data %>%
  filter(Material == "A") %>%
  aov(Response~Temperature, data=.) %>%
  TukeyHSD()

data.t

data.t %>% plot()

## -----------------------------------------------------------------------------
data.t %>%
  ggTukey()

## -----------------------------------------------------------------------------
data %>%
  filter(Material == "A") %>%
  ggTukey(Response~Temperature)

## -----------------------------------------------------------------------------
data %>% 
  ggTukey(Response~Temperature, splt=~Material)

## -----------------------------------------------------------------------------
examples_url("battery.dat") %>%  read.table(header=TRUE)

