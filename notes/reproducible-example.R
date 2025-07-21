# full example
library(tidyverse)

titanic <- read_csv("https://wilkelab.org/DSC385/datasets/titanic.csv") |>
  select(age, sex, class, survived)

ggplot(titanic, aes(x = age, fill = sex)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~survived) +
  scale_x_continuous(
    name = "Age (years)",
    breaks = c(0, 25, 50, 75),
    limits = c(0, 80),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_continuous(
    name = "Density estimate",
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_color_manual(
    values = c("#D55E00", "#0072B2")
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  

# minimal example
library(tidyverse)

titanic <- read_csv("https://wilkelab.org/DSC385/datasets/titanic.csv") |>
  select(age, sex, class, survived)

ggplot(titanic, aes(x = age, fill = sex)) +
  geom_density(alpha = 0.7) +
  scale_color_manual(
    values = c("#D55E00", "#0072B2")
  )


# even more minimal, use built-in dataset
library(tidyverse)

ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(
    values = c("#D55E00", "#0072B2", "#CC79A7")
  )


# made-up dataset
library(tidyverse)

data <- tibble(
  age = c(rnorm(50), rnorm(50) + 0.5),
  sex = rep(c("male", "female"), each = 50)
)

ggplot(data, aes(x = age, fill = sex)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(
    values = c("#D55E00", "#0072B2")
  )
