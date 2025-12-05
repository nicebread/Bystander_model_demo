# Simulate virtual experiments from the model.
# ---------------------------------------------------------------------------
# We assume that the model is the true data generating model.
# As it is a deterministic model, each virtual participant
# will have the same response. Variability comes into the experiment by:
# (a) Different starting values for the baseResp
# (b) Randomness when the actual helping behavior is only
#     shown with a certain probability.

# You can run the script repeatedly to simulate new replication studies,
# and observe the variability in outcomes due to sampling variability.

library(ggplot2)
library(dplyr)

# load the model functions
source("simulation/01-functions.R")

# set the sample size per experimental group (we assume a between-person design)
n_per_group <- 20

# experimental design
df <- expand.grid(
  id = 1:n_per_group,
  NOPB = c(0, 1, 4),
  DoI = c(0.2, 0.8)
)

# interindividual variability in exogenous variables ("personality")
df$baseResp = rbeta(nrow(df), shape1=13, shape2=4)

# compute the outcome variable
df$helpingBeh <- psi(NOPB = df$NOPB, DoI = df$DoI, baseResp = df$baseResp)$helpingBeh

# look at the simulated data
# View(df)

# show a summary of the data
table(help=df$helpingBeh, NOPB=df$NOPB, DoI=df$DoI) |> prop.table(margin=c("DoI", "NOPB")) |> round(2)

# visualize it
ggplot(df, aes(x = factor(NOPB), y = helpingBeh, fill = factor(DoI))) +
  stat_summary(
    fun = mean, geom = "bar",
    position = position_dodge(width = 0.9)
  ) +
  labs(x = "Number of passive bystanders", y = "p(help)", fill="Dangerousness\nof intervention") +
  theme_minimal()


# statistical analysis of the data: Do we find the expected 2-way interaction?
# (this would be the focal test for the hypothesis the "increasing dangerousness diminishes the bystander effect)
# The main effect for NOPB should be negative:
# more bystanders lead to less helping behavior (also for high levels of dangerousness)
##
# The interaction effect should be positive:
# With increasing DoI, the NOPD-effect should get less negative (i.e., more positive)
#------------------------------------------------

t1 <- glm(helpingBeh ~ NOPB * DoI, data = df, family = binomial)
summary(t1)

