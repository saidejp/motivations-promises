
# Promises and Social Closeness -------------------------------------------


# Packages ----------------------------------------------------------------

pacman::p_load(brms, tidyverse, bayesplot)


# Load data ---------------------------------------------------------------

load("data.RData")

# Creating factors --------------------------------------------------------

data <- data %>% 
  mutate(
    prom_keys = factor(prom_keys,
                       levels = c("None", "a", "b", "c", "d"), 
                       labels = c("No response", "Never", "Sometimes", 
                                  "Mostly", "Always")),
    decision = factor(decision, 
                      levels = c("b", "a"), 
                      labels = c("Not to pay back", "Pay back")),
    partner = factor(partner, 
                     levels = c("Computadora", "Desconocido", "Amigo"),
                     labels = c("Computer", "Confederate", "Friend")),
    promise = factor(promise, 
                     levels = c("SinPromesa", "Promesa"),
                     labels = c("No promise", "Promise")),
    dec_partner = factor(dec_partner, 
                         levels = c("NoConfio", "Confio"),
                         labels = c("No trust", "Trust")),
    type_trial = factor(type_trial,
                        levels = c("Promesa", "NoAplica", "SinPromesa"),
                        labels = c("Promise", "Not apply", "No Promise"))
  )



# Creating numeric outcomes -----------------------------------------------

data <- data %>% 
  mutate(
    prom_percent = case_when(
      prom_keys == "Never" | prom_keys == "No response" ~ 0,
      prom_keys == "Sometimes" ~ 33.33,
      prom_keys == "Mostly" ~ 66.66,
      prom_keys == "Always" ~ 100
    ),
    decision_gain = if_else(decision == "Pay back", 5, 10),
    response = if_else(decision == "Pay back", 1, 0) 
  )


# Creating trial number ---------------------------------------------------

data <- data %>% 
  mutate(
    trial = factor(rep(1:24, nlevels(data$id)), ordered = T)   
  ) %>% 
  select(id, trial, everything())

# Creating ordinal predictors ---------------------------------------------

d_ord <- data %>% 
  mutate(
    prom_ord = case_when(
      prom_keys == "No response" ~ 0,
      prom_keys == "Never" ~ 1,
      prom_keys == "Sometimes" ~ 2,
      prom_keys == "Mostly" ~ 3,
      prom_keys == "Always" ~ 4,
      is.na(prom_keys) & promise == "No promise" ~ 0
    ),
    part_ord = case_when(
      partner == "Computer" ~ 0,
      partner == "Confederate" ~ 1,
      partner == "Friend" ~ 2
    )
  ) %>% 
  fill(prom_ord) %>% 
  # filter(dec_partner == "Trust") %>% 
  mutate(not_pay = if_else(response == 1, 0, 1)) %>% 
  select(id, trial, promise, part_ord, prom_ord, dec_partner, response)



# Labeling ordinal predictors ---------------------------------------------

part_levels <- c("Computer", "Confederate", "Friend")
prom_levels <- c("No response", "Never", "Sometimes", "Mostly", "Always")

d_ord <- d_ord %>% 
  mutate(
    part_ord = factor(part_ord, 
                      levels = c(0, 1, 2),
                      labels = part_levels,
                      ordered = T),
    prom_ord = factor(prom_ord,
                      levels = c(0, 1, 2, 3, 4),
                      labels = prom_levels,
                      ordered = T)
  )


# Structure data ----------------------------------------------------------
str(data)


# Visualization -----------------------------------------------------------


# Theme -------------------------------------------------------------------

my_theme <- theme_bw(base_size = 15) + 
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank")) 


# Pay by promise level and partner ----------------------------------------


d_ord <- d_ord %>% 
  mutate(response = replace_na(response, replace = 1))


# Figure 2 ----------------------------------------------------------------

(p2 <- d_ord %>% 
   mutate(pay_back = ifelse(response == 1, "Pay back", "Not to pay back"),
          part_ord = fct_recode(part_ord, Stranger = "Confederate")) %>% 
   filter(dec_partner == "Trust") %>% 
   ggplot(aes(x = prom_ord, fill = pay_back)) +
   geom_bar(position = "fill") + 
   labs(fill = NULL,
        y = "Percent",
        x = NULL) +
   facet_wrap(~part_ord) +
   scale_fill_manual(values = c("#A4CAE3", "#053778")) +
   my_theme +
   coord_flip() +
   theme(legend.position = "top", 
         axis.text.x = element_text(size = 10))
 )

# Modeling ----------------------------------------------------------------

# Analyzing broken promises -----------------------------------------------

broken <- d_ord %>% 
  filter(prom_ord == "Always", dec_partner == "Trust") %>% 
  mutate(broken = if_else(response == 1, 0, 1) %>% 
           as.integer(),
         part_ord = factor(part_ord, ordered = F))


broken %>% 
  group_by(part_ord) %>% 
  summarise(mean = mean(response),
            sd = sd(response),
            sum = sum(response),
            n = n(),
            broken = n - sum)


# Modeling Broken Promises ------------------------------------------------

# Varying effects of social closeness in breaking the promise by subject ----


prior2 <- c(set_prior("normal(0, 2)", class = "b"), 
            set_prior("cauchy(0, 2)", class = "sd"),
            set_prior("lkj(2)", class = "cor"))


fit2 <- brm(broken ~ -1 + part_ord + (-1 + part_ord | id),
            data = broken,
            family = bernoulli("logit"),
            prior = prior2,
            sample_prior = "yes",
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95))

fit3 <- brm(broken ~ part_ord + (part_ord | id),
            data = broken,
            family = bernoulli("logit"),
            prior = prior2,
            sample_prior = "yes",
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95))

# BF for the effect of social closeness -----------------------------------

hypothesis(fit2, "part_ordComputer - ((part_ordConfederate + part_ordFriend) / 2) > 0")
hypothesis(fit2, "part_ordConfederate - part_ordFriend > 0")



# Estimates in probability scale ------------------------------------------

samples_fit2 <- posterior_samples(fit2)[, 1:3] %>% 
  mutate_all(inv_logit_scaled) 

names(samples_fit2) <- c("Computer", "Stranger", "Friend")


# Figure 3  ---------------------------------------------------------------

(p3 <-  bayesplot::mcmc_intervals(
  samples_fit2,
  prob_outer = 0.97
) +
  labs(x = "Probability of a Broken Promise") +
  geom_vline(xintercept = 0, lty = 2, col = "gray") +
  my_theme)


# Effects of social closeness and promises in cooperation -----------------

cases <- data %>% 
  filter(dec_partner == "Trust") %>% 
  select(id, trial, partner, promise, response) %>% 
  mutate(response = replace_na(response, replace = 1))


# Priors ------------------------------------------------------------------

prior <- c(set_prior("normal(0, 5)", class = "b"),
           set_prior("cauchy(0, 2)", class = "sd"),
           set_prior("lkj(2)", class = "cor"))



# Modeling cooperation ----------------------------------------------------

# Varying effects of promises and partner by subject and trial ------------


(m <- brm(response ~ promise + partner + (promise + partner | id + trial), 
          data = cases,
          family = bernoulli("logit"), 
          prior = prior,
          sample_prior = "yes",
          chains = 4, 
          cores = 4,
          control = list(adapt_delta = 0.99)))


# Hypothesis testing ------------------------------------------------------

hypothesis(m, "promisePromise > 0", class = "b")
hypothesis(m, "partnerFriend - partnerConfederate > 0", class = "b") 


# Posterior Samples -------------------------------------------------------

posteriors <- posterior_samples(m)


# Beta posteriors ---------------------------------------------------------

b_posteriors <- select(posteriors, 1:4)
names(b_posteriors) <- c("Intercept", "Promise", "Stranger", "Friend")


# Figure 4 ----------------------------------------------------------------

(p4 <- bayesplot::mcmc_areas(
  b_posteriors,
  point_est = "mean") +
    geom_vline(xintercept = 0, 
               col = "black", 
               lty = 2, 
               alpha = 0.2) +
    my_theme)

# Posterior predictive ----------------------------------------------------

pp <- posterior_predict(m, nsamples = 100) %>% 
  as.tibble()

pp_tidy <- pp %>% 
  gather(key = "row_cases",value = "prediction") %>% 
  mutate(row = gl(ncol(pp), k = 100) %>% as.numeric()) %>% 
  select(row, -row_cases, prediction)

pp_cases <- cases %>% 
  mutate(row = 1:nrow(cases)) %>% 
  select(row, everything()) %>% 
  right_join(pp_tidy, by = "row")


# Figure 5 ----------------------------------------------------------------

pp_cases <- pp_cases %>% 
  mutate(
    treatment = case_when(
      promise == "No promise" & partner == "Computer" ~ "NP/Comp",
      promise == "No promise" & partner == "Confederate" ~ "NP/Stran",
      promise == "No promise" & partner == "Friend" ~ "NP/Frie",
      promise == "Promise" & partner == "Computer" ~ "P/Comp",
      promise == "Promise" & partner == "Confederate" ~ "P/Stran",
      promise == "Promise" & partner == "Friend" ~ "P/Frie"
    ) %>% factor(levels = c("NP/Comp", "NP/Stran", "NP/Frie", "P/Comp", "P/Stran", "P/Frie"))
  )

d_g9 <- pp_cases %>% 
  filter(id %in% 1:12) %>% 
  group_by(id, promise, treatment) %>% 
  summarise(Predicted = mean(prediction),
            Response = mean(response),
            pred_sd = sd(prediction)) %>%
  filter(promise != "No promise") %>% 
  gather(key = "case", value = "rate", -id, -promise, -treatment, -pred_sd) %>% 
  ungroup()

d_g9 <- d_g9 %>% 
  mutate(pred_sd = if_else(case == "Response", 0, pred_sd)) 

(p5 <- d_g9 %>% 
    ggplot(aes(x = treatment, y = rate, col = case)) +
    geom_hline(yintercept = 0.5, lty = 2, col = "gray") +
    geom_line(aes(group = case), show.legend = F) +
    geom_pointrange(aes(ymax = rate + pred_sd, ymin = rate - pred_sd),
                    shape = 21, fill = "white") +
    scale_color_manual(values = c("#053778", "#A4CAE3")) +
    facet_wrap(~id) +
    my_theme +
    theme(axis.text.x = element_text(angle = 75, hjust = 1), 
          legend.position = "top") +
    labs(x = "Treatment", y = NULL, col = NULL))



# Varying effects of social closeness by promise level --------------------

data3 <- d_ord %>% 
  filter(dec_partner == "Trust") %>% 
  mutate(prom_ord = factor(prom_ord, ordered = F),
         part_ord = factor(part_ord, ordered = F),
         response = as.integer(response))


fit <- brm(response ~ part_ord + (part_ord | prom_ord),
           data = data3,
           family = bernoulli("logit"),
           prior = prior2,
           sample_prior = "yes",
           chains = 4,
           cores = 4,
           control = list(adapt_delta = 0.999,
                          max_treedepth = 15))


# Hypothesis --------------------------------------------------------------

hypothesis(fit, "part_ordFriend - part_ordConfederate > 0")
hypothesis(fit, "part_ordConfederate > 0")



# Posterior Predictive ----------------------------------------------------

pp_check(fit, type = "bars_grouped", group = "prom_ord", nsamples = 30)


pp_2 <- posterior_predict(fit, nsamples = 100) %>% 
  as_tibble()

pp_tidy_2 <- pp_2 %>% 
  gather(key = "row_cases",value = "prediction") %>% 
  mutate(row = gl(ncol(pp_2), k = 100) %>% as.numeric()) %>% 
  select(row, -row_cases, prediction)

pp_cases_2 <- data3 %>% 
  mutate(row = 1:nrow(data3)) %>% 
  select(row, everything()) %>% 
  right_join(pp_tidy_2, by = "row")



# Figure 6 ----------------------------------------------------------------

(d_g13 <- pp_cases_2 %>% 
    group_by(part_ord, prom_ord) %>% 
    summarise(Predicted = mean(prediction),
              Response = mean(response),
              pred_sd = sd(prediction)) %>% 
    gather(key = "case", value = "rate", -part_ord, -prom_ord, -pred_sd) %>% 
    ungroup() %>% 
    mutate(pred_sd = ifelse(case == "Response", 0, pred_sd),
           part_ord = fct_recode(part_ord, Stranger = "Confederate")))


(p6 <- ggplot(d_g13, aes(x = part_ord, y = rate, col = case))  +
    geom_hline(yintercept = 0.5, lty = 2, col = "gray") +
    geom_pointrange(aes(ymax = rate + pred_sd, ymin = rate - pred_sd),
                    position = position_dodge(width = .5)) +
    facet_wrap(~prom_ord) +
    scale_color_manual(values = c("#053778", "#A4CAE3")) +
    labs(x = NULL,
         y = "Rate",
         col = NULL) +
    my_theme +
    theme(legend.position = c(0.83, 0.25), 
          axis.text.x = element_text(size = 10)))


# Hierarchical Clustering -------------------------------------------------

perc <- cases %>% 
  group_by(id) %>% 
  summarise(perc = mean(response)) 

x <- cases %>% 
  group_by(id) %>% 
  summarise(perc = mean(response)) %>% 
  pull(perc)

id <- cases %>% 
  group_by(id) %>% 
  summarise(perc = mean(response)) %>% 
  pull(id)

clust <- hclust(dist(x), method = "ward.D", members = id)
plot(clust)



# Two groups with different rates of cooperation --------------------------

low <- c(36, 31, 16, 22, 12, 27, 32, 42, 29, 26, 24, 18, 11, 2, 5, 38, 28, 23, 1, 8, 44, 37,
          33, 4, 19)

high <- 1:45
high <- high[!high %in% low]

data4 <- data3 %>% 
  mutate(group = ifelse(id %in% high, "High", "Low") %>% 
           factor(levels = c("High", "Low"))) %>% 
  mutate(prom_ord = factor(prom_ord, ordered = T))

data4 %>% 
  group_by(group) %>% 
  summarise(mean = mean(response),
            sd = sd(response),
            sum = sum(response),
            n = n())

prop.test(c(259, 300), n = c(450, 360))



# Modeling commitment expresed by promises --------------------------------


data5 <- data4 %>% 
  filter(prom_ord != "No response") %>% 
  mutate(prom_ord = fct_drop(prom_ord),
         group = factor(group, levels = c("Low", "High")))

fit4 <- brm(prom_ord ~ group + (1 | id + trial), 
            data = data5,
            family = cumulative("probit"),
            sample_prior = "yes",
            chains = 4,
            cores = 4,
            prior = set_prior("normal(0, 2)", class = "b"),
            control = list(adapt_delta = 0.99, 
                           max_treedepth = 15))

plt <-  plot(marginal_effects(fit4, categorical = T))


# Figure 7 ----------------------------------------------------------------

(p7 <- plt$`group:cats__` +
  labs(x = NULL,
       col = "Promise",
       fill = "Promise") +
  scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Blues")[c(3, 5, 7, 9)], 
                    aesthetics = "col") +
  my_theme)
