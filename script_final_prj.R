
# PART 1 - DATA PREPARATION
library(tidyverse)
library(haven)

pre <- read_dta("~/Desktop/multivariate_project1/final project/Itanes2022_PRE_CORE_Itanes_release 02_weighted.dta") %>%
  select(seriale, immigrazione = D03_3)
post <- read_dta("~/Desktop/multivariate_project1/final project/Itanes2022_POST_release 02_weighted.dta")

data <- post %>%
  left_join(pre, by = "seriale")

data <- data %>%
  mutate(
    denomination_R = case_when(
      D10 == 1 ~ 1,
      D10 %in% c(6, 7) ~ 2,
      D10 %in% c(2, 3, 4, 5, 8, 99) ~ NA_real_
    ), 
    catholic = ifelse(denomination_R == 1, 1, ifelse(is.na(denomination_R), NA, 0)),
    attendance_R = case_when(
      D11 == 1 ~ 1,
      D11 %in% c(2, 3) ~ 2,
      D11 %in% 4:6 ~ 3,
      D11 %in% c(98, 99) ~ NA_real_
    ),
    attendance_RR = case_when(
      D11 == 1 ~ 1,
      D11 %in% c(2, 3) ~ 2,
      D11 == 4 ~ 3,
      D11 == 5 ~ 4,
      D11 == 6 ~ 5,
      D11 %in% c(98, 99) ~ NA_real_
    ),
    tipology = case_when(
      denomination_R == 1 & attendance_R == 3 ~ 1,
      denomination_R == 1 & attendance_R %in% c(1, 2) ~ 2,
      denomination_R == 2 & attendance_R %in% c(1, 2) ~ 3
    ),
    tipology2 = case_when(
      denomination_R == 1 & attendance_RR == 5 ~ 1,
      denomination_R == 1 & attendance_RR == 4 ~ 2,
      denomination_R == 1 & attendance_RR == 3 ~ 3,
      denomination_R == 1 & attendance_RR %in% c(1, 2) ~ 4,
      denomination_R == 2 & attendance_RR %in% c(1, 2) ~ 5
    ),
    edu = case_when(
      scol %in% 1:2 ~ "Primary",
      scol %in% 3:5 ~ "Secondary",
      scol %in% 6:11 ~ "Tertiary"
    ),
    gender = case_when(
      D01 == 1 ~ "Male",
      D01 == 2 ~ "Female",
      D01 %in% 3:99 ~ NA_character_
    ),
    income = ifelse(D09 %in% c(98, 99), NA, D09),
    vote6 = case_when(
      Q10LHb == 1 ~ "FdI",
      Q10LHb == 2 ~ "Lega",
      Q10LHb == 3 ~ "FI",
      Q10LHb == 4 ~ "Azione+IV",
      Q10LHb == 5 ~ "M5S",
      Q10LHb == 6 ~ "PD",
      Q10LHb %in% 7:99 ~ NA_character_
    ),
    voteCDX = case_when(
      Q10LHb %in% c(1, 2, 3, 8, 11) ~ 1,
      Q10LHb %in% c(4, 5, 6, 7, 9, 10, 12) ~ 0,
      TRUE ~ NA_real_
    ),
    voteCDX_noFI = case_when(
      Q10LHb %in% c(1, 2, 8) ~ 1,
      Q10LHb %in% c(3, 4, 5, 6, 7, 9, 10, 11, 12) ~ 0,
      TRUE ~ NA_real_
    ),
    Area_Geo = case_when(
      regione %in% 1:7 ~ "North",
      regione %in% 8:11 ~ "Red regions",
      regione %in% 12:14 ~ "Center",
      regione %in% 15:20 ~ "South/Islands"
    ),
    age = eta_classe,
    abortoOK = ifelse(ITA01_03 %in% c(1, 2), 1, ifelse(ITA01_03 == 98, NA, 0)),
    omoOK = ifelse(ITA01_05 %in% c(3, 4), 1, ifelse(ITA01_05 == 98, NA, 0)),
    immOK = ifelse(immigrazione %in% c(4, 5, 6, 7), 1, ifelse(immigrazione == 98, NA, 0)),
    abortoNO = ifelse(ITA01_03 %in% c(3, 4), 1, ifelse(ITA01_03 == 98, NA, 0)),
    omoNO = ifelse(ITA01_05 %in% c(1, 2), 1, ifelse(ITA01_05 == 98, NA, 0)),
    immNO = ifelse(immigrazione %in% c(1, 2, 3), 1, ifelse(immigrazione == 98, NA, 0))
  )



# PART 2 - DESCRIPTIVES / PLOTS

library(ggplot2)
library(scales)

data <- data %>%
  mutate(
    tipology_label = case_when(
      tipology == 1 ~ "Practicing Catholic",
      tipology == 2 ~ "Nominal Catholic",
      tipology == 3 ~ "Not religious",
      TRUE ~ NA_character_
    )
  )

#Plot 1: Typology within vote6
plot1_data <- data %>%
  filter(!is.na(vote6), !is.na(tipology_label)) %>%
  count(vote6, tipology_label) %>%
  group_by(vote6) %>%
  mutate(percent = n / sum(n) * 100)

ggplot(plot1_data, aes(x = vote6, y = percent, fill = tipology_label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(percent, 1)), position = position_stack(vjust = 0.5)) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Greys") +
  theme_minimal() +
  labs(title = "Typology distribution within each party",
       x = "Vote choice", y = "Percentage", fill = "Typology")

#Plot 2: Vote6 within typology
plot2_data <- data %>%
  filter(!is.na(vote6), !is.na(tipology_label)) %>%
  count(tipology_label, vote6) %>%
  group_by(tipology_label) %>%
  mutate(percent = n / sum(n) * 100)

ggplot(plot2_data, aes(x = tipology_label, y = percent, fill = vote6)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(percent, 1)), position = position_stack(vjust = 0.5)) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Greys") +
  theme_minimal() +
  labs(title = "Vote distribution within religious typologies",
       x = "Typology", y = "Percentage", fill = "Vote choice")


# PART 3 - MODELS

# MULTINOMIAL LOGIT

library(modelsummary)
library(mediation)
library(nnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(scales)

#Most frequent value
Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
# Filter and prepare data for multinomial model
data_model <- data %>%
  filter(
    !is.na(vote6),
    !is.na(gender),
    !is.na(edu),
    !is.na(age),
    !is.na(tipology),
    !is.na(income),
    !is.na(Area_Geo)
  ) %>%
  mutate(
    vote6 = factor(vote6,
                   levels = c("FdI", "Lega", "FI", "Azione+IV", "M5S", "PD")),
    tipology = factor(tipology,
                      levels = c(1, 2, 3),
                      labels = c("Practicing Catholic", "Nominal Catholic", "Not religious"))
  )

#Create newdata grid for predictions
newdata <- expand.grid(
  tipology = levels(data_model$tipology),
  gender = Mode(data_model$gender),
  edu = Mode(data_model$edu),
  age = mean(data_model$age, na.rm = TRUE),
  income = mean(data_model$income, na.rm = TRUE),
  Area_Geo = Mode(data_model$Area_Geo)
)

# Bootstrap prediction function
bootstrap_pred <- function(data, newdata) {
  sampled_data <- data[sample(1:nrow(data), replace = TRUE), ]
  model <- multinom(vote6 ~ gender + edu + age + tipology + income + Area_Geo,
                    data = sampled_data, trace = FALSE)
  probs <- predict(model, newdata = newdata, type = "probs")
  
  probs_df <- as.data.frame(probs)
  colnames(probs_df) <- levels(data_model$vote6)
  probs_df$tipology <- newdata$tipology
  pivot_longer(probs_df, -tipology, names_to = "Party", values_to = "prob")
}

# Run bootstrap
set.seed(123)
n_boot <- 1000
bootstrap_results <- map_dfr(1:n_boot, ~bootstrap_pred(data_model, newdata), .id = "iter")

#summarize with confidence intervals
ci_summary <- bootstrap_results %>%
  group_by(tipology, Party) %>%
  summarise(
    predicted = mean(prob),
    conf.low = quantile(prob, 0.025),
    conf.high = quantile(prob, 0.975),
    .groups = "drop"
  )

#plot predicted vote probabilities by typology
ggplot(ci_summary, aes(x = tipology, y = predicted, color = Party, fill = Party, group = Party)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_point(size = 2) +
  labs(
    title = "Predicted Vote Probabilities by Religious Typology",
    x = "Religious Typology",
    y = "Predicted Probability",
    color = "Party",
    fill = "Party"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


#LOGIT MODELS FOR VOTECDX


model_logit_M1 <- glm(voteCDX ~ attendance_R + gender + edu + age + income + Area_Geo,
                      family = binomial, data = data)

model_logit_M2 <- glm(voteCDX ~ attendance_R + denomination_R + gender + edu + age + income + Area_Geo,
                      family = binomial, data = data)

model_logit_M3 <- glm(voteCDX ~ attendance_R * denomination_R + gender + edu + age + income + Area_Geo,
                      family = binomial, data = data)


modelsummary(
  list(M1 = model_logit_M1, M2 = model_logit_M2, M3 = model_logit_M3),
  output = "Table_2.docx", stars = TRUE
)


# MEDIATION ANALYSIS

data <- data %>%
  mutate(abortoNO_strict = ifelse(ITA01_03 == 4, 1, ifelse(ITA01_03 == 98, NA, 0)))

data_aborto_strict <- data %>%
  filter(
    !is.na(abortoNO_strict),
    !is.na(catholic),
    !is.na(voteCDX),
    !is.na(gender),
    !is.na(edu),
    !is.na(age),
    !is.na(income),
    !is.na(Area_Geo)
  )

model.m_aborto_strict <- glm(abortoNO_strict ~ catholic + gender + edu + age + income + Area_Geo,
                             family = binomial, data = data_aborto_strict)

model.y_aborto_strict <- glm(voteCDX ~ abortoNO_strict + catholic + gender + edu + age + income + Area_Geo,
                             family = binomial, data = data_aborto_strict)

med_aborto_strict <- mediate(model.m_aborto_strict, model.y_aborto_strict,
                             treat = "catholic", mediator = "abortoNO_strict", boot = TRUE)

summary(med_aborto_strict)


#omosexual-marriage

data_omo_strict <- data %>%
  filter(
    !is.na(omoNO),
    !is.na(catholic),
    !is.na(voteCDX),
    !is.na(gender),
    !is.na(edu),
    !is.na(age),
    !is.na(income),
    !is.na(Area_Geo)
  )


model.m_omo <- glm(omoNO ~ catholic + gender + edu + age + income + Area_Geo,
                   family = binomial, data = data_omo_strict)

model.y_omo <- glm(voteCDX ~ omoNO + catholic + gender + edu + age + income + Area_Geo,
                   family = binomial, data = data_omo_strict)


med_omo_strict <- mediate(model.m_omo, model.y_omo,
                          treat = "catholic", mediator = "omoNO", boot = TRUE)

summary(med_omo_strict)


#immigration

data_imm_strict <- data %>%
  filter(
    !is.na(immNO),
    !is.na(catholic),
    !is.na(voteCDX),
    !is.na(gender),
    !is.na(edu),
    !is.na(age),
    !is.na(income),
    !is.na(Area_Geo)
  )


model.m_imm <- glm(immNO ~ catholic + gender + edu + age + income + Area_Geo,
                   family = binomial, data = data_imm_strict)

model.y_imm <- glm(voteCDX ~ immNO + catholic + gender + edu + age + income + Area_Geo,
                   family = binomial, data = data_imm_strict)


med_imm_strict <- mediate(model.m_imm, model.y_imm,
                          treat = "catholic", mediator = "immNO", boot = TRUE)

summary(med_imm_strict)



# PART 4 - MODEL EXTENSIONS 

# part A: Interaction Effect: Catholic Identity × Age

data$catholic <- factor(data$catholic, levels = c(0, 1),
                        labels = c("Non-Catholic", "Catholic"))
# Fit interaction model
model_interact <- glm(voteCDX ~ catholic * age + gender + edu + income + Area_Geo,
                      family = binomial, data = data)

# Generate predictions using ggeffects
library(ggeffects)
pred_interact <- ggpredict(model_interact, terms = c("age", "catholic"))

# Plot predicted probabilities
library(ggplot2)
ggplot(pred_interact, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "Interaction Effect: Catholic Identity × Age on Right-Wing Voting",
    x = "Age",
    y = "Predicted Probability",
    color = "Religious Identity",
    fill = "Religious Identity"
  ) +
  theme_minimal(base_size = 13)



# part B: index of cultural conservatorism

data <- data %>%
  mutate(
    conserv_score = abortoNO + omoNO + immNO
  )


table(data$conserv_score, useNA = "ifany")


# 2. REGRESSIONE POISSON – cultural conservatism ~ religious typology

library(ggeffects)

data_cons <- data %>%
  filter(
    !is.na(conserv_score),
    !is.na(tipology),
    !is.na(gender),
    !is.na(age),
    !is.na(edu),
    !is.na(income),
    !is.na(Area_Geo)
  )

model_conserv <- glm(conserv_score ~ tipology + gender + age + edu + income + Area_Geo,
                     family = poisson(link = "log"),
                     data = data_cons)


summary(model_conserv)

pred_cons <- ggpredict(model_conserv, terms = "tipology")


pred_cons$x <- factor(pred_cons$x,
                      levels = c("1", "2", "3"),
                      labels = c("Practicing Catholic", "Nominal Catholic", "Not Religious"))

# Plot 
ggplot(pred_cons, aes(x = x, y = predicted, fill = x)) +
  geom_col(color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_y_continuous(
    limits = c(0, 3),
    breaks = 0:3,
    labels = c(
      "Progressive on 
      all issues",
      "Conservative 
      on 1 issue",
      "Conservative 
      on 2 issues",
      "Conservative 
      on all issues"
    )
  ) +
  labs(
    title = "Predicted Conservatism Score by Religious Typology",
    x = "",
    y = NULL,
    fill = "Typology"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")






