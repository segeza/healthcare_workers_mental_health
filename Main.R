# **************************
# Script Name; Main
# Script Purpose; R practice with a mental health for HCW dataset
# Script Author; Jofrey Segeza Amos
# Last edited; 23-02-2025
# *************************

#Loading packages

pacman::p_load(
     tidyverse,
     readxl,
     dplyr,
     gtsummary,
     lubridate,
     epikit,
     here,
     flextable,
     janitor,
     skimr,
     rio,
     janitor,
     readr,
     base,
     glm2,
     plotly
)

#Load data and renaming variables

hcw_main <- read_csv("data/Healthcare Workforce Mental Health Dataset.csv")
skim(hcw_main)
view(hcw_main)

hcw_raw <- hcw_main %>% 
     rename('employee_id' = 'Employee ID', 'employee_type' = 'Employee Type',
            'department' = 'Department', 'workplace_factor' = 'Workplace Factor',
            'stress_level' = 'Stress Level', 'burnout_frequency' = 'Burnout Frequency',
            'job_satisfaction' = 'Job Satisfaction', 'eap_access' = 'Access to EAPs',
            'mh_absence' = 'Mental Health Absences', 'turnover' = 'Turnover Intention'
            )

# Exploration and Visual assessment

# exploring character variables
skim(hcw_raw)                            # No missing information
unique(hcw_raw$employee_type)            # Types of employees
unique(hcw_raw$department)               # Worker's departments
unique(hcw_raw$workplace_factor)         #workplace factors leading to stress

# Visualization of key character variables
figure01 <- hcw_raw %>% 
     select(employee_type, workplace_factor, department) %>% 
     ggplot(aes(employee_type))+
     geom_bar()+
     facet_wrap(vars(department))+
     coord_flip()

hcw_raw %>% 
     select(job_satisfaction) %>% 
     ggplot(aes(job_satisfaction))+
     geom_histogram()+
     stat_bin(binwidth = 1)

hcw_raw %>% 
     select(stress_level, job_satisfaction) %>% 
     ggplot(aes(x = stress_level, y = job_satisfaction))+
     geom_point()+
     geom_line()

# Plotting for relationship between stress level and mental health leave
figure02 <- hcw_raw %>% 
     select(stress_level, mh_absence) %>% 
     ggplot(aes( x= stress_level, y= mh_absence))+
     geom_point()+
     geom_smooth()

# Plotting for relationship between employee type and workplace factorcausing mh

table1 <- hcw_raw %>% 
     select(employee_type, workplace_factor) %>%
     tbl_summary(digits = list(all_categorical() ~ c(0, 2)),
                 label = list(employee_type ~ "Type of employee",
                              workplace_factor ~ "Workplace stress factor")) %>%
     bold_labels() %>% 
     as_flex_table()

print(table01)
save_as_docx(table01, path = 'output/table01.docx')

hcw_raw %>% 
     select(employee_type) %>% 
     ggplot(aes(employee_type, fill = "red"))+
     geom_bar()+
     coord_flip()

# Visualizing distribution of healthcare workers 
hcw_raw %>% 
     select(department, employee_type) %>% 
     ggplot(aes(employee_type, fill = department))+
     geom_bar()+
     coord_flip()

hcw_raw %>% 
     select(department, workplace_factor) %>% 
     ggplot(aes(workplace_factor))+
     geom_bar()+
     coord_flip()+
     facet_wrap(vars(department))


# Categorizing stress level and job satisfaction 
     # 0 -3 ; low, 4 - 6; moderate, 7 -5 ; high.
     # mh_absence, 0-7 = not significant, > 7 = significant

hcw_raw_01 <- hcw_raw %>% 
     mutate(stress = case_when(stress_level < 3 ~ 'Low',
                               stress_level < 6 ~ 'Moderate',
                               stress_level < 10 ~ 'High'),
            satisfaction = case_when(job_satisfaction < 3 ~ 'Low',
                                     job_satisfaction < 6 ~ 'moderate',
                                     job_satisfaction < 10 ~ 'High'),
            sign_absence = case_when(mh_absence < 7 ~ 'not_significant',
                                     mh_absence > 7 ~ 'significant')
            )

# Output tables and figures  according to calssification of different levels

table01 <- hcw_raw_01 %>% 
     select(stress, employee_type, department, workplace_factor) %>%
     tbl_summary( by = stress,
                  label = list( stress ~ 'Level of stress',
                                employee_type ~ 'Type of employment',
                                department ~ 'Healthcare Department',
                                workplace_factor ~ 'Stressing factors at workplace')
                  ) %>% 
     add_p() %>% 
     bold_labels()

table02 <- hcw_raw_01 %>% 
     select(satisfaction, department, employee_type, workplace_factor) %>% 
     tbl_summary( by = satisfaction, 
                 label = list(
                      satisfaction ~ "Level of satisfaction at work",
                      employee_type ~ 'Type of employment',
                      department ~ 'Healthcare Department',
                      workplace_factor ~ 'Stressing factors at workplace')
                 ) %>%
     add_p() %>% 
     bold_labels() %>% 
     as_flex_table()

figure03 <- hcw_raw_01 %>% 
     select(stress, satisfaction, employee_type) %>% 
     ggplot(aes(employee_type, fill = stress))+
     geom_bar() +
     facet_wrap(vars(satisfaction))+
     coord_flip()

# Defining a case of Work-induced depression (wid)
#     Case = job_satisfaction < 6, often burnout frequencies, 
#            stress_level > 6, mental health leaves above 7 days

hcw_raw_02 <- 
     hcw_raw_01 %>% 
     mutate(work_induced = ifelse (job_satisfaction < 6 & stress_level > 6 & 
                               burnout_frequency == 'Often'&
                               mh_absence > 7, "Yes", "No"))


# Classification of cases of work-induced depression

table03 <- hcw_raw_02 %>% 
     select(employee_type, department, work_induced) %>%
     tbl_summary( by = work_induced,
                  label = list(
                       employee_type ~ "Type of employee",
                       department ~ "Heathcare Department"
                  )) %>% 
     bold_labels() %>% 
     add_p() %>% 
     as_flex_table()

#    Assessment of number of healthcare workers with work-induced-depression
#.   who had access to eployee assistance programme (eap)

table04 <- hcw_raw_02 %>% 
     select(eap_access, work_induced) %>% 
     tbl_cross(label = list(work_induced ~ 'Work-induced depression',
                            eap_access ~ 'Access to EAP')
     ) %>% 
     bold_labels() %>% 
     as_flex_table()


# Preparation of the logistic regression model
# Variables of interest; department, employee_type, workplace stress, satisfaction
# Outcome ; Work-induced depression (WID)

hcw_model <- hcw_raw_02
view(hcw_model)

hcw_model$employee_type <- as.factor(hcw_model$employee_type)
hcw_model$department <- as.factor(hcw_model$department)
hcw_model$workplace_factor <- as.factor(hcw_model$workplace_factor)
hcw_model$satisfaction <- as.factor(hcw_model$satisfaction)
hcw_model$work_induced <- as.factor(hcw_model$work_induced)

hcw_model$work_induced <- factor(hcw_model$work_induced, labels = c('No','Yes'))


model_0 <- glm(work_induced~mh_absence,
               data = hcw_model, family = binomial(logit))
summary(model_0)
plot_ly(data = hcw_model,
        x = ~mh_absence,
        y = ~work_induced,
        type = 'scatter',
        name = 'Probability Curve')



# WID and of healthcare department

model_1 <- glm(work_induced~department, 
               data = hcw_model, family = binomial(logit))
summary(model_1)
round(exp(cbind(coef(model_1),confint(model_1))),3)


# WID and type of healthcare employment

model_2 <- glm(work_induced~employee_type,
               data = hcw_model, family = binomial(logit))
summary(model_2)
round(exp(cbind(coef(model_2), confint(model_2))))

# WID and workplace stress

model_3 <- glm(work_induced~workplace_factor,
               data = hcw_model,
               family = binomial(logit))
summary(model_3)
round(exp(cbind(coef(model_3), confint(model_3))))

#WID and level of job satisfaction

model_4 <- glm(work_induced~satisfaction,
               data = hcw_model,
               family = binomial(logit))
summary(model_4)
round(exp(cbind(coef(model_4), confint(model_4))))

# General model
# employee type has a -ve association so it si dropped off a final model
model_5 <- glm(work_induced~department+workplace_factor+
                    satisfaction, data = hcw_model,
               family = binomial(logit))

summary(model_5)
round(exp(cbind(coef(model_5),confint(model_5))),3)


attach(hcw_model)
plot(department, fitted(glm(work_induced~department,binomial)))



