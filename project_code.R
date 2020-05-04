install.packages("devtools")
devtools::install_github("covid19r/coronavirus") # coronavirus dataset - updates every 24 hours
library(coronavirus) 
data("coronavirus")
View(coronavirus)
coronavirus = subset(coronavirus, select = -c(Province.State)) # removes the Province.State column from the data
#EDA
library(tidyr)
summary_df <- coronavirus %>% 
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)
summary_df %>% head(10)
coronavirus$cases <- abs(coronavirus$cases) # remove negative cases
coronavirus.cases <- dplyr::filter(coronavirus, grepl("confirmed", type)) # focus on confirmed cases for now
coronavirus.cases = subset(coronavirus.cases, select = -c(type)) 
## cleaned coronavirus confirmed cases dataset - coronavirus.cases
coronavirus.deaths <- dplyr::filter(coronavirus, grepl("death", type))
coronavirus.deaths = subset(coronavirus.cases, select = -c(type)) 
coronavirus.recovered <- dplyr::filter(coronavirus, grepl("recovered", type))
coronavirus.recovered = subset(coronavirus.recovered, select = -c(type))
# EDA
library(dplyr)
summary_confirmed <- coronavirus.cases %>% group_by(`Country.Region`) %>%
     summarise(cases = sum(cases)) %>%
    arrange(-cases)
summary_deaths <- coronavirus.deaths %>% group_by(`Country.Region`) %>%
     summarise(cases = sum(cases)) %>%
     arrange(-cases)
summary_recovered <- coronavirus.recovered %>% group_by(`Country.Region`) %>%
     summarise(cases = sum(cases)) %>%
     arrange(-cases)
counts <- c(sum(summary_confirmed$cases), sum(summary_recovered$cases),sum(summary_recovered$cases))
barplot(counts, main="Total counts", names.arg = c("confirmed","deaths", "recovered"))
library(dplyr)
summary_df <- coronavirus.cases %>% group_by(Country.Region, date) %>%
    summarise(total_cases = sum(cases)) %>%
    arrange(-total_cases)
summary_df %>% head(10)
top <- summary_df %>% filter(date == max(date)) %>% arrange(desc(total_cases))
top <- top_n(ungroup(top), 5, total_cases)
library(ggplot2)
summary_df %>% filter(`Country.Region` %in% top$`Country.Region`) %>% group_by(date, total_cases) %>% 
	ggplot(aes(x=as.POSIXct.Date(date), y=total_cases, color=`Country.Region`)) geom_point(size = .5) geom_line(alpha=0.5)  
     scale_color_viridis_d(option="A") scale_x_datetime() +
     labs(title="Total COVID-19 Confirmed Case Count By Date") +
     xlab("Date") 
     ylab("Cases")
summary_df <- coronavirus.cases %>% group_by(Country.Region) %>%
    summarise(total_cases = sum(cases)) %>%
    arrange(-total_cases) 
summary_df %>% head(10) %>% ggplot(aes(x=reorder(`Country.Region`,total_cases), y=total_cases)) +
    geom_segment( aes(x=reorder(`Country.Region`,total_cases), xend=reorder(`Country.Region`,total_cases), y=0, yend=total_cases)) +
    geom_point(size=5, color="red", fill=alpha("pink", 0.3), alpha=0.7, shape=21, stroke=2) +
    coord_flip() +
    labs(x="Country", y="Count") +
    labs(title="Top 10 Countries Confirmed Case Count")
summary_confirmed <- coronavirus.cases %>% group_by(date) %>%
     summarise(total_cases = sum(cases)) %>%
     arrange(-total_cases)
summary_deaths <- coronavirus.deaths %>% group_by(date) %>%
     summarise(total_cases = sum(cases)) %>%
     arrange(-total_cases)
summary_recovered <- coronavirus.recovered %>% group_by(date) %>%
     summarise(total_cases = sum(cases)) %>%
     arrange(-total_cases)
plot(summary_confirmed$date, summary_confirmed$total_cases, xlab = "Timeline", ylab = "Cumulative count", col = "red")
points(summary_deaths$date, summary_deaths$total_cases, col = "black")
points(summary_recovered$date, summary_recovered$total_cases, col = "green")
title(main = "Timeline of cumulative count of infected (red), dead(black) and recovereds (green)")
# Train test split
set.seed(100)
train_indices = sample(seq_len(nrow(coronavirus.cases)), size = floor(0.8*nrow(coronavirus.cases)))
train_cases = coronavirus.cases[train_indices,]
test_cases = coronavirus.cases[-train_indices,]
## Regression trees
#feature engineering
train_cases$log_ConfirmedCases <- log(train_cases$cases + 1)
train_cases$Day <- as.integer(train_cases$date - min(train_cases$date)) 
summary(train_cases$Day)
test_cases$Day <- as.integer(test_cases$date - min(test_cases$date)) 
summary(test_cases$Day)
##
library(caret)
num_folds <- trainControl(method = "cv", number = 5) 
parameter_grid <- expand.grid(.cp = seq(0, 0.01, 0.001))
grid_search_1 <- train(
    log_ConfirmedCases ~ Country.Region Lat Long Day,  
    data = train_cases, 
    method = "rpart", # CART algorithm
    trControl = num_folds, 
    tuneGrid = parameter_grid
)
print(grid_search_1)
library(rpart)
tree_1 <- rpart(
    log_ConfirmedCases ~ Country.Region + Lat + Long + Day,
    data = train_cases, 
    cp = 0
)
data.frame(
    variable = names(tree_1$variable.importance), 
    importance = tree_1$variable.importance, 
    stringsAsFactors = FALSE, 
    row.names = NULL
) %>% 
    arrange(desc(importance)) # variable importance
train_cases$Pred_1 <- exp(predict(tree_1, newdata = train_cases)) - 1
summary(train_cases$Pred_1)
RMSLE_1 <- sqrt(mean((log(train_cases$Pred_1 + 1) - log(train_cases$cases + 1))^2))
test_cases$predictedCases <- exp(predict(tree_1, newdata = test_cases)) - 1
plot(train_cases$date, train_cases$cases, xlab="Timeline",ylab="cases", col="green")
points(train_cases$date, train_cases$Pred_1, col="red")
### poisson 
glm_1 <- glm(
    cases ~ log(Day) + Country.Region + Lat + Long,
    data = train_cases, 
    family = "poisson"
)
train_cases$Pred_2 <- predict(glm_1, newdata = train_cases, type = "response") 
RMSLE_2 <- sqrt(mean((log(train_cases$Pred_2 + 1) - log(train_cases$cases + 1))^2))
RMSLE_2

#### Polynomial regression
train <- read.csv("~/Downloads/covid19-global-forecasting-week-4/train.csv")
View(train)
test <- read.csv("~/Downloads/covid19-global-forecasting-week-4/test.csv")
View(test)
df <- train
df$Area <- paste(df$Country_Region, df$Province_State, sep = "-")
colSums(is.na(df))
library(stringr)

#preparation for object
total <- data.frame()

#iterate through month and day
for(month in 1:3) {
    for(day in 1:31) {
        reqDate <- paste("2020-0", month, "-", str_pad(day, 2, pad = "0"), sep = "")
        iter <- as.data.frame(colSums(df[as.character(df$Date) == reqDate,
                                         c("ConfirmedCases", "Fatalities")]))
        iter2 <- data.frame(Num = (month - 1) * 31 + day,
                            Month = month, Day = day, ConfirmedCases = iter[1, ],
                            Fatalities = iter[2, ])
        if(iter[1, ] != 0) total <- rbind(total, iter2)
    }
}

#create plot of cummulative confirmed cases
plot(total$Num, total$ConfirmedCases, 
     type = "l", col = "blue",
     xlab = "Day of (since January 22, 2020)",
     ylab = "Number of Person",
     main = "Cummulative Worldwide Confirmed Cases and Fatalities of Covid-19")

par(new = TRUE)

#create plot of cummulative 
plot(total$Num, total$Fatalities, 
     type = "l", lty = 2, col = "red", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(side = 4)

legend("topleft", inset = .05, 
       legend = c("Confirmed Cases [Left axis]", "Fatalities [Right axis]"), 
       col = c("blue", "red"), bg = "gray",
       lty = c(1, 2))
df2 <- list()
confirmed.cases.model <- list()
fatalities.model <- list()

#extract Area information
areadata <- as.data.frame(table(df$Area))
areadata$Num <- row(areadata)

for(area in areadata$Var1) {
  #get per area data
  buffer <- df[df$Area == area, ]
  rownames(buffer) <- NULL
  buffer$Day <- as.numeric(rownames(buffer))
  df2[[area]] <- buffer
  
  #create models
  #note: polinomial is choosen by trial basis
  confirmed.cases.model[[area]] <- lm(ConfirmedCases ~ Day + I(Day^2) + I(Day^3) + I(Day^5), df2[[area]])
  fatalities.model[[area]] <- lm(Fatalities ~ Day + I(Day^2) + I(Day^3) + I(Day^5), df2[[area]])
}
area <- "Italy-"

#retrieve the data
data <- as.data.frame(df2[[area]])

#create plot
plot(data$Day, data$ConfirmedCases,
     type = "l", lty = 2,
     col = "blue",
     ylim = c(0, max(data$ConfirmedCases)),
     xlab = "Day of (Since January 22, 2020)", ylab = "Number of People",
     main = paste("Covid-19 Confirmed Cases in", area))
par(new = TRUE)
plot(data$Day, fitted(confirmed.cases.model[[area]]),
     type = "l", lty = 3,
     ylim = c(0, max(data$ConfirmedCases)),
     col = "red",
     xlab = "", ylab = "")
par(new = TRUE)

plot(data$Day, data$Fatalities,
     type = "l", lty = 3,
     col = "green",
     ylim = c(0, max(data$Fatalities)),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
par(new = TRUE)
plot(data$Day, fitted(fatalities.model[[area]]),
     type = "l", lty = 4,
     ylim = c(0, max(data$Fatalities)),
     col = "black",
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
par(new = TRUE)
axis(side = 4)

legend("topleft", inset = .05, 
       legend = c("Confirmed Cases [Left Figure]",
                  "Estimated Cases (based on Model) [Left Figure]", 
                  "Confirmed Fatality [Right Figure]",
                  "Estimated Fatality (based on Model) [Right Figure]"), 
       col = c("blue", "red", "green", "black"), bg = "gray",
       lty = c(2, 3, 4, 5),
       cex = 0.75)
day <- max(data$Day)

#prepare the object
accuracy <- data.frame()

#iterate the confirmed vs prediction on each of area
for(area in areadata$Var1) {
  data <- df2[[area]]
  buffer <- data.frame(Area = area, 
                     ConfirmedCases = data$ConfirmedCases[day], 
                     EstimatedCases = round(predict(confirmed.cases.model[[area]], 
                                              newdata = data.frame(Day = day))), 
                     ConfirmedFatalities = data$Fatalities[day], 
                     EstimatedFatalities = round(predict(fatalities.model[[area]],
                                                         newdata = data.frame(Day = day)))
                     )
  accuracy <- rbind(accuracy, buffer)
}

#calculate accuracy for cases and vatality; confirmed vs estimation
accuracy$AccuracyCases <- 1 - (abs(accuracy$ConfirmedCases - accuracy$EstimatedCases) / accuracy$ConfirmedCases)
accuracy$AccuracyFatalities <- 1 - (abs(accuracy$ConfirmedFatalities - accuracy$EstimatedFatalities) / accuracy$ConfirmedFatalities)

#fix the estimated 0 actual 0 calculation
accuracy[is.nan(accuracy$AccuracyFatalities), "AccuracyFatalities"] <- 1

#print the result
accuracy
print(paste("Worldwide Accuracy of Cases: ", mean(accuracy$AccuracyCases)))
print(paste("Worldwide Accuracy of Fatalities: ", mean(accuracy$AccuracyFatalities)))
test$Day <- as.integer(as.Date(test$Date) - as.Date("2020-01-22"))
test$Area <- paste(test$Country_Region, test$Province_State, sep = "-")
final <- data.frame()

#iterate the prediction
for (id in test$ForecastId){
  pred <- test[test$ForecastId == id, ]
  pred.cases <- predict(confirmed.cases.model[[pred$Area]],
                              newdata = data.frame(Day = pred$Day))
  pred.fatality <- predict(fatalities.model[[pred$Area]],
                                 newdata = data.frame(Day = pred$Day))
  buffer <- data.frame(ForecastId = id,
                       ConfirmedCases = pred.cases,
                       Fatalities = pred.fatality)
  final <- rbind(final, buffer)
}
final$ConfirmedCases <- ifelse(final$ConfirmedCases < 0, 0, final$ConfirmedCases)
final$Fatalities <- ifelse(final$Fatalities < 0, 0, final$Fatalities)

### 
coronavirus <- coronavirus %>% pivot_wider(names_from = type, values_from = cases, values_fill = list(cases = 0))
coronavirus$Day <- as.integer(coronavirus$date - min(coronavirus$date))
coronavirus$Area <- paste(coronavirus$Country.Region, coronavirus$Province.State, sep = "-")
# colSums(is.na(coronavirus))
# coronavirus[is.na(coronavirus)] <- 0
coronavirus$death <- abs(coronavirus$death)
coronavirus$confirmed <- abs(coronavirus$confirmed)
coronavirus$recovered <- abs(coronavirus$recovered)

italy <- coronavirus %>% filter(Country.Region == 'Italy')
italy <- subset(italy, select = -c(Province.State))
data <- italy
data$realR <- data$recovered + data$death
data$realI <- data$confirmed - data$realR
library(DEoptim)
evo <- DEoptim(lifespan, data, lower=lower, upper=upper, control=list(itermax=500, trace=100))
summary(evo)
plot(evo, plot.type="bestvalit", type="l")
best <- evo$optim$bestmem

# solve SIR for this best parameters, for 120 days
m <- solveSIR(best, 120)

# assign a date to t
m$date <- as.Date(m$t, origin=as.Date("2020-01-22"))

plot(m$date, m$X1, type="l", col="blue", main="Fitted SIR Model for Italy", xlab="t", ylab="cases")
lines(m$date, m$X2, type="l", col="red")
lines(m$date, m$X3, type="l", col="green")
lines(m$date, m$X2 + m$X3, type="l", col="black")
points(data$date, data$confirmed, pch=4)
points(data$date, data$realR, pch=1)

legend("left", bty="n",
       legend=c("Fitted S", "Fitted I", "Fitted R", "Fitted I cumulative", "Confirmed Cases", "Recovered + Death"),
       col=c("blue", "red", "green", "black", "black", "black"),
       lty=c(1, 1, 1, 1, 0, 0), pch=c(-1, -1, -1, -1, 4, 1))

### public health interventions
by_date <- coronavirus %>% group_by(date) %>% summarise(total_confirmed=sum(confirmed),total_death=sum(death),total_recovered=sum(recovered))
confirmed_plot<- ggplot(by_date, aes(x=date)) + 
    geom_line(aes(y = total_confirmed),color = 'red')
combined_plot<- ggplot(by_date, aes(x=date)) + 
    geom_line(aes(y = total_confirmed), color = "darkred") + 
    geom_line(aes(y = total_death), color="steelblue", linetype="twodash") 
death_plot <- ggplot(by_date, aes(x=date))+
    geom_line(aes(y = total_death), color="steelblue", linetype="twodash") 
install.packages("grid")
install.packages("gridExtra")
library(grid)
library(gridExtra)
grid.arrange(combined_plot, death_plot, nrow = 1)   
who_events <- tribble(
    ~ date, ~ event,
    "2020-01-30", "Global health\nemergency declared",
    "2020-03-11", "Pandemic\ndeclared",
    "2020-02-13", "China reporting\nchange"
) %>%
    mutate(date = as.Date(date))
confirmed_plot +
    geom_vline(data = who_events, aes( xintercept = date),linetype = "dashed") +
    geom_text(data = who_events,y = 1e5, aes(x = date, label = event))+theme_minimal()
library(earlyR)
library(incidence)
library(EpiModel)
# Total pop of 1k 
control <- control.icm(type = "SIR", nsteps = 100, nsims = 10)
init <- init.icm(s.num = 997, i.num = 3, r.num = 0)
# exposure-to-infection rate of 10 times per day
# 0.05% chance of infection in exposure to infection.That is probably realistic, 
# given that many exposures are likely to be minimal, 
# such as touching surfaces contaminated with virus, 
# and only a few will be high-risk, such as being directly coughed or sneezed on by an infected individual
# recovery rate of 0.05 - this will have a mean of about 20 days but median of 14
# recovery for each individual is determined by draws from a binomial distribution
# with mean set to the recovery rate 
# a global crude death rate is 7/1000 per annum.Daily death rate then is (7/365)/1000
# Set the arrival rate at 50% higher than the death rate to account for births and immigration

param <- param.icm(inf.prob = 0.05, act.rate = 10, rec.rate = 1/20, 
    a.rate = (10.5/365)/1000, ds.rate = (7/365)/1000, di.rate = (14/365)/1000, 
    dr.rate = (7/365)/1000)

# Run the simulation 

sim <- icm(param, init, control)

sim

plot(sim)

# Plot the incidence - total new cases per day 

plot(sim, y = "si.flow", mean.col = "red", qnts.col = "red", main = "New cases per day")

# This peak at two weeks and its corresponding R0 value of 2.2 is consistent with
# current WHO estimates for COVID-19
# Despite the model being overly simplistic
# lets explore the effects of various public health interventions 

# First - social distancing, to account for this in our model, we can reduce the average number
# of exposures per day(acts). We will explore the value sof 10,5 and 2 mean exposures per day.

# Second - hygiene measures (masks, hand washing, no face touching)
# we will model these interventions by adjusting the probability of infection (at each occasion of exposure).
# values - 0.05, 0.025, 0.01
 

run_sir_sim <- function(inf_prob, act_rate, pop_size = 1000, 
    i_num = 3, n_steps = 365, n_sims = 10, si_mean = 7.5, si_sd = 3.4) {
    
    # set up simulation parameters
    param <- param.icm(inf.prob = inf_prob, act.rate = act_rate, 
        rec.rate = 1/20, a.rate = (10.5/365)/1000, ds.rate = (7/365)/1000, 
        di.rate = (14/365)/1000, dr.rate = (7/365)/1000)
    init <- init.icm(s.num = pop_size - i_num, i.num = i_num, 
        r.num = 0)
    control <- control.icm(type = "SIR", nsteps = n_steps, nsims = n_sims)
    
    # run the simulation
    sim <- icm(param, init, control)
    
    # collect the relevant results in a data frame
    incidence_rates <- as.data.frame(sim, out = "mean") %>% select(time, 
        si.flow, i.num) %>% mutate(act_rate = act_rate, inf_prob = inf_prob, 
        total_cases = sum(si.flow), max_prev = max(i.num, na.rm = TRUE))
    
    # use the data frame of results to create an incidence()
    # object
    local_case_dates <- incidence_rates %>% filter(time <= 300, 
        act.rate == act_rate, inf.prob == inf_prob) %>% select(time, 
        si.flow) %>% uncount(si.flow) %>% pull(time)
    
    if (length(local_case_dates) > 0) {
        
        local_cases <- local_case_dates %>% incidence(.)
        
        # find the incidence peak from the incidence object
        peaky_blinder <- find_peak(local_cases)
        
        # recreate the incidence object using data only up to the
        # peak
        local_growth_phase_case_dates <- incidence_rates %>% 
            filter(time <= peaky_blinder) %>% select(time, si.flow) %>% 
            uncount(si.flow) %>% pull(time)
        
        local_growth_phase_cases <- local_growth_phase_case_dates %>% 
            incidence(., last_date = peaky_blinder)
        
        # get a MLE estimate of the basic reproduction number, R0
        res <- get_R(local_growth_phase_cases, si_mean = si_mean, 
            si_sd = si_sd)
        
        # add that as a column to the data frame of results
        incidence_rates <- incidence_rates %>% mutate(mle_R0 = res$R_ml)
    } else {
        # can't calculate R0 - set to NA
        incidence_rates <- incidence_rates %>% mutate(mle_R0 = NA)
    }
    # return the data frame 
    return(incidence_rates)
}  # end function definition

# set up an empty data frame to which to append results from
# each simulation
sims_incidence_rates <- tibble(time = integer(0), si.flow = numeric(0), 
    i.num = numeric(0), act_rate = numeric(0), inf_prob = numeric(0), 
    total_cases = numeric(0), max_prev = numeric(0), mle_R0 = numeric(0))

# the parameters to step through
act.rates <- c(10, 5, 2)
inf.probs <- c(0.05, 0.025, 0.01)

# loop through the parameter space
for (act.rate in act.rates) {
    for (inf.prob in inf.probs) {
        sims_incidence_rates <- sims_incidence_rates %>% bind_rows(run_sir_sim(inf.prob, 
            act.rate))
    }
}
## Create plots ##
# create facet columns as descending ordered factors
sims_incidence_rates <- sims_incidence_rates %>% mutate(act_rate_facet_label = paste(act_rate, 
    "exposures per day"), inf_prob_facet_label = paste("Probability of infection\nat each exposure:", 
    inf_prob)) %>% arrange(desc(act_rate)) %>% mutate_at(vars(act_rate_facet_label), 
    funs(factor(., levels = unique(.)))) %>% arrange(desc(inf_prob)) %>% 
    mutate_at(vars(inf_prob_facet_label), funs(factor(., levels = unique(.)))) %>% 
    arrange(desc(act_rate), desc(inf_prob), time)

# add annotation text for each facet
sims_incidence_rates_facet_annotations <- sims_incidence_rates %>% 
    mutate(label = paste("R0 =", format(mle_R0, digits = 3), 
        "\n", round(100 * total_cases/1000, digits = 0), "% of population infected")) %>% 
    select(inf_prob_facet_label, act_rate_facet_label, label) %>% 
    distinct()

sims_incidence_rates %>% filter(time <= 365) %>% ggplot(aes(x = time, 
    y = si.flow)) + geom_line(colour = "blue", size = 1.5) + theme_linedraw()+
    facet_grid(inf_prob_facet_label ~ act_rate_facet_label) + 
    geom_text(data = sims_incidence_rates_facet_annotations, 
        mapping = aes(x = 50, y = 0.8 * max(sims_incidence_rates$si.flow, 
            na.rm = TRUE), label = label), parse = FALSE, hjust = 0, 
        vjust = 0, size = 3) + labs(x = "Days since start of epidemic", 
    y = "New cases per day", title = "Modelling of new cases of COVID-19 per day: incidence rate", 
    subtitle = paste("with varying levels of social mixing (exposures per day)", 
        "and probabilities of infection at each exposure")) + 
    theme(legend.position = "top", strip.text = element_text(size = 14))
## Build on top of the existing model architecture ## 

# function to set-up and run the baseline simulations
simulate <- function(# control.icm params
                     type = "SEIQHRF", 
                     nsteps = 366, 
                     nsims = 8,
                     ncores = 4,
                     prog.rand = FALSE,
                     rec.rand = FALSE,
                     fat.rand = TRUE,
                     quar.rand = FALSE,
                     hosp.rand = FALSE,
                     disch.rand = TRUE,
                     infection.FUN = infection.seiqhrf.icm,
                     recovery.FUN = progress.seiqhrf.icm,
                     departures.FUN = departures.seiqhrf.icm,
                     arrivals.FUN = arrivals.icm,
                     get_prev.FUN = get_prev.seiqhrf.icm,
                     # init.icm params
                     s.num = 9997,
                     e.num=0,
                     i.num = 3,
                     q.num=0,
                     h.num=0,
                     r.num = 0,
                     f.num = 0,
                     # param.icm params
                     inf.prob.e = 0.02, 
                     act.rate.e = 10,
                     inf.prob.i = 0.05, 
                     act.rate.i = 10,
                     inf.prob.q = 0.02, 
                     act.rate.q = 2.5,                    
                     quar.rate = 1/30, 
                     hosp.rate = 1/100,
                     disch.rate = 1/15,
                     prog.rate = 1/10,
                     prog.dist.scale = 5,
                     prog.dist.shape = 1.5,
                     rec.rate = 1/20,
                     rec.dist.scale = 35,
                     rec.dist.shape = 1.5,
                     fat.rate.base = 1/50,
                     hosp.cap = 40,
                     fat.rate.overcap = 1/25,
                     fat.tcoeff = 0.5,
                     vital = TRUE,
                     a.rate = (10.5/365)/1000, 
                     a.prop.e = 0.01,
                     a.prop.i = 0.001,
                     a.prop.q = 0.01,
                     ds.rate = (7/365)/1000, 
                     de.rate = (7/365)/1000, 
                     di.rate = (7/365)/1000,
                     dq.rate = (7/365)/1000,
                     dh.rate = (20/365)/1000,
                     dr.rate = (7/365)/1000,
                     out="mean"
                    ) {

  control <- control.icm(type = type, 
                         nsteps = nsteps, 
                         nsims = nsims,
                         ncores = ncores,
                         prog.rand = prog.rand,
                         rec.rand = rec.rand,
                         infection.FUN = infection.FUN,
                         recovery.FUN = recovery.FUN,
                         arrivals.FUN = arrivals.FUN,
                         departures.FUN = departures.FUN,
                         get_prev.FUN = get_prev.FUN)

  init <- init.icm(s.num = s.num,
                   e.num = e.num,
                   i.num = i.num,
                   q.num = q.num,
                   h.num = h.num,
                   r.num = r.num,
                   f.num = f.num)

  param <-  param.icm(inf.prob.e = inf.prob.e, 
                      act.rate.e = act.rate.e,
                      inf.prob.i = inf.prob.i, 
                      act.rate.i = act.rate.i,
                      inf.prob.q = inf.prob.q, 
                      act.rate.q = act.rate.q,                    
                      quar.rate = quar.rate,
                      hosp.rate = hosp.rate,
                      disch.rate = disch.rate,
                      prog.rate = prog.rate,
                      prog.dist.scale = prog.dist.scale,
                      prog.dist.shape = prog.dist.shape,
                      rec.rate = rec.rate,
                      rec.dist.scale = rec.dist.scale,
                      rec.dist.shape = rec.dist.shape,
                      fat.rate.base = fat.rate.base,
                      hosp.cap = hosp.cap,
                      fat.rate.overcap = fat.rate.overcap,
                      fat.tcoeff = fat.tcoeff,
                      vital = vital,
                      a.rate = a.rate, 
                      a.prop.e = a.prop.e,
                      a.prop.i = a.prop.i,
                      a.prop.q = a.prop.q,
                      ds.rate = ds.rate, 
                      de.rate = de.rate, 
                      di.rate = di.rate,
                      dq.rate = dq.rate,
                      dh.rate = dh.rate,
                      dr.rate = dr.rate)

  sim <- icm.seiqhrf(param, init, control)
  sim_df <- as.data.frame(sim, out=out)

  return(list(sim=sim, df=sim_df))
}
install.packages("ggridges")
library(ggridges)
# create one column for both intervention parameters
sims_incidence_rates <- sims_incidence_rates %>% mutate(intervention_level_label = paste(act_rate, 
                                                                                         "exp/day,", inf_prob * 100, "% inf risk/exp")) %>% arrange(max_prev, 
                                                                                                                                                    time) %>% mutate_at(vars(intervention_level_label), funs(factor(., 
                                                                                                                                                                                                                    levels = unique(.), ordered = TRUE)))

sims_incidence_rates %>% filter(time <= 365) %>% ggplot(aes(x = time, 
                                                            y = intervention_level_label, height = i.num, fill = intervention_level_label)) + 
    geom_density_ridges(stat = "identity", show.legend = FALSE) + 
    labs(x = "Days since start of epidemic", y = "Prevalent (current number of active) cases", 
         title = "Modelling of COVID-19 transmission in 1,000 simulated people", 
         subtitle = paste("with varying levels of social mixing (exposures per day)", 
                          "and risk of infection at each exposure,\n", "ordered by descending maximum number of prevalent cases per day")) + 
    theme_minimal() + theme(legend.position = "top", strip.text = element_text(size = 12)) + 
    scale_fill_brewer(type = "seq", palette = "Blues")
    




