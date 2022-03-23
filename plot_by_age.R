

library(tidyverse)
library(plyr)

###############################
#
# Type 3 Neutral
# 
###############################

setwd("~/Data/AgeStruct/type_3_neutral/")

a <- c(5, 10, 20) # age of sexual maturity
b <- c(25, 50, 100) # avg litter size
reps <- 1:10

tbl <- as.data.frame(crossing(a, b, reps))
id <- NULL
for(i in 1:(length(a) * length(b))) { id <- c(id, rep(i, 10)) }
tbl$id <- id

# plot by age
by_age <- as.data.frame(matrix(nrow = nrow(tbl), ncol = 200))

for(i in 1:nrow(tbl)) {
  
  path <- paste("scenario_", tbl$id[i], "/rep_", tbl$reps[i], "/", sep = "")
  
  count_by_age <- read.table(paste(path, "count_by_age.txt", sep = ""))
  fit_by_age <- read.table(paste(path, "age_vectors.txt", sep = ""))[1,]
  het_by_age <- read.table(paste(path, "age_vectors.txt", sep = ""))[2,]
  NS_by_age <- read.table(paste(path, "age_vectors.txt", sep = ""))[3,]
  
  for(j in 1:length(count_by_age)) {
    if(count_by_age[j] == 0) {
      fit_by_age <- append(fit_by_age, NA, after = j)
      het_by_age <- append(het_by_age, NA, after = j)
      NS_by_age <- append(NS_by_age, NA, after = j)
    }
  }
  
  by_age[i,] <- c(count_by_age, fit_by_age, het_by_age, NS_by_age)
}

dat <- cbind.data.frame(tbl, by_age)
dat$reps <- as.factor(dat$reps)
names(dat) <- c("a", "b", "replicate", "scenario", 
                paste("count_age_", 1:50, sep = ""),
                paste("fit_age_", 1:50, sep = ""),
                paste("het_age_", 1:50, sep = ""),
                paste("NS_age_", 1:50, sep = ""))

write.table(dat, "type_3_by_age.tsv", sep = "\t", col.names = T, row.names = F, quote = F)

dat_counts <- dplyr::select(dat, a, b, replicate, scenario, starts_with("count_")) 
names(dat_counts)[5:ncol(dat_counts)] <- 1:(ncol(dat_counts) - 4)
molten_vals <- pivot_longer(dat_counts, cols = 5:ncol(dat_counts), names_to = "age_bin")

census_plot <- ggplot(molten_vals, aes(x = as.numeric(age_bin), y = value, color = as.factor(replicate)))
census_plot <- census_plot + geom_point(size = 4,  alpha = 1, shape = 1) + theme_bw() 
census_plot <- census_plot + facet_grid(a~b)
census_plot <- census_plot + labs(title = NULL, x = "Age", y = "Count") 
census_plot <- census_plot + theme(text = element_text(size = 14), 
                                   legend.title = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.title.x = element_text(size = 16),
                                   axis.title.y = element_text(size = 16))
ggsave("census_plot.pdf", census_plot, device = "pdf", width = 12, height = 12)

dat_hets <- dplyr::select(dat, a, b, replicate, starts_with("het_")) 
names(dat_hets)[4:ncol(dat_hets)] <- 1:(ncol(dat_hets) - 3)
molten_vals <- pivot_longer(dat_hets, cols = 4:ncol(dat_hets), names_to = "age_bin")

het_plot <- ggplot(molten_vals, aes(x = as.numeric(age_bin), y = value, color = as.factor(replicate)))
het_plot <- het_plot + geom_line(size = 0.5) + theme_bw() 
het_plot <- het_plot + facet_grid(a~b)
het_plot <- het_plot + labs(title = NULL, x = "Age", y = "Het") 
het_plot <- het_plot + theme(text = element_text(size = 14), 
                             legend.title = element_blank(),
                             axis.text.x = element_blank(),
                             axis.title.x = element_text(size = 16),
                             axis.title.y = element_text(size = 16))
ggsave("het_plot.pdf", het_plot, device = "pdf", width = 12, height = 12)


# avg over reps
## function to get the SD using ddply
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
rowSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=1, FUN=sd, na.rm=na.rm)

dat_avgs <- dat[-3]
dat_counts <- dplyr::select(dat, a, b, scenario, starts_with("count_")) 

dat_counts_avg <- ddply(.data = dat_counts, .variables = "scenario", .fun = colMeans)
colnames(dat_counts_avg) <- c(colnames(dat_counts_avg)[1:3], paste("avg_count_", 1:50, sep = ""))

dat_counts_sd <- ddply(.data = dat_counts, .variables = "scenario", .fun = colSd)
colnames(dat_counts_sd) <- c(colnames(dat_counts_sd)[1:3], paste("sd_count_", 1:50, sep = ""))

dat_counts_moments <- cbind.data.frame(dat_counts_avg, dat_counts_sd[4:ncol(dat_counts_sd)])

m1 <- pivot_longer(dat_counts_moments, cols = starts_with("avg"), names_to = "age_bin", values_to = "avg")
m2 <- pivot_longer(dat_counts_moments, cols = starts_with("sd"), names_to = "age_bin", values_to = "sd")
molten_moments <- cbind.data.frame(m1[c(1:3, ncol(m1))], m2[ncol(m2)])
molten_moments$age_bin <- rep(1:50, 9)

count_moments_plot <- ggplot(molten_moments, aes(x = age_bin, y = avg))
count_moments_plot <- count_moments_plot + geom_line(size = 0.5) + theme_bw() 
count_moments_plot <- count_moments_plot + geom_point(shape = 19) 
count_moments_plot <- count_moments_plot + facet_grid(a~b)
count_moments_plot <- count_moments_plot + labs(title = NULL, x = "Age", y = "Count") 
count_moments_plot <- count_moments_plot + theme(text = element_text(size = 14), 
                                                 legend.title = element_blank(),
                                                 axis.text.x = element_blank(),
                                                 axis.title.x = element_text(size = 16),
                                                 axis.title.y = element_text(size = 16))
count_moments_plot <- count_moments_plot + geom_errorbar(aes(ymin = avg -2*sd,ymax = avg +2*sd),width = 0.2,colour = 'red')
ggsave("count_moments_plot.pdf", count_moments_plot, device = "pdf", width = 9, height = 9)


dat_avgs <- dat[-3]
dat_hets <- dplyr::select(dat, a, b, scenario, starts_with("het_")) 

dat_hets_avg <- ddply(.data = dat_hets, .variables = "scenario", .fun = colMeans)
colnames(dat_hets_avg) <- c(colnames(dat_hets_avg)[1:3], paste("avg_het_", 1:50, sep = ""))

dat_hets_sd <- ddply(.data = dat_hets, .variables = "scenario", .fun = colSd)
colnames(dat_hets_sd) <- c(colnames(dat_hets_sd)[1:3], paste("sd_het_", 1:50, sep = ""))

dat_hets_moments <- cbind.data.frame(dat_hets_avg, dat_hets_sd[4:ncol(dat_hets_sd)])

m1 <- pivot_longer(dat_hets_moments, cols = starts_with("avg"), names_to = "age_bin", values_to = "avg")
m2 <- pivot_longer(dat_hets_moments, cols = starts_with("sd"), names_to = "age_bin", values_to = "sd")
molten_moments <- cbind.data.frame(m1[c(1:3, ncol(m1))], m2[ncol(m2)])
molten_moments$age_bin <- rep(1:50, 9)

het_moments_plot <- ggplot(molten_moments, aes(x = age_bin, y = avg))
het_moments_plot <- het_moments_plot + geom_line(size = 0.5) + theme_bw() 
het_moments_plot <- het_moments_plot + geom_point(shape = 19) 
het_moments_plot <- het_moments_plot + facet_grid(a~b)
het_moments_plot <- het_moments_plot + labs(title = NULL, x = "Age", y = "het") 
het_moments_plot <- het_moments_plot + theme(text = element_text(size = 14), 
                                             legend.title = element_blank(),
                                             axis.text.x = element_blank(),
                                             axis.title.x = element_text(size = 16),
                                             axis.title.y = element_text(size = 16))
het_moments_plot <- het_moments_plot + geom_errorbar(aes(ymin = avg -2*sd,ymax = avg +2*sd),width = 0.2,colour = 'red')
ggsave("het_moments_plot.pdf", het_moments_plot, device = "pdf", width = 9, height = 9)


dat_het <- dplyr::select(dat, a, b, replicate, scenario, starts_with("het_")) 
dat_het_pop <- cbind.data.frame(dat_het[1:4], rowMeans(dat_het[5:ncol(dat_het)], na.rm = T))

dat_het_pop_avg <- ddply(.data = dat_het_pop[-3], .variables = "scenario", .fun = colMeans, na.rm = T)
colnames(dat_het_pop_avg)[4] <- "avg"

dat_het_pop_sd <- ddply(.data = dat_het_pop[-3], .variables = "scenario", .fun = colSd, na.rm = T)
colnames(dat_het_pop_sd)[4] <- "sd"

dat_het_pop_moments <- cbind.data.frame(dat_het_pop_avg, dat_het_pop_sd[4])

het_moments_plot <- ggplot(dat_het_pop_moments, aes(x = as.factor(a), y = avg, color = as.factor(b)))
het_moments_plot <- het_moments_plot + geom_point(size = 2.5) + theme_bw() 
het_moments_plot <- het_moments_plot + labs(title = NULL, x = "a", y = "het") 
het_moments_plot <- het_moments_plot + theme(text = element_text(size = 14), 
                                             legend.title = element_blank(),
                                             axis.title.x = element_text(size = 16),
                                             axis.title.y = element_text(size = 16))
het_moments_plot <- het_moments_plot + geom_errorbar(aes(ymin = avg - 2 * sd, ymax = avg + 2 * sd, color = as.factor(b)), width = 0.2)
ggsave("het_moments_pop_plot.pdf", het_moments_plot, device = "pdf", width = 6, height = 6)


###############################
#
# Type 3 with Selecion
# 
###############################

setwd("~/Data/AgeStruct/type_3_selection/")

a <- c(5, 10, 20) # age of sexual maturity
b <- c(25, 50, 100) # avg litter size
reps <- 1:10

tbl <- as.data.frame(crossing(a, b, reps))
id <- NULL
for(i in 1:(length(a) * length(b))) { id <- c(id, rep(i, 10)) }
tbl$id <- id

# plot by age
by_age <- as.data.frame(matrix(nrow = nrow(tbl), ncol = 200))

for(i in 1:nrow(tbl)) {
  
  path <- paste("scenario_", tbl$id[i], "/rep_", tbl$reps[i], "/", sep = "")
  
  count_by_age <- read.table(paste(path, "count_by_age.txt", sep = ""))
  fit_by_age <- read.table(paste(path, "age_vectors.txt", sep = ""))[1,]
  het_by_age <- read.table(paste(path, "age_vectors.txt", sep = ""))[2,]
  NS_by_age <- read.table(paste(path, "age_vectors.txt", sep = ""))[3,]
  
  for(j in 1:length(count_by_age)) {
    if(count_by_age[j] == 0) {
      fit_by_age <- append(fit_by_age, NA, after = j)
      het_by_age <- append(het_by_age, NA, after = j)
      NS_by_age <- append(NS_by_age, NA, after = j)
    }
  }
  
  by_age[i,] <- c(count_by_age, fit_by_age, het_by_age, NS_by_age)
}

dat <- cbind.data.frame(tbl, by_age)
dat$reps <- as.factor(dat$reps)
names(dat) <- c("a", "b", "replicate", "scenario", 
                paste("count_age_", 1:50, sep = ""),
                paste("fit_age_", 1:50, sep = ""),
                paste("het_age_", 1:50, sep = ""),
                paste("NS_age_", 1:50, sep = ""))
  
write.table(dat, "type_3_by_age.tsv", sep = "\t", col.names = T, row.names = F, quote = F)


dat_counts <- dplyr::select(dat, a, b, replicate, scenario, starts_with("count_")) 
names(dat_counts)[5:ncol(dat_counts)] <- 1:(ncol(dat_counts) - 4)
molten_vals <- pivot_longer(dat_counts, cols = 5:ncol(dat_counts), names_to = "age_bin")

census_plot <- ggplot(molten_vals, aes(x = as.numeric(age_bin), y = value, color = as.factor(replicate)))
census_plot <- census_plot + geom_point(size = 4,  alpha = 1, shape = 1) + theme_bw() 
census_plot <- census_plot + facet_grid(a~b)
census_plot <- census_plot + labs(title = NULL, x = "Age", y = "Count") 
census_plot <- census_plot + theme(text = element_text(size = 14), 
                                          legend.title = element_blank(),
                                          axis.text.x = element_blank(),
                                          axis.title.x = element_text(size = 16),
                                          axis.title.y = element_text(size = 16))
ggsave("census_plot.pdf", census_plot, device = "pdf", width = 12, height = 12)

dat_fits <- dplyr::select(dat, a, b, replicate, starts_with("fit_")) 
names(dat_fits)[4:ncol(dat_fits)] <- 1:(ncol(dat_fits) - 3)
molten_vals <- pivot_longer(dat_fits, cols = 4:ncol(dat_fits), names_to = "age_bin")

load_plot <- ggplot(molten_vals, aes(x = as.numeric(age_bin), y = 1 - value, color = as.factor(replicate)))
load_plot <- load_plot + geom_line(size = 0.5) + theme_bw() 
load_plot <- load_plot + facet_grid(a~b)
load_plot <- load_plot + labs(title = NULL, x = "Age", y = "Load") 
load_plot <- load_plot + theme(text = element_text(size = 14), 
                                   legend.title = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.title.x = element_text(size = 16),
                                   axis.title.y = element_text(size = 16))
ggsave("load_plot.pdf", load_plot, device = "pdf", width = 12, height = 12)

dat_hets <- dplyr::select(dat, a, b, replicate, starts_with("het_")) 
names(dat_hets)[4:ncol(dat_hets)] <- 1:(ncol(dat_hets) - 3)
molten_vals <- pivot_longer(dat_hets, cols = 4:ncol(dat_hets), names_to = "age_bin")

het_plot <- ggplot(molten_vals, aes(x = as.numeric(age_bin), y = value, color = as.factor(replicate)))
het_plot <- het_plot + geom_line(size = 0.5) + theme_bw() 
het_plot <- het_plot + facet_grid(a~b)
het_plot <- het_plot + labs(title = NULL, x = "Age", y = "Het") 
het_plot <- het_plot + theme(text = element_text(size = 14), 
                               legend.title = element_blank(),
                               axis.text.x = element_blank(),
                               axis.title.x = element_text(size = 16),
                               axis.title.y = element_text(size = 16))
ggsave("het_plot.pdf", het_plot, device = "pdf", width = 12, height = 12)


dat_ns <- dplyr::select(dat, a, b, replicate, starts_with("NS_")) 
names(dat_ns)[4:ncol(dat_ns)] <- 1:(ncol(dat_ns) - 3)
molten_vals <- pivot_longer(dat_ns, cols = 4:ncol(dat_ns), names_to = "age_bin")

ns_plot <- ggplot(molten_vals, aes(x = as.numeric(age_bin), y = value, color = as.factor(replicate)))
ns_plot <- ns_plot + geom_line(size = 0.5) + theme_bw() 
ns_plot <- ns_plot + facet_grid(a~b)
ns_plot <- ns_plot + labs(title = NULL, x = "Age", y = "NS_count") 
ns_plot <- ns_plot + theme(text = element_text(size = 14), 
                             legend.title = element_blank(),
                             axis.text.x = element_blank(),
                             axis.title.x = element_text(size = 16),
                             axis.title.y = element_text(size = 16))
ggsave("ns_plot.pdf", ns_plot, device = "pdf", width = 12, height = 12)

# avg over reps
## function to get the SD using ddply
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
rowSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=1, FUN=sd, na.rm=na.rm)

dat_avgs <- dat[-3]
dat_counts <- dplyr::select(dat, a, b, scenario, starts_with("count_")) 

dat_counts_avg <- ddply(.data = dat_counts, .variables = "scenario", .fun = colMeans)
colnames(dat_counts_avg) <- c(colnames(dat_counts_avg)[1:3], paste("avg_count_", 1:50, sep = ""))

dat_counts_sd <- ddply(.data = dat_counts, .variables = "scenario", .fun = colSd)
colnames(dat_counts_sd) <- c(colnames(dat_counts_sd)[1:3], paste("sd_count_", 1:50, sep = ""))

dat_counts_moments <- cbind.data.frame(dat_counts_avg, dat_counts_sd[4:ncol(dat_counts_sd)])

m1 <- pivot_longer(dat_counts_moments, cols = starts_with("avg"), names_to = "age_bin", values_to = "avg")
m2 <- pivot_longer(dat_counts_moments, cols = starts_with("sd"), names_to = "age_bin", values_to = "sd")
molten_moments <- cbind.data.frame(m1[c(1:3, ncol(m1))], m2[ncol(m2)])
molten_moments$age_bin <- rep(1:50, 9)
  
count_moments_plot <- ggplot(molten_moments, aes(x = age_bin, y = avg))
count_moments_plot <- count_moments_plot + geom_line(size = 0.5) + theme_bw() 
count_moments_plot <- count_moments_plot + geom_point(shape = 19) 
count_moments_plot <- count_moments_plot + facet_grid(a~b)
count_moments_plot <- count_moments_plot + labs(title = NULL, x = "Age", y = "Count") 
count_moments_plot <- count_moments_plot + theme(text = element_text(size = 14), 
                           legend.title = element_blank(),
                           axis.text.x = element_blank(),
                           axis.title.x = element_text(size = 16),
                           axis.title.y = element_text(size = 16))
count_moments_plot <- count_moments_plot + geom_errorbar(aes(ymin = avg -2*sd,ymax = avg +2*sd),width = 0.2,colour = 'red')
ggsave("count_moments_plot.pdf", count_moments_plot, device = "pdf", width = 9, height = 9)


dat_avgs <- dat[-3]
dat_loads <- dplyr::select(dat, a, b, scenario, starts_with("fit_")) 
dat_loads[4:ncol(dat_loads)] <- 1 - dat_loads[4:ncol(dat_loads)]

dat_loads_avg <- ddply(.data = dat_loads, .variables = "scenario", .fun = colMeans)
colnames(dat_loads_avg) <- c(colnames(dat_loads_avg)[1:3], paste("avg_load_", 1:50, sep = ""))

dat_loads_sd <- ddply(.data = dat_loads, .variables = "scenario", .fun = colSd)
colnames(dat_loads_sd) <- c(colnames(dat_loads_sd)[1:3], paste("sd_load_", 1:50, sep = ""))

dat_loads_moments <- cbind.data.frame(dat_loads_avg, dat_loads_sd[4:ncol(dat_loads_sd)])

m1 <- pivot_longer(dat_loads_moments, cols = starts_with("avg"), names_to = "age_bin", values_to = "avg")
m2 <- pivot_longer(dat_loads_moments, cols = starts_with("sd"), names_to = "age_bin", values_to = "sd")
molten_moments <- cbind.data.frame(m1[c(1:3, ncol(m1))], m2[ncol(m2)])
molten_moments$age_bin <- rep(1:50, 9)

load_moments_plot <- ggplot(molten_moments, aes(x = age_bin, y = avg))
load_moments_plot <- load_moments_plot + geom_line(size = 0.5) + theme_bw() 
load_moments_plot <- load_moments_plot + geom_point(shape = 19) 
load_moments_plot <- load_moments_plot + facet_grid(a~b)
load_moments_plot <- load_moments_plot + labs(title = NULL, x = "Age", y = "load") 
load_moments_plot <- load_moments_plot + theme(text = element_text(size = 14), 
                                                 legend.title = element_blank(),
                                                 axis.text.x = element_blank(),
                                                 axis.title.x = element_text(size = 16),
                                                 axis.title.y = element_text(size = 16))
load_moments_plot <- load_moments_plot + geom_errorbar(aes(ymin = avg -2*sd,ymax = avg +2*sd),width = 0.2,colour = 'red')
ggsave("load_moments_plot.pdf", load_moments_plot, device = "pdf", width = 9, height = 9)

dat_avgs <- dat[-3]
dat_hets <- dplyr::select(dat, a, b, scenario, starts_with("het_")) 

dat_hets_avg <- ddply(.data = dat_hets, .variables = "scenario", .fun = colMeans)
colnames(dat_hets_avg) <- c(colnames(dat_hets_avg)[1:3], paste("avg_het_", 1:50, sep = ""))

dat_hets_sd <- ddply(.data = dat_hets, .variables = "scenario", .fun = colSd)
colnames(dat_hets_sd) <- c(colnames(dat_hets_sd)[1:3], paste("sd_het_", 1:50, sep = ""))

dat_hets_moments <- cbind.data.frame(dat_hets_avg, dat_hets_sd[4:ncol(dat_hets_sd)])

m1 <- pivot_longer(dat_hets_moments, cols = starts_with("avg"), names_to = "age_bin", values_to = "avg")
m2 <- pivot_longer(dat_hets_moments, cols = starts_with("sd"), names_to = "age_bin", values_to = "sd")
molten_moments <- cbind.data.frame(m1[c(1:3, ncol(m1))], m2[ncol(m2)])
molten_moments$age_bin <- rep(1:50, 9)

het_moments_plot <- ggplot(molten_moments, aes(x = age_bin, y = avg))
het_moments_plot <- het_moments_plot + geom_line(size = 0.5) + theme_bw() 
het_moments_plot <- het_moments_plot + geom_point(shape = 19) 
het_moments_plot <- het_moments_plot + facet_grid(a~b)
het_moments_plot <- het_moments_plot + labs(title = NULL, x = "Age", y = "het") 
het_moments_plot <- het_moments_plot + theme(text = element_text(size = 14), 
                                               legend.title = element_blank(),
                                               axis.text.x = element_blank(),
                                               axis.title.x = element_text(size = 16),
                                               axis.title.y = element_text(size = 16))
het_moments_plot <- het_moments_plot + geom_errorbar(aes(ymin = avg -2*sd,ymax = avg +2*sd),width = 0.2,colour = 'red')
ggsave("het_moments_plot.pdf", het_moments_plot, device = "pdf", width = 9, height = 9)

dat_avgs <- dat[-3]
dat_nss <- dplyr::select(dat, a, b, scenario, starts_with("NS_")) 

dat_nss_avg <- ddply(.data = dat_nss, .variables = "scenario", .fun = colMeans)
colnames(dat_nss_avg) <- c(colnames(dat_nss_avg)[1:3], paste("avg_ns_", 1:50, sep = ""))

dat_nss_sd <- ddply(.data = dat_nss, .variables = "scenario", .fun = colSd)
colnames(dat_nss_sd) <- c(colnames(dat_nss_sd)[1:3], paste("sd_ns_", 1:50, sep = ""))

dat_nss_moments <- cbind.data.frame(dat_nss_avg, dat_nss_sd[4:ncol(dat_nss_sd)])

m1 <- pivot_longer(dat_nss_moments, cols = starts_with("avg"), names_to = "age_bin", values_to = "avg")
m2 <- pivot_longer(dat_nss_moments, cols = starts_with("sd"), names_to = "age_bin", values_to = "sd")
molten_moments <- cbind.data.frame(m1[c(1:3, ncol(m1))], m2[ncol(m2)])
molten_moments$age_bin <- rep(1:50, 9)

ns_moments_plot <- ggplot(molten_moments, aes(x = age_bin, y = avg))
ns_moments_plot <- ns_moments_plot + geom_line(size = 0.5) + theme_bw() 
ns_moments_plot <- ns_moments_plot + geom_point(shape = 19) 
ns_moments_plot <- ns_moments_plot + facet_grid(a~b)
ns_moments_plot <- ns_moments_plot + labs(title = NULL, x = "Age", y = "ns") 
ns_moments_plot <- ns_moments_plot + theme(text = element_text(size = 14), 
                                             legend.title = element_blank(),
                                             axis.text.x = element_blank(),
                                             axis.title.x = element_text(size = 16),
                                             axis.title.y = element_text(size = 16))
ns_moments_plot <- ns_moments_plot + geom_errorbar(aes(ymin = avg -2*sd,ymax = avg +2*sd),width = 0.2,colour = 'red')
ggsave("ns_moments_plot.pdf", ns_moments_plot, device = "pdf", width = 9, height = 9)

# averaging over age bins as well
dat_ns <- dplyr::select(dat, a, b, replicate, scenario, starts_with("NS_")) 
dat_ns_pop <- cbind.data.frame(dat_ns[1:4], rowMeans(dat_ns[5:ncol(dat_ns)]))

dat_ns_pop <- ddply(.data = dat_ns, .variables = "scenario", .fun = rowMeans, na.rm = T)
dat_ns_pop <- cbind.data.frame(crossing(a, b), dat_ns_pop)
colnames(dat_ns_pop) <- c(colnames(dat_ns_pop)[1:3], paste("avg_ns_", 1:10, sep = ""))

m1 <- pivot_longer(dat_ns_pop, cols = starts_with("avg"), names_to = "replicate", values_to = "avg_ns")
dat_ns_pop_avg <- ddply(.data = m1[-4], .variables = "scenario", .fun = colMeans)
dat_ns_pop_sd <- ddply(.data = m1[-4], .variables = "scenario", .fun = colSd)
dat_ns_pop_moments <- cbind.data.frame(dat_ns_pop_avg, dat_ns_pop_sd[4])
names(dat_ns_pop_moments)[5] <- "sd"

ns_moments_plot <- ggplot(dat_ns_pop_moments, aes(x = as.factor(a), y = avg_ns, color = as.factor(b)))
ns_moments_plot <- ns_moments_plot + geom_point(size = 2.5) + theme_bw() 
ns_moments_plot <- ns_moments_plot + labs(title = NULL, x = "a", y = "NS count") 
ns_moments_plot <- ns_moments_plot + theme(text = element_text(size = 14), 
                                           legend.title = element_blank(),
                                           axis.title.x = element_text(size = 16),
                                           axis.title.y = element_text(size = 16))
ns_moments_plot <- ns_moments_plot + geom_errorbar(aes(ymin = avg_ns - sd, ymax = avg_ns + sd, color = as.factor(b)), width = 0.2)
ggsave("ns_moments_pop_plot.pdf", ns_moments_plot, device = "pdf", width = 6, height = 6)

dat_het <- dplyr::select(dat, a, b, replicate, scenario, starts_with("het_")) 
dat_het_pop <- cbind.data.frame(dat_het[1:4], rowMeans(dat_het[5:ncol(dat_het)], na.rm = T))

dat_het_pop_avg <- ddply(.data = dat_het_pop[-3], .variables = "scenario", .fun = colMeans, na.rm = T)
colnames(dat_het_pop_avg)[4] <- "avg"

dat_het_pop_sd <- ddply(.data = dat_het_pop[-3], .variables = "scenario", .fun = colSd, na.rm = T)
colnames(dat_het_pop_sd)[4] <- "sd"

dat_het_pop_moments <- cbind.data.frame(dat_het_pop_avg, dat_het_pop_sd[4])

het_moments_plot <- ggplot(dat_het_pop_moments, aes(x = as.factor(a), y = avg, color = as.factor(b)))
het_moments_plot <- het_moments_plot + geom_point(size = 2.5) + theme_bw() 
het_moments_plot <- het_moments_plot + labs(title = NULL, x = "a", y = "het") 
het_moments_plot <- het_moments_plot + theme(text = element_text(size = 14), 
                                           legend.title = element_blank(),
                                           axis.title.x = element_text(size = 16),
                                           axis.title.y = element_text(size = 16))
het_moments_plot <- het_moments_plot + geom_errorbar(aes(ymin = avg - 2 * sd, ymax = avg + 2 * sd, color = as.factor(b)), width = 0.2)
ggsave("het_moments_pop_plot.pdf", het_moments_plot, device = "pdf", width = 6, height = 6)

dat_fit <- dplyr::select(dat, a, b, replicate, scenario, starts_with("fit_")) 
dat_fit_pop <- cbind.data.frame(dat_fit[1:4], rowMeans(1 - dat_fit[5:ncol(dat_fit)], na.rm = T))

dat_fit_pop_avg <- ddply(.data = dat_fit_pop[-3], .variables = "scenario", .fun = colMeans, na.rm = T)
colnames(dat_fit_pop_avg)[4] <- "avg"

dat_fit_pop_sd <- ddply(.data = dat_fit_pop[-3], .variables = "scenario", .fun = colSd, na.rm = T)
colnames(dat_fit_pop_sd)[4] <- "sd"

dat_fit_pop_moments <- cbind.data.frame(dat_fit_pop_avg, dat_fit_pop_sd[4])

fit_moments_plot <- ggplot(dat_fit_pop_moments, aes(x = as.factor(a), y = avg, color = as.factor(b)))
fit_moments_plot <- fit_moments_plot + geom_point(size = 2.5) + theme_bw() 
fit_moments_plot <- fit_moments_plot + labs(title = NULL, x = "a", y = "load") 
fit_moments_plot <- fit_moments_plot + theme(text = element_text(size = 14), 
                                             legend.title = element_blank(),
                                             axis.title.x = element_text(size = 16),
                                             axis.title.y = element_text(size = 16))
fit_moments_plot <- fit_moments_plot + geom_errorbar(aes(ymin = avg - 2 * sd, ymax = avg + 2 * sd, color = as.factor(b)), width = 0.2)
ggsave("load_moments_pop_plot.pdf", fit_moments_plot, device = "pdf", width = 6, height = 6)




# DFE
mean_sim <- -0.01314833
alpha_sim <- 0.186
beta_sim <- - mean_sim / (2 * alpha_sim) # Covert from dadi to slim: E[s] = (-shape*scale*2)
var_sim <- alpha_sim * (beta_sim ^ 2)

gamma_inf <- as.data.frame(matrix(nrow = nrow(tbl), ncol = 2))
names(gamma_inf) <- c("alpha", "beta")
for(i in 1:nrow(tbl)) {
  
  path <- paste("scenario_", tbl$id[i], "/rep_", tbl$reps[i], "/fitdadi_inferred_DFE.txt", sep = "")
  
  alpha_inf <- as.numeric(unlist(unlist(unlist(readLines(path)[4] %>% str_split("array"))[2] %>% str_split(" "))[1] %>% str_split("\\["))[2])
  beta_inf <- as.numeric(unlist(unlist(unlist(readLines(path)[4] %>% str_split("array"))[2] %>% str_split(" "))[2] %>% str_split("]"))[1])
  
  # some next level shit right here...
  # for some reason some parameters output files have an extra space in the "array" and need to grab the 3rd vector position instead of 2nd
  if(is.na(beta_inf)) {
    beta_inf <- as.numeric(unlist(unlist(unlist(unlist(readLines(path)[4] %>% str_split("array"))[2] %>% str_split(" "))[3] %>% str_split("]"))[1])[1])
  }  
  
  gamma_inf[i, 1] <- alpha_inf
  gamma_inf[i, 2] <- beta_inf
}

tbl_dfe <- cbind.data.frame(tbl, gamma_inf)

molten_gamma <- pivot_longer(tbl_dfe, cols = c("alpha", "beta"), names_to = "parameter")

gamma_dfe_plot <- ggplot(tbl_dfe, aes(x = alpha, y = beta))
gamma_dfe_plot <- gamma_dfe_plot + geom_point(size = 4,  alpha = 1, shape = 1) + theme_bw() 
gamma_dfe_plot <- gamma_dfe_plot + scale_shape_manual(values = c(1,2))
gamma_dfe_plot <- gamma_dfe_plot + facet_grid(a~b)
gamma_dfe_plot <- gamma_dfe_plot + labs(title = NULL, x = "Alpha", y = "Beta") 
gamma_dfe_plot <- gamma_dfe_plot + theme(text = element_text(size = 14), 
                                         legend.title = element_blank(),
                                         axis.title.x = element_text(size = 16),
                                         axis.title.y = element_text(size = 16))
gamma_dfe_plot <- gamma_dfe_plot + geom_point(x = alpha_sim, y = beta_sim, shape = 8, col = 'red', size = 4)
ggsave("gamma_dfe_scenarios.pdf", gamma_dfe_plot, device = "pdf", width = 8, height = 8)


tbl_dfe$s <- tbl_dfe$alpha * tbl_dfe$beta * 2

gamma_s_plot <- ggplot(tbl_dfe, aes(y = s)) + facet_grid(a~b)
gamma_s_plot <- gamma_s_plot + geom_point(shape = 1, size = 4, alpha = 1) + theme_bw() 
gamma_s_plot <- gamma_s_plot + geom_hline(aes(yintercept = -mean_sim), color = "red", linetype = "dashed", size = 0.5)
gamma_s_plot <- gamma_s_plot + labs(title = NULL, x = "dominance", y = "mean(|s|)") 
gamma_s_plot <- gamma_s_plot + theme(text = element_text(size = 14), 
                                     legend.title = element_blank(),
                                     axis.title.x = element_text(size = 16),
                                     axis.title.y = element_text(size = 16))
ggsave("mean_s_DFE_scenarios.pdf", gamma_s_plot, device = "pdf", width = 8, height = 8)

gamma_inf$var_s <- gamma_inf$alpha * (gamma_inf$beta ^ 2)

gamma_var_s_plot <- ggplot(gamma_inf, aes(x = dominance, y = var_s)) + facet_grid(demography~recombination)
gamma_var_s_plot <- gamma_var_s_plot + geom_point(shape = 1, size = 4,  alpha = 1) + theme_bw() 
gamma_var_s_plot <- gamma_var_s_plot + geom_hline(aes(yintercept = var_sim), color = "red", linetype = "dashed", size = 0.5)
gamma_var_s_plot <- gamma_var_s_plot + labs(title = NULL, x = "dominance", y = "var(s)") 
gamma_var_s_plot <- gamma_var_s_plot + theme(text = element_text(size = 14), 
                                             legend.title = element_blank(),
                                             axis.title.x = element_text(size = 16),
                                             axis.title.y = element_text(size = 16))
ggsave("var_s_DFE_scenarios.pdf", gamma_var_s_plot, device = "pdf", width = 8, height = 8)


gamma_inf <- as.data.frame(matrix(nrow = 20, ncol = 2))
names(gamma_inf) <- c("alpha", "beta")

idx <- c(11:20, 101:110)

for(i in 1:length(idx)) {
  
  id <- idx[i]
  path <- paste("no_epistasis/script_", id, "/fitdadi_inferred_DFE.txt", sep = "")
  
  alpha_inf <- as.numeric(unlist(unlist(unlist(readLines(path)[4] %>% str_split("array"))[2] %>% str_split(" "))[1] %>% str_split("\\["))[2])
  beta_inf <- as.numeric(unlist(unlist(unlist(readLines(path)[4] %>% str_split("array"))[2] %>% str_split(" "))[2] %>% str_split("]"))[1])
  
  # some next level shit right here...
  # for some reason some parameters output files have an extra space in the "array" and need to grab the 3rd vector position instead of 2nd
  if(is.na(beta_inf)) {
    beta_inf <- as.numeric(unlist(unlist(unlist(unlist(readLines(path)[4] %>% str_split("array"))[2] %>% str_split(" "))[3] %>% str_split("]"))[1])[1])
  }  
  
  gamma_inf[i, 1] <- alpha_inf
  gamma_inf[i, 2] <- beta_inf
}

gamma_inf$rec_rate <- c(rep("high_rec", 10), rep("low_rec", 10))

molten_gamma <- pivot_longer(gamma_inf, cols = c("alpha", "beta"), names_to = "parameter")

gamma_dfe_plot <- ggplot(gamma_inf, aes(x = alpha, y = beta))
gamma_dfe_plot <- gamma_dfe_plot + geom_point(size = 4,  alpha = 1, shape = 1) + theme_bw() 
gamma_dfe_plot <- gamma_dfe_plot + facet_wrap(~rec_rate)
gamma_dfe_plot <- gamma_dfe_plot + labs(title = NULL, x = "Alpha", y = "Beta") 
gamma_dfe_plot <- gamma_dfe_plot + theme(text = element_text(size = 14), 
                                         legend.title = element_blank(),
                                         axis.title.x = element_text(size = 16),
                                         axis.title.y = element_text(size = 16))
gamma_dfe_plot <- gamma_dfe_plot + geom_point(x = alpha_sim, y = beta_sim, shape = 8, col = 'red', size = 4)
ggsave("gamma_dfe_rec.pdf", gamma_dfe_plot, device = "pdf", width = 8, height = 8)



