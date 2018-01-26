set.seed(1680) # for reproducibility
library(ggplot2)
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
df=df[-1,]
data = df[df$provider_type=='Obstetrics/Gynecology' && df$nppes_provider_country=='US',]
data1= sqldf('select hcpcs_code, sum (line_srvc_cnt) as ct from data group by hcpcs_code')
data1 = sqldf('select * from data1 order by ct desc')
#pick top 20 most frequently used hcpcs:  99213 G0101 99214 J0897 Q0091 J1071 81002 99212 G0328 J1644 76830 J9267 99204 G0202
#  J0585 99203 77052 81003 J1050 82270
data3=data[data$hcpcs_code=='G0101',]
data4=data3[data3$nppes_provider_gender!='',]
ggplot(data4,aes(x=average_submitted_chrg_amt, fill=nppes_provider_gender)) + geom_density(alpha=0.25)
ggplot(data4,aes(x=average_submitted_chrg_amt, fill=nppes_provider_gender)) + geom_histogram(alpha=0.25)
ggplot(data4, aes(x=nppes_provider_state, y = average_submitted_chrg_amt, fill =nppes_provider_state )) + geom_boxplot()

#different place of services show slightly different average submitted charge amt 
ggplot(data4, aes(x=place_of_service, y = average_submitted_chrg_amt, fill =place_of_service )) + geom_boxplot()
#medicare participation or not significantly affect average submmited chr_amt for the same service
ggplot(data4, aes(x=medicare_participation_indicator, y = average_submitted_chrg_amt, fill =medicare_participation_indicator )) + geom_boxplot()

data_cluster = subset(data4,select=c(npi,nppes_provider_gender,nppes_provider_state,medicare_participation_indicator,
                                     line_srvc_cnt,average_submitted_chrg_amt))

#get gower_dist which provides a way to calculate distance between discrete variables 
gower_dist <- daisy(data_cluster[, 1:4],
                    metric = "gower",
                    type = list(logratio = 3))

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}


# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

#choose K = and apply pam clustering 
pam_fit <- pam(gower_dist, diss = TRUE, k = 6) 

#result interpretation 
data_cluster[pam_fit$medoids, ]

pam_results <- data_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
