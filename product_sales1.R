library(dplyr)
library(ggplot2)
library(corrplot)
library(factoextra)
library(NbClust)
library(cluster)
library(psych)
library(broom)

Sys.setenv(R_MAX_MEM_SIZE='8G')
Sys.getenv("R_PROFILE_USER")


R_MAX_MEM_SIZE='8G'

#XỬ LÝ DỮ LIỆU
str(product_sales)
summary(product_sales)
unique(product_sales$week)
unique(product_sales$sales_method)
unique(product_sales$customer_id)
unique(product_sales$nb_sold)
unique(product_sales$revenue)
unique(product_sales$years_as_customer)
unique(product_sales$nb_site_visits)
unique(product_sales$state)

product_sales$revenue <- ifelse(
  is.na(product_sales$revenue),
  round(mean(product_sales$revenue, na.rm = TRUE), 2),
  product_sales$revenue)
product_sales$sales_method <- recode(product_sales$sales_method,"Email"="Email",
                                     "Email + Call"="Email + Call",
                                     "em + call"="Email + Call","email"="Email")
product_sales_final <- product_sales[!(product_sales$years_as_customer %in% c(63, 47)), ]

# PHÂN TÍCH KHÁM PHÁ
hist(product_sales_final$week,col="brown",main="Histogram of week",border = "white",breaks=6,right = FALSE)
hist(product_sales_final$years_as_customer,col="brown",main="Histogram of Years as Customer",border = "white",breaks=40,right = FALSE)
ggplot(product_sales_final,aes(x=sales_method,fill=sales_method)) + 
  geom_bar() + 
  labs(title = "Histogram of Sales Method")

# PHÂN TÍCH THĂM DÒ
ggplot(product_sales_final, aes(x = sales_method, y = revenue,fill=sales_method)) +
  geom_col() +
  labs(title = "Bar Chart the Revenue to Sales Method", x = "Sales Method", y = "Revenue") +
  theme_minimal()

ggplot(product_sales_final, aes(x = sales_method, y = nb_sold,fill=sales_method)) +
  geom_col() +
  labs(title = "Bar Chart the number of purchases to Sales Method", x = "Sales Method", y = "The number of purchases") +
  theme_minimal()

ggplot(product_sales_final, aes(x=revenue,y=state,color=state,fill=state)) + 
  geom_col() +
  labs(titel = "Scallet Chart the Revenue to State", x="Revevue", y="State") +
  theme_minimal()

ggplot(product_sales_final, aes(x=nb_sold,y=state,color=state,fill=state)) + 
  geom_col() +
  labs(titel = "Scallet Chart the number of purchases to State", x="Sales", y="State") +
  theme_minimal()

#Số thời gian dành cho customer ở Call và Email + Call
product_sales_time <- product_sales_final %>% 
  mutate(times = case_when(
    sales_method == "Call" ~ 30,
    sales_method == "Email + Call" ~ 10,
    TRUE ~ 0 ))

product_sales_time %>%  group_by(sales_method)  %>% summarize(times=sum(times))

#PHÂN TÍCH PCA
product_sales_clust_new <- product_sales_final %>% select(week, nb_sold, years_as_customer, nb_site_visits)
summary(product_sales_clust_new)
sum(is.na(product_sales_clust_new))


# ĐO LƯỜNG PCA CỦA CÁC BIẾN TƯƠNG QUAN
pca=princomp(~week+nb_sold +nb_site_visits,data=product_sales_clust_new)
summary(pca)
loadings(pca)
head(pca$scores)
var(pca$scores)
fviz_eig(pca)

# BIỂU ĐỒ PCA
fviz_pca_ind(pca,col.ind = "cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_ind(pca,col.var = "contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
 
#PHÂN CỤM KHÁCH HÀNG
clust_data <- product_sales_clust_new %>% select(week, nb_sold,nb_site_visits)
clust_new = scale(clust_data)
cl_new=get_clust_tendency(clust_new,n=50,graph = TRUE)
cl_new$hopkins

# ELBOW METHOD
fviz_nbclust(clust_new, 
             kmeans,
             method='wss') + labs(subtitle = "Elbow method")
# GAP STATISTIC METHOD
gap=clusGap(clust_new,FUN=kmeans,nstart=10,K.max=10,B=50)
fviz_gap_stat(gap)

# CHỌN K=2, VẼ BIỂU ĐỒ PHÂN CỤM
df_clust <- kmeans(clust_new, 
                   centers = 2, 
                   nstart = 10)
summary(df_clust)

fviz_cluster(df_clust, 
             clust_new, 
             palette = c("#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

tidy(df_clust)
augment(df_clust, clust_new) %>%  
  ggplot(aes(week,nb_sold,
             color =.cluster)) + 
  geom_point(alpha = 2) + 
  ggtitle("Clustering") + 
  theme_minimal()


# ĐƯA LẠI VỀ DỮ LIỆU GỐC
clusters <- df_clust$cluster
data_clust_final <- as.data.frame(clusters) 
data_clust_final <- data_clust_final %>% mutate(rowid = row_number())
clust_new_final <- as.data.frame(clust_new)
clust_new_final <- clust_new_final %>% mutate(rowid = row_number())
clust_data_final <- clust_data %>% mutate(rowid=row_number())
clust_final <- merge(data_clust_final,clust_new_final,by="rowid")
clust_final_real <- merge(data_clust_final,clust_data_final,by="rowid")

ggplot(clust_final_real,aes(clusters,nb_sold)) + geom_point()

## DOANH THU
cor_customer <- cor(product_sales_final[, c("nb_sold", "nb_site_visits", "years_as_customer")])
corrplot(cor_customer, 
         method = "square", 
         type = "lower", 
         diag = FALSE, 
         tl.cex = 0.7, new = TRUE, addCoef.col="black")