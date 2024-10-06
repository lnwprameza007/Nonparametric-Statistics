data <- read.csv('C:/Users/Noppon/Desktop/Prame/Non-para/Final/epl_clean.csv')
data

mci <- data[data$club == 'Manchester+City',]
mci

mun <- data[data$club == 'Manchester+United',]
mun

wba <- data[data$club == 'West+Brom',]
wba

head(data, 10)

old <- data[data$age >= 27, ]
old 
young <- data[data$age < 27, ]
young

wilcox.test(old$fpl_value, young$fpl_value, alternative = 'greater')

amr = data[data$region_name == 'America',]
eu = data[data$region_name == 'Europe',]
eng = data[data$region_name == 'England',]
other = data[data$region_name == 'Other',]

combined_data <- data.frame(
  page_views = c(amr$page_views, eu$page_views, eng$page_views, other$page_views),
  region = factor(c(rep('america', nrow(amr)), rep('europe', nrow(eu)), rep('england', nrow(eng)), rep('other', nrow(other))))
)

kruskal.test(page_views ~ region, data = combined_data)

attck <- data[data$position_cat == '1', ]
back <- data[data$position_cat == '3', ]

wilcox.test(attck$market_value, back$market_value, alternative = 'less')

ks.test(data$market_value[data$big_club == 1], 
        data$market_value[data$big_club == 0])

cor.test(data$market_value, data$age, method = "spearman")

table_bigclub_region <- table(data$region, data$big_club)

chisq.test(table_bigclub_region)