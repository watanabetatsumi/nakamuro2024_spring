
# 依存関係 --------------------------------------------------------------------

library(tidyr)

# データ整形 -------------------------------------------------------------------

df_1 <- read.csv("./data/outcomes_1.csv")
df_2 <- read.csv("./data/outcomes_2.csv")
df_3 <- read.csv("./data/outcomes_3.csv")
df_4 <- read.csv("./data/outcomes_4.csv")
df_5 <- read.csv("./data/outcomes_5.csv")
df_chi <- read.csv("./data/children.csv")
df_moth <- read.csv("./data/mothers.csv")

# データフレームを結合
all_df_list <- list(df_1,df_2,df_3, df_4, df_5)
df_alloutcomes <- bind_rows(all_df_list)

# 内部結合(children + outcome)
joined_children_df <- inner_join(df_chi, df_alloutcomes, by = 'CPUBID')

# 年齢
joined_children_df$age =  joined_children_df$year - joined_children_df$CYRB

# 男ダミー
joined_children_df <- joined_children_df %>% 
  mutate(
    ismale = ifelse(CSEX == 1,1,0),
  )

# mother.csvを整然データにする
# childIDが主キーになる
 # ------------------------------------
 # | motherID | childID | birth_order|
 # ------------------------------------
 # |int       | int     | int         |
# こんな感じで


df_moth <- df_moth %>%  
  rename(
    'motherId' = CASEID_1979,
  )

# 地道にselectを使って整形。のち連結。
df_order1 <- select(df_moth,
                    motherId,
                    CPUBID1,
)
df_order1 <- na.omit(df_order1)
colnames(df_order1) <- c("motherID", "childID")
df_order1$birth_order <- 1


df_order2 <- select(df_moth,
                    motherId,
                    CPUBID2,
)
df_order2 <- na.omit(df_order2)
colnames(df_order2) <- c("motherID", "childID")
df_order2$birth_order <- 2


df_order3 <- select(df_moth,
                    motherId,
                    CPUBID3,
)
df_order3 <- na.omit(df_order3)
colnames(df_order3) <- c("motherID", "childID")
df_order3$birth_order <- 3


df_order4 <- select(df_moth,
                    motherId,
                    CPUBID4,
)
df_order4 <- na.omit(df_order4)
colnames(df_order4) <- c("motherID", "childID")
df_order4$birth_order <- 4


df_order5 <- select(df_moth,
                    motherId,
                    CPUBID5,
)
df_order5 <- na.omit(df_order5)
colnames(df_order5) <- c("motherID", "childID")
df_order5$birth_order <- 5


df_order6 <- select(df_moth,
                    motherId,
                    CPUBID6,
)
df_order6 <- na.omit(df_order6)
colnames(df_order6) <- c("motherID", "childID")
df_order6$birth_order <- 6

# 連結
all_df_list <- list(df_order1, df_order2, df_order3, df_order4, df_order5, df_order6)
df_cleaned <- bind_rows(all_df_list)

joined_children_df <- select(joined_children_df,
  -CSEX
)

joined_children_df <- joined_children_df %>% 
  rename(
    childID = CPUBID,
    b_month = CMOB,
    b_year = CYRB,
    survey_year = year
  )

df_cleaned <- df_cleaned %>% 
  group_by(motherID) %>% 
  mutate(
    sibling = max(birth_order)
  )

df_cleaned <- inner_join(df_cleaned, joined_children_df, by = 'childID')

df_cleaned<- df_cleaned %>% 
  filter(`age` <=12 & `age` >= 6) 