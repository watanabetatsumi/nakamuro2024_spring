
# 依存関係 --------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(gtsummary)
library(ggplot2)
#.pngファイルをアウトプットするため
webshot::install_phantomjs()
#高画質に
library(magick)

# 基本統計量 --------------------------------------------------------------------

df_summary <- df_cleaned %>%
  # グループ化を解除
  ungroup()%>% 
  select(
    age,
    b_year,
    b_month,
    survey_year,
    piat_math,
    piat_recog,
    piat_comp,
  ) %>%
  rename(
    '年齢' = age,
    '誕生年' = b_year,
    '誕生月' = b_month,
    '調査年度' = survey_year,
    'PIATスコア(数学)' = piat_math,
    'PIATスコア(語彙)' = piat_recog,
    'PIATスコア(読解)' = piat_comp
  )

file_path <- "./plot_pngs/summary_table.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

summary <- datasummary(All(df_summary) ~ (N + Mean + SD),
                       data = df_summary,
                       na.rm = TRUE,
                       fmt = 3,
                       output = file_path,
)


# df_summary <- datasummary(All(df_summary) ~ (N + Mean + SD),
#                        data = df_summary,
#                        na.rm = TRUE,
#                        fmt = 3,
#                        output = "data.frame"
# )
# 
# colnames(df_summary) <- c('','サンプルサイズ', '平均','標準偏差')
# 
# library(htmlTable)
# 
# # データフレームをHTMLに変換
# html_table <- htmlTable(df_summary)
# 
# # HTMLの出力
# print(html_table)
# 

# ヒストグラム ------------------------------------------------------------------

# qplot(b_year, data=df_cleaned, facets = birth_order ~ ., fill=birth_order)

# ヒストグラムを作成
# https://stats.biopapyrus.jp/r/ggplot/geom_histogram.html
# birth_orderを因子型に変換(アンケートと一緒:1大変満足,2満足...みたいな)結局してもしなくてもいい、ヒストグラムをどう色分けするか次第
# df_cleaned$birth_order <- as.factor(df_cleaned$birth_order)

hist <- ggplot(df_cleaned, aes(x=b_year, group=birth_order, fill =birth_order)) + 
  geom_histogram(position="stack", alpha=0.7, binwidth=1)+
  labs(x="出生年", y="人数", fill="出生順位")+
  scale_fill_gradient(low = "DarkBlue", high = "white")


file_path <- "./plot_pngs/hist.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

#こうやってplot関数を挟むことで出力するファイルディスクリプタを変えることが出来る
png(file_path)
  plot(hist)
dev.off()


# alphaは透過率
# postion = stackは上に重ねるオプション
# bandwithはヒストグラムのx軸の幅
# groupでヒストグラムを別々に表示。fillで色分け


# 折れ線グラフ ------------------------------------------------------------------

# 出生順別(groupby)、各テストの年齢別(groupby)、平均点の推移を折れ線グラフにしたい
# ------------------------------------
# | score    | age　　 | birth_order |
# ------------------------------------
# |int       | int     | int         |


score_df <- df_cleaned %>% 
  ungroup() %>%
  select(
    piat_math,
    piat_recog,
    piat_comp,
    age,
    birth_order
  ) %>% 
  group_by(birth_order,age) %>%
  summarise(
    math = mean(piat_math, na.rm = TRUE),
    recognition = mean(piat_recog, na.rm = TRUE),
    comprehension = mean(piat_comp, na.rm = TRUE)
  ) %>% 
  ungroup()
#na.rmがないとnaを含めて計算してしまうからダメっぽい
#summariseがあることで、代表値が一つだけ（一行分だけ）取得できる


# https://rcatalogue.com/plot/basic-plot/line-chart/
#birth_orderをfactor型にキャスト
score_df$birth_order <- as.factor(score_df$birth_order)

#数学
math_bar <- ggplot(score_df, aes(x=age, y=math, group=birth_order,linetype=birth_order)) +
  geom_line() +
  labs(x="年齢", y="点数（数学）", linetype="出生順位")

file_path <- "./plot_pngs/math_bar.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

#こうやってplot関数を挟むことで出力するファイルディスクリプタを変えることが出来る
png(file_path)
plot(math_bar)
dev.off()


#語彙
recog_bar <- ggplot(score_df, aes(x=age, y=recognition, group=birth_order,linetype=birth_order)) +
  geom_line() +
  labs(x="年齢", y="点数（語彙）", linetype="出生順位")

file_path <- "./plot_pngs/recog_bar.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

#こうやってplot関数を挟むことで出力するファイルディスクリプタを変えることが出来る
png(file_path)
plot(recog_bar)
dev.off()

#読解力
comp_bar <- ggplot(score_df, aes(x=age, y=comprehension, group=birth_order,linetype=birth_order)) +
  geom_line() +
  labs(x="年齢", y="点数（読解）", linetype="出生順位")

file_path <- "./plot_pngs/comp_bar.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

#こうやってplot関数を挟むことで出力するファイルディスクリプタを変えることが出来る
png(file_path)
plot(comp_bar)
dev.off()


# 散布図・回帰直線 ----------------------------------------------------------------
# -----------------------------------------------
# | order_1  | order_2 | survey_year | motherID |
# -----------------------------------------------
# |math      | math    | int         | int      |
# survey_yearでfilterをかけて、散布図を書く

#第一子分だけのデータ(主キーはmotherID)
scat_df_1 <- df_cleaned %>% 
  ungroup()%>% 
  select(
    motherID,
    survey_year,
    piat_math,
    birth_order
  ) %>% 
  filter(`survey_year` == 2000) %>% 
  filter(`birth_order` == 1) %>% 
  select(
    -survey_year,
    -birth_order
  )
scat_df_1 <- dplyr::rename(scat_df_1, "math_1" = 2)

#第二子分だけのデータ(主キーはmotherID)
scat_df_2 <- df_cleaned %>% 
  ungroup()%>% 
  select(
    motherID,
    survey_year,
    piat_math,
    birth_order
  ) %>% 
  filter(`survey_year` == 2000) %>% 
  filter(`birth_order` == 2) %>% 
  select(
    -survey_year,
    -birth_order
  )
scat_df_2 <- dplyr::rename(scat_df_2, "math_2" = 2)

#motherIDに紐づけて、内部結合
scat_df <- inner_join(scat_df_1, scat_df_2, by = 'motherID')

#散布図・回帰直線
scat_plot <- ggplot(scat_df, aes(x=math_1, y=math_2)) +
  geom_point() +
  geom_smooth(method="lm",formula='y~x') +
  labs(x="第一子", y="第二子")

file_path <- "./plot_pngs/scat_plot.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

#こうやってplot関数を挟むことで出力するファイルディスクリプタを変えることが出来る
png(file_path)
plot(scat_plot)
dev.off()
