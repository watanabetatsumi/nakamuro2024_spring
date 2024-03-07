
# 依存関係 --------------------------------------------------------------------

library(dplyr)
library(fixest)

# 推計-1 --------------------------------------------------------------------

#単回帰（出生順位と成績の関係）
#ただし、母親の教育水準が低い⇒子供たくさん⇒教育を受けれない。平均的に出生順位が低いと成績が下がるように見えてしまうなどのバイアスが考えられる。
#⇒固定効果（motherIDはintであることに注意）←やっぱり大丈夫この前はyear(int)固定効果を取ってた
#ほか、子供の年齢や調査年度、性別も考慮しなきゃいけない。あるいは兄弟の数、末っ子だからだめなのかとか考えられる
#使うデータの次元が増えると、必要なデータ数も増えることに注意
#その年の問題が簡単だった説はある⇒時点効果
# Yi,h,t = αBirthOrderi,h + εi,h,t

# --------------------------
# | score    | birth_order |
# --------------------------
# |int       | int         |



# 単回帰-数学 ------------------------------------------------------------------

file_path <- "./plot_pngs/reg_table1_math.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

model_1_math <- fixest::feols(`piat_math` ~ birth_order ,data =  df_cleaned)
#modelsummaryよりmsummaryの方が便利かも
msummary(model_1_math,
         output = file_path,
         title = "成績への出生順位の影響（数学）"
)

# modelsummary(model_1_math,title = "成績への出生順位の影響（数学）")

# etableを使えば、texに出力できる (tex = t)
# p <- etable(model_1_math ,title = "成績への出生順位の影響（数学）") 


# 単回帰-語彙 ------------------------------------------------------------------

file_path <- "./plot_pngs/reg_table1_recog.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

model_1_recognition <- fixest::feols(`piat_recog` ~ birth_order ,data =  df_cleaned)
msummary(model_1_recognition,
         output = file_path,
         title = "成績への出生順位の影響（語彙）"
)
# modelsummary(model_1_recognition ,title = "成績への出生順位の影響（語彙）")
# p <- etable(model_1_recognition ,title = "成績への出生順位の影響（語彙）") 


# 単回帰-読解 ------------------------------------------------------------------


file_path <- "./plot_pngs/reg_table1_comp.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

model_1_comprehension <- fixest::feols(`piat_comp` ~ birth_order ,data=  df_cleaned)
msummary(model_1_comprehension,
         output = file_path,
         title = "成績への出生順位の影響（読解）"
)
# modelsummary(model_1_comprehension ,title = "成績への出生順位の影響（読解）")
# p <- etable(model_1_comprehension ,title = "成績への出生順位の影響（読解）") 



# 推計-2 --------------------------------------------------------------------

# Yi,h,t = αBirthOrderi,h + Xi,h,tβ + λt + λh + εi,h,t

# 変数一覧
# motherID
# childID
# birth_order
# sibling
# b_month
# b_year
# survey_year
# piat_math
# piat_recog
# piat_comp
# age
# ismale

# https://healthpolicyhealthecon.com/2016/09/08/regression-2/
# データが多いと、分母が多くなり、分散が過小評価され、統計的に有意な結果が出やすくなる⇒クラスターロバスト標準誤差
# クラスタリングしてそれぞれ回帰させるイメージ⇒固定効果としてとるのか、クラスタ―ロバスト標準誤差としてとるのか
#クラスタリングして、クラスター間の分散を過小評価させる。クラスター内の分散を見る、、、みたいな？
# ってことが、調査年度によってクラスタ―ロバスト標準誤差を見るべき？固定効果となにが違うの？


# 重回帰-数学 ------------------------------------------------------------------

file_path <- "./plot_pngs/reg_table2_math.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

model_2_math <- fixest::feols(`piat_math` ~ birth_order | `age` + `sibling` + `motherID` + `ismale` , data =  df_cleaned,cluster = "survey_year")
model_3_math <- fixest::feols(`piat_math` ~ birth_order | `age` + `sibling` + `survey_year` + `ismale` , data =  df_cleaned,cluster = "motherID")
# modelsummary(list(model_1_math,model_2_math,model_3_math),title = "成績への出生順位の影響（数学）")
# p <- etable(model_2_math ,title = "成績への出生順位の影響（数学）")

msummary(
        list(model_1_math,model_2_math,model_3_math),
         output = file_path,
         title = "成績への出生順位の影響（数学）"
         )


# 重回帰-語彙 ------------------------------------------------------------------

file_path <- "./plot_pngs/reg_table2_recog.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

model_2_recognition <- fixest::feols(`piat_recog` ~ birth_order | `age` + `sibling` + `motherID` + `ismale` , data =  df_cleaned,cluster = "survey_year")
# modelsummary(list(model_1_recognition,model_2_recognition) ,title = "成績への出生順位の影響（語彙）")

msummary(
  list(model_1_recognition,model_2_recognition),
  output = file_path,
  title = "成績への出生順位の影響（語彙）"
)


# 重回帰-読解 ------------------------------------------------------------------

file_path <- "./plot_pngs/reg_table2_comp.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

model_2_comprehension <- fixest::feols(`piat_comp` ~ birth_order | `age` + `sibling` + `motherID` + `ismale` , data =  df_cleaned,cluster = "survey_year")
# modelsummary(list(model_1_recognition,model_2_comprehension) ,title = "成績への出生順位の影響（読解）")
msummary(
  list(model_1_comprehension,model_2_comprehension),
  output = file_path,
  title = "成績への出生順位の影響（読解）"
)