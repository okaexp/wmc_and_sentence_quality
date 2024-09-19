#最終更新: 2024年8月29日 23:06
# # structure_based_on_exに基づくもの(JCSSはこれ)にしている

# 解析方針 ----
# 1. 解析用データの作成（dat_aggregate）：ID、ターゲットキー（後述）を列に持つ
# 共通のキー: ID(demographic, rst, anno_yamakawaa, anno_hayes, anno_rating)

# dat_demographic:
# - Sex
# - Age
# - AcademicDegree(1~5)
#
# dat_rst: 
# - trial_id_score: RSTの平均正再（0~5.0）
#
# dat_hayes:
# - FreqM, FreqR, FreqO: 参加者ごとのMROラベルの頻度
#
# dat_yamakawa:
# - FreqAssociation, FreqAdd, FreqConcrete: 参加者ごとの連想/付加/具体化の頻度
#
# dat_rating:
# - rt: 執筆試行の開始から終わりまでの時間(秒)
# - mistake, composition, ease: 参加者の自己評価（誤字脱字の少なさ, 構成の良さ, 執筆困難度）
# - average_holistic/selection/coherence/rhetoric: 第一著者とアノテータ(hosoi)の平均評定値
#
# dat_writing_process:
# - planning_rt: 試行開始時間 ~ 最初の文字を書くまでの時間
# - writing_rt: 最初の文字を書くまでの時間 ~ いったん最後まで書ききる時間
# - revision_rt: 一旦さいごまで書き切る時間 ~ trial終了の時間
# -> 今回はアウト（2024/2/4）。データが適切に取れていない。

# 2. 相関分析

library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)
library(scales)
library(stringr)

# 1. 解析用データの作成（dat_aggregate）----

# 各種データの読み込み ----
dat_rst <- read.xlsx("../rst/mean_rst_score.xlsx")
dat_yamakawa <- read.xlsx("../anno_yamakawa/yamakawa_score_freq.xlsx")
dat_hayes <- read.xlsx("../anno_hayes/hayes_score_freq.xlsx")
dat_rating <- read.xlsx("../anno_rating/rating_score_average.xlsx")
dat_demographic <- read.xlsx("../demographic/demographic_valid_drop_dup.xlsx")
dat_ts <- read.xlsx("../text_structure_hayes/hayes_text_structure_after_anno.xlsx")

#2024/9/19: 追記
#str_remove_all(string, pattern) -> str_length(string)で、
#FinalRatingListのMROの数を取得（=>文章数）
#and
#str_length(string)でtrial_textの長さを取得(=> 文長)
dat_ts <- dat_ts %>%
  dplyr::mutate(text_length = stringr::str_length(trial_text),
                sentence_num = str_length(str_remove_all(FinalRatingList, "-")))

# ターゲットキーのみを集計したデータフレーム(dat_aggregate)を作る ----
# dat_rst: そのまま(ID, trial_id_score)
dat_rst_agg <- dat_rst %>%
  dplyr::mutate(ID = as.factor(ID))

# dat_yamakawa:
dat_yamakawa_agg <- dat_yamakawa %>%
  dplyr::select(-trial_text) %>%
  dplyr::mutate(ID = as.factor(ID))

# dat_hayes: 
dat_hayes_agg <- dat_hayes %>% 
  dplyr::select(-trial_text) %>%
  dplyr::mutate(ID = as.factor(ID))

# dat_rating
dat_rating_agg <- dat_rating %>%
  dplyr::mutate(ID = as.factor(ID))

# dat_demographic
dat_demographic_agg <- dat_demographic %>%
  dplyr::mutate(ID = as.factor(ID))

# dat_text_structure
dat_text_structure <- dat_ts %>%
  dplyr::mutate(ID = as.factor(ID), structure_oka = as.factor(structure_oka)) %>%
  dplyr::select(-trial_text, -FinalRatingList, -structure_oka, -structure_based_on_ex)


#追記: 20240205
#年齢と性別を確認するためだけのデータの作成
dat_age <- dat_demographic_agg %>% 
  dplyr::select(age)  %>% 
  summarise(m_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

dat_sex <- dat_demographic_agg %>% 
  dplyr::select(sex) %>% 
  group_by(sex) %>% 
  summarise(count = n())

# 解析用データの作成（dat_aggregate）
dat_aggregate <- dat_rst_agg %>%
  dplyr::inner_join(dat_demographic_agg) %>%
  dplyr::inner_join(dat_yamakawa_agg) %>%
  dplyr::inner_join(dat_hayes_agg) %>%
  dplyr::inner_join(dat_rating_agg) %>%
  dplyr::inner_join(dat_text_structure)

# 2. 相関分析----
source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp") #mycorを持ってくる

#以下、山川の追試の相関係数
#-> SumWAISVocabScoreとSumSSTFinalRatingは.36
dat_replicate_yamakawa <- dplyr::select(dat_aggregate, -ID, -FreqM, -FreqR, -FreqO, -mistake, -composition, -ease)
mycor(1:8, dat_replicate_yamakawa, latex = FALSE) #いつもの#相関係数ようのデータ

dat_yamakawa_and_hayes <- dplyr::select(dat_aggregate, -ID, -mistake, -composition, -ease, -sex, -age, -academic)
mycor(1:12, dat_yamakawa_and_hayes, latex = FALSE) #いつもの#相関係数ようのデータ

#すべての客観指標
dat_objective_correlation <- dplyr::select(dat_aggregate, -ID, -mistake, -composition, -ease)
mycor(1:17, dat_objective_correlation, latex = FALSE) #いつもの#相関係数ようのデータ

# 3. 相関表とヒストグラムの可視化 ----
#psych::pairs.panels(dat_replicate_yamakawa)

# 4. 記述統計量 ----
#記述統計量
describe(dat_aggregate)
describe(dat_objective_correlation[1:17])

# 5. hayesのtext structureとWM容量、holisticの関係
# ref: https://www2.kobe-u.ac.jp/~bunji/files/lecture/MVA/mva-04-preanalysis.pdf
# p.10以降が参考になるか

# dat_text_structure
# 以下のデータのために整形
dat_text_structure <- dat_ts %>%
  dplyr::mutate(ID = as.factor(ID), structure_based_on_ex = as.factor(structure_based_on_ex)) %>%
  dplyr::select(-trial_text, -FinalRatingList, -structure_oka)

dat_aggregate <- dat_rst_agg %>%
  dplyr::inner_join(dat_demographic_agg) %>%
  dplyr::inner_join(dat_yamakawa_agg) %>%
  dplyr::inner_join(dat_hayes_agg) %>%
  dplyr::inner_join(dat_rating_agg) %>%
  dplyr::inner_join(dat_text_structure)

#structure_ex: hayes (2011)のサンプルに基づくもの
#平均値と標準偏差
# structure_based_on_exに基づくもの(JCSSはこれ)
rst_by_structure_ex <- dat_aggregate %>%
  dplyr::group_by(structure_based_on_ex) %>%
  dplyr::reframe(mean_holistic=mean(average_holistic),
                 mean_rt=mean(rt),
                 mean_rsts=mean(trial_id_score),
                 sd_holistic=sd(average_holistic),
                 sd_rt=sd(rt),
                 sd_rsts=sd(trial_id_score))

# anovakunで分析 ----
source("./anovakun_489.txt")
library(forcats)

#trial_id: structure_ex
dat_rsts_anova = dat_aggregate[, c(1, 20, 2)]
anovakun(dat_rsts_anova, "As", long=T, peta=T)

#average_holistic: structure_ex
dat_holistic_anova = dat_aggregate[, c(1, 20, 16)]
anovakun(dat_holistic_anova, "As", long=T, peta=T)


# 6. 以下、グラフ ----
#holisticのグラフ
#棒グラフを書く
dat_holistic_by_structure_graph_ex <- rst_by_structure_ex %>%
  dplyr::select(structure_based_on_ex, mean_holistic, sd_holistic)

#ラベル名を変更
dat_holistic_by_structure_graph_ex <- dat_holistic_by_structure_graph_ex %>% 
  dplyr::mutate(structure = factor(structure_based_on_ex, levels = c("Flexible", "Fixed", "Elaboration"))) %>%
  dplyr::select(-structure_based_on_ex)

#Mr.Unadonを参考にしながら、棒グラフを書く
#https://mrunadon.github.io/ThesisPlot/
#https://qiita.com/takato_oyama/items/5ebc57edb3e5d74ef86e
#quartz(type = "pdf", file = "../result/barplot_mean_holistic_rating_by_structure_ex.pdf")
ggplot()+theme_set(theme_classic(base_size = 12,base_family="Hiragino Mincho Pro W3"))
g<-ggplot(dat_holistic_by_structure_graph_ex, aes(y=mean_holistic, x=structure)) #irisOderedのデータフレームを使う。y軸とｘ軸の変数を指定。
g <- g + geom_bar(position = "dodge", stat = "identity")#平均値を表したbarの準備
g <- g + geom_errorbar(aes(ymin = mean_holistic - sd_holistic, ymax = mean_holistic + sd_holistic), width = .2, position = position_dodge(.9))#標準誤差
g <- g + theme(axis.title.y = element_text(angle=0, vjust=0.5, size=20),
               axis.text.y = element_text(size=20),
               axis.title.x = element_text(size=20),
               axis.text.x = element_text(size = 20))
g <- g + xlab("文構造")
g <- g + scale_y_continuous(name="文\n章\nの\n品\n質\nの\n平\n均\n値", breaks=seq(0,7), limits=c(1,7), oob=oob_keep)
plot(g)
#dev.off()

#RSTの平均正再生グラフ
#棒グラフを書く
dat_rsts_by_structure_graph_ex <- rst_by_structure_ex %>%
  dplyr::select(structure_based_on_ex, mean_rsts, sd_rsts)

#ラベル名を変更
dat_rsts_by_structure_graph_ex <- dat_rsts_by_structure_graph_ex %>% 
  dplyr::mutate(structure = factor(structure_based_on_ex, levels = c("Flexible", "Fixed", "Elaboration"))) %>%
  dplyr::select(-structure_based_on_ex)

#Mr.Unadonを参考にしながら、棒グラフを書く
#https://mrunadon.github.io/ThesisPlot/
#quartz(type = "pdf", file = "../result/barplot_rsts_by_structure_ex.pdf")
#ggplot()+theme_set(theme_classic(base_size = 12,base_family="Hiragino Mincho Pro W3"))
g<-ggplot(dat_rsts_by_structure_graph_ex, aes(y=mean_rsts, x=structure)) #irisOderedのデータフレームを使う。y軸とｘ軸の変数を指定。
g <- g + geom_bar(position = "dodge", stat = "identity")#平均値を表したbarの準備
g <- g + geom_errorbar(aes(ymin = mean_rsts - sd_rsts, ymax = mean_rsts + sd_rsts), width = .2, position = position_dodge(.9))#標準誤差
g <- g + theme(axis.title.y = element_text(angle=0, vjust=0.5, size=20),
               axis.text.y = element_text(size=20),
               axis.title.x = element_text(size=20),
               axis.text.x = element_text(size = 20))
g <- g + xlab("文構造")
g <- g + scale_y_continuous(name="R\nS\nT\nの\n平\n均\n値", breaks=seq(0,5), limits=c(0,5), oob=oob_keep)
plot(g)
#dev.off()

