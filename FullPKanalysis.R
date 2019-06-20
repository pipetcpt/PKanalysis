#### PK analysis ######
library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

a <- read.csv("102dapa.csv", header = T, stringsAsFactors = F)
head(a)
names(a)
colnames(a) = c("prd", "id", "0", "0.5", "1", "1.5", "2", "3", "4", "5", "6", "7", "8" , "10", "12", "24", "36", "48")
dim(a)

head(a)

### drop-out 및 na 제거
a <- a %>%
  filter_all(all_vars(. != "-"))
nrow(a)

### grp, trt, seq 지정
a$grp = substring(a$id, 1, 1)
num = table(a$grp, a$prd)
num
a$trt = c(rep("R", num[1,1]), rep("T", num[1,2]), rep("T", num[2,1]), rep("R", num[2,2]))

## 분산 투여 시 adm 지정하여 사용함
b = c(rep("1", 5), rep("2", 8), rep("1", 5), rep("2", 9))   ## 여기는 visit date 확인해야 함
c = c(b, b)
a$adm = c


### Tidying data - tidyr package (Data 형태에 맞춰서 gather을 조절해준다)
a1 <- a %>%
  gather(time, dv, -c(prd, id, grp, trt, adm)) %>%
  arrange(prd, id, time)
str(a1)
a1$time <- as.numeric(a1$time)
head(a1)

### Removal of BLQ - Tmax 적당히 확인해서 기준점 정해준다
?mutate_all

a2 <- a1 %>%
  filter(time <= 6) %>%
  mutate_all(funs(str_replace(.,"BLQ", "0")))

a3 <- a1 %>%
  filter(time > 6) %>%
  mutate_all(funs(str_replace(., "BLQ", "")))

a4 <- rbind(a2, a3)
a4$dv = as.numeric(a4$dv)
a4$time = as.numeric(a4$time)

## Plotting


## color Reference를 dark grey로 하기 / 축 break 4 로 설정할

linecolor <- c("Black", "darkgrey")
posn <- position_dodge(0.2)

p <- a4 %>%
  group_by(trt, time) %>%
  summarise(con = mean(dv, na.rm = T), sd = sd(dv, na.rm = T)) %>%
  arrange(trt, time)

p$trt = as.factor(p$trt)
p = as.data.frame(p)  

 p %>% ggplot(aes(x = time, y = con, col = trt)) +
  geom_line(position = posn) +
  geom_point(position = posn) +
  geom_errorbar(aes(ymin = con, ymax = con + sd), position = posn, alpha = 0.7) +
  theme_bw() +
  labs(title = "Plasma concentration of ___", y = "Concentration of ____(ng/mL)", x = "Time")+
  scale_colour_manual(name = "Treatment", labels = c("Reference", "Treatment"), values = linecolor) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 11), 
        legend.position = c(0.85, 0.85), 
        legend.background = element_rect(fill = "white", color = "Black"),
        legend.key.size = unit(1.5, "lines")) +
        scale_x_continuous(breaks = seq(0, 72, 8)) 

#### Real time file - 파일명 확인하기
pc1 <- read.csv("PC1.csv", stringsAsFactors = F, header = T)
pc2 <- read.csv("PC2.csv", stringsAsFactors = F, header = T)
pc3 <- read.csv("PC3.CSV", stringsAsFactors = F, header = T)
pc4 <- read.csv("PC4.CSV", stringsAsFactors = F, header = T)

pc <- rbind(pc1, pc2, pc3, pc4)
names(pc)

### 전북대학교 DB 기준으로한 변수명 ㅡ 아닌경우 기준표 확인할 것
pc <- pc[, c("Subject", "Folder", "PCTPT", "PCTPT_STD", "PCELTM", "PCEVLINT", "PCEVLINT_RAW", "PCTIMYN", "PCTIMYN_STD")]

pc$prd = substring(pc$Folder, 2, 2)

pc <- pc %>%
  filter(Subject %in% unique(a1$id)) %>%
  arrange(prd, Subject, PCTPT_STD)

str(a4)
a4$time <- as.numeric(a4$time)
a4$time <- a4$time + pc$PCEVLINT_RAW / 60
a4$time <- ifelse(a4$time < 0, 0, a4$time)
a4 <- a4 %>%
  arrange(prd, id, time)

write.csv(a4, "a4.csv")

### NCA 용량 및 단위 확인할 것

a5 = unite(a4, id, c(prd, id, grp, trt, adm))
head(a5)
library(NonCompart)
NCA1 = tblNCA(a5, key = "id", colTime = "time", colConc = "dv", dose = 10, adm = "Extravascular", dur = 0, doseUnit = "mg", timeUnit = "h", concUnit = "ng/mL", down = "Linear", R2ADJ = 0.5, MW = 0)
NCA1 = NCA1 %>%
  separate(id, into = c("prd", "id", "grp", "trt", "adm"), sep = "_") %>%
  select(id, trt, CMAX, AUCLST, AUCIFO, TMAX, LAMZHL, CLFO, VZFO)

names(NCA1)
NCA1$Ratio = NCA1$AUCLST / NCA1$AUCIFO

gm  = function(a){prod(a)^(1/length(a))}
cv = function(x){sd(x)/mean(x) * 100}

NCAs = NCA1 %>%
  select(-id) %>%
  group_by(trt) %>%
  summarise_all(funs(mean, sd, gm, cv))

Tmax <- NCA1 %>%
  group_by(trt) %>%
  summarise(median = median(TMAX), min = min(TMAX), max = max(TMAX)) %>%
  mutate(variable = "Tmax")

NCAm = NCAs %>%
  select(ends_with('mean'), trt) %>%
  gather(variable, mean, -trt)

NCAd = NCAs %>%
  select(ends_with('sd'), trt) %>%
  gather(variable, sd, -trt)
NCAg = NCAs %>%
  select(ends_with('gm'), trt) %>%
  gather(variable, gm, -trt)
NCAc = NCAs %>%
  select(ends_with('cv'), trt) %>%
  gather(variable, cv, -trt)

NCAt = cbind(NCAm, NCAd, NCAg, NCAc)
NCAt = NCAt[, c(1,2,3,6,9,12)]

NCAt1 = NCAt %>%
  full_join(Tmax) %>%
  mutate_at(c("mean", "sd", "gm", "cv", "max"), ~round(., 2)) %>%
  arrange(trt)

head(NCAt1)

write.csv(NCAt1, "NCA_csr.csv")
write.csv(NCA1, "NCA_id.csv")
