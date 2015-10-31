require(dplyr)
require(ggplot2)
require(tidyr)
require(magrittr)
require(reshape2)

# 자본 capital // 자산 asset //부채 liability // 매출 sales // 순이익 profit
setwd('D://dropbox/Dropbox/git/parkgongshi')
quarterly <- read.csv('gdata.csv')

quarterly <- quarterly[, 2:6]
colnames(quarterly) <- c('자산', '부채', '자본', '매출', '순익' )
# quarterly <- t(quarterly)
rownames(quarterly) <- c('current', 'past')


quarterly <- quarterly %>%
  mutate(부채비율 = 부채/자산, 자본비율 = 자본/자산)
quarterly$time <- c('당기','전기')


## 영업실적
tiff("영업.tiff", width = 4, height = 4, units = 'in', res = 300)
pl <- quarterly[1:2, c(4:5,8)]
pl <- pl %>% gather(구분, stat, -time)
bar <- ggplot(data= pl, aes(x=factor(time), y =stat, fill=factor(구분))) +
  geom_bar(stat ='identity', position = 'dodge') + labs(x= '', y = '') +
  ggtitle('영업실적(백만원)')
bar <- bar + theme(plot.title = element_text(face = 'bold', size = 20)) + labs(fill= '구분') +
  theme(axis.title.x = element_text(face='bold', size = 15)) +
  theme(axis.title.y = element_text(face = 'bold', size =15))
bar <- bar + theme(axis.text.x  = element_text(size=16, face = 'bold')) +
  theme(axis.text.y = element_text(size=20, angle =0, vjust = 0.2)) # angle =60, -30 가능
bar
dev.off()

## 자산 구성비율
ratio <- quarterly[1:2, 6:8]
ratio <- ratio %>% gather(구분, stat, -time)

# A pie chart = stacked bar chart + polar coordinates

# png(file = 'ratio_plot.tiff', width = 480, height = 480)  ## png 버젼
tiff("자본.tiff", width = 4, height = 4, units = 'in', res = 300)
pie <- ggplot(ratio, aes(x = factor(time), y = stat, fill = factor(구분))) +
  geom_bar(width = 1, stat='identity', colour='black')
pie <- pie + labs(x= '', y = '')
pie <- pie + theme(plot.title = element_text(face = 'bold', size = 20)) + labs(fill= '구분') +
  theme(axis.title.x = element_text(face='bold', size = 10)) +
  theme(axis.title.y = element_text(face = 'bold', size =10))
pie <- pie + theme(axis.text.x  = element_text(size=10, face = 'bold')) +
  theme(axis.text.y = element_text(size=13, angle =45, vjust = 0.2)) # angle =60, -30 가능
pie <- pie +ggtitle('자본, 부채비율') + theme(plot.title = element_text(face='bold'))
pie <- pie + coord_polar(theta='y')
pie
dev.off()
