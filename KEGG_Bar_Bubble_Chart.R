# 加载必要的包
library(ggplot2)

# 读取文件到 kegg_frame
file_path <- "C:\\Users\\Lamarck\\Desktop\\kegg_frame.csv"
kegg_frame <- read.csv(file_path)

# 确保 order 是因子类型并按 Count 排序
kegg_frame$order <- factor(kegg_frame$order, levels = kegg_frame$order[order(kegg_frame$Count)])

# 绘制柱状图
ggplot(kegg_frame,aes(y=order,x=Count,fill=p.adjust))+
  geom_bar(stat = "identity",width=0.7)+####柱子宽度
  #coord_flip()+##颠倒横纵轴
  scale_fill_gradient(low = "red",high ="blue" )+#颜色自己可以换
  labs(title = "KEGG Pathways Enrichment",
       x = "Gene numbers", 
       y = "Pathways")+
  theme(axis.title.x = element_text(face = "bold",size = 16),
        axis.title.y = element_text(face = "bold",size = 16),
        legend.title = element_text(face = "bold",size = 16))+
  theme_bw()


# 绘制气泡图
kegg_frame$order=factor(rev(as.integer(rownames(kegg_frame))),labels = rev(kegg_frame$Description))
ggplot(kegg_frame,aes(y=order,x=Count))+
  geom_point(aes(size=Count,color=-1*p.adjust))+# 修改点的大小
  scale_color_gradient(low="green",high = "red")+
  labs(color=expression(p.adjust,size="Count"), 
       x="Gene Number",y="Pathways",title="KEGG Pathway Enrichment")+
  theme_bw()

