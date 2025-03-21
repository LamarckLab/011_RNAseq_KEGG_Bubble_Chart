# 加载必要的包
library(ggplot2)
library(scales)

# 设置文件路径
file_path <- "C:\\Users\\Lamarck\\Desktop\\kegg_frame.csv"
kegg_frame <- read.csv(file_path)

# GeneRatio 转换为数值
kegg_frame$GeneRatio <- sapply(kegg_frame$GeneRatio, function(x) {
  parts <- strsplit(as.character(x), "/")[[1]]
  as.numeric(parts[1]) / as.numeric(parts[2])
})

# 设置 y 轴顺序
kegg_frame$order <- factor(kegg_frame$order, levels = kegg_frame$order[order(kegg_frame$Count)])
kegg_frame$order <- factor(rev(as.integer(rownames(kegg_frame))), labels = rev(kegg_frame$Description))

# 输出 PDF
output_path <- "C:\\Users\\Lamarck\\Desktop\\kegg_bubble.pdf"
pdf(output_path, width = 10, height = 6)

# 绘图
p <- ggplot(kegg_frame, aes(x = GeneRatio, y = order)) +
  geom_point(aes(size = Count, color = pvalue)) +
  
  # X轴刻度
  scale_x_continuous(
    breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
    labels = number_format(accuracy = 0.01)
  ) +
  
  # 颜色刻度（pvalue）
  scale_color_gradient(
    low = "green", high = "red",
    name = "P value",
    limits = c(0, 0.15),
    breaks = c(0.02, 0.06, 0.10, 0.14),
    labels = c("0.02", "0.06", "0.10", "0.14")
  ) +
  
  # ✅ 大小刻度 + 手动设置气泡实际大小（range 控制视觉大小）
  scale_size_continuous(
    name = "Gene Count",
    breaks = c(4, 8, 12),
    labels = c("4", "8", "12"),
    range = c(6, 12)  # 你可以改成 c(2, 10) 或其他更细微的值
  ) +
  
  # 其他主题设置
  labs(
    x = "Gene Ratio",
    y = "Pathways",
    title = "KEGG Pathway Enrichment"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )

print(p)
dev.off()
