library(ggplot2)
library(plotly)

plot_ly(x = ~df$texture_mean, y = ~df$smoothness_worst, z = ~df$area_worst, 
        type = 'scatter3d', mode = 'markers', color = ~df$diagnosis,
        colors = c('red', 'lightblue'))

ggplot(df, aes(compactness_mean, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(fractal_dimension_mean, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(fractal_dimension_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(symmetry_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(radius_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(compactness_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(concave.points_mean, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(concave.points_se, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)
  
ggplot(df, aes(concavity_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)
  
ggplot(df, aes(concavity_se, colour=diagnosis, fill=diagnosis))+
    theme_bw()+
    geom_density(alpha=0.5)

ggplot(df, aes(concavity_mean, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(area_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)
  
  ggplot(df, aes(area_se, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(area_mean, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(smoothness_mean, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(smoothness_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(texture_mean, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(texture_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(perimeter_worst, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(concave.points_worst, colour=diagnosis, fill=diagnosis))+
  xlab('Worst concave points')+
  ylab('Densidad')+
  theme_bw()+
  geom_density(alpha=0.5)

ggplot(df, aes(symmetry_se, colour=diagnosis, fill=diagnosis))+
  theme_bw()+
  geom_density(alpha=0.5)
