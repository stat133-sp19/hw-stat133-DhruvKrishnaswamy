
# Make-shot-script 
# MAKE SHOT CHARTS SCRIPT
# Input: This script takes in the .csv files that we had modified in the previous data script 
# output: This makes visualizations such as shot charts about the player shooting percentages
#         and returns them in the form of pdfs or pngs
img <- 'Desktop/workout1/images/nba-court.jpg'

court_image <- rasterGrob(
  readJPEG(img),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

klay_shot_chart <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

dray_shot_chart <- ggplot(data = dray) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()

andre_shot_chart <- ggplot(data = andre) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()

kevin_shot_chart <- ggplot(data = kevin) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()

pdf('Klay-thompson-shot-chart.pdf', width = 6.5, height = 5)
klay_shot_chart

pdf('Draymond-Green-shot-chart.pdf', width = 6.5, height = 5)
dray_shot_chart

pdf('Stephen-Curry-shot-chart.pdf', width = 6.5, height = 5)
curry_shot_chart

pdf('Andre-iguodala-shot-chart.pdf', width = 6.5, height = 5)
curry_shot_chart

pdf('Kevin-Durant-shot-chart.pdf', width = 6.5, height = 5)
curry_shot_chart

gsw_shot_chart <- ggplot(data = temp4)+
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() + facet_wrap(~ name)

pdf('fgsw-shot-charts.pdf', width = 8, height = 7)
gsw_shot_chart

png("gsw-shot-charts.png", res = 56)
gsw_shot_chart
dev.off()