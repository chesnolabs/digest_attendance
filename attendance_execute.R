# functions from attendance_functions

library(tidyverse)
library(readxl)
library(extrafont)
library(stringr)
library(forcats)

# Спершу запускаємо код attendance_functions.R (найпростіше - курсор
# внизу і Ctrl+Alt+B). Тепер у нас працюватимуть кастомні функції
# для аналізу участі в голосуваннях

# Копіюємо вручну в ексель-файл табличку за відповідний період
# звідси: http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_el_h_stat
# Зчитуємо її та дані депутатів і фракцій за допомогою кастомних функцій
# з attendance_functions.R

month <- 7 # вказати місяць і рік дайджесту
year <- 2018

attendance <- get_attendance("data/attendance.xlsx")

factions <- get_factions_open()
mps <- get_all_mps() %>% 
  filter(is.na(resignation_date))

att_period <- assemble_data() %>% 
  filter(end=="")

# % присутніх і тих, хто брав участь

att_period %>%
  summarize(prop_present = 100 - round(sum(absent, na.rm = T)/
                                         sum(total, na.rm = T)*100, 1),
            prop_voted = round(sum(voted, na.rm = T)/
                               sum(total, na.rm = T)*100, 1))

# статистика за фракціями

summary_factions <- att_period %>% group_by(faction) %>% 
  summarize(prop_absent = round(sum(absent, na.rm = T)/sum(total, na.rm = T)*100, 1),
    prop_not = round((sum(didnotvote, na.rm = T) + sum(absent, na.rm = T))/
              sum(total, na.rm = T)*100, 1)) %>% 
  mutate(prop = round(100 - prop_not)) %>% 
  arrange(desc(prop_not))

# не взяли участь у жодному голосуванні

att_period %>% 
  filter((didnotvote + absent) == total) %>% 
  arrange(faction, fullname)

# відсутні на всіх голосуваннях

totally_absent <- att_period %>% 
  filter(absent == total) %>% 
  arrange(by = faction)

totally_absent_short <- totally_absent %>% 
  select(fullname, faction)

# Формат списку для копіювання з консолі в текст дайджесту
# (увага, обов'язково треба перевірити на сайті Ради дані
# письмової реєстрації - можлива відсутність з поважних причин
# (хвороба, відпустка) - тоді таку особу не включаємо в дайджест)

cat(with(totally_absent, paste0(c(1:nrow(totally_absent)), ". ", firstname, " ", surname, ' ("', faction, '")')), sep = "\n")

# поіменні дані відсутності та неучасті в голосуванніх

absence_summary <- att_period %>% 
  select(fullname, faction, absence_v, absence_s) %>% 
  arrange(desc(absence_v))

no_vote_summary <- att_period %>% 
  mutate(no_vote = round((absent + didnotvote)/total*100, 1)) %>% 
  select(fullname, faction, no_vote) %>% 
  arrange(desc(no_vote))

# статистика для списочників і мажоритарників

summary_major <- att_period %>% 
  mutate(major = ifelse(is.na(district_num), "l", "m")) %>% 
  group_by(major) %>% 
  summarize(prop_absent = round(sum(absent, na.rm = T)/
                                         sum(total, na.rm = T)*100, 1),
            prop_voted = round(sum(voted, na.rm = T)/
                                 sum(total, na.rm = T)*100, 1),
            prop_didnotvote = round(sum(didnotvote, na.rm = T)/
                                 sum(total, na.rm = T)*100, 1))

# статистика за гендером

summary_gender <- att_period %>% 
  group_by(gender) %>% 
  summarize(prop_absent = round(sum(absent, na.rm = T)/
                                  sum(total, na.rm = T)*100, 1),
            prop_didnotvote = round(sum(didnotvote, na.rm = T)/
                                      sum(total, na.rm = T)*100, 1))
summary_gender$gender <- c("m", "f")

# запис у файл

dir.create("output")
month_to_write <- paste0(str_sub(year, 3), str_pad(month, 2, side = "left", pad = "0"))

write.xlsx(as.data.frame(summary_factions), file=paste0("output/attendance_summary_", month_to_write, ".xlsx"),
           sheetName="За фракціями", row.names=FALSE)
write.xlsx(as.data.frame(totally_absent_short), file=paste0("output/attendance_summary_", month_to_write, ".xlsx"),
           sheetName="Особи", append = TRUE, row.names=FALSE)
write.xlsx(as.data.frame(absence_summary), file=paste0("output/attendance_summary_", month_to_write, ".xlsx"),
           sheetName="Топ прогулів", append = TRUE, row.names=FALSE)
write.xlsx(as.data.frame(no_vote_summary), file=paste0("output/attendance_summary_", month_to_write, ".xlsx"),
           sheetName="Топ неучасті в голосуваннях", append = TRUE, row.names=FALSE)
write.xlsx(as.data.frame(summary_major), file=paste0("output/attendance_summary_", month_to_write, ".xlsx"),
           sheetName="Мажоритарники", append = TRUE, row.names=FALSE)
write.xlsx(as.data.frame(summary_gender), file=paste0("output/attendance_summary_", month_to_write, ".xlsx"),
           sheetName="За статтю", append = TRUE, row.names=FALSE)

# Графік участі за фракціями

att_for_plot <- att_period %>% 
  select(faction, voted, didnotvote, absent, voting_part) %>% 
  mutate(faction = fct_relevel(faction, levels(summary_factions$faction))) %>% 
  gather(activity, number, voted:absent)

g <- att_for_plot %>% 
  ggplot(aes(x = faction, y = number)) + 
  geom_bar(stat = "identity", aes(fill = activity), pos = "fill") +
  scale_fill_manual(labels = c("Відсутні", "Не голосували", "Голосували"),
    values = c("absent" = "firebrick3", "didnotvote" = "grey 90", "voted" = "purple 4"),
    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  ggtitle("УЧАСТЬ НАРОДНИХ ДЕПУТАТІВ У ГОЛОСУВАННЯХ, ЛИПЕНЬ 2018") + # скоригувати назву!
  theme(text = element_text(family = "PF DinText Pro"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")) + 
  theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5), "cm")) +
  coord_flip()

# Додаємо на графік написи відсотків

color_vote <- ifelse(summary_factions$prop > 2, "grey 90", "purple 4")

g +
  geom_text(data = summary_factions, aes (x = faction, y = 0.005, 
                label = c(prop[1:(length(prop)-1)], paste0(prop[length(prop)], "%"))),
            col = color_vote, hjust = 0, size = 6.5,
            family = "PF DinText Pro", fontface = "bold")

ggsave(paste0("output/attendance_graph_", month, ".png"))
