library(tidyverse)
library(haven)
library(viridis)

# source : https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764
data_rp2018 <- read_delim(
  "FD_INDREG_2018.csv",
  delim = ";",
  col_select = c("AGEREV", "NAT13", "NATN12", "PNAI12", "TACT", "IPONDI"),
  col_type = c("iiiiid")
) %>%
  mutate(
    AGEREV = as.integer(AGEREV),
    NAT13 = as.integer(NAT13),
    NATN12 = as.integer(NATN12),
    PNAI12 = as.integer(PNAI12),
    nationalité_condensé = case_when(
      NAT13 %in% c(0, 1) ~ "Français",
      NAT13 %in% c(2, 3, 4, 5) ~ "Étrangers\n(UE à 28)",
      NAT13 %in% c(6, 7, 8, 9, 10, 11, 12) ~ "Étrangers\n(hors UE à 28)",
      TRUE ~ NA_character_
    ),
    nationalité_détaillé = case_when(
      NAT13 %in% c(0, 1) ~ "Français",
      NAT13 %in% c(2, 3, 4, 5) ~ "Étrangers\n(UE à 28)",
      NAT13 == 6 ~ "Étrangers\n(reste de l'Europe)",
      NAT13 %in% c(7, 8, 9) ~ "Étrangers\n(Maghreb)",
      NAT13 == 10 ~ "Étrangers\n(reste de l'Afrique)",
      NAT13 == 11 ~ "Étrangers\n(Turquie)",
      NAT13 == 12 ~ "Étrangers\n(reste du monde)",
      TRUE ~ NA_character_
    ),
    immigration_condensé = case_when(
      NAT13 == 0 | (NAT13 == 1 & PNAI12 == 1) ~ "Natifs",
      (NAT13 %in% c(2, 3, 4, 5)) | (NAT13 == 1 & PNAI12 != 1 & NATN12 %in% c(2, 3, 4, 5)) ~ "Immigrés\n(UE à 28)",
      (NAT13 %in% c(6, 7, 8, 9, 10, 11, 12)) | (NAT13 == 1 & PNAI12 != 1 & NATN12 %in% c(6, 7, 8, 9, 10, 11, 12)) ~ "Immigrés\n(hors UE à 28)",
      TRUE ~ NA_character_
    ),
    immigration_détaillé = case_when(
      NAT13 == 0 | (NAT13 == 1 & PNAI12 == 1) ~ "Natifs",
      (NAT13 %in% c(2, 3, 4, 5)) | (NAT13 == 1 & PNAI12 != 1 & NATN12 %in% c(2, 3, 4, 5)) ~ "Immigrés\n(UE à 28)",
      (NAT13 == 6) | (NAT13 == 1 & PNAI12 != 1 & NATN12 == 6) ~ "Immigrés\n(reste de l'Europe)",
      (NAT13 %in% c(7, 8, 9)) | (NAT13 == 1 & PNAI12 != 1 & NATN12 %in% c(7, 8, 9)) ~ "Immigrés\n(Maghreb)",
      (NAT13 == 10) | (NAT13 == 1 & PNAI12 != 1 & NATN12 == 10) ~ "Immigrés\n(reste de l'Afrique)",
      (NAT13 == 11) | (NAT13 == 1 & PNAI12 != 1 & NATN12 == 11) ~ "Immigrés\n(Turquie)",
      (NAT13 == 12) | (NAT13 == 1 & PNAI12 != 1 & NATN12 == 12) ~ "Immigrés\n(reste du monde)",
      TRUE ~ NA_character_
    )
  )

rp2018_nationalité_condensé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64,
    TACT != 22
  ) %>%
  group_by(nationalité_condensé) %>%
  summarize(
    taux_activité = sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = 1 - sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE),
    taux_emploi = sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
  )

rp2018_nationalité_détaillé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64,
    TACT != 22
  ) %>%
  group_by(nationalité_détaillé) %>%
  summarize(
    taux_activité = sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = 1 - sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE),
    taux_emploi = sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
  )

rp2018_immigration_condensé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64,
    TACT != 22
  ) %>%
  group_by(immigration_condensé) %>%
  summarize(
    taux_activité = sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = 1 - sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE),
    taux_emploi = sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
  )

rp2018_immigration_détaillé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64,
    TACT != 22
  ) %>%
  group_by(immigration_détaillé) %>%
  summarize(
    taux_activité = sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = 1 - sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum((TACT == 11 | TACT == 12) * IPONDI, na.rm = TRUE),
    taux_emploi = sum((TACT == 11) * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
  )

ggplot(data = rp2018_nationalité_condensé, mapping = aes(x = reorder(nationalité_condensé, -taux_activité), y = taux_activité)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_activité * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'activité selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'activité") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'activité selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (condensé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_nationalité_condensé, mapping = aes(x = reorder(nationalité_condensé, taux_chômage), y = taux_chômage)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_chômage * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux de chômage selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux de chômage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux de chômage selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (condensé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_nationalité_condensé, mapping = aes(x = reorder(nationalité_condensé, -taux_emploi), y = taux_emploi)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_emploi * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'emploi selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'emploi") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'emploi selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (condensé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_nationalité_détaillé, mapping = aes(x = reorder(nationalité_détaillé, -taux_activité), y = taux_activité)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_activité * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'activité selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'activité") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'activité selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (détaillé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_nationalité_détaillé, mapping = aes(x = reorder(nationalité_détaillé, taux_chômage), y = taux_chômage)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_chômage * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux de chômage selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux de chômage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux de chômage selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (détaillé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_nationalité_détaillé, mapping = aes(x = reorder(nationalité_détaillé, -taux_emploi), y = taux_emploi)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_emploi * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'emploi selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'emploi") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'emploi selon la nationalité chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (détaillé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_immigration_condensé, mapping = aes(x = reorder(immigration_condensé, -taux_activité), y = taux_activité)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_activité * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'activité") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (condensé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_immigration_condensé, mapping = aes(x = reorder(immigration_condensé, taux_chômage), y = taux_chômage)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_chômage * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux de chômage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (condensé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_immigration_condensé, mapping = aes(x = reorder(immigration_condensé, -taux_emploi), y = taux_emploi)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_emploi * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'emploi") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (condensé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_immigration_détaillé, mapping = aes(x = reorder(immigration_détaillé, -taux_activité), y = taux_activité)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_activité * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'activité") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (détaillé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_immigration_détaillé, mapping = aes(x = reorder(immigration_détaillé, taux_chômage), y = taux_chômage)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_chômage * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux de chômage") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (détaillé).png",
  width = 12,
  height = 6
)

ggplot(data = rp2018_immigration_détaillé, mapping = aes(x = reorder(immigration_détaillé, -taux_emploi), y = taux_emploi)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(taux_emploi * 100, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 5
  ) +
  theme_bw() +
  ggtitle("Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018") +
  xlab("Catégorie") +
  ylab("Taux d'emploi") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    caption = "Source : INSEE (https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764) - Calculs et graphique par @DecodageZemmour"
  )

ggsave(
  "Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans (sans les étudiants) d'après le recensement de population de 2018 (détaillé).png",
  width = 12,
  height = 6
)
