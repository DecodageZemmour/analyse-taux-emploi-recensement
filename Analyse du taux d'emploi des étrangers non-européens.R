library(tidyverse)
library(haven)
library(viridis)

# source : https://www.insee.fr/fr/statistiques/5395740?sommaire=5395764 pour le fichier détail et sa documentation et
# https://www.insee.fr/fr/information/2383177 pour les conseils d'utilisation des résultats du recensement de population
data_rp2018 <- read_delim(
  "FD_INDREG_2018.csv",
  delim = ";",
  col_select = c("AGEREV", "NAT13", "NATN12", "PNAI12", "RECH", "TACT", "IPONDI"),
  col_type = cols_only(
    AGEREV = col_integer(),
    NAT13 = col_character(),
    NATN12 = col_character(),
    PNAI12 = col_character(),
    RECH = col_character(),
    TACT = col_character(),
    IPONDI = col_double()
  )
) %>%
  mutate(
    nationalité_condensé = case_when(
      NAT13 %in% c("00", "01") ~ "Français",
      NAT13 %in% c("02", "03", "04", "05") ~ "Étrangers\n(UE à 28)",
      NAT13 %in% c("06", "07", "08", "09", "10", "11", "12") ~ "Étrangers\n(hors UE à 28)",
      TRUE ~ NA_character_
    ),
    nationalité_détaillé = case_when(
      NAT13 %in% c("00", "01") ~ "Français",
      NAT13 %in% c("02", "03", "04", "05") ~ "Étrangers\n(UE à 28)",
      NAT13 == "06" ~ "Étrangers\n(reste de l'Europe)",
      NAT13 %in% c("07", "08", "09") ~ "Étrangers\n(Maghreb)",
      NAT13 == "10" ~ "Étrangers\n(reste de l'Afrique)",
      NAT13 == "11" ~ "Étrangers\n(Turquie)",
      NAT13 == "12" ~ "Étrangers\n(reste du monde)",
      TRUE ~ NA_character_
    ),
    immigration_condensé = case_when(
      NAT13 == "00" | (NAT13 == "01" & PNAI12 == "01") ~ "Natifs",
      (NAT13 %in% c("02", "03", "04", "05")) | (NAT13 == "01" & PNAI12 != "01" & NATN12 %in% c("02", "03", "04", "05")) ~ "Immigrés\n(UE à 28)",
      (NAT13 %in% c("06", "07", "08", "09", "10", "11", "12")) | (NAT13 == "01" & PNAI12 != "01" & NATN12 %in% c("06", "07", "08", "09", "10", "11", "12")) ~ "Immigrés\n(hors UE à 28)",
      TRUE ~ NA_character_
    ),
    immigration_détaillé = case_when(
      NAT13 == "00" | (NAT13 == "01" & PNAI12 == "01") ~ "Natifs",
      (NAT13 %in% c("02", "03", "04", "05")) | (NAT13 == "01" & PNAI12 != "01" & NATN12 %in% c("02", "03", "04", "05")) ~ "Immigrés\n(UE à 28)",
      (NAT13 == "06") | (NAT13 == "01" & PNAI12 != "01" & NATN12 == "06") ~ "Immigrés\n(reste de l'Europe)",
      (NAT13 %in% c("07", "08", "09")) | (NAT13 == "01" & PNAI12 != "01" & NATN12 %in% c("07", "08", "09")) ~ "Immigrés\n(Maghreb)",
      (NAT13 == "10") | (NAT13 == "01" & PNAI12 != "01" & NATN12 == "10") ~ "Immigrés\n(reste de l'Afrique)",
      (NAT13 == "11") | (NAT13 == "01" & PNAI12 != "01" & NATN12 == "11") ~ "Immigrés\n(Turquie)",
      (NAT13 == "12") | (NAT13 == "01" & PNAI12 != "01" & NATN12 == "12") ~ "Immigrés\n(reste du monde)",
      TRUE ~ NA_character_
    ),
    actif = TACT %in% c("11", "12"),
    emploi = TACT %in% c("11"),
    chômeur = TACT %in% c("12")
  )

rp2018_nationalité_condensé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64
  ) %>%
  group_by(nationalité_condensé) %>%
  summarize(
    taux_activité = sum(actif * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = sum(chômeur * IPONDI, na.rm = TRUE) / sum(actif * IPONDI, na.rm = TRUE),
    taux_emploi = sum(emploi * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
  )

rp2018_nationalité_détaillé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64
  ) %>%
  group_by(nationalité_détaillé) %>%
  summarize(
    taux_activité = sum(actif * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = sum(chômeur * IPONDI, na.rm = TRUE) / sum(actif * IPONDI, na.rm = TRUE),
    taux_emploi = sum(emploi * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
  )

rp2018_immigration_condensé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64
  ) %>%
  group_by(immigration_condensé) %>%
  summarize(
    taux_activité = sum(actif * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = sum(chômeur * IPONDI, na.rm = TRUE) / sum(actif * IPONDI, na.rm = TRUE),
    taux_emploi = sum(emploi * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
  )

rp2018_immigration_détaillé <- data_rp2018 %>%
  filter(
    AGEREV >= 15 & AGEREV <= 64
  ) %>%
  group_by(immigration_détaillé) %>%
  summarize(
    taux_activité = sum(actif * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE),
    taux_chômage = sum(chômeur * IPONDI, na.rm = TRUE) / sum(actif * IPONDI, na.rm = TRUE),
    taux_emploi = sum(emploi * IPONDI, na.rm = TRUE) / sum(IPONDI, na.rm = TRUE)
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
  ggtitle("Taux d'activité selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'activité selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018 (condensé).png",
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
  ggtitle("Taux de chômage selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux de chômage selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018 (condensé).png",
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
  ggtitle("Taux d'emploi selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'emploi selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018 (condensé).png",
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
  ggtitle("Taux d'activité selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'activité selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018 (détaillé).png",
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
  ggtitle("Taux de chômage selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux de chômage selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018 (détaillé).png",
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
  ggtitle("Taux d'emploi selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'emploi selon la nationalité chez les 15-64 ans d'après le recensement de population de 2018 (détaillé).png",
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
  ggtitle("Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018 (condensé).png",
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
  ggtitle("Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018 (condensé).png",
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
  ggtitle("Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018 (condensé).png",
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
  ggtitle("Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'activité selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018 (détaillé).png",
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
  ggtitle("Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux de chômage selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018 (détaillé).png",
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
  ggtitle("Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018") +
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
  "Taux d'emploi selon le statut par rapport à l'immigration chez les 15-64 ans d'après le recensement de population de 2018 (détaillé).png",
  width = 12,
  height = 6
)
