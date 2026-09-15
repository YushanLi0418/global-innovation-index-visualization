# ============================================================
# WIPO Global Innovation Index
# R Data Visualization Teaching Example
#
# Data:
# wipo-pub-2000-2025-tech1.xlsx
#
# Figures:
# 1. Top 15 GII economies
# 2. World map of GII
# 3. GII vs GDP per capita
# 4. Innovation Inputs vs Outputs
# 5. Seven Pillars Heatmap
# 6. Regional GII distribution
# 7. Japan vs World Average
# 8. Japan Strengths and Weaknesses
# ============================================================



# ============================================================
# STEP 0
# Install packages
# ============================================================

# 第一次使用时运行下面这一段
# 安装完成以后，以后不需要再次运行

install.packages(c(
  "tidyverse",
  "readxl",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "ggrepel",
  "scales"
))


# ============================================================
# STEP 1
# Load packages
# ============================================================

library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(scales)



# ============================================================
# STEP 2
# Read Excel file
# ============================================================

# 把 Excel 文件放在 R Project 文件夹中
# 如果文件不在当前文件夹，需要写完整路径

file_path <- "wipo-pub-2000-2025-tech1.xlsx"


# 读取四个 Sheet

index_structure <- read_excel(
  file_path,
  sheet = "Index Structure"
)

economies <- read_excel(
  file_path,
  sheet = "Economies"
)

data <- read_excel(
  file_path,
  sheet = "Data"
)

metadata <- read_excel(
  file_path,
  sheet = "Metadata"
)



# ============================================================
# STEP 3
# Check the data
# ============================================================

# 查看前几行
head(data)

head(economies)

head(index_structure)


# 查看变量名称
names(data)

names(economies)

names(index_structure)


# 查看数据大小
dim(data)

dim(economies)

dim(index_structure)



# ============================================================
# STEP 4
# Extract Global Innovation Index
# ============================================================

# Data Sheet 中包含很多指标
# 我们首先只留下 Global Innovation Index

gii <- data %>%
  
  filter(
    NAME == "Global Innovation Index"
  )


# 检查结果

head(gii)

dim(gii)


# 应该大约是一个国家一行



# ============================================================
#
# FIGURE 1
# Global Innovation Index Top 15
#
# ============================================================


# ------------------------------------------------------------
# Step 1.1
# Find the Top 15 economies
# ------------------------------------------------------------

gii_top15 <- gii %>%
  
  filter(
    !is.na(SCORE)
  ) %>%
  
  slice_max(
    order_by = SCORE,
    n = 15,
    with_ties = FALSE
  )


# 查看结果

gii_top15 %>%
  select(
    ECONOMY_NAME,
    SCORE,
    RANK
  )



# ------------------------------------------------------------
# Step 1.2
# Create bar chart
# ------------------------------------------------------------

p1 <- ggplot(
  
  data = gii_top15,
  
  aes(
    x = reorder(
      ECONOMY_NAME,
      SCORE
    ),
    y = SCORE
  )
  
) +
  
  geom_col(
    width = 0.7
  ) +
  
  geom_text(
    aes(
      label = round(
        SCORE,
        1
      )
    ),
    
    hjust = -0.15,
    size = 3.5
  ) +
  
  coord_flip() +
  
  expand_limits(
    y = max(
      gii_top15$SCORE
    ) + 5
  ) +
  
  labs(
    title = "Top 15 Economies in the Global Innovation Index",
    subtitle = "Higher scores indicate stronger innovation performance",
    x = NULL,
    y = "GII Score",
    caption = "Source: WIPO Global Innovation Index"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    panel.grid.major.y =
      element_blank(),
    
    plot.title =
      element_text(
        face = "bold",
        size = 17
      )
  )


# 显示图片

p1



# ============================================================
#
# FIGURE 2
# Global Innovation Index World Map
#
# ============================================================


# ------------------------------------------------------------
# Step 2.1
# Download / load world map
# ------------------------------------------------------------

world <- ne_countries(
  
  scale = "medium",
  
  returnclass = "sf"
  
)


# 查看地图数据

world

names(world)



# ------------------------------------------------------------
# Step 2.2
# Create ISO3 variable
# ------------------------------------------------------------

# Natural Earth 中有少数国家 iso_a3 = -99
# 因此如果 iso_a3 不可用
# 就使用 adm0_a3

world <- world %>%
  
  mutate(
    
    ISO3 = if_else(
      
      iso_a3 == "-99",
      
      adm0_a3,
      
      iso_a3
      
    )
    
  )


# 删除南极洲
# 这样世界地图看起来更正常

world <- world %>%
  
  filter(
    continent != "Antarctica"
  )



# ------------------------------------------------------------
# Step 2.3
# Join WIPO data with map data
# ------------------------------------------------------------

world_gii <- world %>%
  
  left_join(
    
    gii %>%
      
      select(
        ISO3,
        ECONOMY_NAME,
        SCORE,
        RANK
      ),
    
    by = "ISO3"
    
  )



# 检查匹配后的数据

world_gii %>%
  
  select(
    admin,
    ISO3,
    SCORE,
    RANK
  ) %>%
  
  head(20)



# ------------------------------------------------------------
# Step 2.4
# Draw world map
# ------------------------------------------------------------

p2 <- ggplot(
  
  data = world_gii
  
) +
  
  geom_sf(
    
    aes(
      fill = SCORE
    ),
    
    color = "white",
    
    linewidth = 0.15
    
  ) +
  
  scale_fill_viridis_c(
    
    option = "C",
    
    na.value = "grey90",
    
    name = "GII Score"
    
  ) +
  
  labs(
    
    title =
      "Global Innovation Index",
    
    subtitle =
      "Innovation performance across economies",
    
    caption =
      "Source: WIPO Global Innovation Index"
    
  ) +
  
  theme_void() +
  
  theme(
    
    legend.position =
      "right",
    
    plot.title =
      element_text(
        size = 18,
        face = "bold"
      ),
    
    plot.subtitle =
      element_text(
        size = 11
      )
    
  )


p2



# ============================================================
#
# FIGURE 3
# Innovation Score vs GDP per capita
#
# ============================================================


# ------------------------------------------------------------
# Step 3.1
# Join GII with economy information
# ------------------------------------------------------------

gii_income <- gii %>%
  
  select(
    
    ISO3,
    
    ECONOMY_NAME,
    
    SCORE,
    
    RANK
    
  ) %>%
  
  left_join(
    
    economies %>%
      
      select(
        
        ISO3,
        
        INCOME,
        
        REG_UN,
        
        PPPPC
        
      ),
    
    by = "ISO3"
    
  ) %>%
  
  filter(
    
    !is.na(SCORE),
    
    !is.na(PPPPC),
    
    PPPPC > 0
    
  )



# 查看数据

head(gii_income)



# ------------------------------------------------------------
# Step 3.2
# Scatter plot
# ------------------------------------------------------------

p3 <- ggplot(
  
  data = gii_income,
  
  aes(
    
    x = PPPPC,
    
    y = SCORE,
    
    color = INCOME
    
  )
  
) +
  
  geom_point(
    
    size = 3,
    
    alpha = 0.75
    
  ) +
  
  scale_x_log10(
    
    labels = comma
    
  ) +
  
  labs(
    
    title =
      "Innovation and Economic Development",
    
    subtitle =
      "Global Innovation Index versus GDP per capita",
    
    x =
      "GDP per capita, PPP (log scale)",
    
    y =
      "Global Innovation Index Score",
    
    color =
      "Income Group",
    
    caption =
      "Source: WIPO Global Innovation Index"
    
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 17
      )
    
  )


p3



# ------------------------------------------------------------
# Step 3.3
# Add labels for Top 10 countries
# ------------------------------------------------------------

p3_label <- ggplot(
  
  data = gii_income,
  
  aes(
    
    x = PPPPC,
    
    y = SCORE,
    
    color = INCOME
    
  )
  
) +
  
  geom_point(
    
    size = 3,
    
    alpha = 0.7
    
  ) +
  
  geom_text_repel(
    
    data = gii_income %>%
      
      filter(
        RANK <= 10
      ),
    
    aes(
      label = ECONOMY_NAME
    ),
    
    size = 3.2,
    
    show.legend = FALSE
    
  ) +
  
  scale_x_log10(
    
    labels = comma
    
  ) +
  
  labs(
    
    title =
      "Innovation and Economic Development",
    
    subtitle =
      "Top 10 innovation economies are labelled",
    
    x =
      "GDP per capita, PPP (log scale)",
    
    y =
      "Global Innovation Index Score",
    
    color =
      "Income Group"
    
  ) +
  
  theme_minimal(
    base_size = 13
  )


p3_label



# ============================================================
#
# FIGURE 4
# Innovation Inputs vs Innovation Outputs
#
# ============================================================


# ------------------------------------------------------------
# Step 4.1
# Select input and output indexes
# ------------------------------------------------------------

input_output <- data %>%
  
  filter(
    
    NAME %in% c(
      
      "Innovation inputs",
      
      "Innovation outputs"
      
    )
    
  ) %>%
  
  select(
    
    ISO3,
    
    ECONOMY_NAME,
    
    NAME,
    
    SCORE
    
  )


# 查看长格式数据

head(
  input_output,
  10
)



# ------------------------------------------------------------
# Step 4.2
# Convert long format into wide format
# ------------------------------------------------------------

input_output_wide <- input_output %>%
  
  pivot_wider(
    
    names_from = NAME,
    
    values_from = SCORE
    
  ) %>%
  
  rename(
    
    Input =
      `Innovation inputs`,
    
    Output =
      `Innovation outputs`
    
  )



# 查看宽格式数据

head(
  input_output_wide
)



# ------------------------------------------------------------
# Step 4.3
# Add overall GII rank
# ------------------------------------------------------------

input_output_wide <- input_output_wide %>%
  
  left_join(
    
    gii %>%
      
      select(
        
        ISO3,
        
        RANK
        
      ),
    
    by = "ISO3"
    
  ) %>%
  
  filter(
    
    !is.na(Input),
    
    !is.na(Output)
    
  )



# ------------------------------------------------------------
# Step 4.4
# Plot
# ------------------------------------------------------------

p4 <- ggplot(
  
  data = input_output_wide,
  
  aes(
    
    x = Input,
    
    y = Output
    
  )
  
) +
  
  geom_abline(
    
    slope = 1,
    
    intercept = 0,
    
    linetype = "dashed",
    
    color = "grey50"
    
  ) +
  
  geom_point(
    
    size = 3,
    
    alpha = 0.7
    
  ) +
  
  geom_text_repel(
    
    data = input_output_wide %>%
      
      filter(
        RANK <= 10
      ),
    
    aes(
      label = ECONOMY_NAME
    ),
    
    size = 3.2
    
  ) +
  
  labs(
    
    title =
      "Innovation Inputs and Innovation Outputs",
    
    subtitle =
      "Countries above the 45-degree line have relatively high innovation outputs",
    
    x =
      "Innovation Input Score",
    
    y =
      "Innovation Output Score",
    
    caption =
      "Source: WIPO Global Innovation Index"
    
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 17
      )
    
  )


p4



# ============================================================
#
# FIGURE 5
# Seven GII Pillars Heatmap
#
# ============================================================


# ------------------------------------------------------------
# Step 5.1
# Find the seven GII pillars
# ------------------------------------------------------------

pillars <- index_structure %>%
  
  filter(
    
    LEVEL == "Pillar"
    
  )


# 看看七大 Pillar

pillars %>%
  
  select(
    NUM,
    NAME
  )



# ------------------------------------------------------------
# Step 5.2
# Select countries
# ------------------------------------------------------------

selected_countries <- c(
  
  "CHN",  # China
  
  "JPN",  # Japan
  
  "USA",  # United States
  
  "DEU",  # Germany
  
  "CHE",  # Switzerland
  
  "KOR",  # South Korea
  
  "SGP",  # Singapore
  
  "IND"   # India
  
)



# ------------------------------------------------------------
# Step 5.3
# Extract pillar data
# ------------------------------------------------------------

pillar_data <- data %>%
  
  filter(
    
    ISO3 %in%
      selected_countries,
    
    NUM %in%
      pillars$NUM
    
  ) %>%
  
  select(
    
    ISO3,
    
    ECONOMY_NAME,
    
    NUM,
    
    NAME,
    
    SCORE
    
  ) %>%
  
  filter(
    !is.na(SCORE)
  )



# 查看

head(
  pillar_data,
  20
)



# ------------------------------------------------------------
# Step 5.4
# Heatmap
# ------------------------------------------------------------

p5 <- ggplot(
  
  data = pillar_data,
  
  aes(
    
    x = ECONOMY_NAME,
    
    y = NAME,
    
    fill = SCORE
    
  )
  
) +
  
  geom_tile(
    
    color = "white",
    
    linewidth = 0.8
    
  ) +
  
  geom_text(
    
    aes(
      
      label =
        round(
          SCORE,
          1
        )
      
    ),
    
    size = 3.2
    
  ) +
  
  scale_fill_viridis_c(
    
    name = "Score"
    
  ) +
  
  labs(
    
    title =
      "Innovation Performance across Seven GII Pillars",
    
    subtitle =
      "Selected economies",
    
    x = NULL,
    
    y = NULL,
    
    caption =
      "Source: WIPO Global Innovation Index"
    
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    axis.text.x =
      element_text(
        
        angle = 45,
        
        hjust = 1
        
      ),
    
    panel.grid =
      element_blank(),
    
    plot.title =
      element_text(
        face = "bold",
        size = 17
      )
    
  )


p5



# ============================================================
#
# FIGURE 6
# GII Distribution by Region
#
# ============================================================


# ------------------------------------------------------------
# Step 6.1
# Add region information
# ------------------------------------------------------------

gii_region <- gii %>%
  
  select(
    
    ISO3,
    
    ECONOMY_NAME,
    
    SCORE
    
  ) %>%
  
  left_join(
    
    economies %>%
      
      select(
        
        ISO3,
        
        REG_UN
        
      ),
    
    by = "ISO3"
    
  ) %>%
  
  filter(
    
    !is.na(REG_UN),
    
    !is.na(SCORE)
    
  )



# 查看地区分类

unique(
  gii_region$REG_UN
)



# ------------------------------------------------------------
# Step 6.2
# Boxplot + individual countries
# ------------------------------------------------------------

p6 <- ggplot(
  
  data = gii_region,
  
  aes(
    
    x = fct_reorder(
      
      REG_UN,
      
      SCORE,
      
      .fun = median,
      
      na.rm = TRUE
      
    ),
    
    y = SCORE
    
  )
  
) +
  
  geom_boxplot(
    
    outlier.shape = NA,
    
    width = 0.6
    
  ) +
  
  geom_jitter(
    
    width = 0.15,
    
    alpha = 0.5,
    
    size = 2
    
  ) +
  
  coord_flip() +
  
  labs(
    
    title =
      "Distribution of Innovation Performance by Region",
    
    subtitle =
      "Each point represents one economy",
    
    x = NULL,
    
    y =
      "Global Innovation Index Score",
    
    caption =
      "Source: WIPO Global Innovation Index"
    
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    panel.grid.major.y =
      element_blank(),
    
    plot.title =
      element_text(
        face = "bold",
        size = 17
      )
    
  )


p6



# ============================================================
#
# FIGURE 7
# Japan vs World Average
#
# ============================================================


# ------------------------------------------------------------
# Step 7.1
# Choose a country
# ------------------------------------------------------------

# JPN = Japan
# CHN = China
# USA = United States
# DEU = Germany

country_code <- "JPN"



# ------------------------------------------------------------
# Step 7.2
# Calculate world average
# ------------------------------------------------------------

world_average <- data %>%
  
  filter(
    
    NUM %in%
      pillars$NUM
    
  ) %>%
  
  group_by(
    
    NUM,
    
    NAME
    
  ) %>%
  
  summarise(
    
    World =
      mean(
        
        SCORE,
        
        na.rm = TRUE
        
      ),
    
    .groups =
      "drop"
    
  )



# 查看

world_average



# ------------------------------------------------------------
# Step 7.3
# Extract Japan pillar scores
# ------------------------------------------------------------

country_pillar <- data %>%
  
  filter(
    
    ISO3 ==
      country_code,
    
    NUM %in%
      pillars$NUM
    
  ) %>%
  
  select(
    
    NUM,
    
    NAME,
    
    ECONOMY_NAME,
    
    SCORE
    
  ) %>%
  
  rename(
    
    Country =
      SCORE
    
  )



# ------------------------------------------------------------
# Step 7.4
# Join with world average
# ------------------------------------------------------------

benchmark <- country_pillar %>%
  
  left_join(
    
    world_average,
    
    by =
      c(
        "NUM",
        "NAME"
      )
    
  )



# 查看比较数据

benchmark



# ------------------------------------------------------------
# Step 7.5
# Dumbbell-style comparison chart
# ------------------------------------------------------------

p7 <- ggplot(
  
  data = benchmark,
  
  aes(
    
    y =
      reorder(
        
        NAME,
        
        Country
        
      )
    
  )
  
) +
  
  geom_segment(
    
    aes(
      
      x = World,
      
      xend = Country,
      
      yend =
        reorder(
          
          NAME,
          
          Country
          
        )
      
    ),
    
    linewidth = 1,
    
    color = "grey70"
    
  ) +
  
  geom_point(
    
    aes(
      x = World
    ),
    
    size = 4,
    
    color = "grey40"
    
  ) +
  
  geom_point(
    
    aes(
      x = Country
    ),
    
    size = 4
    
  ) +
  
  labs(
    
    title =
      paste0(
        
        unique(
          benchmark$ECONOMY_NAME
        ),
        
        " versus Global Average"
        
      ),
    
    subtitle =
      "Comparison across the seven GII pillars",
    
    x =
      "Score",
    
    y = NULL,
    
    caption =
      "Grey = World average; Black = Selected economy"
    
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    panel.grid.major.y =
      element_blank(),
    
    plot.title =
      element_text(
        
        face = "bold",
        
        size = 17
        
      )
    
  )


p7



# ============================================================
#
# FIGURE 8
# Japan Innovation Strengths and Weaknesses
#
# ============================================================


# ------------------------------------------------------------
# Step 8.1
# Find all bottom-level indicators
# ------------------------------------------------------------

indicators <- index_structure %>%
  
  filter(
    
    LEVEL ==
      "Indicator"
    
  )



# 查看 Indicator 数量

nrow(
  indicators
)



# ------------------------------------------------------------
# Step 8.2
# Extract Japanese indicator data
# ------------------------------------------------------------

japan_indicators <- data %>%
  
  filter(
    
    ISO3 ==
      "JPN",
    
    NUM %in%
      indicators$NUM,
    
    SW_OVERALL %in%
      c(
        "S",
        "W"
      )
    
  ) %>%
  
  filter(
    
    !is.na(SCORE),
    
    !is.na(RANK)
    
  )



# 查看

japan_indicators %>%
  
  select(
    
    NAME,
    
    SCORE,
    
    RANK,
    
    SW_OVERALL
    
  )



# ------------------------------------------------------------
# Step 8.3
# Select five strengths
# ------------------------------------------------------------

# Rank 越小表示排名越高

strengths <- japan_indicators %>%
  
  filter(
    
    SW_OVERALL ==
      "S"
    
  ) %>%
  
  slice_min(
    
    order_by = RANK,
    
    n = 5,
    
    with_ties = FALSE
    
  )



# ------------------------------------------------------------
# Step 8.4
# Select five weaknesses
# ------------------------------------------------------------

# Weakness 中选择排名较后的指标

weaknesses <- japan_indicators %>%
  
  filter(
    
    SW_OVERALL ==
      "W"
    
  ) %>%
  
  slice_max(
    
    order_by = RANK,
    
    n = 5,
    
    with_ties = FALSE
    
  )



# ------------------------------------------------------------
# Step 8.5
# Combine strengths and weaknesses
# ------------------------------------------------------------

sw_data <- bind_rows(
  
  strengths,
  
  weaknesses
  
) %>%
  
  mutate(
    
    Category =
      case_when(
        
        SW_OVERALL == "S" ~
          "Strength",
        
        SW_OVERALL == "W" ~
          "Weakness"
        
      )
    
  )



# 查看最终数据

sw_data %>%
  
  select(
    
    NAME,
    
    SCORE,
    
    RANK,
    
    Category
    
  )



# ------------------------------------------------------------
# Step 8.6
# Plot strengths and weaknesses
# ------------------------------------------------------------

p8 <- ggplot(
  
  data = sw_data,
  
  aes(
    
    x = SCORE,
    
    y =
      reorder(
        
        NAME,
        
        SCORE
        
      ),
    
    color =
      Category
    
  )
  
) +
  
  geom_segment(
    
    aes(
      
      x = 0,
      
      xend = SCORE,
      
      yend =
        reorder(
          
          NAME,
          
          SCORE
          
        )
      
    ),
    
    color =
      "grey75",
    
    linewidth =
      0.8
    
  ) +
  
  geom_point(
    
    size = 4
    
  ) +
  
  geom_text(
    
    aes(
      
      label =
        paste0(
          
          "Rank ",
          
          RANK
          
        )
      
    ),
    
    hjust =
      -0.2,
    
    size =
      3.2,
    
    show.legend =
      FALSE
    
  ) +
  
  scale_color_manual(
    
    values =
      c(
        
        "Strength" =
          "#0072B2",
        
        "Weakness" =
          "#D55E00"
        
      )
    
  ) +
  
  expand_limits(
    
    x =
      max(
        
        sw_data$SCORE,
        
        na.rm = TRUE
        
      ) + 15
    
  ) +
  
  labs(
    
    title =
      "Japan's Innovation Strengths and Weaknesses",
    
    subtitle =
      "Selected indicators from the Global Innovation Index",
    
    x =
      "Normalized Score",
    
    y = NULL,
    
    color = NULL,
    
    caption =
      "Source: WIPO Global Innovation Index"
    
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    panel.grid.major.y =
      element_blank(),
    
    legend.position =
      "top",
    
    plot.title =
      element_text(
        
        face = "bold",
        
        size = 17
        
      )
    
  )


p8



# ============================================================
# STEP 9
# Save all figures
# ============================================================


ggsave(
  
  "01_GII_Top15.png",
  
  plot = p1,
  
  width = 9,
  
  height = 6,
  
  dpi = 300
  
)


ggsave(
  
  "02_GII_World_Map.png",
  
  plot = p2,
  
  width = 12,
  
  height = 7,
  
  dpi = 300
  
)


ggsave(
  
  "03_GII_GDP.png",
  
  plot = p3_label,
  
  width = 9,
  
  height = 6,
  
  dpi = 300
  
)


ggsave(
  
  "04_Input_Output.png",
  
  plot = p4,
  
  width = 8,
  
  height = 7,
  
  dpi = 300
  
)


ggsave(
  
  "05_Pillar_Heatmap.png",
  
  plot = p5,
  
  width = 11,
  
  height = 7,
  
  dpi = 300
  
)


ggsave(
  
  "06_Regional_Distribution.png",
  
  plot = p6,
  
  width = 10,
  
  height = 7,
  
  dpi = 300
  
)


ggsave(
  
  "07_Japan_World.png",
  
  plot = p7,
  
  width = 9,
  
  height = 6,
  
  dpi = 300
  
)


ggsave(
  
  "08_Japan_Strength_Weakness.png",
  
  plot = p8,
  
  width = 10,
  
  height = 7,
  
  dpi = 300
  
)



# ============================================================
# STEP 10
# Display all figures again
# ============================================================

p1

p2

p3_label

p4

p5

p6

p7

p8

