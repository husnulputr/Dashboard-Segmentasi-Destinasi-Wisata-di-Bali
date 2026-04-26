library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(leaflet)
library(plotly)
library(cluster)
library(factoextra)
library(fmsb)

# ==================================================
# 1. DATA ENGINE (BACKEND)
# ==================================================

# Palet Warna
my_pal <- c("#867B18", "#8C2E3E", "#5E5E5E", "#A49E8D")

# Fungsi Pembersih
clean_val <- function(x) {
  res <- as.numeric(gsub("[^0-9.]", "", as.character(x)))
  return(ifelse(is.na(res), 0, res))
}

std_n <- function(x) {
  x %>% str_to_lower() %>% 
    str_replace_all("temple|beach|rice terrace|forest", "") %>%
    str_replace_all("[^a-z]", "") %>%
    str_trim()
}

# Load Data (Sesuaikan path jika diperlukan)
df_destinasi <- read.csv("Dashboard Pariwisata/DATASET DESTINASI BALI.csv", check.names = FALSE)
df_kunjungan <- read.csv("Dashboard Pariwisata/DATASET KUNJUNGAN WISATA.csv", skip = 3, check.names = FALSE)
df_penilaian <- read.csv("Dashboard Pariwisata/DATASET PENILAIAN WISATA BALI.csv", check.names = FALSE)
df_fasilitas <- read.csv("Dashboard Pariwisata/Dataset_Fasilitas_Pendukung_Bali.csv", check.names = FALSE)

# --- PROSES DATA KUNJUNGAN (UPGRADE) ---
df_kunjungan_clean <- df_kunjungan %>%
  filter(!is.na(Bulan) & Bulan != "" & Bulan != "Total") %>%
  mutate(across(-Bulan, clean_val))

df_tren_tahunan <- df_kunjungan_clean %>%
  pivot_longer(cols = -Bulan, names_to = "Tahun", values_to = "Jumlah") %>%
  group_by(Tahun) %>%
  summarise(Total_Kunjungan = sum(Jumlah, na.rm = TRUE)) %>%
  mutate(Tahun = as.numeric(Tahun)) %>%
  filter(!is.na(Tahun))

# Kalkulasi Kunjungan Rata-rata (Tetap menggunakan logika Anda)
avg_kunjungan <- df_kunjungan %>%
  filter(Bulan == "BALI") %>%
  select(matches("^[0-9]{4}")) %>%
  gather(key = "Tahun", value = "Jumlah") %>%
  summarise(avg = mean(clean_val(Jumlah), na.rm = TRUE)) %>% pull(avg)

# Integrasi Dataset
df_final <- df_penilaian %>%
  mutate(key = std_n(`Tempat Wisata`)) %>%
  left_join(df_destinasi %>% mutate(key = std_n(Place)) %>% 
              select(key, Coordinate, `Google Maps Rating`, `Google Reviews (Count)`), by = "key") %>%
  left_join(df_fasilitas %>% mutate(key = std_n(`Tempat Wisata`)) %>% 
              select(key, `Fasilitas Score`), by = "key") %>%
  mutate(
    `Jarak dari Bandara (km)` = clean_val(`Jarak dari Bandara (km)`),
    `Jumlah Review` = clean_val(`Jumlah Review`),
    `Google Reviews (Count)` = clean_val(`Google Reviews (Count)`),
    `Fasilitas Score` = coalesce(`Fasilitas Score`, 5),
    
    # Feature Engineering (Scoring)
    Skor_Aksesibilitas = (100 - pmin(`Jarak dari Bandara (km)` * 1.2, 50)) + 
      (case_when(`Kondisi Jalan` == "Sangat Baik" ~ 50, `Kondisi Jalan` == "Baik" ~ 30, TRUE ~ 15)),
    Skor_Kenyamanan = (Rating * 10) + (clean_val(`Google Maps Rating`) * 8) + 
      (ifelse(Kenyamanan == "Tinggi", 20, 5)),
    Skor_Fasilitas = `Fasilitas Score` * 11,
    Skor_Popularitas = (`Google Reviews (Count)` / 1000) + (avg_kunjungan / 100000),
    
    # Koordinat Parser
    lat = as.numeric(str_extract(Coordinate, "-?[0-9]+\\.[0-9]+")),
    lng = as.numeric(str_extract(Coordinate, "(?<=, )(-?[0-9]+\\.[0-9]+)|(?<=\\s)(-?[0-9]+\\.[0-9]+)"))
  ) %>%
  filter(!is.na(lat)) 

# Scaling untuk HCA
cols_hca <- c("Skor_Aksesibilitas", "Skor_Kenyamanan", "Skor_Fasilitas", "Skor_Popularitas")
df_scaled <- scale(df_final[, cols_hca])
rownames(df_scaled) <- df_final$`Tempat Wisata`

# ==================================================
# 2. UI (USER INTERFACE)
# ==================================================

ui <- bs4DashPage(
  title = "Bali Tourism Analytics",
  dark = NULL,
  header = bs4DashNavbar(
    status = "white",
    title = span(icon("umbrella-beach"), " Bali Tourism Intelligence", style="font-weight: bold; color: #8C2E3E;")
  ),
  sidebar = bs4DashSidebar(
    status = "olive", skin = "light",
    bs4SidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "tab_home", icon = icon("chart-pie")),
      menuItem("Tren Kunjungan", tabName = "tab_kunjungan", icon = icon("users")), # TAB BARU
      menuItem("Segmentasi (HCA)", tabName = "tab_hca", icon = icon("layer-group")),
      menuItem("Aksesibilitas", tabName = "tab_analisis", icon = icon("car-side")),
      menuItem("Peta & Data", tabName = "tab_map", icon = icon("map-marked-alt"))
    )
  ),
  body = bs4DashBody(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap');
      body { font-family: 'Poppins', sans-serif; background-color: #EDE6D4 !important; }
      .card { border-radius: 15px !important; }
    "))),
    tabItems(
      # Tab Home
      tabItem(tabName = "tab_home",
              fluidRow(
                bs4ValueBoxOutput("box1", width = 3),
                bs4ValueBoxOutput("box2", width = 3),
                bs4ValueBoxOutput("box3", width = 3),
                bs4ValueBoxOutput("box4", width = 3)
              ),
              fluidRow(
                bs4Card(title = "Top 10 Destinasi Popularitas", width = 8, status = "maroon", plotlyOutput("plot_bar")),
                bs4Card(title = "Distribusi Klaster", width = 4, status = "olive", plotlyOutput("plot_pie"))
              )
      ),
      
      # TAB BARU: Tren Kunjungan Domestik
      tabItem(tabName = "tab_kunjungan",
              fluidRow(
                bs4Card(title = "Pertumbuhan Kunjungan Domestik ke Bali", width = 12, status = "maroon",
                        plotlyOutput("plot_tren_domestik", height = "400px")),
                bs4Card(title = "Tabel Analisa Informasi Kunjungan (2004-2025)", width = 12, status = "olive",
                        DTOutput("table_kunjungan"))
              )
      ),
      
      # Tab HCA
      tabItem(tabName = "tab_hca",
              fluidRow(
                column(3, bs4Card(title = "Parameter", width = 12, status = "maroon",
                                  selectInput("linkage", "Linkage Method:", choices = c("ward.D2", "complete", "average")),
                                  sliderInput("k_num", "Jumlah Cluster (k):", 2, 5, 3))),
                column(9, bs4Card(title = "Visualisasi Cluster (PCA)", width = 12, status = "olive", plotOutput("plot_pca")))
              ),
              fluidRow(bs4Card(title = "Struktur Dendrogram", width = 12, status = "maroon", plotOutput("plot_dendro")))
      ),
      # Tab Analisis Aksesibilitas
      tabItem(tabName = "tab_analisis",
              fluidRow(
                bs4Card(title = "Filter Aksesibilitas", width = 12, status = "maroon",
                        fluidRow(
                          column(4, sliderInput("range_jarak", "Jarak Bandara (km):", 0, 150, c(0, 150))),
                          column(4, selectInput("filter_kab", "Kabupaten:", choices = c("Semua", unique(df_final$Kabupaten)))),
                          column(4, checkboxGroupInput("filter_nyaman", "Kenyamanan:", choices = unique(df_final$Kenyamanan), selected = unique(df_final$Kenyamanan), inline = TRUE))
                        ))
              ),
              fluidRow(
                bs4Card(title = "Korelasi Jarak vs Rating", width = 8, status = "olive", plotlyOutput("scatter_analisis")),
                bs4Card(title = "Jarak per Kenyamanan", width = 4, status = "olive", plotOutput("boxplot_analisis"))
              ),
              fluidRow(
                bs4Card(title = "Insight Otomatis", width = 12, status = "maroon",
                        fluidRow(
                          column(6, uiOutput("text_insight")),
                          column(3, infoBoxOutput("best_accessible", width = 12)),
                          column(3, infoBoxOutput("hidden_gem", width = 12))
                        ))
              )
      ),
      # Tab Map
      tabItem(tabName = "tab_map",
              fluidRow(
                bs4Card(title = "Peta Klaster Wisata", width = 8, status = "olive", leafletOutput("map_bali", height = "500px")),
                bs4Card(title = "Radar Profil", width = 4, status = "maroon", plotOutput("plot_radar"))
              ),
              bs4Card(title = "Database Lengkap", width = 12, status = "olive", DTOutput("table_main"))
      )
    )
  )
)

# ==================================================
# 3. SERVER (LOGIC)
# ==================================================

server <- function(input, output, session) {
  
  # --- Logic Tab Kunjungan (UPGRADE) ---
  output$plot_tren_domestik <- renderPlotly({
    p <- ggplot(df_tren_tahunan, aes(x = Tahun, y = Total_Kunjungan)) +
      geom_line(color = "#8C2E3E", size = 1.2) +
      geom_point(color = "#867B18", size = 3) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Tahun", y = "Total Wisatawan Domestik")
    ggplotly(p)
  })
  
  output$table_kunjungan <- renderDT({
    datatable(df_kunjungan_clean, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })
  
  # Reactive Data HCA
  res_data <- reactive({
    fit <- hclust(dist(df_scaled), method = input$linkage)
    df_final %>% mutate(Cluster = as.factor(cutree(fit, k = input$k_num)))
  })
  
  # Reactive Filter Analisis
  data_ana <- reactive({
    df <- res_data() %>% 
      filter(`Jarak dari Bandara (km)` >= input$range_jarak[1],
             `Jarak dari Bandara (km)` <= input$range_jarak[2],
             Kenyamanan %in% input$filter_nyaman)
    if (input$filter_kab != "Semua") df <- df %>% filter(Kabupaten == input$filter_kab)
    df
  })
  
  # Outputs Home
  output$box1 <- renderbs4ValueBox({ bs4ValueBox(nrow(df_final), "Destinasi", icon = icon("map-marker-alt"), color = "olive") })
  output$box2 <- renderbs4ValueBox({ bs4ValueBox(round(mean(df_final$Rating),1), "Avg Rating", icon = icon("star"), color = "maroon") })
  output$box3 <- renderbs4ValueBox({ bs4ValueBox(max(df_final$`Jarak dari Bandara (km)`), "Jarak Max (km)", icon = icon("road"), color = "olive") })
  output$box4 <- renderbs4ValueBox({ bs4ValueBox(format(round(avg_kunjungan/1000), big.mark="."), "Domestik (K)", icon = icon("users"), color = "maroon") })
  
  output$plot_bar <- renderPlotly({
    d <- df_final %>% arrange(desc(Skor_Popularitas)) %>% head(10)
    plot_ly(d, x = ~reorder(`Tempat Wisata`, Skor_Popularitas), y = ~Skor_Popularitas, type = "bar", marker = list(color = "#8C2E3E"))
  })
  
  output$plot_pie <- renderPlotly({
    d <- res_data() %>% count(Cluster)
    plot_ly(d, labels = ~paste("Cluster", Cluster), values = ~n, type = 'pie', marker = list(colors = my_pal))
  })
  
  # Outputs HCA
  output$plot_pca <- renderPlot({
    fviz_cluster(list(data = df_scaled, cluster = cutree(hclust(dist(df_scaled), method = input$linkage), k = input$k_num)),
                 palette = my_pal, ellipse.type = "convex", ggtheme = theme_minimal(), main = "")
  })
  
  output$plot_dendro <- renderPlot({
    fviz_dend(hclust(dist(df_scaled), method = input$linkage), k = input$k_num, k_colors = my_pal, rect = TRUE, main = "")
  })
  
  # Outputs Analisis
  output$scatter_analisis <- renderPlotly({
    p <- ggplot(data_ana(), aes(x = `Jarak dari Bandara (km)`, y = Rating, size = `Jumlah Review`, color = Kenyamanan, text = `Tempat Wisata`)) +
      geom_point(alpha = 0.7) + geom_smooth(method = "lm", color = "#8C2E3E", se = FALSE, linetype = "dashed") +
      scale_color_manual(values = c("Tinggi" = "#867B18", "Sedang" = "#A49E8D", "Rendah" = "#8C2E3E")) + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$boxplot_analisis <- renderPlot({
    ggplot(data_ana(), aes(x = Kenyamanan, y = `Jarak dari Bandara (km)`, fill = Kenyamanan)) +
      geom_boxplot() + scale_fill_manual(values = c("Tinggi" = "#867B18", "Sedang" = "#A49E8D", "Rendah" = "#8C2E3E")) + theme_minimal()
  })
  
  output$text_insight <- renderUI({
    df <- data_ana()
    cor_val <- cor(df$`Jarak dari Bandara (km)`, df$Rating, use = "complete.obs")
    tagList(
      h5("Insight:"),
      p(paste("Korelasi Jarak vs Rating:", round(cor_val, 2))),
      p(ifelse(cor_val > 0, "Destinasi jauh cenderung punya rating lebih tinggi (kualitas terjaga).", "Destinasi dekat lebih disukai pengunjung."))
    )
  })
  
  output$best_accessible <- renderInfoBox({
    best <- df_final %>% filter(`Jarak dari Bandara (km)` < 20) %>% arrange(desc(Rating)) %>% head(1)
    infoBox("Akses Terbaik", best$`Tempat Wisata`, icon = icon("car"), color = "olive", fill = TRUE)
  })
  
  output$hidden_gem <- renderInfoBox({
    gem <- df_final %>% filter(`Jarak dari Bandara (km)` > 40, Rating >= 4.5) %>% arrange(desc(Rating)) %>% head(1)
    infoBox("Hidden Gem", gem$`Tempat Wisata`, icon = icon("gem"), color = "maroon", fill = TRUE)
  })
  
  # Map & Radar
  output$map_bali <- renderLeaflet({
    res <- res_data()
    pal <- colorFactor(my_pal, res$Cluster)
    leaflet(res) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~lng, ~lat, color = ~pal(Cluster), radius = 8, fillOpacity = 0.8,
                       popup = ~paste0("<b>", `Tempat Wisata`, "</b><br>Cluster: ", Cluster))
  })
  
  output$table_main <- renderDT({
    datatable(res_data() %>% select(`Tempat Wisata`, Kabupaten, Cluster, Rating, `Jarak dari Bandara (km)`),
              selection = "single", options = list(pageLength = 5), rownames = FALSE)
  })
  
  output$plot_radar <- renderPlot({
    idx <- if(is.null(input$table_main_rows_selected)) 1 else input$table_main_rows_selected
    row <- res_data()[idx, ]
    radar_df <- as.data.frame(matrix(c(120, 120, 120, 120, 0, 0, 0, 0, 
                                       row$Skor_Aksesibilitas, row$Skor_Kenyamanan, row$Skor_Fasilitas, row$Skor_Popularitas), 
                                     nrow = 3, byrow = TRUE))
    colnames(radar_df) <- c("Akses", "Nyaman", "Fasilitas", "Populer")
    radarchart(radar_df, pcol = "#8C2E3E", pfcol = rgb(140/255, 46/255, 62/255, 0.4), plwd = 4, title = row$`Tempat Wisata`)
  })
}

shinyApp(ui, server)
