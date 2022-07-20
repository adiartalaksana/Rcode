library(shiny)
library(plotly)

topWidget <- function(icon, category, title, iconFooter, labelFooter) {
  div(
    class = "card card-stats",
    div(
      class = "card-body",
      div(
        class = "row",
        div(
          class = "col-3",   
          div(
            class = "info-icon text-center icon-warning", 
            span(class = paste("las", icon))
          )
        ),
        div(
          class = "col-9",  
          div(
            class = "numbers",
            h4(style="color:red",class = "cardcategory", category), 
            h3(class = "card-title", title),
          )
        ),
        
      )
    ),
    div(
       class = "card-footer",
       hr(),
       div(
         class = "stats",
         labelFooter
       )
     )
  )
}

bottomWidget <- function(content) {
  div(
    class = "card card-stats",
    div(
      class = "card-body",
      content
    )
  )
}

ui <- div(
  class = "wrapper",
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css?family=Poppins');
    @import url('https://demos.creative-tim.com/marketplace/black-dashboard-pro/assets/css/black-dashboard.min.css?v=1.1.1');
    @import url('https://maxst.icons8.com/vue-static/landings/line-awesome/line-awesome/1.3.0/css/line-awesome.min.css');

    .navbar {
      top: auto;
    }
    .card-stats .info-icon span {
      color: #fff;
      font-size: 1.7em;
      padding: 14px 13px;
    }
    .card-chart .chart-area {
      height: 450px;
    }
    .card-chart .chart-area-1 {
      height: 300px;
    }
    .plotly {
      height: auto !important;
    }
  ")),
  div(
    class = "main-panel",
    div(
      class = "navbar justify-content-center",
      div(class = "navbar-brand", "KORELASI 6 SAHAM TERHADAP IHSG",
          style="color:white;font-size:40px")
    ),
    div(
      class = "navbar justify-content-center",
      div(class = "navbar-brand", textOutput("navbar_footer"),
          style="color:white;font-size:20px")
    ),
   
    div(
      class = "p-4",
      div(
        class = "row",
        
        div(
          class = "col-lg-2 col-md-4", 
          topWidget(icon = "la-chart-area", category = textOutput("kode_saham_1"), title = textOutput("harga_saham_1"), iconFooter = "la-random", labelFooter =textOutput("korelasi_saham_1"))
        ),
        div(
          class = "col-lg-2 col-md-4", 
          topWidget(icon = "la-chart-area", category = textOutput("kode_saham_2"), title = textOutput("harga_saham_2"), iconFooter = "la-random", labelFooter =textOutput("korelasi_saham_2"))
        ),
        div(
          class = "col-lg-2 col-md-4", 
          topWidget(icon = "la-chart-area", category = textOutput("kode_saham_3"), title = textOutput("harga_saham_3"), iconFooter = "la-random", labelFooter =textOutput("korelasi_saham_3"))
        ),
        div(
          class = "col-lg-2 col-md-4", 
          topWidget(icon = "la-chart-area", category = textOutput("kode_saham_4"), title = textOutput("harga_saham_4"), iconFooter = "la-random", labelFooter =textOutput("korelasi_saham_4"))
        ),
        div(
          class = "col-lg-2 col-md-4", 
          topWidget(icon = "la-chart-area", category = textOutput("kode_saham_5"), title = textOutput("harga_saham_5"), iconFooter = "la-random", labelFooter =textOutput("korelasi_saham_5"))
        ),
        div(
          class = "col-lg-2 col-md-4", 
          topWidget(icon = "la-chart-area", category = textOutput("kode_saham_6"), title = textOutput("harga_saham_6"), iconFooter = "la-random", labelFooter =textOutput("korelasi_saham_6"))
        ),
        
        div(
          class = "col-12",   
          div(
            class = "card card-chart",  
            div(
              class = "card-header", 
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Plot Deret Waktu"),
                  h2(class = "card-title", span(class = "las la-chart-line"), "IHSG")
                ),
                div(
                  class = "col-sm-6"
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area",
                plotlyOutput("plot_IHSG")
              )
            )
          )
        ),
        div(
          class = "col-lg-6 col-md-12",  
          div(
            class = "card card-chart",  
            div(
              class = "card-header",  
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Plot Deret Waktu"),
                  h2(class = "card-title", span(class = "las la-chart-line"), "PTPP")
                ),
                div(
                  class = "col-sm-6"
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area-1",
                plotlyOutput("plot_PTPP")
              )
            )
          )
        ),
        div(
          class = "col-lg-6 col-md-12",  
          div(
            class = "card card-chart",  
            div(
              class = "card-header", 
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Plot Deret Waktu"),
                  h2(class = "card-title", span(class = "las la-chart-line"), "MNCN")
                ),
                div(
                  class = "col-sm-6"
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area-1",
                plotlyOutput("plot_MNCN")
              )
            )
          )
        ),
        div(
          class = "col-lg-6 col-md-12", 
          div(
            class = "card card-chart",  
            div(
              class = "card-header",  
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Plot Deret Waktu"),
                  h2(class = "card-title", span(class = "las la-chart-line"), "CTRA")
                ),
                div(
                  class = "col-sm-6"
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area-1",
                plotlyOutput("plot_CTRA")
              )
            )
          )
        ),
        div(
          class = "col-lg-6 col-md-12",   
          div(
            class = "card card-chart",  
            div(
              class = "card-header",  
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Plot Deret Waktu"),
                  h2(class = "card-title", span(class = "las la-chart-line"), "BBNI")
                ),
                div(
                  class = "col-sm-6"
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area-1",
                plotlyOutput("plot_BBNI")
              )
            )
          )
        ),
        div(
          class = "col-lg-6 col-md-12",  
          div(
            class = "card card-chart",  
            div(
              class = "card-header", 
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Plot Deret Waktu"),
                  h2(class = "card-title", span(class = "las la-chart-line"), "SIDO")
                ),
                div(
                  class = "col-sm-6"
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area-1",
                plotlyOutput("plot_SIDO")
              )
            )
          )
        ),
        div(
          class = "col-lg-6 col-md-12",  
          div(
            class = "card card-chart",  
            div(
              class = "card-header", 
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Plot Deret Waktu"),
                  h2(class = "card-title", span(class = "las la-chart-line"), "TLKM")
                ),
                div(
                  class = "col-sm-6"
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area-1",
                plotlyOutput("plot_TLKM")
              )
            )
          )
        )
      )
    )
  )
)