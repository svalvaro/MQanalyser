uiLogo <-   tabPanel(id = 'logo_tab',
                     position = 'right',
                     title =   tags$img(
                         #src='logo.png',
                         src='images/logo.png',
                         width = 150),

                     fluidRow(
                         column(9,

                                p("App created by ",
                                  tags$a(href = "https://www.linkedin.com/in/svalvaro/",
                                         'Alvaro Sanchez-Villalba',
                                         target = '_blank'),
                                  HTML("&bull;"),
                                  style = "font-size: 85%"),

                                p("Have a question? Spot an error? Send an email ",
                                  tags$a(href = "mailto:alvaro.sanchez@fgu.cas.cz",
                                         tags$i(class = 'fa fa-envelope',
                                                style = 'color:#990000'),
                                         target = '_blank'),
                                  style = "font-size: 85%"),
                                p(tags$em("Last updated: March 2022"),
                                  style = 'font-size:75%'))
                     )
)

