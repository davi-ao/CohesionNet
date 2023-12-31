
# ------------------------------------------------------------------------ #
#
# Descriptif : Radio Group Buttons : fonctions R
#     Detail : http://getbootstrap.com/javascript/#buttons-checkbox-radio
#
#
# Auteur : Victor PERRIER
#
# Date creation : 01/07/2016
# Date modification : 01/07/2016
#
# Version 1.0
#
# ------------------------------------------------------------------------ #



# radioGroupButtons ----------------------------------------------------

library("shiny")
library("shinyWidgets")

fluidPage(
  theme = bslib::bs_theme(version = 4),

  tags$h1("radioGroupButtons", style = "color: steelblue;"),
  tags$h3("Des radio en forme de boutons."),
  br(),
  tags$h4("Usage classique"),
  fluidRow(
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test1",
        label = "Juste avec les labels",
        choices = c("Choice 1", "Choice 2", "Choice 3")
      ),
      verbatimTextOutput(outputId = "res1")
    ),
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test2",
        label = "Avec des values différentes des labels",
        choices = c("Choice 1" = "A", "Choice 2" = "B", "Choice 3" = "C")
      ),
      verbatimTextOutput(outputId = "res2")
    ),
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test3",
        label = "Avec des Choice par défauts",
        choices = c("Choice 1" = "A", "Choice 2" = "B", "Choice 3" = "C", "Choice 4" = "D"),
        selected = "B"
      ),
      verbatimTextOutput(outputId = "res3")
    )
  ),

  br(), br(),

  tags$h4("Avec des statuts bootstrap"),
  fluidRow(
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test4",
        label = "Juste avec les labels",
        choices = c("Choice 1", "Choice 2", "Choice 3"),
        status = "primary"
      ),
      verbatimTextOutput(outputId = "res4")
    ),
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test5",
        label = "Avec des values différentes des labels",
        choices = c("Choice 1" = "A", "Choice 2" = "B", "Choice 3" = "C"),
        status = "danger"
      ),
      verbatimTextOutput(outputId = "res5")
    ),
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test6", label = "Avec des Choice par défauts",
        choices = c("Choice 1" = "A", "Choice 2" = "B", "Choice 3" = "C", "Choice 4" = "D"),
        selected = "B",
        status = "success"
      ),
      verbatimTextOutput(outputId = "res6")
    )
  ),

  br(), br(),

  tags$h4("Avec effets de style"),
  fluidRow(
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test7",
        label = "Button size :",
        choices = c("Choice 1", "Choice 2"),
        size = "lg"
      ),
      verbatimTextOutput(outputId = "res7"),

      radioGroupButtons(
        inputId = "test11",
        label = "Icon for checked button:",
        choices = c("Choice 1", "Choice 2", "Choice 3"),
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),
      verbatimTextOutput(outputId = "res11")
    ),
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test8",
        label = "Vertical buttons",
        choices = c("Choice 1", "Choice 2", "Choice 3"),
        direction = "vertical",
        width = "100%"
      ),
      verbatimTextOutput(outputId = "res8"),

      radioGroupButtons(
        inputId = "test13",
        label = "Icon for checked button:",
        choices = c("Choice 1", "Choice 2", "Choice 3"),
        checkIcon = list(
          yes = icon("check"),
          no = icon("xmark")
        )
      ),
      verbatimTextOutput(outputId = "res13")
    ),
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test9",
        label = "Full width:",
        choices = c("Choice 1", "Choice 2"),
        justified = TRUE
      ),
      verbatimTextOutput(outputId = "res9"),

      radioGroupButtons(
        inputId = "test14",
        label = "Individual buttons:",
        choices = c("Choice 1", "Choice 2", "Choice 3"),
        individual = TRUE
      ),
      verbatimTextOutput(outputId = "res14")
    )
  ),

  br(), br(),

  tags$h4("Update !"),
  fluidRow(
    column(
      width = 4,
      radioGroupButtons(
        inputId = "test10",
        choices = c("A", "B", "C", "D")
      ),
      verbatimTextOutput(outputId = "res10")
    ),
    column(
      width = 4,
      selectizeInput(inputId = "maj", label = "Update radioGroupButtons", choices = c("Aucun", "A", "B", "C", "D"), selected = "Aucun")
    )
  ), br(), br(),
  tags$h4(icon("thumbs-up", "fa-2x"), "Icons !"),
  fluidRow(
    column(
      width = 12,
      tags$div(
        style = "width: 100%;",
        radioGroupButtons(
          inputId = "test12",
          choices = c(
            "<i class='fa fa-bar-chart'></i>" = 1,
            "<i class='fa fa-bar-chart'></i> (stacked)" = 2,
            "<i class='fa fa-bar-chart'></i> (dodged)" = 3,
            "<i class='fa fa-pie-chart'></i>" = 4
          ), justified = TRUE
        ),
        verbatimTextOutput(outputId = "res12")
      )
    )
  ), br(), br(),

  fluidRow(
    column(
      width = 4,
      tags$h4("radioButtons de shiny"),
      radioButtons(inputId = "verif", label = "Classique", choices = c("Choice 1", "Choice 2", "Choice 3")),
      verbatimTextOutput(outputId = "resverif")
    ),
    column(
      width = 4,
      tags$h4("Conflicts with DT"),
      dataTableOutput(outputId = "verifDT")
    )
  )
)



