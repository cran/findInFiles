library(findInFiles)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinyjqui)


onKeyDown <- HTML(
  'function onKeyDown(event) {',
  '  var key = event.which || event.keyCode;',
  '  if(key === 13) {',
  '    Shiny.setInputValue(',
  '      "pattern", event.target.value, {priority: "event"}',
  '    );',
  '  }',
  '}'
)

css <- '
a.list-group-item>h4 {
  font-size: 12px;
  word-break: break-all;
}
'

ui <- fluidPage(
  tags$head(
    tags$script(src = "shinyFIF.js"),
    tags$script(onKeyDown),
    tags$style(HTML(css))
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "ext", "Extension",
        choices = c("R", "js", "css")
      ),
      tags$div(
        class = "form-group shiny-input-container",
        tags$label(
          class = "control-label",
          "Pattern"
        ),
        tags$input(
          type = "text",
          class = "form-control",
          onkeydown = "onKeyDown(event);",
          placeholder = "Press Enter when ready"
        )
      ),
      numericInput(
        "depth", "Depth (set -1 for unlimited depth)",
        value = -1, min = -1, step = 1
      ),
      checkboxInput(
        "wholeWord", "Whole word"
      ),
      checkboxInput(
        "ignoreCase", "Ignore case"
      )
    ),
    mainPanel(
      width = 9,
      style = "display:flex; flex-flow:column; height: 95vh",
      jqui_resizable(tags$div(
        id = "editors",
        # conditionalPanel(
        #   "output.folderOK",
        #   style = "display: none;",
          # verticalTabsetPanel(
          #   id = "tabset"
          # )
        # )
      ), options = list(
        handles = "s",
        alsoResize = ".ace_editor"
      )),
      br(),
      FIFOutput("results", height = "100%"),
      br()
    )
  )
)


server <- function(input, output, session){

  setwd("C:/SL/MyPackages/findInFiles")

  Tabsets <- reactiveVal(character(0L))
  Editors <- reactiveVal(character(0L))

  observeEvent(input$closetab, {
    print(input$closetab)
    # file <- Tabsets()[input$closetab]
    index <- match(input$closetab, names(Tabsets()))
    Tabsets(Tabsets()[-index])
    print(Tabsets())
    if(length(Tabsets()) == 0L) {
      removeUI("#tabset-tabbable")
    }else{
      removeVerticalTab("tabset", index)
    }
  })

  observeEvent(input$filewithline, {
    print(Tabsets())
    notabset <- length(Tabsets()) == 0L
    file <- input$filewithline$file
    if(!is.element(file, Tabsets())){
      outputId <- paste0("editor", length(Editors()) + 1L)
      Editors(c(Editors(), outputId))
      names(file) <- outputId
      Tabsets(c(Tabsets(), file))
      tab <- verticalTabPanel(
        title = file,
        aceEditor(
          outputId = outputId,
          value = paste0(readLines(file), collapse = "\n"),
          mode = "r",
          theme = "cobalt",
          height = "40vh"
        ),
        box_height = NULL
      )
      if(notabset){
        tabset <- verticalTabsetPanel(tab, id = "tabset")
        insertUI(
          "#editors",
          where = "afterBegin",
          ui = tabset
        )
      }else{
        appendVerticalTab(
          "tabset",
          tab
        )
        updateVerticalTabsetPanel(
          session,
          "tabset",
          selected = file[[1L]]
        )
      }
      session$sendCustomMessage("closeButton", outputId)
      print(Tabsets())

    }
  })

  output[["results"]] <- renderFIF({
    req(input[["pattern"]])
    findInFiles(
      ext = isolate(input[["ext"]]),
      pattern = input[["pattern"]],
      depth = isolate(input[["depth"]]),
      wholeWord = isolate(input[["wholeWord"]]),
      ignoreCase = isolate(input[["ignoreCase"]])
    )
  })

}


shinyApp(ui, server)

