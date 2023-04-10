original <- toupper("kennedy mwavu")
split_orig <- strsplit(x = original, split = "")[[1]]

ui <- htmltools::tags$html(
  lang = "en",
  htmltools::tags$head(
    htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$meta(
      name = "viewport", content = "width=device-width, initial-scale=1"
    ),
    htmltools::tags$link(
      href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
      rel = "stylesheet",
      integrity = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
      crossorigin = "anonymous"
    ),
    htmltools::tags$link(
      href = "styles.css",
      rel = "stylesheet"
    ),
    htmltools::tags$link(
      href = "alt.svg",
      rel = "icon",
      type = "image/x-icon"
    ),
    shinyjs::useShinyjs(),
    htmltools::tags$title("Hacker Effect")
  ),
  
  htmltools::tags$body(
    class = paste(
      "d-flex flex-column justify-content-center align-items-center min-vh-100"
    ),
    
    htmltools::tags$div(
      htmltools::tags$h1(id = "text", original),
      htmltools::tags$h6("Software Developer", class = "text-white text-end")
    )
  )
)

server <- function(input, output, session) {
  rv_iterations <- shiny::reactiveVal(0)
  
  observer <- shiny::observe({
    shiny::invalidateLater(millis = 30)
    
    shinyjs::html(
      id = "text",
      html = {
        sapply(
          X = seq_along(split_orig),
          FUN = \(ind) {
            if (split_orig[ind] == " ") {
              return(" ")
            }
            if (ind <= rv_iterations()) {
              return(split_orig[ind])
            }
            LETTERS[floor(runif(n = 1, min = 1, max = 26))]
          }
        ) |> paste(collapse = "")
      }
    )
    
    if (rv_iterations() >= length(split_orig)) {
      # suspend observer and reset iteratons:
      observer$suspend()
      rv_iterations(0)
      return()
    }
    
    # increment iterations:
    n <- rv_iterations()
    rv_iterations(n + 1 / 20) # 20 iterations per letter
  },
  suspended = TRUE
  )
  
  shinyjs::onevent(
    event = "mouseover",
    id = "text",
    expr = observer$resume()
  )
}

shiny::shinyApp(ui, server)
