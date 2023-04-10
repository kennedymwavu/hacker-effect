original <- "Hyperplexed"

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
    shinyjs::useShinyjs(),
    htmltools::tags$title("Hacker Effect")
  ),
  
  htmltools::tags$body(
    htmltools::tags$h1(id = "text", original)
  )
)

server <- function(input, output, session) {
  # keep track of state of 'flag' and number of 'iterations':
  rv_flag <- shiny::reactiveVal(FALSE)
  rv_iterations <- shiny::reactiveVal(0)
  
  shiny::observe({
    # to run code in this observer, 'flag' must be TRUE
    req(rv_flag())
    
    # evaluate code in this observer 30 times per second
    shiny::invalidateLater(millis = 30, session = session)
    
    n <- rv_iterations()
    
    split_orig <- strsplit(original, split = "")[[1]]
    rand_word <- sapply(
      X = seq_along(split_orig),
      FUN = \(index) {
        if (index < n) {
          return(split_orig[index])
        }
        
        rand_ind <- floor(runif(n = 1, min = 1, max = 26))
        LETTERS[rand_ind]
      }
    ) |> paste0(collapse = "")
    
    shinyjs::html(id = "text", html = rand_word)
    
    if (n >= 100) {
      rv_flag(FALSE)
      rv_iterations(0)
      return()
    }
    
    rv_iterations(n + 1)
  })
  
  shinyjs::onevent(
    event = "mouseover",
    id = "text",
    expr = {
      rv_flag(TRUE)
    }
  )
}

shiny::shinyApp(ui, server)
