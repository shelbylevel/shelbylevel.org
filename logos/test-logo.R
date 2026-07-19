library(shiny)
library(bslib)

# Read the SVG logo
logo_svg <- readLines("logos/sl-logo.svg")
logo_html <- paste(logo_svg, collapse = "\n")

logo_html <- paste(readLines("logos/sl-logo.svg"), collapse = "\n")

ui <- page_navbar(
  title = HTML(paste(readLines("logos/sl-logo.svg"), collapse = "\n")),
  theme = bs_theme(
    preset = "cosmo",
    primary = "#45767a",
    secondary = "#ff6b6b"
  ),

  nav_panel(
    "Home",
    class = "p-4",
    h1("Welcome to Shelby Level"),
    p("This is a test of the SVG logo in a Shiny navbar."),
    p("Hover over the logo in the navbar to see the animation!"),
    br(),
    div(
      class = "alert alert-info",
      h4("Logo Details"),
      p("The logo features:"),
      tags$ul(
        tags$li("Responsive SVG design"),
        tags$li("Hover animation on the horizontal line"),
        tags$li("Even spacing across all elements"),
        tags$li("Dark mode compatible")
      )
    )
  ),

  nav_panel(
    "Test Controls",
    class = "p-4",
    h2("Logo Customization"),
    p("Use these controls to test different navbar configurations:"),
    br(),
    fluidRow(
      column(
        6,
        h4("Navbar Options"),
        checkboxInput("sticky_nav", "Sticky navbar", value = FALSE),
        checkboxInput("show_logo", "Show logo", value = TRUE),
        br(),
        actionButton("reload_logo", "Reload Logo", class = "btn-primary")
      ),
      column(
        6,
        h4("Preview"),
        p("Current settings:"),
        verbatimTextOutput("settings_output")
      )
    )
  ),

  nav_panel(
    "About",
    class = "p-4",
    h2("About This App"),
    p("This Shiny app is a testing ground for the Shelby Level logo design."),
    p("Features tested:"),
    tags$ul(
      tags$li("Logo rendering in navbar"),
      tags$li("Responsive sizing"),
      tags$li("Hover animations"),
      tags$li("Dark/light mode compatibility")
    ),
    br(),
    div(
      class = "card",
      div(
        class = "card-body",
        h5(class = "card-title", "Brand Colors"),
        p("Primary: ", code("#45767a"), " (Faded Jade)"),
        p("Secondary: ", code("#ff6b6b"), " (Coral Accent)"),
        style = "background-color: #f8f9fa;"
      )
    )
  )
)

server <- function(input, output, session) {
  output$settings_output <- renderText({
    paste(
      "Sticky navbar:",
      input$sticky_nav,
      "\nShow logo:",
      input$show_logo,
      "\nLoaded at:",
      Sys.time()
    )
  })

  observeEvent(input$reload_logo, {
    showNotification("Logo reloaded!", type = "message", duration = 2)
  })
}

shinyApp(ui, server)
