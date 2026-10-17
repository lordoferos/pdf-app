# =============================================================================
# PDF Toolkit — Shiny app (merge, split, extract, rotate)
# Rebuilt 17 Oct 2026. Runs fully offline; files never leave your machine.
#
# Packages: install.packages(c("shiny", "qpdf", "zip"))
# Run:      shiny::runApp("path/to/pdf_toolkit")   or open app.R and click Run App
# =============================================================================

library(shiny)
library(qpdf)

options(shiny.maxRequestSize = 200 * 1024^2)  # allow uploads up to 200 MB

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# "1-3, 5, 8-10" -> c(1,2,3,5,8,9,10). Stops with a readable message on bad input.
parse_pages <- function(txt, n_pages) {
  txt <- gsub("\\s+", "", txt)
  if (!nzchar(txt)) stop("Enter at least one page or range, e.g. 1-3, 5")
  parts <- strsplit(txt, ",", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  pages <- unlist(lapply(parts, function(p) {
    if (grepl("^[0-9]+$", p)) return(as.integer(p))
    if (grepl("^[0-9]+-[0-9]+$", p)) {
      ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
      return(seq(ab[1], ab[2]))          # 5-3 also works (reversed order)
    }
    if (grepl("^[0-9]+-$", p)) return(seq(as.integer(sub("-$", "", p)), n_pages))  # "7-" = 7 to end
    stop("Can't read '", p, "'. Use numbers and ranges like 1-3, 5, 8-")
  }))
  bad <- pages[pages < 1 | pages > n_pages]
  if (length(bad)) stop("Page(s) out of range: ", paste(unique(bad), collapse = ", "),
                        ". This file has ", n_pages, " pages.")
  pages
}

# Split points "4, 9" on a 12-page file -> list(1:4, 5:9, 10:12)
split_ranges <- function(cuts, n_pages) {
  cuts <- sort(unique(cuts[cuts < n_pages]))
  starts <- c(1, cuts + 1)
  ends   <- c(cuts, n_pages)
  Map(seq, starts, ends)
}

clean_name <- function(x) sub("\\.pdf$", "", basename(x), ignore.case = TRUE)

make_zip <- function(zipfile, files) {
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile, files)
  } else {
    utils::zip(zipfile, files, flags = "-j9X")   # needs a system zip tool on Windows
  }
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { max-width: 1100px; margin: auto; }
    .note { color: #555; font-size: 0.9em; }
    .err  { color: #b00020; font-weight: 600; }
    .ok   { color: #1b6e3a; font-weight: 600; }
  "))),
  titlePanel("PDF Toolkit"),
  p(class = "note", "Merge, split, extract and rotate PDFs. Everything runs locally."),
  
  tabsetPanel(
    # ---- Merge ---------------------------------------------------------------
    tabPanel("Merge",
             br(),
             sidebarLayout(
               sidebarPanel(
                 fileInput("merge_files", "Choose PDFs (select several)",
                           multiple = TRUE, accept = ".pdf"),
                 textInput("merge_order", "Order (file numbers from the table)", ""),
                 p(class = "note", "Leave blank to keep the upload order. Example: 3,1,2"),
                 textInput("merge_name", "Output file name", "merged.pdf"),
                 downloadButton("merge_dl", "Merge & download")
               ),
               mainPanel(
                 tableOutput("merge_table"),
                 uiOutput("merge_msg")
               )
             )
    ),
    
    # ---- Split ---------------------------------------------------------------
    tabPanel("Split",
             br(),
             sidebarLayout(
               sidebarPanel(
                 fileInput("split_file", "Choose a PDF", accept = ".pdf"),
                 radioButtons("split_mode", "Split how?",
                              c("After specific pages" = "at",
                                "Every N pages"        = "every",
                                "Every page separately" = "single")),
                 conditionalPanel("input.split_mode == 'at'",
                                  textInput("split_at", "Split after page(s)", "")),
                 conditionalPanel("input.split_mode == 'every'",
                                  numericInput("split_n", "Pages per file", 2, min = 1, step = 1)),
                 downloadButton("split_dl", "Split & download (.zip)")
               ),
               mainPanel(uiOutput("split_info"))
             )
    ),
    
    # ---- Extract -------------------------------------------------------------
    tabPanel("Extract / reorder",
             br(),
             sidebarLayout(
               sidebarPanel(
                 fileInput("ext_file", "Choose a PDF", accept = ".pdf"),
                 textInput("ext_pages", "Pages to keep, in this order", ""),
                 p(class = "note", "Examples: 1-3, 7   |   5-1 (reverse)   |   4-  (page 4 to end)"),
                 downloadButton("ext_dl", "Extract & download")
               ),
               mainPanel(uiOutput("ext_info"))
             )
    ),
    
    # ---- Rotate --------------------------------------------------------------
    tabPanel("Rotate",
             br(),
             sidebarLayout(
               sidebarPanel(
                 fileInput("rot_file", "Choose a PDF", accept = ".pdf"),
                 textInput("rot_pages", "Pages to rotate (blank = all)", ""),
                 selectInput("rot_angle", "Rotate clockwise by",
                             c("90°" = 90, "180°" = 180, "270°" = 270)),
                 downloadButton("rot_dl", "Rotate & download")
               ),
               mainPanel(uiOutput("rot_info"))
             )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Shows the error in the app instead of silently failing the download
  safely <- function(expr, msg_id) {
    tryCatch(expr, error = function(e) {
      output[[msg_id]] <- renderUI(p(class = "err", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error", duration = 8)
      stop(e)
    })
  }
  
  # ---- Merge ----------------------------------------------------------------
  merge_info <- reactive({
    req(input$merge_files)
    f <- input$merge_files
    f$pages <- vapply(f$datapath, function(p) {
      tryCatch(as.integer(pdf_length(p)), error = function(e) NA_integer_)
    }, integer(1))
    f
  })
  
  output$merge_table <- renderTable({
    f <- merge_info()
    data.frame(`#` = seq_len(nrow(f)), File = f$name, Pages = f$pages,
               check.names = FALSE)
  })
  
  output$merge_msg <- renderUI({
    f <- merge_info()
    if (any(is.na(f$pages)))
      p(class = "err", "Can't read: ", paste(f$name[is.na(f$pages)], collapse = ", "),
        " (damaged or password-protected).")
    else p(class = "ok", nrow(f), " files, ", sum(f$pages), " pages in total.")
  })
  
  output$merge_dl <- downloadHandler(
    filename = function() {
      nm <- trimws(input$merge_name)
      if (!nzchar(nm)) nm <- "merged.pdf"
      if (!grepl("\\.pdf$", nm, ignore.case = TRUE)) nm <- paste0(nm, ".pdf")
      nm
    },
    content = function(file) {
      safely({
        f <- merge_info()
        if (any(is.na(f$pages))) stop("Remove the unreadable file(s) first.")
        ord <- if (nzchar(trimws(input$merge_order))) {
          parse_pages(input$merge_order, nrow(f))
        } else seq_len(nrow(f))
        pdf_combine(f$datapath[ord], output = file)
      }, "merge_msg")
    }
  )
  
  # ---- Split ----------------------------------------------------------------
  split_n_pages <- reactive({ req(input$split_file); pdf_length(input$split_file$datapath) })
  
  split_plan <- reactive({
    n <- split_n_pages()
    switch(input$split_mode,
           at     = { req(nzchar(trimws(input$split_at)))
             split_ranges(parse_pages(input$split_at, n), n) },
           every  = { k <- max(1, as.integer(input$split_n))
           split_ranges(seq(k, n, by = k), n) },
           single = as.list(seq_len(n)))
  })
  
  output$split_info <- renderUI({
    n <- split_n_pages()
    plan <- tryCatch(split_plan(), error = function(e) conditionMessage(e))
    if (is.character(plan)) return(tagList(p(n, " pages."), p(class = "err", plan)))
    labels <- vapply(plan, function(r)
      if (length(r) == 1) paste("page", r) else paste0("pages ", min(r), "-", max(r)),
      character(1))
    tagList(p(n, " pages -> ", length(plan), " files:"),
            tags$ol(lapply(labels, tags$li)))
  })
  
  output$split_dl <- downloadHandler(
    filename = function() paste0(clean_name(input$split_file$name), "_split.zip"),
    content = function(file) {
      safely({
        plan <- split_plan()
        src  <- input$split_file$datapath
        base <- clean_name(input$split_file$name)
        tmp  <- file.path(tempdir(), paste0("split_", as.integer(Sys.time())))
        dir.create(tmp)
        width <- nchar(length(plan))
        outs <- vapply(seq_along(plan), function(i) {
          out <- file.path(tmp, sprintf(paste0("%s_part%0", width, "d.pdf"), base, i))
          pdf_subset(src, pages = plan[[i]], output = out)
        }, character(1))
        make_zip(file, outs)
      }, "split_info")
    }
  )
  
  # ---- Extract --------------------------------------------------------------
  output$ext_info <- renderUI({
    req(input$ext_file)
    n <- pdf_length(input$ext_file$datapath)
    if (!nzchar(trimws(input$ext_pages))) return(p(n, " pages."))
    pg <- tryCatch(parse_pages(input$ext_pages, n), error = function(e) conditionMessage(e))
    if (is.character(pg)) tagList(p(n, " pages."), p(class = "err", pg))
    else p(class = "ok", "Output will have ", length(pg), " pages: ",
           paste(head(pg, 40), collapse = ", "), if (length(pg) > 40) " ...")
  })
  
  output$ext_dl <- downloadHandler(
    filename = function() paste0(clean_name(input$ext_file$name), "_extract.pdf"),
    content = function(file) {
      safely({
        src <- input$ext_file$datapath
        pg  <- parse_pages(input$ext_pages, pdf_length(src))
        pdf_subset(src, pages = pg, output = file)
      }, "ext_info")
    }
  )
  
  # ---- Rotate ---------------------------------------------------------------
  output$rot_info <- renderUI({
    req(input$rot_file)
    p(pdf_length(input$rot_file$datapath), " pages.")
  })
  
  output$rot_dl <- downloadHandler(
    filename = function() paste0(clean_name(input$rot_file$name), "_rotated.pdf"),
    content = function(file) {
      safely({
        src <- input$rot_file$datapath
        n   <- pdf_length(src)
        pg  <- if (nzchar(trimws(input$rot_pages))) parse_pages(input$rot_pages, n) else seq_len(n)
        pdf_rotate_pages(src, pages = unique(pg), angle = as.integer(input$rot_angle),
                         relative = TRUE, output = file)
      }, "rot_info")
    }
  )
}

shinyApp(ui, server)