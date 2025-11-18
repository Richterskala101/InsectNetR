#' Run the Segment Reviewer Shiny App
#'
#' This Shiny application provides an interactive interface for manually
#' validating predicted audio segments from a classification model.
#'
#' Users can:
#' \itemize{
#'   \item Select a directory containing audio segment files (WAV format).
#'   \item Specify a save location and filename for validation results.
#'   \item Manually review each segment and annotate it as \emph{Correct},
#'         \emph{Incorrect}, or \emph{Skip}.
#'   \item Inspect spectrograms with zooming functionality.
#'   \item Listen to audio clips directly in the browser.
#'   \item View a logistic regression calibration plot to assess model score
#'         probabilities.
#'   \item Download plots and export validated results to CSV.
#'   \item Automatically compute score thresholds for target probabilities
#'         (0.7, 0.8, 0.9).
#' }
#'
#' This tool is useful for calibrating and validating machine learning models
#' for audio classification tasks.
#'
#' @details
#' The app expects a folder structure where subdirectories represent classes
#' (e.g., `Bird`, `Frog`, etc.), each containing `.wav` audio segments.
#' The last underscore-separated part of each filename should encode the
#' model score, which is used in calibration.
#'
#' @return
#' Launches the Shiny application in the user’s default browser.
#'
#' @examples
#' \dontrun{
#'   run_shiny_app()
#' }
#'
#' @export
run_shiny_app <- function() {
  # ==== UI ====
  ui <- shiny::fluidPage(
    shiny::titlePanel("Segment Reviewer for Audio Classification"),

    shiny::tabsetPanel(
      id = "main_tabs",

      shiny::tabPanel("1. Setup",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shinyFiles::shinyDirButton("segment_dir_btn", "Choose Segment Directory", "Select segment folder"),
                          shiny::verbatimTextOutput("segment_dir_display"),

                          shinyFiles::shinyDirButton("save_dir_btn", "Choose Save Directory", "Select where to save results"),
                          shiny::verbatimTextOutput("save_dir_display"),

                          shiny::textInput("validation_file", "Validation File Name", value = "validation_results.csv"),
                          shiny::verbatimTextOutput("final_save_path"),
                          shiny::br(),
                          shiny::actionButton("proceed", "Go to Review Page", class = "btn-primary")
                        ),
                        shiny::mainPanel(
                          shiny::helpText(
                            "Step 1: Choose the folder containing the audio segments.",
                            "Then choose where the results should be saved and set a filename."
                          )
                        )
                      )
      ),

      shiny::tabPanel("2. Review",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::actionButton("load_data", "Load Segment Classes"),
                          shiny::uiOutput("class_ui"),
                          shiny::numericInput("segment_duration", "Segment Duration (seconds)", value = 5, min = 1),

                          shiny::hr(),
                          shiny::h4("Spectrogram Settings"),
                          shiny::numericInput("fft_window_length", "FFT Window Length", value = 1024, min = 128),
                          shiny::sliderInput("overlap", "Overlap", min = 0, max = 0.95, value = 0.75, step = 0.05),
                          shiny::numericInput("max_freq", "Max Frequency (kHz)", value = 16, min = 1),
                          shiny::checkboxInput("dark_mode", "Dark Mode", value = FALSE),
                          shiny::hr(),

                          shiny::actionButton("correct", "Correct", class = "btn-success"),
                          shiny::actionButton("incorrect", "Incorrect", class = "btn-danger"),
                          shiny::actionButton("skip", "Skip"),
                          shiny::actionButton("prev", "Previous"),
                          shiny::br(), shiny::br(),
                          shiny::verbatimTextOutput("clip_info"),
                          shiny::uiOutput("audio_ui"),
                          shiny::br(), shiny::br(),
                          shiny::helpText(
                            "This app allows you to manually validate predicted segments from an audio classification model.",
                            "After enough annotations, a logistic regression is fitted to calibrate model scores into probabilities.",
                            "Thresholds are calculated for precision levels 0.7, 0.8, and 0.9."
                          ),
                          shiny::plotOutput("logisticPlot"),
                          shiny::downloadButton("downloadPlot", "Download Logistic Plot"),
                          shiny::tableOutput("thresholds")
                        ),

                        shiny::mainPanel(
                          shiny::plotOutput(
                            "spectrogram",
                            brush = shiny::brushOpts(id = "spec_brush", resetOnNew = TRUE),
                            dblclick = "spec_dblclick",
                            height = "500px"
                          ),
                          shiny::br(),
                          shiny::HTML("<p><strong>Zoom Instructions:</strong><br>
               Click and drag on the spectrogram to zoom into a region.<br>
               Double-click anywhere on the spectrogram to reset the zoom.</p>")
                        )
                      )
      )
    )
  )

  # ==== SERVER ====

  server <- function(input, output, session) {
    # Ensure folder exists before registering it
    if (!dir.exists("www/segments")) {
      dir.create("www/segments", recursive = TRUE)
    }
    
    shiny::addResourcePath("segments", "www/segments")
    volumes <- c(Home = fs::path_home(), "B:" = "B:/", "C:" = "C:/", "D:" = "D:/")
    shinyFiles::shinyDirChoose(input, "segment_dir_btn", roots = volumes, session = session)
    shinyFiles::shinyDirChoose(input, "save_dir_btn", roots = volumes, session = session)

    segment_dir <- shiny::reactiveVal(NULL)
    save_dir <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$segment_dir_btn, {
      dir_path <- shinyFiles::parseDirPath(volumes, input$segment_dir_btn)
      if (length(dir_path) > 0 && dir.exists(dir_path)) {
        segment_dir(normalizePath(dir_path))
      }
    })

    shiny::observeEvent(input$save_dir_btn, {
      dir_path <- shinyFiles::parseDirPath(volumes, input$save_dir_btn)
      if (length(dir_path) > 0 && dir.exists(dir_path)) {
        save_dir(normalizePath(dir_path))
      }
    })

    output$segment_dir_display <- shiny::renderPrint({ segment_dir() })
    output$save_dir_display <- shiny::renderPrint({ save_dir() })

    save_path <- shiny::reactive({
      shiny::req(save_dir(), input$validation_file)
      file_name <- input$validation_file
      if (!grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        file_name <- paste0(file_name, ".csv")
      }
      file.path(save_dir(), file_name)
    })

    output$final_save_path <- shiny::renderText({
      shiny::req(save_path())
      paste("Results will be saved to:", save_path())
    })

    shiny::observeEvent(input$proceed, {
      shiny::updateTabsetPanel(session, "main_tabs", selected = "2. Review")
    })

    state <- shiny::reactiveValues(
      files = NULL,
      index = 1,
      data = data.frame(file = character(), score = numeric(), class = character(), outcome = integer()),
      classes = character(),
      zoom_time = NULL,
      zoom_freq = NULL
    )

    # Load classes
    shiny::observeEvent(input$load_data, {
      shiny::req(segment_dir())
      classes <- list.dirs(segment_dir(), full.names = FALSE, recursive = FALSE)
      state$classes <- classes
      shiny::updateSelectInput(session, "class", choices = classes)
    })

    # Class selector
    output$class_ui <- shiny::renderUI({
      shiny::req(state$classes)
      shiny::selectInput("class", "Choose Class:", choices = state$classes)
    })

    # When class changes
    shiny::observeEvent(input$class, {
      shiny::req(segment_dir())

      class_name <- input$class
      class_path <- file.path(segment_dir(), class_name)
      www_class_path <- file.path("www", "segments", class_name)

      if (!dir.exists(www_class_path)) {
        dir.create(www_class_path, recursive = TRUE)
      }

      files <- list.files(class_path, pattern = "\\.wav$", full.names = TRUE)

      for (f in files) {
        dest <- file.path(www_class_path, basename(f))
        if (!file.exists(dest)) {
          file.copy(f, dest)
        }
      }

      # Store absolute and www paths separately
      state$abs_files <- files
      state$www_files <- file.path("segments", class_name, basename(files))
      state$index <- 1

    })

    current_file <- shiny::reactive({
     state$abs_files[state$index]  # real path for spectrogram
    })

    current_file_www <- shiny::reactive({
     state$www_files[state$index]  # browser path for playback
    })

    
    # ----------------------------------------------------------------------
    # 1️⃣  Helper that moves to the next clip (used by both skip and save)
    # ----------------------------------------------------------------------
    advance <- function() {
      if (state$index < length(state$abs_files)) {
        state$index <- state$index + 1
      } else {
        shiny::showNotification("You have reached the last clip.", type = "message")
      }
      # reset zoom when we jump to a new clip
      state$zoom_time <- NULL
      state$zoom_freq <- NULL
    }
    
    # ----------------------------------------------------------------------
    # 2️⃣  Save annotation and then call `advance()`
    # ----------------------------------------------------------------------
    save_outcome <- function(outcome_val) {
      shiny::req(state$abs_files, state$index, input$class)
      
      # ---- extract the score from the file name ----------------------------
      file   <- basename(current_file())
      parts  <- strsplit(file, "_")[[1]]
      score  <- as.numeric(gsub("\\.wav$", "", parts[length(parts)]))
      
      # ---- add row to data ------------------------------------------------
      new_entry <- data.frame(
        file    = file,
        score   = score,
        class   = input$class,
        outcome = outcome_val,
        stringsAsFactors = FALSE
      )
      state$data <- dplyr::bind_rows(state$data, new_entry)
      
      # ---- optional disk write --------------------------------------------
      if (!is.null(save_path())) {
        utils::write.csv(state$data, save_path(), row.names = FALSE)
      }
      
      # ---- move on --------------------------------------------------------
      advance()
    }
    
    # ----------------------------------------------------------------------
    # 3️⃣  Wire the three buttons
    # ----------------------------------------------------------------------
    shiny::observeEvent(input$correct,   { save_outcome(1) })
    shiny::observeEvent(input$incorrect, { save_outcome(0) })
    shiny::observeEvent(input$skip,      { advance() })


    # Zoom logic
    shiny::observeEvent(input$spec_brush, {
      brush <- input$spec_brush
      if (!is.null(brush)) {
        state$zoom_time <- c(brush$xmin, brush$xmax)
        state$zoom_freq <- c(brush$ymin, brush$ymax)
      }
    })

    shiny::observeEvent(input$spec_dblclick, {
      state$zoom_time <- NULL
      state$zoom_freq <- NULL
    })

    # ==== CUSTOM SPECTROGRAM ====
    output$spectrogram <- shiny::renderPlot({
      shiny::req(current_file())

      plot_av_fft(
        path_to_file = current_file(),
        fft_window_length = input$fft_window_length,
        overlap = input$overlap,
        max_freq = input$max_freq,
        dark = input$dark_mode,
        tstart = if (!is.null(state$zoom_time)) state$zoom_time[1] else 0,
        tende  = if (!is.null(state$zoom_time)) state$zoom_time[2] else NULL
      )
    })

    # Logistic regression calibration
    output$logisticPlot <- shiny::renderPlot({
      val <- state$data
      shiny::req(input$class)
      class_val <- input$class
      val_class <- val[val$class == class_val, ]

      if (nrow(val_class) < 10) return(NULL)

      model <- stats::glm(outcome ~ score, family = "binomial", data = val_class, na.action = stats::na.omit)
      scores <- seq(0, 1, length.out = 100)
      probs <- stats::predict(model, newdata = data.frame(score = scores), type = "response")

      ggplot2::ggplot(data.frame(score = scores, prob = probs), ggplot2::aes(x = score, y = prob)) +
        ggplot2::geom_line(color = "blue") +
        ggplot2::labs(title = paste("Logistic Regression Calibration -", class_val),
                      x = "Score", y = "Probability Correct") +
        ggplot2::geom_hline(yintercept = c(0.7, 0.8, 0.9), linetype = "dashed", color = "grey")
    })

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("logistic_plot_", input$class, ".png")
      },
      content = function(file) {
        val <- state$data
        shiny::req(input$class)
        class_val <- input$class
        val_class <- val[val$class == class_val, ]

        if (nrow(val_class) < 10) return(NULL)

        model <- stats::glm(outcome ~ score, family = "binomial", data = val_class, na.action = stats::na.omit)
        scores <- seq(0, 1, length.out = 100)
        probs <- stats::predict(model, newdata = data.frame(score = scores), type = "response")

        p <- ggplot2::ggplot(data.frame(score = scores, prob = probs), ggplot2::aes(x = score, y = prob)) +
          ggplot2::geom_line(color = "blue") +
          ggplot2::labs(title = paste("Logistic Regression Calibration -", class_val),
                        x = "Score", y = "Probability Correct") +
          ggplot2::geom_hline(yintercept = c(0.7, 0.8, 0.9), linetype = "dashed", color = "grey")

        ggplot2::ggsave(file, plot = p, device = "png", width = 7, height = 5)
      }
    )

    output$thresholds <- shiny::renderTable({
      val <- state$data
      if (sum(!is.na(val$outcome)) < 10) return(NULL)
      class_val <- input$class
      val_class <- val[val$class == class_val, ]

      if (nrow(val_class) < 10) return(NULL)

      model <- stats::glm(outcome ~ score, family = "binomial", data = val_class, na.action = stats::na.omit)

      p_vals <- c(0.7, 0.8, 0.9)
      thresholds <- sapply(p_vals, function(p) {
        (log(p / (1 - p)) - coef(model)[1]) / coef(model)[2]
      })
      data.frame(`Target Probability` = p_vals, `Score Threshold` = round(thresholds, 3))
    })

    shiny::onStop(function() {
      if (!is.null(save_path())) {
        utils::write.csv(state$data, save_path(), row.names = FALSE)
      }
    })
  }

  # ==== RUN APP ====
  shiny::shinyApp(ui = ui, server = server)
}
