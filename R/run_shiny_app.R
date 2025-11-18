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
#--------------------------------------------------------------------
#   Segment Reviewer Shiny App
#--------------------------------------------------------------------
#   Save this whole chunk as a script (e.g. app.R) and call
#   run_shiny_app()  to start the application.
#--------------------------------------------------------------------

run_shiny_app <- function() {
  
  #==================================================================
  #   UI
  #==================================================================
  ui <- shiny::fluidPage(
    shiny::titlePanel("Segment Reviewer for Audio Classification"),
    shiny::tabsetPanel(
      id = "main_tabs",
      
      #--------------------------- 1. Setup ---------------------------
      shiny::tabPanel(
        "1. Setup",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shinyFiles::shinyDirButton(
              "segment_dir_btn", "Choose Segment Directory",
              "Select segment folder"
            ),
            shiny::verbatimTextOutput("segment_dir_display"),
            shinyFiles::shinyDirButton(
              "save_dir_btn", "Choose Save Directory",
              "Select where to save results"
            ),
            shiny::verbatimTextOutput("save_dir_display"),
            shiny::textInput(
              "validation_file", "Validation File Name",
              value = "validation_results.csv"
            ),
            shiny::verbatimTextOutput("final_save_path"),
            shiny::br(),
            shiny::actionButton("proceed", "Go to Review Page",
                                class = "btn-primary")
          ),
          shiny::mainPanel(
            shiny::helpText(
              "Step 1: Choose the folder containing the audio segments.",
              "Then choose where the results should be saved and set a filename."
            )
          )
        )
      ),
      
      #--------------------------- 2. Review -------------------------
      shiny::tabPanel(
        "2. Review",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::actionButton("load_data", "Load Segment Classes"),
            shiny::uiOutput("class_ui"),
            shiny::numericInput("segment_duration",
                                "Segment Duration (seconds)",
                                value = 5, min = 1),
            
            shiny::hr(),
            shiny::h4("Spectrogram Settings"),
            shiny::numericInput("fft_window_length",
                                "FFT Window Length",
                                value = 1024, min = 128),
            shiny::sliderInput("overlap", "Overlap",
                               min = 0, max = 0.95,
                               value = 0.75, step = 0.05),
            shiny::numericInput("max_freq",
                                "Max Frequency (kHz)",
                                value = 16, min = 1),
            shiny::checkboxInput("dark_mode", "Dark Mode", value = FALSE),
            
            shiny::hr(),
            shiny::actionButton("correct", "Correct",
                                class = "btn-success"),
            shiny::actionButton("incorrect", "Incorrect",
                                class = "btn-danger"),
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
              brush = shiny::brushOpts(id = "spec_brush",
                                       resetOnNew = TRUE),
              dblclick = "spec_dblclick",
              height = "500px"
            ),
            shiny::br(),
            shiny::HTML(
              "<p><strong>Zoom Instructions:</strong><br>
               Click and drag on the spectrogram to zoom into a region.<br>
               Double-click anywhere on the spectrogram to reset the zoom.</p>"
            )
          )
        )
      )
    )
  )
  
  #==================================================================
  #   SERVER
  #==================================================================
  server <- function(input, output, session) {
    
    #----------------------- 1. Folder handling -----------------------
    # create a resource folder for the audio files
    if (!dir.exists("www/segments")) {
      dir.create("www/segments", recursive = TRUE)
    }
    shiny::addResourcePath("segments", "www/segments")
    
    # roots for shinyFiles (you can add/remove drives as you like)
    volumes <- c(
      Home = fs::path_home(),
      "B:" = "B:/",
      "C:" = "C:/",
      "D:" = "D:/"
    )
    shinyFiles::shinyDirChoose(input, "segment_dir_btn",
                               roots = volumes, session = session)
    shinyFiles::shinyDirChoose(input, "save_dir_btn",
                               roots = volumes, session = session)
    
    segment_dir <- shiny::reactiveVal(NULL)
    save_dir    <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$segment_dir_btn, {
      dir_path <- shinyFiles::parseDirPath(volumes,
                                           input$segment_dir_btn)
      if (length(dir_path) && dir.exists(dir_path)) {
        segment_dir(normalizePath(dir_path))
      }
    })
    shiny::observeEvent(input$save_dir_btn, {
      dir_path <- shinyFiles::parseDirPath(volumes,
                                           input$save_dir_btn)
      if (length(dir_path) && dir.exists(dir_path)) {
        save_dir(normalizePath(dir_path))
      }
    })
    
    output$segment_dir_display <- shiny::renderPrint({ segment_dir() })
    output$save_dir_display    <- shiny::renderPrint({ save_dir() })
    
    # compute the final CSV path (adds .csv if missing)
    save_path <- shiny::reactive({
      shiny::req(save_dir(), input$validation_file)
      f <- input$validation_file
      if (!grepl("\\.csv$", f, ignore.case = TRUE)) f <- paste0(f, ".csv")
      file.path(save_dir(), f)
    })
    output$final_save_path <- shiny::renderText({
      shiny::req(save_path())
      paste("Results will be saved to:", save_path())
    })
    
    # go to Review tab -------------------------------------------------
    shiny::observeEvent(input$proceed, {
      shiny::updateTabsetPanel(session, "main_tabs",
                               selected = "2. Review")
    })
    
    #----------------------- 2. Reactive state -----------------------
    state <- shiny::reactiveValues(
      # files for the currently selected class
      files_abs = NULL,   # absolute paths on disk (spectrogram)
      files_www = NULL,   # web‑accessible paths (audio tag)
      index     = 1,
      
      # stored annotations
      data      = data.frame(
        file    = character(),
        score   = numeric(),
        class   = character(),
        outcome = integer(),
        stringsAsFactors = FALSE
      ),
      
      # list of class names (sub‑folders)
      classes   = character(),
      
      # zoom extents for the spectrogram
      zoom_time = NULL,
      zoom_freq = NULL
    )
    
    #----------------------- 3. Load classes -------------------------
    shiny::observeEvent(input$load_data, {
      shiny::req(segment_dir())
      classes <- list.dirs(segment_dir(),
                           full.names = FALSE,
                           recursive = FALSE)
      state$classes <- classes
    })
    output$class_ui <- shiny::renderUI({
      shiny::req(state$classes)
      shiny::selectInput("class", "Choose Class:",
                         choices = state$classes)
    })
    
    #----------------------- 4. When a class is chosen ---------------
    shiny::observeEvent(input$class, {
      shiny::req(segment_dir())
      class_name <- input$class
      class_path <- file.path(segment_dir(), class_name)
      
      # copy wav files to the www folder (so they can be streamed)
      www_class_path <- file.path("www", "segments", class_name)
      if (!dir.exists(www_class_path)) {
        dir.create(www_class_path, recursive = TRUE)
      }
      
      # absolute paths on disk
      files_abs <- list.files(class_path,
                              pattern = "\\.wav$", full.names = TRUE)
      
      # copy files that are not already there (fast for large sets)
      for (f in files_abs) {
        dest <- file.path(www_class_path, basename(f))
        if (!file.exists(dest)) {
          file.copy(f, dest, overwrite = FALSE)
        }
      }
      
      # store vectors reactively
      state$files_abs <- files_abs
      state$files_www <- file.path("segments", class_name,
                                   basename(files_abs))
      
      # start at the first clip for this class
      state$index <- 1
    })
    
    #----------------------- 5. Helpers for current file -------------
    current_file <- shiny::reactive({
      shiny::req(state$files_abs, state$index)
      state$files_abs[state$index]
    })
    current_file_www <- shiny::reactive({
      shiny::req(state$files_www, state$index)
      state$files_www[state$index]
    })
    
    #----------------------- 6. Navigation helpers -------------------
    advance <- function() {
      if (state$index < length(state$files_abs)) {
        state$index <- state$index + 1
      } else {
        shiny::showNotification("You have reached the last clip.",
                                type = "message")
      }
      # reset any zoom that was applied to the previous spectrogram
      state$zoom_time <- NULL
      state$zoom_freq <- NULL
    }
    
    #----------------------- 7. Save annotation ----------------------
    save_outcome <- function(outcome_val) {
      shiny::req(state$files_abs, state$index, input$class)
      
      # ---- extract score from filename (last "_##.wav" part) ----
      file_name <- basename(current_file())
      parts <- strsplit(file_name, "_")[[1]]
      score <- as.numeric(gsub("\\.wav$", "", parts[length(parts)]))
      
      # ---- add a row to the data frame -------------------------
      new_row <- data.frame(
        file    = file_name,
        score   = score,
        class   = input$class,
        outcome = outcome_val,
        stringsAsFactors = FALSE
      )
      state$data <- dplyr::bind_rows(state$data, new_row)
      
      # ---- write to CSV (optional, but nice) -------------------
      if (!is.null(save_path())) {
        utils::write.csv(state$data, save_path(),
                         row.names = FALSE)
      }
      
      # ---- move to next clip -----------------------------------
      advance()
    }
    
    #----------------------- 8. Button observers --------------------
    shiny::observeEvent(input$correct,   { save_outcome(1) })
    shiny::observeEvent(input$incorrect, { save_outcome(0) })
    shiny::observeEvent(input$skip,      { advance() })
    shiny::observeEvent(input$prev, {
      state$index <- max(1, state$index - 1)
      # also clear zoom when going back
      state$zoom_time <- NULL
      state$zoom_freq <- NULL
    })
    
    #----------------------- 9. UI outputs --------------------------
    output$clip_info <- shiny::renderPrint({
      shiny::req(current_file())
      paste0("Clip: ", basename(current_file()),
             " [", state$index, "/", length(state$files_abs), "]")
    })
    
    output$audio_ui <- shiny::renderUI({
      shiny::req(current_file_www())
      shiny::tags$audio(
        id = "audio",
        src = current_file_www(),
        type = "audio/wav",
        controls = NA
      )
    })
    
    #----------------------- 10. Spectrogram ------------------------
    output$spectrogram <- shiny::renderPlot({
      shiny::req(current_file())
      # NOTE: you need to have a function `plot_av_fft()` defined
      # somewhere in your package/script.  The call below is unchanged
      # from the original code – only the arguments that come from
      # the UI are kept.
      plot_av_fft(
        path_to_file   = current_file(),
        fft_window_length = input$fft_window_length,
        overlap           = input$overlap,
        max_freq          = input$max_freq,
        dark              = input$dark_mode,
        tstart = if (!is.null(state$zoom_time))
          state$zoom_time[1] else 0,
        tende  = if (!is.null(state$zoom_time))
          state$zoom_time[2] else NULL
      )
    })
    
    # zoom handling ----------------------------------------------------
    shiny::observeEvent(input$spec_brush, {
      br <- input$spec_brush
      if (!is.null(br)) {
        state$zoom_time <- c(br$xmin, br$xmax)
        state$zoom_freq <- c(br$ymin, br$ymax)
      }
    })
    shiny::observeEvent(input$spec_dblclick, {
      state$zoom_time <- NULL
      state$zoom_freq <- NULL
    })
    
    #----------------------- 11. Logistic regression ---------------
    output$logisticPlot <- shiny::renderPlot({
      df <- state$data
      shiny::req(input$class)
      df_class <- df[df$class == input$class, ]
      if (nrow(df_class) < 10) return(NULL)
      
      model <- stats::glm(outcome ~ score,
                          family = binomial(),
                          data = df_class,
                          na.action = na.omit)
      
      scores <- seq(0, 1, length.out = 100)
      probs  <- stats::predict(model,
                               newdata = data.frame(score = scores),
                               type = "response")
      
      ggplot2::ggplot(
        data.frame(score = scores, prob = probs),
        ggplot2::aes(x = score, y = prob)
      ) +
        ggplot2::geom_line(colour = "steelblue", size = 1) +
        ggplot2::labs(
          title = paste("Logistic Regression Calibration -", input$class),
          x = "Score", y = "Probability Correct"
        ) +
        ggplot2::geom_hline(yintercept = c(0.7, 0.8, 0.9),
                            linetype = "dashed",
                            colour = "grey60")
    })
    
    # download handler for the logistic plot ---------------------------
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("logistic_plot_", input$class, ".png")
      },
      content = function(file) {
        df <- state$data
        shiny::req(input$class)
        df_class <- df[df$class == input$class, ]
        if (nrow(df_class) < 10) return(NULL)
        
        model <- stats::glm(outcome ~ score,
                            family = binomial(),
                            data = df_class,
                            na.action = na.omit)
        
        scores <- seq(0, 1, length.out = 100)
        probs  <- stats::predict(model,
                                 newdata = data.frame(score = scores),
                                 type = "response")
        
        p <- ggplot2::ggplot(
          data.frame(score = scores, prob = probs),
          ggplot2::aes(x = score, y = prob)
        ) +
          ggplot2::geom_line(colour = "steelblue", size = 1) +
          ggplot2::labs(
            title = paste("Logistic Regression Calibration -", input$class),
            x = "Score", y = "Probability Correct"
          ) +
          ggplot2::geom_hline(yintercept = c(0.7, 0.8, 0.9),
                              linetype = "dashed",
                              colour = "grey60")
        
        ggplot2::ggsave(file, plot = p, device = "png",
                        width = 7, height = 5, dpi = 150)
      }
    )
    
    #----------------------- 12. Threshold table --------------------
    output$thresholds <- shiny::renderTable({
      df <- state$data
      shiny::req(df$outcome, input$class)
      
      df_class <- df[df$class == input$class, ]
      if (nrow(df_class) < 10) return(NULL)
      
      model <- stats::glm(outcome ~ score,
                          family = binomial(),
                          data = df_class,
                          na.action = na.omit)
      
      target_probs <- c(0.7, 0.8, 0.9)
      
      # Solve the logistic equation for each target probability
      thresh <- sapply(target_probs, function(p) {
        (log(p / (1 - p)) - coef(model)[1]) / coef(model)[2]
      })
      
      data.frame(
        `Target Probability` = target_probs,
        `Score Threshold`    = round(thresh, 3)
      )
    }, rownames = FALSE)
    
    #----------------------- 13. Save on app exit -------------------
    shiny::onStop(function() {
      if (!is.null(save_path())) {
        utils::write.csv(state$data, save_path(),
                         row.names = FALSE)
      }
    })
  }
  
  #==================================================================
  #   RUN THE APP
  #==================================================================
  shiny::shinyApp(ui = ui, server = server)
}