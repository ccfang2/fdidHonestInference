
server <- function(input, output, session) {

  # *****************************************************************
  # ************* Examples Tab **************************************
  # *****************************************************************

  # ------------ Observe Start or Reset-----------------------------

  example_v <- reactiveValues(result = NULL)

  selected_example <- reactive({
    if(input$example == "1") example <- fdid::LWdata
    if(input$example == "2") example <- fdid::BCVdata
    if(input$example == "3") example <- fdid::Ldata
    if(input$example == "4") example <- fdid::Gdata
    if(input$example == "5") {fdid_est <- fdid::fdid(data=fdid::simulated_stagger_data, treatment=fdid::simulated_stagger_treatment); example <- list(beta=fdid_est$beta$coef, cov=fdid_est$beta$cov, t0=fdid_est$t0)}
    if(input$example == "6") {fdid_est <- fdid::fdid(data=fdid::simulated_nonstagger_data, treatment=fdid::simulated_nonstagger_treatment); example <- list(beta=fdid_est$beta$coef, cov=fdid_est$beta$cov, t0=fdid_est$t0)}

    example
  })
  
  # Observe Change of Example
  observeEvent(input$example, {
    example <- selected_example()
    updateNumericInput(session,"example_ta.t0", value= example$t0)
  })

  # Observe Start
  observeEvent(input$example_start,execute_safely({
    example          <- selected_example()
    example_ta.t0    <- if(is.na(input$example_ta.t0)) NULL else input$example_ta.t0
    
    if (!is.null(example_ta.t0) && !(example_ta.t0 %in% example$beta[,2][which(example$beta[,2]<=example$t0)])) stop("The time point defined in [Step 5] should be among the pre-treatment event time.")
    if (input$example_control_par<0) stop("The control parameter should be non-negative.")
    if (!is.null(example_ta.t0) && example_ta.t0 == example$beta[,2][1] && input$example_diff_trend != "4") stop("If the time point in [Step 5] is defined to be the first event time, there must be no violations of parallel trends assumption, because no data is available for computing pre-trend differences.")
    
    example_df       <- if(is.na(input$example_df)) NULL else input$example_df
    example_ta.s     <- if(is.na(input$example_ta.s)) NULL else input$example_ta.s
    
    if (!is.null(example_ta.s) && (example_ta.s < 0)) stop("The control parameter for treatment anticipation should be either blank or a non-negative number.")

    fdid_scb_est     <- fdid::fdid_scb(beta=example$beta, cov=example$cov, t0=example$t0, df=example_df, conf.level = input$example_conf.level)
    example_v$result <- list(fdid_scb_est=fdid_scb_est, post.trt=input$example_post.trt, ci=input$example_ci, ta.t0=example_ta.t0, ta.s=example_ta.s, diff_trend=input$example_diff_trend, control_par= input$example_control_par, pos.legend=input$example_pos.legend )
  }))

  example_eventstudyplot <- reactive({
    if (is.null(example_v$result)) return()
    plot(object     = example_v$result$fdid_scb_est,
         post.trt   = ifelse(example_v$result$post.trt=="1", TRUE, FALSE),
         ci         = ifelse(example_v$result$ci=="1", TRUE, FALSE),
         ta.t0      = example_v$result$ta.t0,
         ta.s       = example_v$result$ta.s,
         frm.mbar   = if(example_v$result$diff_trend=="1") example_v$result$control_par else NULL,
         ftr.m      = if(example_v$result$diff_trend=="2") example_v$result$control_par else NULL,
         frmtr.mbar = if(example_v$result$diff_trend=="3") example_v$result$control_par else NULL,
         pos.legend = if(example_v$result$pos.legend != "none") example_v$result$pos.legend else NULL,
         scale.legend = 1.4)
    })

  # Observe Reset
  observeEvent(input$example_reset, {
    example_v$result <- NULL
    updateSelectInput(session, "example", selected = 1)
    updateNumericInput(session, "example_df", value = "")
    updateSliderInput(session, "example_conf.level", value = 0.95)
    updateNumericInput(session, "example_ta.t0", value = -2)
    updateNumericInput(session, "example_ta.s", value = "")
    updateRadioButtons(session, "example_diff_trend", selected = 4)
    updateNumericInput(session, "example_control_par", value = 1)
    updateRadioButtons(session, "example_post.trt", selected = 2)
    updateRadioButtons(session, "example_ci", selected = 1)
    updateRadioButtons(session, "example_pos.legend", selected = "top")
    updateNumericInput(session, "example_width", value = 6)
    updateNumericInput(session, "example_height", value = 4)
    updateRadioButtons(session, "example_format", selected = "PNG")
   })

  # ------------ Outputs -----------------------------
  
  # Clock
  output$example_currentTime <- renderText({
    invalidateLater(1000, session)
    example_tz <- Sys.timezone(location = TRUE)
    example_tz <- if (class(example_tz)!="character") "UTC" else example_tz
    paste("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), example_tz, "]")
  })

  # Event Study Plot
  output$example_plot <- renderPlot({
    input$example_start
    example_eventstudyplot()
  })

  # ------------ Downloads -----------------------------

  output$example_plotdownload <- downloadHandler(
    filename = function() {paste("Example_Plot", tolower(input$example_format), sep = ".")},
    content = function(file) {
      if (input$example_format == "PNG") {grDevices::png(file, width = input$example_width*100 , height =input$example_height*100)}
      if (input$example_format == "PDF") {grDevices::pdf(file, width = input$example_width, height =input$example_height)}
      if (input$example_format == "JPEG") {grDevices::jpeg(file, width =input$example_width *100, height = input$example_height*100)}

      plot(object     = example_v$result$fdid_scb_est,
           post.trt   = ifelse(example_v$result$post.trt=="1", TRUE, FALSE),
           ci         = ifelse(example_v$result$ci=="1", TRUE, FALSE),
           ta.t0      = example_v$result$ta.t0,
           ta.s       = example_v$result$ta.s,
           frm.mbar   = if(example_v$result$diff_trend=="1") example_v$result$control_par else NULL,
           ftr.m      = if(example_v$result$diff_trend=="2") example_v$result$control_par else NULL,
           frmtr.mbar = if(example_v$result$diff_trend=="3") example_v$result$control_par else NULL,
           pos.legend = if(example_v$result$pos.legend != "none") example_v$result$pos.legend else NULL,
           scale.legend = 1.4)

      dev.off()}
    )

  # *****************************************************************
  # ************* Own Data Tab **************************************
  # *****************************************************************
  
  # ------------ Alerts----------------------------------------------

  # Alert below will trigger if beta file uploaded is not csv
  resetOwnDataFileUpload_coef <- reactiveVal(FALSE)
  
  output$owndata_fileupload_coef <- renderUI({
    resetOwnDataFileUpload_coef() 
    resetOwnDataFileUpload_coef(FALSE)
    fileInput("owndata_file_coef",
              p("Upload estimates of coefficients in csv format", span(a("[ Example File ]", href="https://raw.githubusercontent.com/ccfang2/fdidHonestInference/main/data/CoefficientsFormat.csv")),
                tags$li(h6("In the first column are estimates of event study coefficients")),
                tags$li(h6("In the second column are corresponding event time")),
                tags$li(h6("The estimate at reference time point should be normalized to 0"))
                ), accept = c(".csv"))
  })
  
  observeEvent(input$owndata_file_coef, {
    if(file_ext(input$owndata_file_coef$name) != "csv"){
      resetOwnDataFileUpload_coef(TRUE)
      showModal(modalDialog("Please upload a csv file"))
    }
  })
  
  # Alert below will trigger if cov file uploaded is not csv
  resetOwnDataFileUpload_cov <- reactiveVal(FALSE)
  
  output$owndata_fileupload_cov <- renderUI({
    resetOwnDataFileUpload_cov()
    resetOwnDataFileUpload_cov(FALSE)
    fileInput("owndata_file_cov",
              p("Upload estimates of covariance matrix in csv format", span(a("[ Example File ]", href="https://raw.githubusercontent.com/ccfang2/fdidHonestInference/main/data/CovarianceFormat.csv")),
                tags$li(h6("The sequence of event time should coincide with that at [Step 2]")),
                tags$li(h6("The estimates at reference time point should also be normalized to 0"))
              ),
              accept = c(".csv"))
  })

  observeEvent(input$owndata_file_cov, {
    if(file_ext(input$owndata_file_cov$name) != "csv"){
      resetOwnDataFileUpload_cov(TRUE)
      showModal(modalDialog("Please upload a csv file"))
    }
  })
  
  # ------------ Observe Start or Reset-----------------------------
  
  owndata_v <- reactiveValues(result = NULL)
  
  # Read in Data Files
  owndata_read_coef <- reactive({
    ext <- tools::file_ext(input$owndata_file_coef$datapath)
    req(input$owndata_file_coef)
    validate(need(ext == "csv", "Please upload a csv file"))
    return(read.csv(input$owndata_file_coef$datapath, header=TRUE))
  })
  
  owndata_read_cov <- reactive({
    ext <- tools::file_ext(input$owndata_file_cov$datapath)
    req(input$owndata_file_cov)
    validate(need(ext == "csv", "Please upload a csv file"))
    return(read.csv(input$owndata_file_cov$datapath, header = TRUE))
  })
  
  # Observe Change of Beta File
  observeEvent(input$owndata_file_coef, {
    owndata_beta <- as.matrix(owndata_read_coef())
    owndata_t0 <- owndata_beta[,2][which(owndata_beta[,1]==0)]
    updateNumericInput(session,"owndata_ta.t0", value=owndata_t0)
  })
  
  # Observe Start
  observeEvent(input$owndata_start,execute_safely({
    
    # Reformat data
    owndata_beta <- as.matrix(owndata_read_coef())
    owndata_cov  <- as.matrix(owndata_read_cov())
    
    # Check the reference time
    if (sum(owndata_beta[,1]==0) != 1) stop("The estimate of event study coefficient at reference time point should be normalized to 0.")
    if (sum(rowSums((owndata_cov!=0), na.rm = TRUE)==0) != 1 | sum(colSums((owndata_cov!=0), na.rm = TRUE)==0) != 1) stop("The estimate of covariance matrix at reference time point should be normalized to 0.")
    if (which(colSums((owndata_cov!=0), na.rm = TRUE)==0) != which(rowSums((owndata_cov!=0), na.rm = TRUE)==0)) stop("The reference time point indicated by two dimensions of covariance matrix should coincide.")
    if (which(owndata_beta[,1] ==0) != which(rowSums((owndata_cov!=0))==0)) stop("The reference time point indicated at [Step 1] should coincide with that indicated at [Step 2].")
     
    # Check if data file is missing
    #if (length(input$owndata_file_coef)==0) stop("Please upload estimates of event study coefficients.")
    #if (length(input$owndata_file_cov)==0) stop("Please upload estimates of covariance matrix.")

    owndata_t0 <- owndata_beta[,2][which(owndata_beta[,1]==0)]
    owndata_ta.t0    <- if(is.na(input$owndata_ta.t0)) NULL else input$owndata_ta.t0

    # Check other inputs
    if (!is.null(owndata_ta.t0) && !(owndata_ta.t0 %in% owndata_beta[,2][which(owndata_beta[,2]<=owndata_t0)])) stop("The input in [Step 6] should be among the pre-treatment event time.")
    if (input$owndata_control_par<0) stop("The control parameter should be non-negative.")
    if (!is.null(owndata_ta.t0) && owndata_ta.t0 == owndata_beta[,2][1] && input$owndata_diff_trend != "4") stop("If the input in [Step 6] is defined to be the first event time, there must be no violations of parallel trends assumption, because no data is available for computing pre-trend differences.")
    
    owndata_df       <- if(is.na(input$owndata_df)) NULL else input$owndata_df
    owndata_ta.s    <- if(is.na(input$owndata_ta.s)) NULL else input$owndata_ta.s
    
    if (!is.null(owndata_ta.s) && (owndata_ta.s < 0)) stop("The control parameter for treatment anticipation should be either blank or a non-negative number.")

    fdid_scb_est     <- fdid::fdid_scb(beta=owndata_beta, cov=owndata_cov, t0=owndata_t0, df=owndata_df, conf.level = input$owndata_conf.level)
    owndata_v$result <- list(fdid_scb_est=fdid_scb_est, post.trt=input$owndata_post.trt, ci=input$owndata_ci, ta.t0=owndata_ta.t0, ta.s=owndata_ta.s, diff_trend=input$owndata_diff_trend, control_par= input$owndata_control_par, pos.legend=input$owndata_pos.legend )
    
    }))
  
  owndata_eventstudyplot <- reactive({
    if (is.null(owndata_v$result)) return()
    
    plot(object     = owndata_v$result$fdid_scb_est,
         post.trt   = ifelse(owndata_v$result$post.trt=="1", TRUE, FALSE),
         ci         = ifelse(owndata_v$result$ci=="1", TRUE, FALSE),
         ta.t0      = owndata_v$result$ta.t0,
         ta.s       = owndata_v$result$ta.s,
         frm.mbar   = if(owndata_v$result$diff_trend=="1") owndata_v$result$control_par else NULL,
         ftr.m      = if(owndata_v$result$diff_trend=="2") owndata_v$result$control_par else NULL,
         frmtr.mbar = if(owndata_v$result$diff_trend=="3") owndata_v$result$control_par else NULL,
         pos.legend = if(owndata_v$result$pos.legend != "none") owndata_v$result$pos.legend else NULL,
         scale.legend = 1.4)
    })
  
  # Observe Reset
  observeEvent(input$owndata_reset, {
    owndata_v$result <- NULL
    shinyjs::reset("owndata_fileupload_coef")
    shinyjs::reset("owndata_fileupload_cov")
    updateNumericInput(session, "owndata_df", value = "")
    updateSliderInput(session, "owndata_conf.level", value = 0.95)
    updateNumericInput(session, "owndata_ta.t0", value = "")
    updateNumericInput(session, "owndata_ta.s", value = "")
    updateRadioButtons(session, "owndata_diff_trend", selected = 4)
    updateNumericInput(session, "owndata_control_par", value = 1)
    updateRadioButtons(session, "owndata_post.trt", selected = 2)
    updateRadioButtons(session, "owndata_ci", selected = 1)
    updateRadioButtons(session, "owndata_pos.legend", selected = "top")
    updateNumericInput(session, "owndata_width", value = 6)
    updateNumericInput(session, "owndata_height", value = 4)
    updateRadioButtons(session, "owndata_format", selected = "PNG")
  })
  
  # ------------ Outputs -----------------------------
  
  # Clock
  output$owndata_currentTime <- renderText({
    invalidateLater(1000, session)
    owndata_tz <- Sys.timezone(location = TRUE)
    owndata_tz <- if (class(owndata_tz)!="character") "UTC" else owndata_tz
    paste("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), owndata_tz, "]")
  })
  
  # Event Study Plot
  output$owndata_plot <- renderPlot({
    input$owndata_start
    owndata_eventstudyplot()
  })
  
  # ------------ Downloads -----------------------------
  
  output$owndata_plotdownload <- downloadHandler(
    filename = function() {paste("Event_Study_Plot", tolower(input$owndata_format), sep = ".")},
    content = function(file) {
      if (input$owndata_format == "PNG") {grDevices::png(file, width = input$owndata_width*100 , height =input$owndata_height*100)}
      if (input$owndata_format == "PDF") {grDevices::pdf(file, width = input$owndata_width, height =input$owndata_height)}
      if (input$owndata_format == "JPEG") {grDevices::jpeg(file, width =input$owndata_width *100, height = input$owndata_height*100)}
      
      plot(object     = owndata_v$result$fdid_scb_est,
           post.trt   = ifelse(owndata_v$result$post.trt=="1", TRUE, FALSE),
           ci         = ifelse(owndata_v$result$ci=="1", TRUE, FALSE),
           ta.t0      = owndata_v$result$ta.t0,
           ta.s       = owndata_v$result$ta.s,
           frm.mbar   = if(owndata_v$result$diff_trend=="1") owndata_v$result$control_par else NULL,
           ftr.m      = if(owndata_v$result$diff_trend=="2") owndata_v$result$control_par else NULL,
           frmtr.mbar = if(owndata_v$result$diff_trend=="3") owndata_v$result$control_par else NULL,
           pos.legend = if(owndata_v$result$pos.legend != "none") owndata_v$result$pos.legend else NULL,
           scale.legend = 1.4)
      
      dev.off()}
  )
  
  # *****************************************************************
  # ************* Help Tab*******************************************
  # *****************************************************************

  # ------------ Alerts----------------------------------------------

  resetFileUpload_data <- reactiveVal(FALSE)
  output$help_fileupload_data <- renderUI({
    resetFileUpload_data() 
    resetFileUpload_data(FALSE)
    fileInput("help_file_data",
              p("Upload original data file in csv format", span(a("[ Example File ]", href="https://raw.githubusercontent.com/ccfang2/fdidHonestInference/main/data/DataFormat.csv")),
                tags$li(h6("In the first column is the outcome variable.")),
                tags$li(h6("In the latter two columns are time and unit indices.")),
                tags$li(h6("The outcome should be numeric.")),
                tags$li(h6("The column names do not need to be the same as [ Example File ]."))),
              accept = ".csv")
  })

  observeEvent(input$help_file_data, {
    if(file_ext(input$help_file_data$name) != "csv"){
      resetFileUpload_data(TRUE)
      showModal(modalDialog("That's not a csv file"))
    }
  })
  
  resetFileUpload_treatment <- reactiveVal(FALSE)
  output$help_fileupload_treatment <- renderUI({
    resetFileUpload_treatment() 
    resetFileUpload_treatment(FALSE)
    fileInput("help_file_treatment",
              p("Upload a treatment file in csv format", span(a("[ Example File ]", href="https://raw.githubusercontent.com/ccfang2/fdidHonestInference/main/data/TreatmentFormat.csv")),
                tags$li(h6("In the first column is unit indice.")),
                tags$li(h6("In the second column is the time, right after which the treatment is given, for each unit.")),
                tags$li(h6("NA indicates the control units.")),
                tags$li(h6("All pre-determined covariates are placed afterwards. Covariates should be numeric."))
              ),
              accept = ".csv")
  })
  
  observeEvent(input$help_file_treatment, {
    if(file_ext(input$help_file_treatment$name) != "csv"){
      resetFileUpload_treatment(TRUE)
      showModal(modalDialog("That's not a csv file"))
    }})

  # ------------ Observe Start or Reset-----------------------------
  help_v <- reactiveValues(result = NULL)

  # Read in csv Data
  help_read_data <- reactive({
    ext <- tools::file_ext(input$help_file_data$datapath)
    req(input$help_file_data)
    validate(need(ext == "csv", "Please upload a csv file"))
    return(read.csv(input$help_file_data$datapath, header=TRUE))
  })
  
  help_read_treatment <- reactive({
    ext <- tools::file_ext(input$help_file_treatment$datapath)
    req(input$help_file_treatment)
    validate(need(ext == "csv", "Please upload a csv file"))
    return(read.csv(input$help_file_treatment$datapath, header=TRUE))
  })

  # # Observe Start
  observeEvent(input$help_start,execute_safely({

    help_data      <- as.data.frame(help_read_data())
    help_treatment <- as.data.frame(help_read_treatment())
    
    # colnames(help_data)[c(1, ncol(help_data)-1, ncol(help_data))] <- c("y", "t", "i")
    # colnames(help_treatment) <- c("i","t0")
    colnames(help_data) <- c("y", "t", "i")
    colnames(help_treatment)[1:2] <- c("i","t0")

    if (sum(is.na(help_treatment$t0))==0) stop("There must be some control units marked with NA in 't0' at 'treatment' file.")
    if(!is.numeric(help_data$t)) stop("The time index in 'data' file should be numeric.")
    if(!is.numeric(help_data$y)) stop("The outcome variable in 'data' file should be numeric.")
    if(!all(na.omit(unique(help_treatment$t0)) %in% unique(help_data$t))) stop("The reference time in 'treatment' file should be among the time index in 'data' file.")
    if(!all(na.omit(unique(help_treatment$t0)) > min(unique(help_data$t)) & na.omit(unique(help_treatment$t0)) < max(unique(help_data$t)))) stop("The reference time in 'treatment' file should be greater/less than the min/max of time index in 'data' file.")
    
    fdid_est      <- fdid::fdid(data=help_data, treatment=help_treatment)
    help_v$result <- fdid_est
    
    }))

  # Observe Reset
  observeEvent(input$help_reset, {
    help_v$result <- NULL
    reset("help_fileupload_data")
    reset("help_fileupload_treatment")
  })

  # ------------ Outputs -----------------------------

  # Clock
  output$help_currentTime <- renderText({
    invalidateLater(1000, session)
    help_tz <- Sys.timezone(location = TRUE)
    help_tz <- if (class(help_tz)!="character") "UTC" else help_tz
    paste("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), help_tz, "]")
  })

  # Plots
  help_betaplot <- reactive({
    if (is.null(help_v$result)) return()
    
    timeVec <- help_v$result$beta$coef[,2]
    betahat <- help_v$result$beta$coef[,1]
    ci_lower <- betahat-qnorm(1-0.05/2)*sqrt(diag(help_v$result$beta$cov))
    ci_upper <- betahat+qnorm(1-0.05/2)*sqrt(diag(help_v$result$beta$cov))
    y_range <- range(c(ci_lower, ci_upper), na.rm=TRUE)
    
    plot(timeVec, betahat,  type = "p", ylim =y_range, xlab = "Event Time", ylab = "", pch = 16, col = "blue", cex.axis=1.4, cex.lab=1.4)
    #title(ylab=expression(hat(beta)), mgp=c(2.1,1,0), cex.lab=1.4)
    arrows(timeVec, ci_lower, timeVec, ci_upper, angle = 90, code = 3, length = 0.025, col = "blue4")
    grid()
    abline(h=0, lty=3, lwd=4, col="red")
  })

  help_covplot <- reactive({
    if (is.null(help_v$result)) return()
    timeVec <- help_v$result$beta$coef[,2]
    graphics::image(timeVec, timeVec, help_v$result$beta$cov, xlab="Event Time", ylab="Event Time",cex.axis=1.4, cex.lab=1.4) 
  })
  
  output$help_betaplot <- renderPlot({
    input$help_start
    help_betaplot()
  })
  
  output$help_covplot <- renderPlot({
    input$help_start
    help_covplot()
  })

  # ------------ Downloads -----------------------------

  output$help_outputdownload_coef <- downloadHandler(
    filename = function(){
      paste("CoefficientsEstimates.csv",sep="")
    },
    content = function(file){
      write.csv(help_v$result$beta$coef, file, row.names = FALSE)
    }
  )
  
  output$help_outputdownload_cov <- downloadHandler(
    filename = function(){
      paste("CovarianceEstimates.csv",sep="")
    },
    content = function(file){
      write.csv(help_v$result$beta$cov,file, row.names = FALSE)
    }
  )
}

