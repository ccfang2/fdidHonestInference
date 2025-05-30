
# Home Panel
Home <- tabPanel(strong("Home"), icon = icon("house"),
                 h4(strong("About this Shiny App")),
                 p("This application is intended to facilitate the honest inference approach discussed in ", a("Fang and Liebl (2025).", href="https://github.com/ccfang2/fdid"), "That paper presents a novel functional perspective on Difference-in-Differences (DiD) that allows for honest inference using event study plots under violations of parallel trends and/or no-anticipation assumptions. Below, you may find a video of short presentation for that paper."),
                 p("In tab panel ", strong("[ Examples ], "), "you are able to conduct honest inference for an example you select. Once you follow the steps to complete the setup, you will get an event study plot with simultaneous confidence band for that example. Moreover, with the bounds you define for honest inference, you can find the time spans over which the treatment effect is uniformly significant. Most importantly, you could fine-tune the control parameter of bounds such that the treatment effect is borderline significant over a specific time span. The plot can be downloaded with a click."),
                 p("In tab panel ", strong("[ Analyze Your Own Data ], "), "you can upload data files containing the estimates of event-study coefficients and covariance matrix for your own research to perform an honest inference."),
                 p("If you haven't estimated coefficients and/or covariance matrix yet, you need to go to tab panel ", strong("[ Quick Help ],"), "where you can get your estimates done quickly with the method proposed in ", a("Fang and Liebl (2025).", href="https://github.com/ccfang2/fdid"), "It works for both staggered and non-staggered adoption settings. You can directly download the estimates and upload them in tab panel [ Analyze Your Own Data ] for analysis."),
                 hr(),
                 fluidRow(
                   column(6, 
                          h4(strong("Video of Paper Presentation")),
                          box(
                            width = 12, 
                            HTML('<iframe width="595" height="335" src="https://www.youtube.com/embed/HVa42mJYppE?si=ACEU-U306FLXlvf5" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen> </iframe>'),
                            p("To be updated.")
                          )
                         ),
                   column(6,
                          h4(strong("Video of this Shiny App")),
                          box(
                            width = 12, 
                            HTML('<iframe width="595" height="335" src="https://www.youtube.com/embed/HVa42mJYppE?si=ACEU-U306FLXlvf5" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen> </iframe>'),
                            p("To be updated")
                            #p("Note: The layout of Shiny app shown in this video may be slightly different from its latest version.")
                            )
                          )  
                   ),
                 hr(),
                 p("I hope you find this app useful and/or interesting. Any comments are welcome at", a("ccfang[at]uni-bonn.de",href="mailto:ccfang@uni-bonn.de"),", and you are encouraged to report any issues", a("here.", href="https://github.com/ccfang2/fdid/issues"), "[ Latest Update: 2025-03-30 ]"),
                 p("Built with",
                    img(src = "shiny.png", height = "30px"),
                    "by",
                    img(src = "RStudio-Logo-Flat.png", height = "30px"),
                    ".")
)
                   
                   
# Examples Panel
Examples <- tabPanel(strong("Examples"), fluid = TRUE, icon = icon("magnifying-glass"),
                     
                     sidebarLayout(
                       # Examples Sidebar Panel
                       sidebarPanel(
                         tags$h4(style = "text-align: center; background-color: #D3D3D3; padding: 5px; border-radius: 5px;",strong("Honest Inference for a Selected Example")),
                         width=4,
                         id="example_sidebar_panel",
                         
                         hr(),
                         tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Estimates of Coefficients and Covariance"), #font-weight: bold; font-size: 18px;
                         hr(),
                         
                         # Step 1: select example
                         p(strong(span("[ Step 1 ] : ", style="color:black"))),
                         selectInput("example",
                                     label="Select an example to analyze",
                                     choices = list("Lovenheim and Willen (2019)"=1,
                                                    "Bosch and Campos-Vazquez (2014)"=2,
                                                    "Lafortune et al. (2018)"=3,
                                                    "Gallagher (2014)"=4,
                                                    "Simulated Example of Staggered Adoption"=5,
                                                    "Simulated Example of Non-staggered Adoption"=6
                                                    ),
                                     selected = 1),
                         
                         hr(),
                         tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Simultaneous Confidence Band"),
                         hr(),
                         
                         # Step 2: degree of freedom
                         tags$div(title="Degree of freedom for t-distribution based simultaneous confidence bands. If you intend to construct Gaussian based bands, leave it blank.",
                                  p(strong(span("[ Step 2 ] : ", style="color:black")), span(" [ Hover here to see hints ] "))),
                         numericInput("example_df", label="Enter the degree of freedom", value = ""),
                         
                         # Step 3: confidence level
                         p(strong(span("[ Step 3 ] : ", style="color:black"))),
                         sliderInput("example_conf.level", "Select a confidence level", min = 0, max = 1, value = 0.95),
                         
                         hr(),
                         tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Bounds for Honest Inference"),
                         hr(),
                         
                         # Step 4: the time point right after which there is an anticipation of treatment
                         p(strong(span("[ Step 4 ] : ", style="color:black"))),
                         numericInput("example_ta.t0", label=p("Enter the time point, right after which there is an anticipation of treatment",
                                                               tags$li(h6("The default value is the reference time in the example you select at [Step 1], meaning no anticipation")),
                                                               tags$li(h6("Leaving it blank also means no anticipation")),
                                                               tags$li(h6("If it is defined to be the first event time, there must be no violations of parallel trends assumption, because no data is available for computing pre-trend differences"))), 
                                      value = -2),
                         numericInput("example_ta.s", label=p("Enter the control parameter for treatment anticipation (min=0, max=20, step=0.01)",
                                                              tags$li(h6("Leaving it blank means that the point-wise confidence interval at time point specified above is used to define the bounds for anticipation")),
                                                              tags$li(h6("If no anticipation is defined above, this value will be simply ignored"))),
                                      value = "", min=0, max=20, step=0.01),

                         # Step 5: violations of parallel trends assumption
                         p(strong(span("[ Step 5 ] : ", style="color:black"))),
                         radioButtons("example_diff_trend", label="Select the type of parallel trends violations",
                                      choices = list("Functional Relative Magnitudes (FRM)"=1,
                                                     "Functional Trend Restrictions (FTR)"=2,
                                                     "Functional Relative Magnitudes + Trend Restrictions (FRMTR)"=3,
                                                     "No Parallel Trends Violations"=4),
                                      selected = 4),
                         conditionalPanel(
                           condition = "input.example_diff_trend != '4'",
                           p("Enter the control prameter for the type of parallel trends violations you select (min=0, max=20, step=0.01)"),
                           numericInput("example_control_par", "", value = 1, min = 0, max=20, step=0.01)
                         ),
                         
                         hr(),
                         tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Other Plot Arguments"),
                         hr(),
                         
                         #Step 6: indicate if only the bounds for post-treatment periods are plotted and used for honest inference
                         p(strong(span("[ Step 6 ] : ", style="color:black"))),
                         radioButtons("example_post.trt", label="Indicate if only the bounds for post-treatment periods are plotted and used for honest inference",
                                      choices = list("Yes"=1,
                                                     "No"=2),
                                      selected = 2),
                         
                         #Step 7: indicate if point-wise confidence intervals are added
                         p(strong(span("[ Step 7 ] : ", style="color:black"))),
                         radioButtons("example_ci", label="Indicate if point-wise confidence intervals are plotted",
                                      choices = list("Yes"=1,
                                                     "No"=2),
                                      selected = 1),
                         
                         # Step 8: position of legend
                         p(strong(span("[ Step 8 ] : ", style="color:black"))),
                         radioButtons("example_pos.legend", "Select position of legend for the plot",
                                      choices = c("top", "bottom", "none"),
                                      selected = "top"),
                         
                         # Start and Clear Buttons
                         fluidRow(
                           column(6,actionButton("example_start", strong("Start"))),
                           column(6,actionButton("example_reset", strong("Reset")))
                           )),
                       
                       # Examples Main Panel
                       mainPanel(
                         fluidRow(
                           column(10,
                                  h4(textOutput("example_currentTime")),
                                  tags$li(p("Once you click", strong("Start,"), "an event study plot with simultaneous confidence band will be displayed below.")),
                                  tags$li(p("With the bounds you define for honest inference, you can find the time spans over which the treatment effect is uniformly significant. They are marked in grey. It may take a few seconds to compute the significant time spans. You could fine-tune the control parameter in [Step 6] such that the treatment effect is borderline significant over a certain time span.")),
                                  tags$li(p("The inference result around the reference time point (i.e. between two event time closest to the reference time) should be treated with caution.")),
                                  tabPanel("Event Study Plot", 
                                                       fluidRow(
                                                         br(),
                                                         column(12,
                                                                shinycssloaders::withSpinner(plotOutput("example_plot"))
                                                                )
                                                         )
                                                       ),
                                              ),
                           column(2,
                                  p(strong(span("[ Downloads ]", style="color:black"))),
                                  # tags$style(".btn {width: 80%}"),
                                  numericInput("example_width", "Plot width (inches)", value = 6, min = 1, max = 20),
                                  numericInput("example_height", "Plot height (inches)", value = 4, min = 1, max = 20),
                                  radioButtons("example_format", "Format", choices = c("PNG"="PNG", "PDF"="PDF", "JPEG"="JPEG"), selected = "PNG"),
                                  downloadButton("example_plotdownload","Plot")
                                  )
                           )
                         )
                       )
                     )


# Own Data Panel
OwnData <- tabPanel(strong("Analyze Your Own Estimates"), fluid = TRUE, icon = icon("pen-to-square"),
                 
                    sidebarLayout(
                      # OwnData Input Panel
                      sidebarPanel(
                        tags$h4(style = "text-align: center; background-color: #D3D3D3; padding: 5px; border-radius: 5px;",strong("Honest Inference for Your Own Estimates")),
                        width=4,
                        id="owndata_sidebar_panel",
                        
                        hr(),
                        tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Estimates of Coefficients and Covariance"), #font-weight: bold; font-size: 18px;
                        tags$h6(style = "text-align: center; ","Please go to [ Quick Help ], if you do not have estimates yet."),
                        hr(),
                        
                        # Step 1
                        tags$div(title="File must be in csv format, and Click [Example File] to check the exact format.",
                                 p(strong(span("[ Step 1 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        uiOutput("owndata_fileupload_coef"),
                        
                        # Step 2
                        tags$div(title="File must be in csv format, and Click [Example File] to check the exact format.",
                                 p(strong(span("[ Step 2 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        uiOutput("owndata_fileupload_cov"),
                        
                        hr(),
                        tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Simultaneous Confidence Band"),
                        hr(),
                        
                        # Step 3: degree of freedom
                        tags$div(title="Degree of freedom for t-distribution based simultaneous confidence bands. If you intend to construct Gaussian based bands, leave it blank.",
                                 p(strong(span("[ Step 3 ] : ", style="color:black")), span(" [ Hover here to see hints ] "))),
                        numericInput("owndata_df", label="Enter the degree of freedom", value = ""),
                        
                        # Step 4: confidence level
                        p(strong(span("[ Step 4 ] : ", style="color:black"))),
                        sliderInput("owndata_conf.level", "Select a confidence level", min = 0, max = 1, value = 0.95),
                        
                        hr(),
                        tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Bounds for Honest Inference"),
                        hr(),
                        
                        # Step 5: the time point right after which there is an anticipation of treatment
                        p(strong(span("[ Step 5 ] : ", style="color:black"))),
                        numericInput("owndata_ta.t0", label=p("Enter the time point, right after which there is an anticipation of treatment",
                                             tags$li(h6("The default value is the reference time in the data you upload at [Step 1], meaning no anticipation")),
                                             tags$li(h6("Leaving it blank also means no anticipation")),
                                             tags$li(h6("If it is defined to be the first event time, there must be no violations of parallel trends assumption, because no data is available for computing pre-trend differences")) 
                                             ), value = ""),
                        numericInput("owndata_ta.s", label=p("Enter the control parameter for treatment anticipation (min=0, max=20, step=0.01)",
                                                             tags$li(h6("Leaving it blank means that the point-wise confidence interval at time point specified above is used to define the bounds for anticipation")),
                                                             tags$li(h6("If no anticipation is defined above, this value will be simply ignored"))),
                                     value = "", min=0, max=20, step=0.01),

                        # Step 6: violations of parallel trends assumption
                        p(strong(span("[ Step 6 ] : ", style="color:black"))),
                        radioButtons("owndata_diff_trend", label="Select the type of parallel trends violations",
                                     choices = list("Functional Relative Magnitudes (FRM)"=1,
                                                    "Functional Trend Restrictions (FTR)"=2,
                                                    "Functional Relative Magnitudes + Trend Restrictions (FRMTR)"=3,
                                                    "No Parallel Trends Violations"=4),
                                     selected = 4),
                        conditionalPanel(
                          condition = "input.owndata_diff_trend != '4'",
                          p("Enter the control prameter for the type of parallel trends violations you select (min=0, max=20, step=0.01)"),
                          numericInput("owndata_control_par", "", value = 1, min = 0, max=20, step=0.01)
                        ),
                        
                        hr(),
                        tags$h4(style = "text-align: center; background-color: #E0E0E0; padding: 5px; border-radius: 5px; ","Other Plot Arguments"),
                        hr(),
                        
                        #Step 7: indicate if only the bounds for post-treatment periods are plotted and used for honest inference
                        p(strong(span("[ Step 7 ] : ", style="color:black"))),
                        radioButtons("owndata_post.trt", label="Indicate if only the bounds for post-treatment periods are plotted and used for honest inference",
                                     choices = list("Yes"=1,
                                                    "No"=2),
                                     selected = 2),
                        
                        # Step 8: indicate if point-wise confidence intervals are added
                        p(strong(span("[ Step 8 ] : ", style="color:black"))),
                        radioButtons("owndata_ci", label="Indicate if point-wise confidence intervals are plotted",
                                     choices = list("Yes"=1,
                                                    "No"=2),
                                     selected = 1),
                        
                        # Step 9: position of legend
                        p(strong(span("[ Step 9 ] : ", style="color:black"))),
                        radioButtons("owndata_pos.legend", "Select position of legend for the plot",
                                     choices = c("top", "bottom", "none"),
                                     selected = "top"),
                        
                        # Start and Clear Buttons
                        fluidRow(
                          column(6,actionButton("owndata_start", "Start")),
                          column(6,actionButton("owndata_reset", "Reset"))
                        )
                      ),
                      
                      # OwnData Main Panel
                      mainPanel(
                        fluidRow(
                          column(10,
                                 h4(textOutput("owndata_currentTime")),
                                 tags$li(p("Once you click", strong("Start,"), "an event study plot with simultaneous confidence band will be displayed below.")),
                                 tags$li(p("With the bounds you define for honest inference, you can find the time spans over which the treatment effect is uniformly significant. They are marked in grey. It may take a few seconds to compute the significant time spans. You could fine-tune the control parameter in [Step 7] such that the treatment effect is borderline significant over a certain time span.")),
                                 tags$li(p("The inference result around the reference time point (i.e. between two event time closest to the reference time) should be treated with caution.")),
                                 tabPanel("Event Study Plot", 
                                          fluidRow(
                                            br(),
                                            column(12,
                                                   shinycssloaders::withSpinner(plotOutput("owndata_plot"))
                                            )
                                          )
                                 ),
                          ),
                          column(2,
                                 p(strong(span("[ Downloads ]", style="color:black"))),
                                 # tags$style(".btn {width: 80%}"),
                                 numericInput("owndata_width", "Plot width (inches)", value = 6, min = 1, max = 20),
                                 numericInput("owndata_height", "Plot height (inches)", value = 4, min = 1, max = 20),
                                 radioButtons("owndata_format", "Format", choices = c("PNG"="PNG", "PDF"="PDF", "JPEG"="JPEG"), selected = "PNG"),
                                 downloadButton("owndata_plotdownload","Plot")
                          )
                        )
                      )
                    )
                    )



# Help Panel
Help <- tabPanel(strong("Quick Help"), fluid = TRUE, icon = icon("compass") ,

         sidebarLayout(
           sidebarPanel(
             tags$h4(style = "text-align: center; background-color: #D3D3D3; padding: 5px; border-radius: 5px;",strong("Estimation of Event Study Coefficients and Covariance Matrix Using Functional TWFE Model")),
             width=4,
             id="help_sidebar_panel",
             hr(),
             
             # Step 1
             tags$div(title="File must be in csv format, and Click [Example File] to check the exact format.",
                      p(strong(span("[ Step 1 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
             uiOutput("help_fileupload_data"),
             # Step 2
             tags$div(title="File must be in csv format, and Click [Example File] to check the exact format.",
                      p(strong(span("[ Step 2 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
             uiOutput("help_fileupload_treatment"),

             # Start and Clear Buttons
             fluidRow(
               column(6,actionButton("help_start", "Start")),
               column(6,actionButton("help_reset", "Reset"))
               )
             ),

           # Help Main Panel
           mainPanel(
             fluidRow(
               column(10,
                      h4(textOutput("help_currentTime")),
                      tags$li(p("Once you click", strong("Start,"), "a traditional event study plot with 95% point-wise confidence intervals is displayed as below, together with a covariance plot.")),
                      tags$li(p("You are able to download the csv files of coefficients and covariance estimates on the right and use them directly in tab panel [ Analyze Your Own Estimates ].")),
                      tags$li(p("Alternatively, you may use the function", strong('fdid'), "in R package", strong(a("fdid.", href="https://github.com/ccfang2/fdid")))),
                      br(),
                      tabsetPanel(type = "tabs",
                                  tabPanel("Traditional Event Study Plot",
                                           fluidRow(
                                             br(),
                                             column(12,
                                                    shinycssloaders::withSpinner(plotOutput("help_betaplot"))
                                             )
                                           )
                                  ),
                                  tabPanel("Covariance Plot",
                                           fluidRow(
                                             br(),
                                             column(12,
                                           shinycssloaders::withSpinner(plotOutput("help_covplot"))
                                             ))
                                  )
                      )
               ),
               column(2,
                      p(strong(span("[ Downloads ]", style="color:black"))),
                      # tags$style(".btn {width: 80%}"),
                      downloadButton("help_outputdownload_coef", "Coefficients"),
                      br(),
                      br(),
                      downloadButton("help_outputdownload_cov", "Covariance")
               )
             )
           )
)
)
         
# More Panel
More <- navbarMenu(strong("More"), icon = icon("list"),
                   # Resources Tab
                   tabPanel(strong("Resources"), fluid = TRUE,
                            p(h4("Related Researches")),
                            tags$ul(
                              tags$li(p(a("Fang and Liebl (2025):", href="https://github.com/ccfang2/fdid"), "Honest Causal Inference with Difference-in-Differences: A Functional Data Perspective.", em("Working Paper."))),
                              tags$li(p(a("Liebl and Reimherr (2023):", href="https://academic.oup.com/jrsssb/article/85/3/842/7133768"), "Fast and fair simultaneous confidence bands for functional parameters.", em("Journal of the Royal Statistical Society Series B: Statistical Methodology"), "85 (3): 842–868.")),
                              tags$li(p(a("Rambachan and Roth (2023):", href="https://academic.oup.com/restud/article-abstract/90/5/2555/7039335"), "A More Credible Approach to Parallel Trends.", em("The Review of Economic Studies"), "90 (5): 2555–2591."))
                              ),
                            p(h4("Related Packages")),
                            tags$ul(
                              tags$li(p("Honest inference in this shiny app is built on the R package: ", a("fdid.", href="https://github.com/ccfang2/fdid"))),
                              tags$li(p("Simultaneous confidence band is built with the R package: ", a("ffscb.",href="https://github.com/lidom/ffscb")))
                            ),
                            hr(),
                            p(h4("Papers Listed in Examples")),
                            tags$ul(
                              tags$li(p(a("Lovenheim and Willen (2019):", href="https://www.aeaweb.org/articles?id=10.1257/pol.20170570"), "The Long-Run Effects of Teacher Collective Bargaining.", em("American Economic Journal: Economic Policy"), "11 (3): 292–324.")),
                              tags$li(p(a("Bosch and Campos-Vazquez (2014):", href="https://www.aeaweb.org/articles?id=10.1257/pol.6.4.71"), "The Trade-Offs of Welfare Policies in Labor Markets with Informal Jobs: The Case of the ‘Seguro Popular’ Program in Mexico.", em("American Economic Journal: Economic Policy"), "6 (4): 71–99.")),
                              tags$li(p(a("Lafortune et al. (2018):", href="https://www.aeaweb.org/articles?id=10.1257/app.20160567"), "School Finance Reform and the Distribution of Student Achievement.", em("American Economic Journal: Applied Economics"), "10 (2): 12–6.")),
                              tags$li(p(a("Gallagher (2014):", href="https://www.aeaweb.org/articles?id=10.1257/app.6.3.206"), "Learning about an Infrequent Event: Evidence from Flood Insurance Take-Up in the United States.", em("American Economic Journal: Applied Economics"), "6 (3): 206–33."))
                              ),
                            p(h4("Simulated Datasets Listed in Examples")),
                            tags$ul(
                              tags$li(p("Simulated Example of Staggered Adoption: ", a("simulated_stagger_example.rda", href="https://raw.githubusercontent.com/ccfang2/fdid/main/data/simulated_stagger_example.rda"))),
                              tags$li(p("Simulated Example of Non-Staggered Adoption: ", a("simulated_nonstagger_example.rda", href="https://raw.githubusercontent.com/ccfang2/fdid/main/data/simulated_nonstagger_example.rda")))
                              ),
                            hr(),
                            ),
                   # FAQ Tab
                   tabPanel(strong("FAQ"), fluid = TRUE,
                            p(h4("Frequently Asked Questions")),
                            hr(),
                            #p(HTML("<b>[ General Questions ]</b>"),
                            #    style="text-align:center"),
                            tags$ol(
                              tags$li(p("Could I download plots or csv files generated from old analyses?"),
                                      p(span("Unfortunately, only results from latest analysis are downloadable. Please save the results timely if needed."), style="font-weight: normal")),
                              tags$li(p("Could the names of variables in csv file be different from those in Example files?"),
                                      p(span("Yes, the names of variables can be different, but the sequences of variables in data files are not supposed to change. Please download the Example File in each section for reference."), style="font-weight: normal")),
                              tags$li(p("What does traditional event study plot mean at tab panel [Quick Help]?"),
                                      p(span("A traditional event study plot is a plot with point-wise confidence intervals, and it can be susceptible to multiple testing problem. Also, it cannot be manipulated for honest inference."), style="font-weight: normal")),
                              tags$li(p("Where can I access the videos if those on Home page do not work for me?"),
                                      p(span("Please be aware that you need to stay online to watch the videos on Home page. If they still don't work, then you can watch them on"), a("YouTube", href="https://www.youtube.com/watch?v=PyY-2ptiLTU"), "directly.", style="font-weight: normal")),
                              tags$li(p("Where can I find source code of this app?"),
                                      p(span("The source code is available from"), a("here.", href="https://github.com/ccfang2/fdidHonestInference"), span("You are also encouraged to report any issues"),a("here.", href="https://github.com/ccfang2/fdidHonestInference/issues"), style="font-weight: normal")),
                              tags$li(p("Where can I download the data specifically for the example I select at tab panel [ Examples ]?"),
                                      p(span("You can download data files for all examples at [ Examples ] from", a("here.", href="https://raw.githubusercontent.com/ccfang2/fdid/main/data")), style="font-weight: normal")),
                              style="font-weight: bold"
                              ),
                            hr()
                            )
                   )

