        
#The code below allows me to print 2 plots in the same panel.   
  shinyUI( 
    fluidPage( 
      
        titlePanel(title= "Descriptive and Inferential Statistics"),

        sidebarLayout(position= "right",
                      sidebarPanel(
                      ),       
       
      mainPanel(
        tabsetPanel(                                      #Creates multiple tabs. 
          tabPanel("Data",  
                   h4("Download, Upload and Save Data."),
                   h5("Whenever saving R data file names, use the '.RData' extension in the name (e.g., myData.RData)."),
                   br(),
                   fluidRow(
                     column(3, 
                            textInput("dataframe", "Enter the data frame name",
                                      value="mtcars"))
                   ),
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("View_main_df"))  
                   ),
                   tableOutput("view_main_data_out"),
                   br(),
                   h5("Upload R Data."),
                   ### 
          uiOutput("upload_r_df"),
          verbatimTextOutput("datastr"),
          br(),
          h5("Upload a text file."),
          uiOutput("text_file_type"),
          uiOutput("upload_df"),
          verbatimTextOutput("datastr_txt"),
          h6("Determines if the text file is loaded as model builder data. If yes, text file data is automatically loaded to the model builder tab although mtcars is listed."),
          uiOutput("use_txt"),
          #uiOutput("data"),
          br(),
          h5("Save the uploaded data frame into an R data file"),
          h6("This is the new name given to the uploaded text or R data frame you will 'save as' in the new Rdata file"),
          uiOutput("new_df_name"),
          h6("Answering yes will download the uploaded data into an RData file"),
          uiOutput("new_df"),
#          h6("Dataset object name that I'm requesting is saved in the Rdata file"),
#          uiOutput("df_save_nm"),
          #uiOutput("downloadSave_txt"),
          h6("This is the download button text...click on this to save the new file into Rdata"),
          downloadLink('downloadSave', 'Download RData file'),
          h6("Newly created data summary"),
          verbatimTextOutput("new_smry_df"),
br(),
  ############### Begin here
h4("Modify the Data Frame"),
br(),
h5("Change the data type"),
h6("Convert a variable to a character, factor, or numeric type."),

fluidRow(
  column(3,
         uiOutput("modify_character")),  
  column(3, offset=1,
         uiOutput("modify_factor")),  
  column(3, offset=1,
         uiOutput("modify_numeric"))  
  ),
br(),
h6("Create date formats. For #4, group variables by format type if multiple types. For #5, format examples for JAN 1, 2021. For #6, indicate how many X per format (e.g., c(3,1))."),
fluidRow(
  column(3,
         uiOutput("modify_time_X")),  
  column(3, offset=1,
         uiOutput("modify_time_format")),  
  column(3, offset=1,
         uiOutput("modify_tm_fmt_rep"))  
),
br(),
h6("Subset the data you need. In #8, 'subset' indicates rows, 'select' indicates columns (e.g., subset= gender==male, select= 1:7)."),
fluidRow(
  column(3,
         uiOutput("subset_df_yes_no")),  
  column(3, offset=1,
         uiOutput("subset_args")),
  column(3, offset=1,
         uiOutput("modify_df_yes_no")) 
),
br(),
h6("Create 'Time' as the difference between 2 dates, select 'Time1', 'Time2'. If none, 'No.Time.Var=NA'. Create numeric 'YYMM' as month + year: 202101. And 'Month': ordered(YYMM)."),
fluidRow(
  column(3,
         uiOutput("modify_2_var_Time")),  
  column(3, offset=1,
         uiOutput("modify_Time_add_month")),
  column(3, offset=1,
         uiOutput("modify_add_time_YN")) 
),
br(),
fluidRow(
  column(3,
         uiOutput("modified_df_save")),  
  column(3, offset=1,
         uiOutput("modified_df_name")),  
  column(3, offset=1,
         downloadLink('download_modified_df', '15. Click to download data.'))
),

################# End here

          ## Transformed/Imputed ##
          br(),
          h5("Once data is downloaded, it can be uploaded and entered at the top for analysis. Make sure #3 at top is 'No'."),
br()
          ),
          ###
                    
############## Describe and Missing #############################
tabPanel("Describe",
         h4("Calculator/Grapher"),
         fluidRow(
           column(4, 
                  uiOutput("calculator_box")),
           column(4, offset=1,
                  uiOutput("calculator_yesno"))
         ),
         h5("Basic mathematical functions (+, -, *, /) and runs other commands (e.g., sqrt(16) + 6 = 10, sd(mtcars$mpg) )."),         
         h5("To load an object to the global environment, use assign(), e.g., assign(\"my_data\", my_data[1:5000, ], envir=globalenv() )"),
         h5("Add leading 0s to a numeric X: assign(\"airquality\", within(airquality, {Temp <- sprintf(\"%012d\", Temp) }), envir=globalenv() )"),
         verbatimTextOutput("prnt_calculation"),
         plotOutput("plt_calculation", height = 500, width = "100%"),
         br(),
         h4("Descriptives on means and missing values. Plot the target variable, stratifying by factors."),
         br(),
         h4("Summarize the data's central tendency and dispersion."),
         fluidRow(   
           column(3, 
                  uiOutput("desc_summ_vars")),
           column(3, offset=1,
                  uiOutput("des_summ_yesno"))
         ),
         verbatimTextOutput("prnt_desc_summ"),
         h5("Coefficient of variation = Standard Deviation / Mean. COV returns errors ('NA') for non-numerical variables."),
         br(),
         #Histogram
         h4("Histogram of a key variable"),
         fluidRow(   
           column(3, 
                  uiOutput("smry_var_hist_var")),
           column(3, offset=1,
                  uiOutput("smry_var_hist_fac_yesno")),
           column(3, offset=1,
                  uiOutput("smry_var_hist_fac"))
         ),
         fluidRow(   
           column(3,
                  uiOutput("smry_var_hist_bars")),
           column(3, offset=1,
                  uiOutput("smry_var_hist_bar_clr"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("smry_hist_mn_med_yesno")),
           column(3, offset=1,
                  uiOutput("smry_var_hist_ln_clr")),
           column(3, offset=1,
                  uiOutput("smry_var_hist_yesno"))
         ),
         plotOutput("summary_var_histogram_out", height = 800, width = "100%"),
         h5("Note: Mean= solid vertical line, Median= dashed vertical line."),
         br(),
         #Means by factors
         h4("Explore the mean values of an outcome variable by factor levels."),
         fluidRow(   
           column(3, 
                uiOutput("desc_y")),
           column(3, offset=1,
                  uiOutput("desc_x")),
           column(3, offset=1,
                  uiOutput("desc_choice"))
         ),
         plotOutput("DescSmryPlt", height = 800, width = 1200), 
         ## scatter plot with correlation ## 
         h4("Scatterplot with a lowess smoothed line and a correlation test."),
         br(),
         h5("Method indicates which correlation coefficient is used for the test."),
         h5("'pearson'= both continuous variables; 'kendall' tau= continuous, ordinal, and binary scales. 'spearman' rho= continuous, ordinal, and binary scales; nonparametric method will detect not only non-linear relationships but non-monotonic ones."),
         h5("Exact indicates whether an exact p-value should be computed. Used for 'kendall' and 'spearman'. Default is to leave #7 blank."),
         h5("Continuity correction used for 'kendall' and 'spearman' when not computed exactly."),
         br(),
         fluidRow(   
           column(3, 
                  uiOutput("sctr_crtst_y")),
           column(3, offset=1,
                  uiOutput("sctr_crtst_x")),
           column(3, offset=1,
                  uiOutput("sctr_crtst_meth"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("sctr_crtst_alt")),
           column(3, offset=1,
                  uiOutput("scatter_cor_test_CI")),
           column(3, offset=1,
                  uiOutput("scatter_cor_test_exct"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("scatter_cor_test_cnt")),
           column(3, offset=1,
                  uiOutput("sctr_crtst_clr")),
           column(3, offset=1,
                  uiOutput("scatter_cor_test_run_YN"))
         ),
         plotOutput("scatter_cor_test_plt_out", height = 800, width = 1200),
         br(),
         h5("Correlation test results"),
         verbatimTextOutput("scatter_cor_test_cor_test_out"),
         
         ## summaryRc plot ## 
         br(),
         h4("Graphical Summarization of Continuous Variables Against a Continuous Response"),
         fluidRow(
           br(),
           column(3, 
                  uiOutput("smryRc_y")),
           column(3, offset=1,
                  uiOutput("smryRc_x"))
         ),
         br(),
         fluidRow(
           column(3, 
                  uiOutput("smryRc_strat_yes_no")),
           column(3, offset=1,
                  uiOutput("smryRc_z")),
           column(3, offset=1,
                  uiOutput("smryrc_choice"))
         ),
         h5("Summary of lowess smoothed X against Y by stratification levels. Percentiles and tick marks below indicate X's data density."),
         plotOutput("summaryRC_plot_function_out", height = 800, width = "100%"),          
         br(),
         ## Density plot of trend over time by groups ## 
         h4("Trend over time by groups"),
         br(),
         h5("A density plot per time period. Requires complete data for all time periods."),
         h5("Press the play button to see the trend. Highlight specific groups. Names are above group rates, randomly stacked. Select >1 time increments for rolling average."),
         fluidRow(   
           column(3, 
                  uiOutput("dnsty_grp_trnd_Yvar")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_Xvar")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_Xlevs")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_Zvar"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("dnsty_grp_trnd_Z_inc")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_ln_clr")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_trgt")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_lgd_loc"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("dnsty_grp_trnd_st_sed")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_run_yesno")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_sec")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_ply"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("dnsty_grp_trnd_Xlim1")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_Xlim2")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_Ylim1")),
           column(3, 
                  uiOutput("dnsty_grp_trnd_Ylim2"))
         ),
         br(),
         plotOutput("dnsty_grp_trnd_plot", height = 800, width = "100%"),          
         br(),
         h5("Rates by period from the density plot above"),
         br(),
         verbatimTextOutput("dnsty_grp_trnd_out_by_tm"),
         br(),
         h4("Explore the missingness of a variable by factor levels."),
         br(),
         fluidRow(
           column(3, 
                  uiOutput("miss_y")),
           column(3, offset=1,
                  uiOutput("miss_x")),
           column(3, offset=1,
                  uiOutput("miss_choice"))
           ),
         br(),
         br(),
         h5("Proportion of missng values stratified by factors."),
         plotOutput("MissSmryPlt", height = 700), 
         br(),
         h5("Total proportion of missng values for each factor."),
         plotOutput("naPlt", height = 700), 
         br()
),    

################################################################################
#                       Power analysis                                         #
################################################################################

tabPanel("Power" ,
         h4("One- and two-sample binomial proportion tests or a one- and two-sample and paired t-tests"),
         h5("One-sample tests compare data with a hypothetical value (e.g., heads from a coin toss vs. 0.50 rate). Two-sample tests compare independent groups (e.g., group 1's age vs. group 2's age). Paired t-test compares dependent samples (e.g., patient's blood pressure time 1 vs time 2)."),
         br(),
         h4("Proportion tests"),
         fluidRow(
           column(3,
                  uiOutput("prp_tst_y")),
           column(3, 
                  uiOutput("prp_tst_x")),
           column(3, 
                  uiOutput("prp_tst_1_smpl")),
           column(3, 
                  uiOutput("prp_tst_prp"))
         ),
         fluidRow(  
           column(3, 
                  uiOutput("prp_tst_alt")),
           column(3,
                  uiOutput("prp_tst_CI")),
           column(3, 
                  uiOutput("prp_tst_yts")),
           column(3, 
                  uiOutput("prp_tst_YN"))
         ),
         verbatimTextOutput("proportion_test_out"),
         br(),
         h4("t-tests"),
         fluidRow(
           column(3,
                  uiOutput("t_tst_y")),
           column(3, 
                  uiOutput("t_tst_x")),
           column(3, 
                  uiOutput("t_tst_1_smpl")),
           column(3, 
                  uiOutput("t_tst_mn"))
         ),
         fluidRow(  
           column(3, 
                  uiOutput("t_tst_alt")),
           column(3, 
                  uiOutput("t_tst_CI")),
           column(3,
                  uiOutput("t_tst_pr")),
           column(3, 
                  uiOutput("t_tst_YN")) 
         ),
         verbatimTextOutput("t_test_out"),
         br(),
         ##
         br(),
         h4("Power analysis using a two-sample binomial proportion test or a one- or two-sample t-test"),
           h5("Demo values are used as defaults, including harmonic mean sample sizes for uneven group Ns."),
         h4("Binary outcomes"),
         fluidRow(
             column(3,
                    uiOutput("power_bin")),
             column(3, offset=1,
                    uiOutput("p1_bin")),
             column(3, offset=1,
                    uiOutput("sig_bin"))
           ),
           fluidRow(
             column(3,
                    uiOutput("n_bin")),
             column(3, offset=1,
                    uiOutput("p2_bin")),
             column(3, offset=1,
                    uiOutput("one_two_side_bin"))
           ),
           fluidRow(
             column(3, 
                    uiOutput("pwr_smp_bin"))
           ),
           br(),
         h4("Continuous outcomes"),
         fluidRow(
             column(3,
                    uiOutput("power_con")),
             column(3, offset=1,
                    uiOutput("delta_con")),
             column(3, offset=1,
                    uiOutput("sig_con"))
           ),
           fluidRow(
             column(3,
                    uiOutput("n_con")),
             column(3, offset=1,
                    uiOutput("sd_Con")),
             column(3, offset=1,
                    uiOutput("type_con"))
           ),
           fluidRow(
             column(3, 
                    uiOutput("one_two_side_con")),
             column(3, offset=1,
                    uiOutput("pwr_smp_con"))
           ),
           br(),
         h4("Do you have uneven group sizes?"),
         fluidRow(
             column(3,
                    uiOutput("grp1_n")),
             column(3, offset=1,
                    uiOutput("grp2_n")),
             column(3, offset=1,
                    uiOutput("harmonic_n"))
           ),
           
         h4("Power analysis summary"),
         verbatimTextOutput("power_summary"),
         h5("'Delta' represents the difference between the two group's mean."),
         h5("The pooled standard deviation (SD) is calculated as the average of each group's SD (Cohen, 1988)."),
         br(),
         h4("Effect size summary"),
         verbatimTextOutput("effect_size_summary"),
         h5("Effect sizes are standardized values that represent the magnitude of differences between the groups."),
         h5("Binary outcomes: Values of 0.20, 0.50, and 0.80 represent small, medium, and large effects (Cohen, 1988)."),
         h5("Continuous outcomes: Values of 0.20, 0.50, and 0.80 represent small, medium, and large effects. Consult Cohen, 1988, about paired t-tests.")
), 

###
tabPanel("95% CIs",
         h4("This plot produces unadjusted confidence intervals for each level of a factor."),
         fluidRow(   
           column(3, 
                  uiOutput("CIy")),
           column(3, offset=1,
                  uiOutput("CIx")),
           column(3, offset=1,
                  uiOutput("Ci_Choice_Type")),
         ),
         fluidRow(   
           column(3, 
                  uiOutput("Ci_Conf_Lev")),
           column(3, offset=1,
                  uiOutput("Ci_Tgt_Line")),
           column(3, offset=1,
                  uiOutput("Ci_create"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("ci_plot_ln_clrs")),
           column(3, offset=1,
                  uiOutput("ci_plot_pt_clrs")),
           column(3, offset=1,
                  uiOutput("Ci_Alpha_Num"))
         ),
         plotOutput("Plot_Ci_output", height = 800, width="100%"),
         h6("Note: You can sort alphabetically by the factor level name or numerically by the point estimate. Left side = factor level, right side = point estimate."),
         verbatimTextOutput("Cidf_output"),
         h6("Note: The values above are point estimates and confidence limits that are sorted alphabetically and numerically. Poisson group and pairwise comparisons assume normal approximation."),
         br(),
         h4("Performance of groups over time (need >= 6 time points for spline knots, use 'straight trend lines' when < 6)"),
         h5("This plot has straight or smoothed spline trajectories, with or without confidence bands. Smoothed \"trend\" lines may not have cooridnate values that equal rates."),
         br(),
         fluidRow(   
           column(3, 
                  uiOutput("FCIy")),
           column(3, 
                  uiOutput("FCIx")),
           column(3, 
                  uiOutput("FCIz")),
           column(3, 
                  uiOutput("FCIzInc"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("fciplot_grp_levs")),
           column(3, 
                  uiOutput("FCi_Choice_Type")),
           column(3, 
                  uiOutput("FCi_Conf_Lev")),
           column(3, 
                  uiOutput("FCI_bands"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("fci_plot_ln_clrs")),
           column(3, 
                  uiOutput("fci_plot_ln_wdth")),
           column(3, 
                  uiOutput("fci_plot_TgtTpt_ln_wdth")),
           column(3, 
                  uiOutput("FCI_nk_knots")),
         ),
         fluidRow(   
           column(3, 
                  uiOutput("FCi_ovral_line")),
           column(3, 
                  uiOutput("FCi_Tgt_Line")),
           column(3, 
                  uiOutput("FCi_Tm_Pt_Line")),
           column(3, 
                  uiOutput("fci_plot_txt_lbl_sz"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("fci_plot_ovral_ln_clrs")),
           column(3, 
                  uiOutput("fci_plot_tgt_ln_clrs")),
           column(3, 
                  uiOutput("fci_plot_time_pt_ln_clrs")),
           column(3, 
                  uiOutput("FCi_create"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("FCi_strght_ln"))
         ),
         fluidRow(   
           column(3, 
                  uiOutput("FCI__Xlim1")),
           column(3, 
                  uiOutput("FCI__Xlim2")),
           column(3, 
                  uiOutput("FCI__Ylim1")),
           column(3, 
                  uiOutput("FCI__Ylim2"))
         ),  
         h5("Modify the plot space in #22-25. Enter values into 'c()' for #13 and #14 when needed, separate values with ',' (e.g., c(1, 2) ). For single time point data, use 'Yes: Aggregated data only' in #21."),
         h5("Note: The colors of the lines are assigned according to the group order of the 'point estimate and confidence interval' output below."),
         br(),
         plotOutput("Plot_Fci_output", height = 800, width="100%"),
         h5("These are point estimates and confidence intervals. These may not match up with smoothed lines. x_lev and z_lev match with #2 and #3 above."),
         tableOutput("time_ci_out1"),
         h5("Overall rates. Uses all data and used for the 'overall group trend line'."),
         tableOutput("all_time_ci_out1")
)
###
   

############## TEST SECTION #############################
#, #THIS COMMA IS COMMENTED OUT IN CASE I EVER NEED THE TEST FUNCTION BELOW    
 
#tabPanel("Test it",                                #Creates a new panel named "Test"
#         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
#           verbatimTextOutput("test1")
#           plotOutput("testplot1")
#         ))
############## TEST SECTION #############################


        )             ####From this point down, this closes the main sections at the top
      )
    )
  )
)
