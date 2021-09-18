#Install these packages to run this Shiny App
#install.packages("shiny")
#install.packages("jsonlite")
#install.packages("rms") 
#install.packages("nlme")
#install.packages("lm.beta")
#install.packages("sensitivity")
#install.packages("meta")
#install.packages("rpart")
#install.packages("coxme")
 
library(shiny)
#library(jsonlite)
library(rms)
#library(nlme)
#library(lm.beta)
#library(sensitivity)
#library(meta)
#library(rpart)
#library(coxme)
#Surival package data: cancer colon diabetic flchain heart mgus nafld1 pbc transplant   
#data(lungcancer)
options(shiny.maxRequestSize=1000*1024^2)    #This will increase the shiny file upload limit from current 5MB max
options(scipen=10)                                                               #General option to see low decimals

####################################

shinyServer(
  
  
  function(input, output) {
    
    #Reactives values  
    values <- reactiveValues()
    
    df <- reactive({                  #This indicates the data frame I will use.
            if ( input$UseText == "No") {
              get(input$dataframe)  
            }  else {
              dataInput_txt()
            }
    })

    atch <- reactive({                  #Used to attach the data frame. 
      attach(df())      #I can only attach within a function...I think
    })
    
    var <- reactive({                  #I use this to get the variable names from the data frame. 
      names(df())  
    })  
    
    #XYplot line color names
    xyplot_Line_Color_Names <- reactive({                 
      colors()[c(552, 498,652, 254, 26,547, 24, 152,32, 52:56, 66, 68, 85,97, 120, 128,139,
                 142, 147:151, 175, 300, 310, 320, 330, 340, 350, 367,372,393, 448, 399,450, 
                 485, 514, 529,562,568, 584, 589, 610, 615, 620, 625, 630, 635,640,646,651,657)]    
    })    

################################################################################    
#                       Download/Upload/Save data                              #
################################################################################    

    #This is the new data from this question: 4. Do you want to create a new data frame?
    #Use this as a summary to confirm new data
    new_smry_df <- reactive({     
      if(input$create_new_df == "Yes") {
      str(newdf())  
      }
    })
    
    #Don't need this for "Data" tab
    output$new_smry_df <- renderPrint({
      if(input$create_new_df == "Yes") {
        new_smry_df()
      }
    })
    
    #Use
    output$use_txt <- renderUI({
      radioButtons("UseText", "3. Analyze/Save the text file?", 
                   choices= c("No", "Yes"), selected="No")     
    })

    #The new data frame name for the text file
    output$new_df_name <- renderUI({ 
      textInput("NewDfName", "4. Enter the new data frame name", 
                value= "new")     
    })
    
    #The new data frame name for the transformed/imputed and some original
    output$new_df_name_all <- renderUI({ 
      textInput("NewDfNameAll", "1. Enter the new name for transformed/imputed data", 
                value= "transformed")     
    })

    #The new data frame name for the transformed/imputed/factor scores/some original
    output$new_df_name_all_fs <- renderUI({ 
      textInput("NewDfNameAllFs", "1. Enter the new name for transformed/imputed/factor data", 
                value= "factor")     
    })

#Modify the dataset
    #Change the data type
    #Character
    output$modify_character <- renderUI({                                 #Same idea as output$vy
      selectInput("ModifyCharacter", "1. Select variables to convert to 'character'.", 
                  choices = var(), multiple=TRUE)
    })
    #Factor
    output$modify_factor <- renderUI({                                 #Same idea as output$vy
      selectInput("ModifyFactor", "2. Select variables to convert to 'factor'.", 
                  choices = var(), multiple=TRUE)
    })
    #Numeric
    output$modify_numeric <- renderUI({                                 
      selectInput("ModifyNumeric", "3. Select variables to convert to 'numeric'.", 
                  choices = var(), multiple=TRUE)
    })
    #4. Time
    output$modify_time_X <- renderUI({                                 
      selectInput("ModifyTimeX", "4. Select variables to format as 'Date'.", 
                  choices = var(), multiple=TRUE)
    })
    #5. Select Time format. Not using pure strptime ("1/31/2021 21:15")
    output$modify_time_format <- renderUI({  
      selectInput("ModifyTimeFmt", "5. Choose the correct time format.", 
                  choices = c("31JAN2021", "31JAN21","31-JAN-2021","31-JAN-21","01/31/2021", "01/31/21", 
                              "01-31-2021", "01-31-21", "2021-01-31", "21-01-31", #"1/31/2021 21:15",
                              "1/31/2021 21:15 as 1/31/2021", #"1/31/2021 21:15:30",
                              "1/31/2021 21:15:30 as 1/31/2021", #"1/31/2021 12:00:00 AM", 
                              "1/31/2021 12:00:00 AM as 1/31/2021", "31JAN2021:12:00:00 as 1/31/2021",
                              "44227 in Excel"), 
                  multiple=TRUE, selected="01-31-2021")
    })
    #6. Number of replications per format used
    output$modify_tm_fmt_rep <- renderUI({ 
      textInput("ModifyTimeFmtReps", "6. Number of variables per format.", 
                value= paste0('c(', length(input$ModifyTimeX), ')') )     
    })
    
    ## Modify the dataset ##
    #7. Subset the dataset
    output$subset_df_yes_no <- renderUI({                                 
      selectInput("SubsetYesNo", "7. Want to subset the data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #8. Formula for the subset
    output$subset_args <- renderUI({ 
      textInput("SubsetArgs", "8. Enter the formula to subset data.", 
                value= "subset= , select=")     
    })
    #9. Modify the dataset
    output$modify_df_yes_no <- renderUI({                                 
      selectInput("ModifyDfYesNo", "9. Want to create the modified dataset?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #10. Time
    output$modify_2_var_Time <- renderUI({                                 
      selectInput("Modify2VrTm", "10. Create 'Time': Select 2 variables.", 
                  choices = var(), multiple=TRUE)
    })
    #11. Make month indicators
    output$modify_Time_add_month <- renderUI({                                 
      selectInput("ModifyTmAddMth", "11. Select X to create 'YYMM' and 'Month'.", 
                  choices = var(), multiple=FALSE)
    })
    #12. Add time variables to dataset
    output$modify_add_time_YN <- renderUI({                                 
      selectInput("ModAddTmYN", "12. Add time variables to modified data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    
    #####################
    # Save modified data #
    #####################
    output$modified_df_save <- renderUI({  
      selectInput("ModifiedDfSave", "13. Save the modified data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    ## Subset the dataset ##
    modifySubsetFnc <- function(df, ModifyDfYesNo, SubsetArgs, SubsetYesNo) {
        if (SubsetYesNo == "Yes") {
          df_mod <- eval(parse(text=paste0("subset(df,", SubsetArgs, ")" )))
        } else {
          df_mod <- df
      }
      return(df_mod)
    }
    #Runs the function above
    modifySubsetDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        modifySubsetFnc(df=df(), ModifyDfYesNo=input$ModifyDfYesNo, 
                      SubsetArgs=input$SubsetArgs, SubsetYesNo=input$SubsetYesNo)
      }
    })
## Modify the variable type of data   
    #Character
    modifiedCharFnc <- function(df, ModifyCharacter) {
      df_mod <- df
        if (!is.null(ModifyCharacter)) {
          df_mod[, which(colnames(df_mod) %in% ModifyCharacter)] <- lapply(df_mod[which(colnames(df_mod) %in% ModifyCharacter)], as.character)
      }
      return(df_mod[, which(colnames(df_mod) %in% ModifyCharacter), drop = FALSE])
    }
    #Runs the function above
    modifiedCharDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        modifiedCharFnc(df=modifySubsetDf(), ModifyCharacter=input$ModifyCharacter)
      }
    })
    #Factor
    modifiedFacFnc <- function(df, ModifyFactor) {
      df_mod <- df
      if (!is.null(ModifyFactor)) {
        df_mod[, which(colnames(df_mod) %in% ModifyFactor)] <- lapply(df_mod[which(colnames(df_mod) %in% ModifyFactor)], as.factor)
      }
      return(df_mod[, which(colnames(df_mod) %in% ModifyFactor), drop = FALSE])
    }
    #Runs the function above
    modifiedFacDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        modifiedFacFnc(df=modifySubsetDf(), ModifyFactor=input$ModifyFactor)
      }
    })
    #Numeric
    modifiedNumFnc <- function(df, ModifyNumeric) {
      df_mod <- df
        if (!is.null(ModifyNumeric)) {
        df_mod[, which(colnames(df_mod) %in% ModifyNumeric)] <- lapply(df_mod[which(colnames(df_mod) %in% ModifyNumeric)], as.numeric)
      }
      return(df_mod[, which(colnames(df_mod) %in% ModifyNumeric) ,drop=FALSE])
    }
    #Runs the function above
    modifiedNumDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
      modifiedNumFnc(df=modifySubsetDf(), ModifyNumeric=input$ModifyNumeric)
      }
    })
    ## Date formats ##
    #Create formats
    fncFmtVec <- function(Format, CntVec) {
      Full.Format <- rep(Format, CntVec )
      return(Full.Format)
    }
    #Runs the function above
    modifiedFullFormat <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        fncFmtVec(Format= input$ModifyTimeFmt, 
                  CntVec= eval(parse(text=input$ModifyTimeFmtReps  )) )
      }
    })
    
    ## FUnction to create formatted data ##
    fncDateFmt <- function(DF, X, Format) {
      if(is.null(X)) {
        df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
      } else {
        df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
      for(i in 1:length(X)) {
        switch(Format[i], 
               "31JAN2021" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d%b%Y"), 
               "31JAN21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d%b%y"), 
               "31-JAN-2021" =  df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d-%b-%Y"),
               "31-JAN-21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d-%b-%y"),
               "01/31/2021" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m/%d/%Y"),
               "01/31/21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m/%d/%y"),
               "01-31-2021" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m-%d-%Y"),
               "01-31-21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m-%d-%y"),
               "2021-01-31" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%Y-%m-%d"),
               "21-01-31" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%y-%m-%d"),
               "1/31/2021 21:15" = df_mod[, X[i]] <- strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M"),
               "1/31/2021 21:15 as 1/31/2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M")),
               "1/31/2021 21:15:30" = df_mod[, X[i]] <- strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S"),
               "1/31/2021 21:15:30 as 1/31/2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S")),
               "1/31/2021 12:00:00 AM" = df_mod[, X[i]] <- strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S %p"),
               "1/31/2021 12:00:00 AM as 1/31/2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S %p")),
               "31JAN2021:12:00:00 as 1/31/2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%d%b%Y:%H:%M:%S")),
               "44227 in Excel" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], origin="1899-12-30")) 
      }
      }
      return(df_mod)
    }
    #Runs the function above
    modifiedDateDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        fncDateFmt(DF=modifySubsetDf(),  X=input$ModifyTimeX, Format= modifiedFullFormat() )
      }
    })
    
    ## Variables that are not modified ##
    non_modified_vars <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        setdiff(var(),  c(input$ModifyCharacter,input$ModifyFactor, 
                          input$ModifyNumeric, input$ModifyTimeX ))
      }
    })

    #Create modified dataset, without time variables
    modifiedDf1 <- reactive({
      #if(input$ModifiedDfSave == "Yes") {
        if(input$ModifyDfYesNo == "Yes") {
        cbind(modifySubsetDf()[, which(colnames(modifySubsetDf()) %in% non_modified_vars()), drop=FALSE], 
              modifiedCharDf(), modifiedFacDf(), modifiedNumDf(), modifiedDateDf() ) 
      }
    })
    
    ## FUnction to create time variable ##
    fncTimeCrt <- function(DF, X) {
      df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
      df_mod[, "Time"] <- as.numeric(difftime(DF[, X[2]] , DF[, X[1]], units = "days"))
      #Make final data frame
      df_mod <- df_mod[, "Time", drop = FALSE] 
      return(df_mod)
    }
    #Runs the function above
    modifiedTimeVarCrt <- reactive({
      if(input$ModAddTmYN == "Yes") {
        if( !is.null(input$Modify2VrTm) ) {
          fncTimeCrt(DF=modifiedDf1(),  X=input$Modify2VrTm )
        } else {
          #"No.Time.Var"= c(NA)
          data.frame("No.Time.Var"= rep(NA, nrow(modifiedDf1())))
        } 
      }
    })
    ## FUnction to create YYMM and Month variables ##
    fncYYMMmthCrt <- function(DF, X) {
      df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
      df_mod[, "YYMM"] <- (as.numeric(format(df_mod[, X], "%Y")) * 100) + as.numeric(format(df_mod[, X], "%m"))
      #Create Month
      df_mod[, "Month"] <- as.numeric(ordered( df_mod[, "YYMM"] ))
      #Make final data frame
      df_mod <- df_mod[, which(colnames(df_mod) %in% c("YYMM", "Month") )] 
      return(df_mod)
    }
    #Runs the function above
    modifiedYMmonthCrt <- reactive({
      if(input$ModAddTmYN == "Yes") {
        fncYYMMmthCrt(DF=modifiedDf1(),  X=input$ModifyTmAddMth )
      }
    })
    
    output$modified_df_name <- renderUI({ 
      textInput("ModifiedDfName", "14. Enter the data frame name.", 
                value= "mod_df")     
    })
    
    #Create modified dataset
    modifiedDf <- reactive({
      if(input$ModifiedDfSave == "Yes") {
      if(input$ModAddTmYN == "Yes") {
        cbind(modifiedDf1(), modifiedTimeVarCrt(), modifiedYMmonthCrt() ) 
      } else {
        modifiedDf1()
      }
      }
    })
    #Save data
    output$download_modified_df <- downloadHandler(
      filename = "modified_df.RData",
      content = function(con) {
        assign(input$ModifiedDfName, modifiedDf())
        save(list=input$ModifiedDfName, file=con)
      }
    )
    
    #################################################
    #New data from the text file
    newdf <- reactive({
      if(input$create_new_df == "Yes") {
        df()
      }
    })
    
    #This reactive function identifies which predictors not used in the transformation/imputation will be saved
    keep_some <- reactive({
      setdiff(var(), ptrns_trnfd_vls())
    })
    
    #NEW DF function for transformed and imputed data
    newDFfnc <- function(questn, keeps, df1, df2, df3) {
      if(questn == "Yes") {
        df <- cbind(
          df1[, keeps], df2, df3) 
      }
      colnames(df) <- c(colnames(df1[, keeps]), paste0(colnames(df2), "_i"), paste0(colnames(df3), "_t"))
      return(df)
    }

        
    #Runs the above function, creates transformed and imputed data 
    newdf_all <- reactive({
      if(input$create_new_df_all == "Yes") {
        newDFfnc(questn=input$create_new_df_all, keeps=keep_some(), df1=df(), df2=imputed(), df3=as.data.frame(ptrans()[["transformed"]]))
      }
    })
    

    #NEW DF function for transformed/imputed and factor scores data
    newDFfnc_fs <- function(questn, keeps, df1, df2, df3, df4) {
      if(questn == "Yes") {
        df <- cbind(
          df1[, keeps], df2, df3, df4) 
      }
      colnames(df) <- c(colnames(df1[, keeps]), paste0(colnames(df2), "_i"), paste0(colnames(df3), "_t"), colnames(df4))
      return(df)
    }

    #Select variables, transformed, imputed 
    newdf_all_fs <- reactive({
      if(input$create_new_df_all_fs == "Yes") {
        newDFfnc_fs(questn=input$create_new_df_all_fs, keeps=keep_some(), df1=df(), df2=imputed(), df3=as.data.frame(ptrans()[["transformed"]]), df4=pca_fac_df())
      }
    })
    

    ######  Upload  #####
    #1. View the data frame name
    output$View_main_df <- renderUI({ 
      selectInput("viewMainDF", "Want to view the top of the model data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #1A. Object with transition names
    view_main_data_yesno <- reactive({
      input$viewMainDF
    })
    #2. This prints the main data
    output$view_main_data_out <- renderTable({
      if(view_main_data_yesno() == "Yes") {
        head(df(), 10)
      }
    }, rownames = TRUE)
    
    #Text files    
    output$text_file_type <- renderUI({
      radioButtons(inputId="TextFile", label="1. What is the text file type?", choices = c("Comma", "Tab"))
    })
    
    #Type of text file, identifies the type of seperator I will use
    sep_type <- reactive({
      if(input$TextFile == "Comma") {
        ","
      } else {
        "\t"
      }
    })
    
    output$upload_df <- renderUI({
      fileInput("file1", "2. Choose a text File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    })
    
    #Text file data
    dataInput_txt <- reactive({
      sessionEnvir <- sys.frame()
      file1 <- input$file1
      if (!is.null(input$file1)) read.csv(file=file1$datapath, header = TRUE, sep= sep_type())
    })
    
    #This gives the data structure of the text file
    output$datastr_txt <- renderPrint({
      if (is.null(dataInput_txt()))  return()  else names(dataInput_txt())
    })

    #R data file upload
    output$upload_r_df <- renderUI({
      fileInput('f1', 'Choose an RData File', accept=c('.RData'))
    })

    #Loads R file
    dataInput <- reactive({
      sessionEnvir <- sys.frame()
      if (!is.null(input$f1)) load(input$f1$datapath, sessionEnvir)
    })
    
    #Data structure of the R file
    output$datastr <- renderPrint({
      if (is.null(dataInput()))  return()  else str(dataInput())
    })
    
    ######  Downaload  #####
    output$downloadSave <- downloadHandler(
      filename = "Rdata.RData",
      content = function(con) {
        assign(input$NewDfName, newdf())
        save(list=input$NewDfName, file=con)
      }
    )

    #R data file upload of imputed, transformed and some original data
    output$downloadSaveAll <- downloadHandler(
      filename = "Rdata.RData",
      content = function(con) {
        assign(input$NewDfNameAll, newdf_all())
        save(list=input$NewDfNameAll, file=con)
      }
    )

    #R data file upload of imputed, transformed, factor scores, and some original data
    output$downloadSaveAllFs <- downloadHandler(
      filename = "Rdata.RData",
      content = function(con) {
        assign(input$NewDfNameAllFs, newdf_all_fs())
        save(list=input$NewDfNameAllFs, file=con)
      }
    )
    

    #################################################
    #From text file
    output$new_df <- renderUI({                                 #Same idea as output$vy
      selectInput("create_new_df", "5. Do you want to create a new data frame?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    

################################################################################    
################################################################################    
    


################################################################################
#               Proportion and t-tests and POWER ANALYSIS                      #
################################################################################

#####################
## Proportion test ##
#####################

#1. Y variable "Select the response variable"
output$prp_tst_y <- renderUI({                                
  selectInput("prpTstY", "1. Select the outcome",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
proportion_test_y <- reactive({
  input$prpTstY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$prp_tst_x <- renderUI({                                 #Same idea as output$vy
  selectInput("prpTstX", "2. Select the group", 
              choices = setdiff(var(), proportion_test_y()), multiple=FALSE) 
})
#2A. Reactive function for the X variable
proportion_test_x <- reactive({
  input$prpTstX
})

#3. 1-sample test 
output$prp_tst_1_smpl <- renderUI({                                #
  selectInput("prpTst1s", "3. Is this a 1 sample test?",       #
              choices = c("No","Yes"), multiple=FALSE, selected= "No")
})
#3A. Reactive function for proportion level
proportion_test_one_sample <- reactive({
  input$prpTst1s
})

#4. Select 1-sample test probability
output$prp_tst_prp <- renderUI({                                #
  numericInput("prpTstPrp", "4. Select 1-sample test probability",       #
               value = 0.5, min=0, max=1, step=.01 )   #
})
#5A. Reactive function for proportion level
proportion_test_prop <- reactive({
  input$prpTstPrp
})
#5. Alternative hypothesis test
output$prp_tst_alt <- renderUI({                                
  selectInput("prpTstAlt", "5. Select a one- or two-sided test",        
              choices = c("two.sided", "less", "greater"), multiple=FALSE, selected="two.sided" ) 
})
#5A. Reactive function for alternative hypothesis test
proportion_test_alternative <- reactive({
  input$prpTstAlt
})
#6. 
output$prp_tst_CI <- renderUI({                               
  numericInput("prpTstCI", "6. Select the confidence interval level", 
               value = 0.95, min=0, max=1, step=.01 )   
})
#6A. Reactive function for confidence interval
proportion_test_Conf_Int <- reactive({
  input$prpTstCI
})
#7. Yates' correction
output$prp_tst_yts <- renderUI({  
  selectInput("prpTstYC", "7. Use the Yates' correction?", 
            choices = c(TRUE, FALSE), multiple=FALSE, selected=TRUE)
})
#7A. Reactive function for Exact method
proportion_test_Yates <- reactive({
  input$prpTstYC
})
#8. Exact method
output$prp_tst_YN <- renderUI({  
  selectInput("prpTstYN", "8. Run the proportion test?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#8A. Reactive function for Exact method
proportion_test_Yes_No <- reactive({
  input$prpTstYN
})
#9. Run the function below
proportion_test_run <- reactive({
  if(proportion_test_Yes_No() == "Yes") {    
    fncPrpTst(DF=df(), Y=proportion_test_y(), X=proportion_test_x(), P=proportion_test_prop(), 
              Alt=proportion_test_alternative(), CI=proportion_test_Conf_Int(), 
              Correct=proportion_test_Yates(), Samp1= proportion_test_one_sample())
  }  
})
#9A. proportion test output  
output$proportion_test_out <- renderPrint({
  if(proportion_test_Yes_No() == "Yes") {
    proportion_test_run()
  }
})

#############
## t-tests ##
#############
#1. Y variable "Select the response variable"
output$t_tst_y <- renderUI({                                
  selectInput("tTstY", "1. Select the outcome",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
t_test_y <- reactive({
  input$tTstY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$t_tst_x <- renderUI({                                 #Same idea as output$vy
  selectInput("tTstX", "2. Select the group", 
              choices = setdiff(var(), t_test_y()), multiple=FALSE) 
})
#2A. Reactive function for the X variable
t_test_x <- reactive({
  input$tTstX
})

#3. 1-sample test 
output$t_tst_1_smpl <- renderUI({                                #
  selectInput("tTst1s", "3. Is this a 1 sample test?",       #
              choices = c("No","Yes"), multiple=FALSE, selected= "No")
})
#3A. Reactive function for proportion level
t_test_one_sample <- reactive({
  input$tTst1s
})

#4. Select 1-sample test mean
output$t_tst_mn <- renderUI({                                #
  numericInput("tTstMn", "4. Select 1-sample test mean",       #
               value = 1, step=1 )   #
})
#5A. Reactive function for proportion level
t_test_mean <- reactive({
  input$tTstMn
})
#5. Alternative hypothesis test
output$t_tst_alt <- renderUI({                                
  selectInput("tTstAlt", "5. Select a one- or two-sided test",        
              choices = c("two.sided", "less", "greater"), multiple=FALSE, selected="two.sided" ) 
})
#5A. Reactive function for alternative hypothesis test
t_test_alternative <- reactive({
  input$tTstAlt
})
#6. 
output$t_tst_CI <- renderUI({                               
  numericInput("tTstCI", "6. Select the confidence interval level", 
               value = 0.95, min=0, max=1, step=.01 )   
})
#6A. Reactive function for confidence interval
t_test_Conf_Int <- reactive({
  input$tTstCI
})
#7. Paired sample
output$t_tst_pr <- renderUI({  
  selectInput("tTstPr", "7. Is this a paired sample test?", 
              choices = c(TRUE, FALSE), multiple=FALSE, selected=FALSE)
})
#7A. Reactive function for Exact method
t_test_pair <- reactive({
  input$tTstPr
})
#8. Run the test
output$t_tst_YN <- renderUI({  
  selectInput("tTstYN", "8. Run the t-test?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#8A. Reactive function for above
t_test_Yes_No <- reactive({
  input$tTstYN
})
#9. Run the function below
t_test_run <- reactive({
  if(t_test_Yes_No() == "Yes") {    
    fncTTst(DF=df(), Y=t_test_y(), X=t_test_x(), M=t_test_mean(), 
            Pair=t_test_pair(), Alt=t_test_alternative(), CI=t_test_Conf_Int(), 
              Samp1= t_test_one_sample())
  }  
})
#9A. proportion test output  
output$t_test_out <- renderPrint({
  if(t_test_Yes_No() == "Yes") {
    t_test_run()
  }
})

#####################
## Power analysis  ##
#####################

############
## Binary ##
############
#Treatment group
output$power_bin <- renderUI({                                #
  numericInput("powerBin", "1. Select the power level.",       #
               value = 0.80, min=0, max=1, step=.01 )   #
})
output$n_bin <- renderUI({                                #
  numericInput("nBin", "2. Select the sample size.",       #
               value = 100, min=0, step=1)   #
})
output$p1_bin <- renderUI({                                #
  numericInput("p1Bin", "3. Select the proportion of the treatment group.",       #
               value = 0.15, min=0, max=1, step=.01 )   #
})
output$p2_bin <- renderUI({                                #
  numericInput("p2Bin", "4. Select the proportion of the control group.",       #
               value = 0.30, min=0, max=1, step=.01 )   #
})
output$sig_bin <- renderUI({                                #
  numericInput("sigBin", "5. Select the significance level (alpha).",       #
               value = 0.05, min=0, max=1, step=.01 )   #
})
output$one_two_side_bin <- renderUI({                                #
  selectInput("oneTwoSideBin", "6. Select a one- or two-sided test.",       #
              choices = c("two.sided", "one.sided"), multiple=FALSE, 
              selected="two.sided" )   #
})
output$pwr_smp_bin <- renderUI({                                #
  selectInput("pwrsmpBin", "7. Do you want to determine power or sample size.",       #
              choices = c("Power", "Sample Size"), multiple=FALSE, selected="Sample Size" )   #
})

################
## Continuous ##
################
#Treatment group
output$power_con <- renderUI({                                #
  numericInput("powerCon", "1. Select the power level.",       #
               value = 0.80, min=0, max=1, step=.01 )   #
})
output$n_con <- renderUI({                                #
  numericInput("nCon", "2. Select the sample size.",       #
               value = 100, min=0, step=1)   #
})
output$delta_con <- renderUI({                                #
  numericInput("deltaCon", "3. Select the delta level.",       #
               value = 0.5, min=0, step=.01 )   #
})
output$sd_Con <- renderUI({                                #
  numericInput("sdCon", "4. Select the standard deviation.",       #
               value = 2, min=0, step=.01 )   #
})
output$sig_con <- renderUI({                                #
  numericInput("sigCon", "5. Select the significance level (alpha).",       #
               value = 0.05, min=0, max=1, step=.01 )   #
})
output$type_con <- renderUI({                                #
  selectInput("typeCon", "6. Select the type of t-test.",       #
              choices = c("two.sample", "one.sample", "paired"), multiple=FALSE, 
              selected="two.sample" )   #
})
output$one_two_side_con <- renderUI({                                #
  selectInput("oneTwoSideCon", "7. Select a one- or two-sided test.",       #
              choices = c("two.sided", "one.sided"), multiple=FALSE, 
              selected="two.sided" )   #
})
output$pwr_smp_con <- renderUI({                                #
  selectInput("pwrsmpCon", "8. Do you want to determine power or sample size.",       #
              choices = c("Power", "Sample Size"), multiple=FALSE, selected="Sample Size" )   #
})

###################
## Harmonic mean ##
###################
output$grp1_n <- renderUI({                                #
  numericInput("grp1N", "1. N for group 1.",       #
               value = 100, min=0,  step=1 )   #
})
output$grp2_n <- renderUI({                                #
  numericInput("grp2N", "2. N for group 2.",       #
               value = 200, min=0,  step=1 )   #
})
output$harmonic_n <- renderUI({                                #
  textInput("harmonicN", "3. Use this N for power analysis.",       #
            value= deparse(harmonicMn()))     
})
#Harmonic mean N formula
harmonicMn <- reactive({
  round((2*(input$grp1N * input$grp2N))/(input$grp1N + input$grp2N), 0)
})

##############
## Analysis ##
##############
## Summary ##  typeBin
output$power_summary <- renderPrint({ 
  list("Binary Outcome"= if (input$pwrsmpBin == "Sample Size") {
    power.prop.test(power=input$powerBin, p1=input$p1Bin, p2=input$p2Bin, 
                    sig.level=input$sigBin, alternative=input$oneTwoSideBin )
  }  else {
    power.prop.test(n=input$nBin, p1=input$p1Bin, p2=input$p2Bin, 
                    sig.level=input$sigBin, alternative=input$oneTwoSideBin)
  },
  "Continuous Outcome"= if (input$pwrsmpCon == "Sample Size") {
    power.t.test(power=input$powerCon, delta=input$deltaCon, sd=input$sdCon, 
                    sig.level=input$sigCon, type=input$typeCon, alternative=input$oneTwoSideCon)
  }  else {
    power.t.test(n=input$nCon, delta=input$deltaCon, sd=input$sdCon, 
                    sig.level=input$sigCon, type=input$typeCon, alternative=input$oneTwoSideCon)
  }
  
  )
  
  })

###################
## Effect size   ##
###################
## Effect sizes for proportions ##
as1 <- reactive({                  
  (asin(sign(input$p1Bin) * sqrt(abs(input$p1Bin))))*2  #Arcsine tranformation for treatment group
})
as2 <- reactive({                  
  (asin(sign(input$p2Bin) * sqrt(abs(input$p2Bin))))*2  #Arcsine tranformation for control group
})
#Effect size
ES_prop <- reactive({ 
  abs(as1() - as2())
})

## Effect size for means ##
ES_mean <- reactive({ 
  input$deltaCon/input$sdCon
})

## Summary ##
output$effect_size_summary <- renderPrint({ 
  list("Binary Outcome"= if (!is.null(c(input$p1Bin, input$p2Bin))) {
    ES_prop()
  }  else {
    NULL
  },
  "Continuous Outcome"= if (!is.null(c(input$deltaCon, input$sdCon))) {
    ES_mean()
  }  else {
    NULL
  }
  
  )
  
})

########################################
## Function to create proportion test ##
########################################
fncPrpTst <- function(DF, Y, X, P, Alt, CI, Correct, Samp1) {
  #Determine if it is 1 or 2 sample test
  if(Samp1 == "Yes" ) {
    Tbl <- table(DF[, Y])
    #Conduct proportion test
    P.Test <- prop.test(Tbl, p=P, alternative = Alt,
                        conf.level = CI, correct = Correct)
  } else {
    Tbl <- table(DF[, X], DF[, Y])
    Tbl <- Tbl[, 2:1]
    #Conduct proportion test
    P.Test <- prop.test(Tbl, alternative = Alt,
                        conf.level = CI, correct = Correct)
  }
  return("Results"= P.Test)
}

####################################################
## Function to do proportion pairwise comparisons ##
####################################################
fncPrwsPrpTst <- function(DF, Y, X) {
  #Determine if it is 1 or 2 sample test
  Tbl <- table(DF[, X], DF[, Y])
  Tbl <- Tbl[, 2:1]
  #Conduct proportion test
  P.Test <- pairwise.prop.test(Tbl, alternative = "two.sided", p.adjust.method="bonferroni")
  return("Results"= P.Test)
}

#################################
## Function to create a t-test ##
#################################
fncTTst <- function(DF, Y, X, M, Pair, Alt, CI, Samp1) {
  #Determine if it is 1 or 2 sample test
  if(Pair ==FALSE)  {
    if(Samp1 == "Yes" ) {
      #Make formula
      FMLA <- as.formula( paste0(Y, "~1") )
      #Conduct proportion test
      T.Tst <- t.test(formula=FMLA, data=DF, mu=M, alternative = Alt, conf.level = CI)
    } else {
      #Make formula
      FMLA <- as.formula(paste(Y, "~", X))
      #Conduct proportion test
      T.Tst <- t.test(formula=FMLA, data=DF,  alternative = Alt, conf.level = CI)
    } 
  } else {
    FMLA <- as.formula( paste0("Pair(", Y, ",",X, ")", "~1") )
    #Conduct proportion test
    T.Tst <- t.test(formula=FMLA, data=DF, alternative = Alt, conf.level = CI)
  }
  return("Results"=T.Tst)
}

############################################
## Function to do group tests for 95% CIs ##
############################################
fncCnfGrpTst <- function(X, Y, DF, ci_type) {
  if (ci_type %in% c("Mean (t)", "Poisson (exact)") ) {
    Formula_1 <- as.formula(paste(paste0(Y, "~"),   
                                  paste( "as.factor(", X,")" )))
  }
  switch(ci_type,                
         "Mean (t)" =  summary(aov(formula= Formula_1, data=DF)), 
         "Proportion (binomial)" = fncPrpTst(DF, Y, X, P=NULL, Alt="two.sided", CI=.95, 
                                             Correct=TRUE, Samp1= "No"), 
         "Poisson (exact)" =  summary(aov(formula= Formula_1, data=DF)) 
  )
}

#############################################
## Function for post-hoc tests for 95% CIs ##
#############################################
fncCnfPstHc <- function(X, Y, DF, ci_type) {
  if (ci_type %in% c("Mean (t)", "Poisson (exact)") ) {
    Formula_1 <- as.formula(paste(paste0(Y, "~"),   
                                  paste( "as.factor(", X,")" )))
  }
  switch(ci_type,                
         "Mean (t)" =  TukeyHSD(aov(formula= Formula_1, data=DF)), 
         "Proportion (binomial)" = fncPrwsPrpTst(DF, Y, X), 
         "Poisson (exact)" =  TukeyHSD(aov(formula= Formula_1, data=DF)) 
  )
}

################################################################################
#                    Confidence interval plots                                 #
################################################################################
#Continuous outcomes
tconf <- function(x, y, dataf, conf_lev) {
  #Aggregates outcome by factor 
  #agr_m <- aggregate(dataf[, y] ~ dataf[, x], FUN="mean", data= dataf)
  #agr_sd <- aggregate(dataf[, y] ~ dataf[, x], FUN="sd", data= dataf)
  #agr_n <- aggregate(dataf[, y] ~ dataf[, x], FUN="length", data= dataf)
  agr_m <- aggregate(dataf[, y], list(dataf[, x]), FUN="mean", na.rm=T)
  agr_sd <- aggregate(dataf[, y], list(dataf[, x]), FUN="sd", na.rm=T)
  agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_m[, 1], agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
  
  #Calculates confidence intervals
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  rownames(adf_alpha) <- agr_df$x_lev
#  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  alpha_o <- order(agr_df$x_lev, decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric) ) 
}

##############

#Binary outcomes
bconf <- function(x, y, dataf, conf_lev) {
  #Aggregates outcome by factor 
#  agr_sum <- aggregate(dataf[, y] ~ dataf[, x], FUN="sum", data= dataf)
#  agr_n <- aggregate(dataf[, y] ~ dataf[, x], FUN="length", data= dataf)
  agr_sum <- aggregate(dataf[, y], list(dataf[, x]),  FUN="sum", na.rm=T)
  agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  #Calculates confidence intervals
  adf_alpha <- binconf(x=agr_df[,2], n=agr_df[,3], alpha=1 - conf_lev)
  adf_alpha <- data.frame(adf_alpha)
  rownames(adf_alpha) <- agr_df$x_lev
#  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  alpha_o <- order(agr_df$x_lev, decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric) ) 
}

##############

#Exact Poisson
pconf <- function(x, y, dataf, conf_lev) {
  #Aggregates outcome by factor 
#  agr_sum <- aggregate(dataf[, y] ~ dataf[, x], FUN="sum", data= dataf)
#  agr_n <- aggregate(dataf[, y] ~ dataf[, x], FUN="length", data= dataf)
  agr_sum <- aggregate(dataf[, y], list(dataf[, x]), FUN="sum", na.rm=T)
  agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  #Calculates confidence intervals
  adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
  for (i in 1:nrow(agr_df)) {
    adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i,3], conf.level= .95)[c("estimate","conf.int")])
  }
  adf_alpha <- data.frame(adf_alpha)
  colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
  rownames(adf_alpha) <- agr_df$x_lev
#  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  alpha_o <- order(agr_df$x_lev, decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric) ) 
}

conf <- function(x=xcivar, y=ycivar, dataf=df(), conf_lev=ciconf_lev) {
  switch(input$ci_type,                #"var" and can be used anywhere in server.r.
         "Mean (t)" =  tconf(x, y, dataf, conf_lev), 
         "Proportion (binomial)" =  bconf(x, y, dataf, conf_lev), 
         "Poisson (exact)" =  pconf(x, y, dataf, conf_lev) 
  )
}


#Point estimates and confidence intervals reactive function
cidf <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
   conf(x=input$xcivar, y=input$ycivar, dataf=df(), conf_lev=input$ciconf_lev)
  }
})
#Use this for putting the output in the correct order
#cidf2 <- reactive({                  #This indicates the data frame I will use.
#  list("Alphabetical"=cidf()[["adf_alpha"]][order(rownames(cidf()[["adf_alpha"]]), decreasing = F), ], 
#       "Numerical"=cidf()[["adf_numeric"]][order(cidf()[["adf_numeric"]][["PointEst"]], decreasing = F), ])
#})

##########################################
# Plot function for confidence intervals #
##########################################
plot_ci_fnc <- function(xcivar, ycivar, ydf, cidf, ciconf_lev, alpha_num, Lcol, Pcol, tgt) {
  if (alpha_num=="Alphabetical") {
    adf <- cidf$adf_alpha
  }
  if (alpha_num=="Numerical") {
    adf <- cidf$adf_numeric
  }  
  mainYmn <- mean(ydf[, ycivar], na.rm=T)
  main_ttl <- paste0(ciconf_lev * 100, "% ", "Confidence Intervals of ", ycivar, " by ", xcivar)
  rng <- seq(min(adf), max(adf),length.out=nrow(adf))
  par(mar=c(5,7,4,4))
  plot(rng, 1:nrow(adf), type="n", ylab="", 
       xlab= paste0("Value (the grey vertical line is the overall mean of ", round(mainYmn, 3), ")"),
       main=main_ttl, axes=F ) 
    for (i in 1:nrow(adf)) {
    lines(c(adf[,'Lower'][i], adf[,'Upper'][i]), c(i,i), lwd=4, col=Lcol) 
    points(adf[,'PointEst'][i],i, pch=24, col=Pcol, lwd=1, bg=Pcol, cex=1.75) 
    }
  #Mean line
  abline(v=mainYmn, lwd=3, col="grey", lty=3)
  #Target line
  abline(v=tgt, lwd=3, col="green", lty=1)
  axis(1) 
  axis(2,at=1:nrow(adf),labels=substr(rownames(adf), 1, 10), las=1, cex.axis=1)
#  axis(2,at=1:nrow(adf),labels=rownames(adf), las=1, cex.axis=1)
  axis(4,at=1:nrow(adf),labels=round(adf[, "PointEst"],2), las=1, cex.axis=1)
  box()
}

#Confidence interval plot reactive function
plot_ci <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
  plot_ci_fnc(xcivar=input$xcivar, ycivar=input$ycivar, ydf=df(), cidf=cidf(), 
              ciconf_lev=input$ciconf_lev, alpha_num=input$alpha_num, 
              Lcol=ci_plot_Line_Colors() , Pcol=ci_plot_Point_Colors(), tgt=ci_target_line() )
  }
})

############################
## UI selection functions ##
############################

#Select the outcome
output$CIy <- renderUI({                                #Creates a UI function here but it will
  selectInput("ycivar", "1. Select the outcome.",       #get called to the UI file.
              choices = var(), multiple=FALSE, selected=var()[1] )   #Will make choices based on my reactive function.
})

#Select the predictors.
output$CIx <- renderUI({                                 #Same idea as output$vy
  selectInput("xcivar", "2. Select the factor.", 
              choices = setdiff(var(), input$ycivar), multiple=FALSE, selected=var()[2])     #Will make choices based on my reactive function.
})

#Select the CI type
output$Ci_Choice_Type <- renderUI({                                
  selectInput("ci_type", "3. Select the type of confidence interval.",
              choices = c("Proportion (binomial)", "Mean (t)", "Poisson (exact)"),
              selected= "Mean (t)", multiple=FALSE)
})

#Select the confidence interval level
output$Ci_Conf_Lev <- renderUI({                                 
  numericInput("ciconf_lev", "4. Enter the confidence level.",
               value = .95, min=.01, max = .99, step = .01)
})
#Add a target line
output$Ci_Tgt_Line <- renderUI({                                 
  numericInput("ciTgtLn", "5. Add a target line.",
               value = NULL, step = .1)
})
#Reactive function for directly above
ci_target_line <- reactive({                 
  input$ciTgtLn 
})
#Select the sorting order.
output$Ci_Alpha_Num <- renderUI({                                #Creates a UI function here but it will
  radioButtons("alpha_num", "9. Sort by factor name or numerical value?",
               choices = c("Alphabetical", "Numerical"),
               selected="Alphabetical")
})

#Confidence interval plot
output$Plot_Ci_output <- renderPlot({ 
  if(input$CiCreate == "Yes") {
  plot_ci()
  }
  }, height = 700)

#Confidence interval values
output$Cidf_output <- renderPrint({ 
  #cidf2()
  if(input$CiCreate == "Yes") {
    list("Alphabetical"=cidf()[["adf_alpha"]][nrow(cidf()[["adf_alpha"]]):1,], 
         "Numerical"=cidf()[["adf_numeric"]][nrow(cidf()[["adf_numeric"]]):1,],
         "Group.Tests"= conf_group_test(),
         "Pairwise.Comparisons"= conf_post_hoc())
  }
})

#Select whether to run the 95% confidence interval or not
output$Ci_create <- renderUI({                                #Creates a UI function here but it will
  radioButtons("CiCreate", "6. Create confidence intervals?",
               choices = c("No", "Yes"),
               selected="No")
})
#Select line colors
output$ci_plot_ln_clrs <- renderUI({                                 
  selectInput("ciPltLnClr", "7. Select line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "blue")     
})
#Reactive function for directly above
ci_plot_Line_Colors <- reactive({                 
  input$ciPltLnClr 
})
#Select point colors
output$ci_plot_pt_clrs <- renderUI({                                 
  selectInput("ciPltPtClr", "8. Select point color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="gold")     
})
#Reactive function for directly above
ci_plot_Point_Colors <- reactive({                 
  input$ciPltPtClr 
})

## Reactive function to do group tests ##
conf_group_test <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
    fncCnfGrpTst(X=input$xcivar, Y=input$ycivar, DF=df(), ci_type= input$ci_type)
  }
})

## Reactive function to do pairwise tests ##
conf_post_hoc <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
    fncCnfPstHc(X=input$xcivar, Y=input$ycivar, DF=df(), ci_type= input$ci_type)
  }
})

#######################################
# Graphs for trajectories by time     #
#######################################
#Binomial
fbconf <- function(x, xlev, y, z, dataf, conf_lev, Increment) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  #Calculates confidence intervals for single units or in increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
  } else {
    agr_sum <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="length")
  }
  agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  agr_df <- cbind(agr_df, binconf(x=agr_df[,3], n=agr_df[,4], alpha=1 - conf_lev))
  rownames(agr_df) <- 1:nrow(agr_df)
  return(agr_df) 
}

#Continuous outcomes
ftconf <- function(x, xlev, y, z, dataf, conf_lev, Increment) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_m <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
    agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])) , agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
  } else {
    agr_m <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="length")
    agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])), agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
  }
  #Calculates confidence intervals
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df ) 
}

#Exact Poisson
fpconf <- function(x, xlev, y, z, dataf, conf_lev, Increment) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  } else {
    agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ ceiling(dataf[, z]/Increment), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ ceiling(dataf[, z]/Increment), FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  }
  #Calculates confidence intervals
  adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
  for (i in 1:nrow(agr_df)) {
    adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,3], T=agr_df[i,4], conf.level= conf_lev)[c("estimate","conf.int")])
  }
  adf_alpha <- data.frame(adf_alpha)
  colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df=agr_df ) 
}

fconf <- function(x=xcivar, xlev=xlev, y=ycivar, z=zcivar, dataf, conf_lev=ciconf_lev, Increment, Fci_type) {
  switch(Fci_type,                #"var" and can be used anywhere in server.r.
         "Mean (t)" =  ftconf(x, xlev, y, z, dataf, conf_lev, Increment), 
         "Proportion (binomial)" =  fbconf(x, xlev, y, z, dataf, conf_lev, Increment), 
         "Poisson (exact)" =  fpconf(x, xlev, y, z, dataf, conf_lev, Increment) 
  )
}

#Reactive function that runs fconf above
fcidf <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    fconf(x=input$fxcivar, xlev=fci_plot_Group_Levels(), y=input$fycivar, z=input$fzcivar, 
          dataf=df(), conf_lev=input$fciconf_lev, Increment=fci_Z_Increment(), Fci_type=input$fci_type)
  }
})

################################################################################
#      Function to get overall trend confidence intervals, no grouping         # 
################################################################################
#Binomial
ftotBconf <- function(y, z, dataf, conf_lev, Increment) {
  #Calculates confidence intervals for single units or in increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y], list(dataf[, z]), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, z]), FUN="length")
  } else {
    agr_sum <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="length")
  }
  agr_df <- data.frame(z_lev=as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  agr_df <- cbind(agr_df, binconf(x=agr_df[,2], n=agr_df[,3], alpha=1 - conf_lev))
  rownames(agr_df) <- 1:nrow(agr_df)
  return(agr_df) 
}

#Continuous outcomes
ftotTconf <- function(y, z, dataf, conf_lev, Increment) {
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_m <- aggregate(dataf[, y], list( dataf[, z]), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(dataf[, z]), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, z]), FUN="length")
    agr_df <- data.frame(z_lev=as.integer(c(agr_m[, 1])), agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
  } else {
    agr_m <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="length")
    agr_df <- data.frame(z_lev= as.integer(c(agr_m[, 1])), agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
  }
  #Calculates confidence intervals
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df ) 
}

#Exact Poisson
ftotPconf <- function(y, z, dataf, conf_lev, Increment) {
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y] ~ dataf[, z], FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ dataf[, z], FUN="length")
    agr_df <- data.frame(z_lev= as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  } else {
    agr_sum <- aggregate(dataf[, y] ~ ceiling(dataf[, z]/Increment), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ ceiling(dataf[, z]/Increment), FUN="length")
    agr_df <- data.frame(z_lev= as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  }
  #Calculates confidence intervals
  adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
  for (i in 1:nrow(agr_df)) {
    adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i, 3], conf.level= conf_lev)[c("estimate","conf.int")])
  }
  adf_alpha <- data.frame(adf_alpha)
  colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df=agr_df ) 
}

ftotconf <- function(y=ycivar, z=zcivar, dataf, conf_lev=ciconf_lev, Increment, Fci_type) {
  switch(Fci_type,                #"var" and can be used anywhere in server.r.
         "Mean (t)" =  ftotTconf(y, z, dataf, conf_lev, Increment), 
         "Proportion (binomial)" =  ftotBconf(y, z, dataf, conf_lev, Increment), 
         "Poisson (exact)" =  ftotPconf(y, z, dataf, conf_lev, Increment) 
  )
}

#Reactive function that runs ftotconf above
ftotCidf <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    ftotconf(y=input$fycivar, z=input$fzcivar, dataf=df(), conf_lev=input$fciconf_lev, 
             Increment=fci_Z_Increment(), Fci_type=input$fci_type)
  }
})

###############################################################
## Function here for the point estimate, lower, upper bounds ##
###############################################################
ci_fac_fnc <- function(x_lev, z_lev, agr_df, NK, Straight.Line) {
  #ci_fac_fnc <- function(x_lev, z_lev, agr_df) {
  prmtrs <- c("PointEst", "Lower", "Upper")
  ctrs <- as.vector(unique(agr_df[, x_lev]))
  #Point est 
  ci_p <- list()
  if(Straight.Line== "No") {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev] ==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev] ==ctrs[i], prmtrs[1]]
      xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
      knots <- attr(xx, "knots")
      coef <- lsfit(xx, y)$coef
      w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
      xtrans <- eval(attr(w, "function"))
      ci_p[[i]] <- cbind(x, y_p=coef[1] + xtrans(x), y)
    } 
  } else {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev] ==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev] ==ctrs[i], prmtrs[1]]
      ci_p[[i]] <- cbind(x, y_p=y, y)
    }
  }
  #Lower CI
  ci_l <- list()
  if(Straight.Line== "No") {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[2]]
      xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
      knots <- attr(xx, "knots")
      coef <- lsfit(xx, y)$coef
      w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
      xtrans <- eval(attr(w, "function"))
      ci_l[[i]] <- cbind(x, y_l=coef[1] + xtrans(x), y)
    }
  } else {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[2]]
      ci_l[[i]] <- cbind(x, y_l=y, y)
    }
  }
  #Upper CI
  ci_u <- list()
  if(Straight.Line== "No") {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[3]]
      xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
      knots <- attr(xx, "knots")
      coef <- lsfit(xx, y)$coef
      w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
      xtrans <- eval(attr(w, "function"))
      ci_u[[i]] <- cbind(x, y_u=coef[1] + xtrans(x), y)
    }
  } else {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[3]]
      ci_u[[i]] <- cbind(x, y_u=y, y)
    }
  }
  #For point estimtaes only
  max_pest <- vector() 
  min_pest <- vector() 
  for(i in 1:length(ci_p)) {
    max_pest[i] <- max(ci_p[[i]][,2])
    min_pest[i] <- min(ci_p[[i]][,2])
  }
  #For CIs only
  max_ci <- vector() 
  min_ci <- vector() 
  for(i in 1:length(ci_p)) {
    max_ci[i] <- max(ci_u[[i]][,2])
    min_ci[i] <- min(ci_l[[i]][,2])
  }
  return(list(ci_p=ci_p, ci_l=ci_l, ci_u=ci_u, max_pest=max_pest, min_pest=min_pest,
              max_ci=max_ci, min_ci=min_ci, ctrs=ctrs))
}

#Reactive function that runs ci_fac_fnc() above
fci_fac <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    ci_fac_fnc(x_lev="x_lev", z_lev="z_lev", agr_df=fcidf(), NK=input$FciNkKnots, Straight.Line= fCi_straight_line())
  }
})


############################################################
## Function for total point estimate, lower, upper bounds ##
############################################################ 
ci_tot_fac_fnc <- function(z_lev, agr_df, NK, Straight.Line) {
  #Parameters
  prmtrs <- c("PointEst", "Lower", "Upper")
  #Point est 
  ci_p <- list()
  if(Straight.Line== "No") {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[1]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_p <- cbind(x, y_p=coef[1] + xtrans(x), y)
  } else {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[1]]
    ci_p <- cbind(x, y_p=y, y)
  }
  #Lower CI
  ci_l <- list()
  if(Straight.Line== "No") {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[2]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_l <- cbind(x, y_l=coef[1] + xtrans(x), y)
  } else {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[2]]
    ci_l <- cbind(x, y_l=y, y)
  }
  #Upper CI
  ci_u <- list()
  if(Straight.Line== "No") {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[3]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_u <- cbind(x, y_u=coef[1] + xtrans(x), y)
  } else {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[3]]
    ci_u <- cbind(x, y_u=y, y)
  }
  return(list(ci_p=ci_p, ci_l=ci_l, ci_u=ci_u #, 
              #max_pest=max_pest, min_pest=min_pest, max_ci=max_ci, min_ci=min_ci
  ))
}

#Reactive function that runs ci_fac_fnc() above
fci_tot_fac <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    ci_tot_fac_fnc(z_lev="z_lev", agr_df=ftotCidf(), NK=input$FciNkKnots, 
                   Straight.Line= fCi_straight_line())
  }
})


############################################################
##            Function to create the time plot            ##
############################################################
plot_fci_fnc <- function(x, y, z, xcivar, ycivar, zcivar, dataf, LCol, LWd, Fci.Fac, 
                         ci_p, ci_l, ci_u,max_pest, min_pest, max_ci, min_ci, ctrs, 
                         cibands, fCiXLim1, fCiXLim2, fCiYLim1, fCiYLim2, Tot.Line, FCI.Tot,
                         FCI.Tot.Straight, Conf.Intrv, Tgt.Line, Straight.Line, Time.Pt.Line,
                         ci_p_tot, ci_l_tot, ci_u_tot, Tot.Color, Tgt.Color, Tpt.Color, T3.Line.Width, Text.Size) {
  #Make text out of the confidence level
  ConINT <- paste0(as.character(Conf.Intrv*100), "%")
  #Main title
  if(cibands == "Yes") {
    Main.Title <- paste0( ycivar, " trajectories of ", xcivar,  " by ", zcivar, " with ", ConINT, " confidence bands")
  } else {
    Main.Title <- paste0( ycivar, " trajectories of ", xcivar,  " by ", zcivar)
  }
  
  #Set up colors
  my_clr <- LCol
  plot(unique(dataf[, z]), seq(min(min_ci, na.rm=T), max(max_ci, na.rm=T), length.out=length(unique(dataf[, z]))), type="n",  
       cex.lab=1.35,cex.main=1.35,cex.sub=1.35, 
       ylab=ycivar, xlab=zcivar, xlim=c(fCiXLim1, fCiXLim2), ylim=c(fCiYLim1, fCiYLim2),
       main= Main.Title )
  #Plot point estimate lines
  ci_time <- list() 
  l95 <- list() 
  u95 <- list() 
  xx_t <- list() 
  yy_t <- list() 
  #Confidence bands
  if(cibands == "Yes") {
    if(Straight.Line == "Yes") {
      for (i in 1:length(ctrs)) {
        ci_time[[i]] <- ci_l[[i]][,1]
        l95[[i]] <- ci_l[[i]][, "y"]
        u95[[i]] <- ci_u[[i]][, "y"]
        xx_t[[i]] <- c(ci_time[[i]], rev(ci_time[[i]]))
        yy_t[[i]] <- c(l95[[i]], rev(u95[[i]]))
        polygon(unlist(xx_t[[i]]), unlist(yy_t[[i]]), col = adjustcolor(my_clr[i], alpha.f = 0.4), 
                border=adjustcolor(my_clr[i], alpha.f = 0.4))
      }   
    } else {
      for (i in 1:length(ctrs)) {
        ci_time[[i]] <- ci_l[[i]][,1]
        l95[[i]] <- ci_l[[i]][, 2] #"y_p"
        u95[[i]] <- ci_u[[i]][, 2] #"y_p"
        xx_t[[i]] <- c(ci_time[[i]], rev(ci_time[[i]]))
        yy_t[[i]] <- c(l95[[i]], rev(u95[[i]]))
        polygon(unlist(xx_t[[i]]), unlist(yy_t[[i]]), col = adjustcolor(my_clr[i], alpha.f = 0.4), 
                border=adjustcolor(my_clr[i], alpha.f = 0.4))
      }
    } 
  }
  #Add text names
  if(Straight.Line == "Yes") {
    for (i in 1:length(ctrs)) {
      lines(ci_p[[i]][, "x"], ci_p[[i]][, "y"], lty=i, col= my_clr[i], lwd=LWd)
      text(ci_p[[i]][1, "x"], ci_p[[i]][1, "y"], ctrs[i], cex= Text.Size)
      text(ci_p[[i]][nrow(ci_p[[i]]), "x"], ci_p[[i]][nrow(ci_p[[i]]), "y"], ctrs[i], cex= Text.Size)
    }
  } else {
    for (i in 1:length(ctrs)) {
      lines(ci_p[[i]][, "x"], ci_p[[i]][, "y_p"], lty=i, col= my_clr[i], lwd=LWd)
      text(ci_p[[i]][1, "x"], ci_p[[i]][1, "y_p"], ctrs[i], cex= Text.Size)
      text(ci_p[[i]][nrow(ci_p[[i]]), "x"], ci_p[[i]][nrow(ci_p[[i]]), "y_p"], ctrs[i], cex= Text.Size)
    }
  }
  
  #Plot point estimate lines for the overall trend
  ci_time_tot <- list() 
  l95_tot <- list() 
  u95_tot <- list() 
  xx_t_tot <- list() 
  yy_t_tot <- list() 
  #Confidence bands
  if (Tot.Line == "Line with band") {
    if(Straight.Line == "Yes") {
      ci_time_tot <- ci_l_tot[,1]
      l95_tot <- ci_l_tot[, "y"]
      u95_tot <- ci_u_tot[, "y"]
      xx_t_tot <- c(ci_time_tot, rev(ci_time_tot))
      yy_t_tot <- c(l95_tot, rev(u95_tot))
      polygon(unlist(xx_t_tot), unlist(yy_t_tot), col = adjustcolor(Tot.Color, alpha.f = 0.4), 
              border=adjustcolor(Tot.Color, alpha.f = 0.4))
    } else {
      ci_time_tot <- ci_l_tot[,1]
      l95_tot <- ci_l_tot[, 2] #"y_p"
      u95_tot <- ci_u_tot[, 2] #"y_p"
      xx_t_tot <- c(ci_time_tot, rev(ci_time_tot))
      yy_t_tot <- c(l95_tot, rev(u95_tot))
      polygon(unlist(xx_t_tot), unlist(yy_t_tot), col = adjustcolor(Tot.Color, alpha.f = 0.4), 
              border=adjustcolor(Tot.Color, alpha.f = 0.4))
    } 
  }
  #Add overall lines
  if (Tot.Line %in% c("Line", "Line with band") ) {
    if(Straight.Line == "Yes") {
      lines(ci_p_tot[, "x"], ci_p_tot[, "y"], lty=1, col= Tot.Color, lwd=T3.Line.Width)
      text(ci_p_tot[1, "x"], ci_p_tot[1, "y"], labels= "ALL", cex= Text.Size)
      text(ci_p_tot[nrow(ci_p_tot), "x"], ci_p_tot[nrow(ci_p_tot), "y"], labels= "ALL", cex= Text.Size)
    } else {
      lines(ci_p_tot[, "x"], ci_p_tot[, "y_p"], lty=1, col= Tot.Color, lwd=T3.Line.Width)
      text(ci_p_tot[1, "x"], ci_p_tot[1, "y_p"], labels= "ALL", cex= Text.Size)
      text(ci_p_tot[nrow(ci_p_tot), "x"], ci_p_tot[nrow(ci_p_tot), "y_p"], labels= "ALL", cex= Text.Size)
    }
  }
  
  #Add target line
  for (i in 1:length(Tgt.Line)) {
    abline(h= as.numeric(eval(parse(text=Tgt.Line[i] )) ), 
           col=Tgt.Color, lty=3, lwd=T3.Line.Width)
  }
  #Add time point line
  for (i in 1:length(Time.Pt.Line)) {
    abline(v= as.numeric(eval(parse(text=Time.Pt.Line[i] )) ), 
           col=Tpt.Color, lty=1, lwd=T3.Line.Width)
  }
  
}

#Confidence interval plot reactive function
plot_fci <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    plot_fci_fnc(x="x_lev", y="PointEst", z="z_lev", xcivar=input$fxcivar, ycivar=input$fycivar, zcivar=input$fzcivar,
                 dataf=fcidf(), LCol= fci_plot_Line_Colors(), LWd=fci_plot_Line_Width(), ci_p=fci_fac()$ci_p, 
                 ci_l=fci_fac()$ci_l, ci_u=fci_fac()$ci_u,
                 max_pest=fci_fac()$max_pest, min_pest=fci_fac()$min_pest, max_ci=fci_fac()$max_ci, min_ci=fci_fac()$min_ci, 
                 ctrs=fci_fac()$ctrs, cibands=input$fcibands, fCiXLim1=input$fCiXLim1, fCiXLim2=input$fCiXLim2, 
                 fCiYLim1=input$fCiYLim1, fCiYLim2=input$fCiYLim2, Tot.Line=fci_overall_line(), FCI.Tot=fci_all_line(), 
                 FCI.Tot.Straight=fci_tot_group_aggr(), Conf.Intrv=input$fciconf_lev, 
                 Tgt.Line=fCi_target_line(), Straight.Line= fCi_straight_line(), Time.Pt.Line= fCi_time_point_line(), 
                 ci_p_tot=fci_tot_fac()$ci_p, ci_l_tot=fci_tot_fac()$ci_l, ci_u_tot=fci_tot_fac()$ci_u,
                 Tot.Color=fci_plot_Overall_Line_Colors(), Tgt.Color=fci_plot_Target_Line_Colors(), 
                 Tpt.Color=fci_plot_Time_Point_Line_Colors(), T3.Line.Width=fci_plot_targ_time_Line_Wd(), 
                 Text.Size= fci_plot_text_label_size())
  }
})

## Get the overall group trend rates ##
fci_tot_group_aggr <- reactive({
  fncFciTotMn(y=input$fycivar, z=input$fzcivar, dataf=df(), Increment= fci_Z_Increment())
})
## Get the overall group trend line ##
fci_all_line <- reactive({
  fncAllTrndSpln(agr_df=fci_tot_group_aggr(), NK=input$FciNkKnots, Straight.Line= fCi_straight_line())
})

############
##   UI   ##
############
#Select the outcome
output$FCIy <- renderUI({                                #Creates a UI function here but it will
  selectInput("fycivar", "1. Select the outcome.",       #get called to the UI file.
              choices = var(), multiple=FALSE, selected=var()[1] )   #Will make choices based on my reactive function.
})

#Select the predictors.
output$FCIx <- renderUI({                                 #Same idea as output$vy
  selectInput("fxcivar", "2. Select the factor.", 
              choices = setdiff(var(), input$fycivar), multiple=FALSE, selected=var()[2])     #Will make choices based on my reactive function.
})

#Select the predictors.
output$FCIz <- renderUI({                                 #Same idea as output$vy
  selectInput("fzcivar", "3. Select a time variable.", 
              choices = setdiff(var(), c(input$fycivar, input$fxcivar)), multiple=FALSE, selected=var()[2])     #Will make choices based on my reactive function.
})
#4. Select the rolling time period 
output$FCIzInc <- renderUI({                                 
  numericInput("fciZinc", "4. Select time increments (3= 3 months).", 
               value = 1, step = 1, min=1)     
})
fci_Z_Increment <- reactive({                 
  input$fciZinc 
})
#Select specific groups
output$fciplot_grp_levs <- renderUI({                                 
  selectInput("fciPlotGrpLvs", "5. Highlight specific groups?", 
              choices = sort(fci_plot_groups()), multiple=TRUE)     
})
#Reactive function to get group levels
fci_plot_Group_Levels <- reactive({                 
  input$fciPlotGrpLvs 
})
#Reactive function to get group levels
fci_plot_groups <- reactive({                 
  unique(df()[, input$fxcivar]) 
})
#Select the CI type
output$FCi_Choice_Type <- renderUI({                                
  selectInput("fci_type", "6. Select the type of confidence interval.",
              choices = c("Proportion (binomial)", "Mean (t)", "Poisson (exact)"),
              selected= "Mean (t)", multiple=FALSE)
})

#Select the confidence interval level
output$FCi_Conf_Lev <- renderUI({                                 
  numericInput("fciconf_lev", "7. Enter the confidence level.",
               value = .95, min=.01, max = .99, step = .01)
})
#Select the Confidence bands.
output$FCI_bands <- renderUI({                                 #Same idea as output$vy
  selectInput("fcibands", "8. Do you want confidence bands?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
#Select line colors
output$fci_plot_ln_clrs <- renderUI({                                 
  selectInput("fciPltLnClr", "9. Select line colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE, 
              selected= xyplot_Line_Color_Names()[1:length(fci_plot_groups())] )     
})
#Reactive function for directly above
fci_plot_Line_Colors <- reactive({                 
  input$fciPltLnClr 
})

#Select line width
output$fci_plot_ln_wdth <- renderUI({                                 
  numericInput("fciPltLnWd", "10. Select the group line width.", 
               value = 2, min=0, step = 1)     
})
#Reactive function for directly above
fci_plot_Line_Width <- reactive({                 
  input$fciPltLnWd 
})

#Select how many knots I want
output$FCI_nk_knots <- renderUI({                                
  numericInput("FciNkKnots", "11. Select the number of spline knots.",
               value = 3, min=3, max = 10, step = 1)
})
#Select whether to run the 95% confidence interval or not
output$FCi_create <- renderUI({                                
  selectInput("FCiCreate", "20. Create the time plot?",
              choices = c("No", "Yes"),
              selected="No")
})
#Select whether to run the 95% confidence interval or not
output$FCi_ovral_line <- renderUI({
  selectInput("fciOvrLn", "12. Add the overall group line?",
              #              choices = c("No", "Yes"),
              choices = c("No", "Line", "Line with band"),
              selected="No")
})
#Reactive function for above
fci_overall_line <- reactive({ 
  input$fciOvrLn  
})
#Add a target line
output$FCi_Tgt_Line <- renderUI({                                 
  textInput("fciTgtLn", "13. Add a target line.",
            value = paste0('c( ', ')') )
})
#Reactive function for above
fCi_target_line <- reactive({ 
  input$fciTgtLn  
})
#Add a time point line
output$FCi_Tm_Pt_Line <- renderUI({                                 
  textInput("fciTmPtLn", "14. Add a time point line.",
            value = paste0('c( ', ')'))
})

#Reactive function for above
fCi_time_point_line <- reactive({ 
  input$fciTmPtLn  
})

## Code for plot range
#Range of X value
range_fzcivar <- reactive({ 
  range(as.numeric(df()[, input$fzcivar]), na.rm=TRUE )  
})
#Range of Y value
range_fycivar <- reactive({ 
  range(as.numeric(df()[, input$fycivar]), na.rm=TRUE )  
})

#13. Indicate if you want a straight line
output$FCi_strght_ln <- renderUI({                                
  selectInput("fciStrtLn", "15. Use straight trend lines?",
              choices = c("No", "Yes"),
              selected="No")
})
#13A. Reactive function for above
fCi_straight_line <- reactive({ 
  input$fciStrtLn  
})
#Select target and time line width
output$fci_plot_TgtTpt_ln_wdth <- renderUI({                                 
  numericInput("fciPlTgTpLnWd", "16. Select other line's width.", 
               value = 2, min=0, step = 1)     
})
#Reactive function for directly above
fci_plot_targ_time_Line_Wd <- reactive({                 
  input$fciPlTgTpLnWd 
})

#17. Select overall line color
output$fci_plot_ovral_ln_clrs <- renderUI({                                 
  selectInput("fciPltOLnClr", "17. Select 'overall' line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="black")     
})
#Reactive function for directly above
fci_plot_Overall_Line_Colors <- reactive({                 
  input$fciPltOLnClr 
})
#18. Select target line color
output$fci_plot_tgt_ln_clrs <- renderUI({                                 
  selectInput("fciPltTLnClr", "18. Select target line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="black")     
})
#Reactive function for directly above
fci_plot_Target_Line_Colors <- reactive({                 
  input$fciPltTLnClr 
})
#19. Select time point line color
output$fci_plot_time_pt_ln_clrs <- renderUI({                                 
  selectInput("fciPltTPLnClr", "19. Select time point line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="black")     
})
#Reactive function for directly above
fci_plot_Time_Point_Line_Colors <- reactive({                 
  input$fciPltTPLnClr 
})
#21. Select the line label size
output$fci_plot_txt_lbl_sz <- renderUI({                                 
  numericInput("fciPlTxtLblSz", "21. Select the line label size.", 
               value = 2, min=0, step = .1)     
})
#Reactive function for directly above
fci_plot_text_label_size <- reactive({                 
  input$fciPlTxtLblSz 
})
#14. Indicate lower limit of x-axis
output$FCI__Xlim1 <- renderUI({
  numericInput("fCiXLim1", "22. Lower X-axis limit.",
               value = range_fzcivar()[1], step = 1)
})
#15. Indicate upper limit of x-axis
output$FCI__Xlim2 <- renderUI({
  numericInput("fCiXLim2", "23. Upper X-axis limit.",
               value = if(fci_Z_Increment() ==1) { range_fzcivar()[2] } else {ceiling(range_fzcivar()[2]/fci_Z_Increment() ) } , 
               step = 1)
})
#16. Indicate lower limit of y-axis
output$FCI__Ylim1 <- renderUI({
  numericInput("fCiYLim1", "24. Lower Y-axis limit.",
               value = range_fycivar()[1], step = .1)
})
#17. Indicate upper limit of x-axis
output$FCI__Ylim2 <- renderUI({
  numericInput("fCiYLim2", "25. Upper Y-axis limit.",
               value = range_fycivar()[2], step = .1)
})
#Confidence interval plot for time
output$Plot_Fci_output <- renderPlot({ 
  if(input$FCiCreate == "Yes") {
    plot_fci()
  }
}, height = 800 )

#This prints the point estimates and confidence intervals
output$time_ci_out1 <- renderTable({
  if(input$FCiCreate == "Yes") {
    fcidf()
  }
}, rownames = TRUE, digits =3)
#This prints the point estimates and confidence intervals
output$all_time_ci_out1 <- renderTable({
  if(input$FCiCreate == "Yes") {
    ftotCidf()
  }
}, rownames = TRUE, digits =3)


############



############################
#       Describe           #
############################
#Render UIs to get variables
#Summary plot
#Select the outcome
output$desc_y <- renderUI({                                
  selectInput("describeY", "1. Select the target variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#Select the predictors.
output$desc_x <- renderUI({                                 #Same idea as output$vy
  selectInput("describeX", "2. Select the predictors", 
              choices = setdiff(var(), desc_outcome()), multiple=TRUE, selected=var()[2]) 
})
output$desc_choice <- renderUI({  
  selectInput("DescChoice", "3. Run the summary now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
## Reactive functions ##
#Formula for describe section
desc_outcome <- reactive({                  #Outcome is my reactive function name I will use below. 
  input$describeY                      #variableY comes from the UI file drop down box.
})

desc_predictor <- reactive({             #Same idea as "outcome" 
  input$describeX  
})
desc_fmla <- reactive({             #Spline terms 
  as.formula(paste(paste0(desc_outcome() , "~"),   
                   paste(desc_predictor(), collapse= "+")))
})
#Summary plot for outcome by predictors
desc_smry <- reactive({             #Spline terms 
  if (input$DescChoice == "Yes") {
    summary(desc_fmla(), data=df())
  }
})
#This renders the summary plot
output$DescSmryPlt <- renderPlot({ 
  par(mar=c(2, 6, 1.5, 6))
  if (input$DescChoice == "Yes") {
    plot(desc_smry(), 
         cex.lab=.95, cex=1.25, col=2
         )
  }
#}, height = 700, width = 1000  )
}, height = 800, width = 1200  )

##########################################################################
##      Scatter plot with a correlation test and loess smoothing        ##
##########################################################################

#1. Y variable "Select the response variable"
output$sctr_crtst_y <- renderUI({                                
  selectInput("sctrCrtstY", "1. Select the Y-axis variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
scatter_cor_test_y <- reactive({
  input$sctrCrtstY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$sctr_crtst_x <- renderUI({                                 #Same idea as output$vy
  selectInput("sctrCrtstX", "2. Select the X-axis variable", 
              choices = setdiff(var(), scatter_cor_test_y()), multiple=FALSE, selected=var()[2]) 
})
#2A. Reactive function for the X variable
scatter_cor_test_x <- reactive({
  input$sctrCrtstX
})
#3. correlation method
output$sctr_crtst_meth <- renderUI({                                
  selectInput("sctrCrtstMth", "3. Select the correlation method.",        
              choices = c("pearson", "kendall", "spearman"), multiple=FALSE, selected="pearson" ) 
})
#3A. Reactive function for the correlation method
scatter_cor_test_method <- reactive({
  input$sctrCrtstMth
})
#4. Alternative hypothesis test
output$sctr_crtst_alt <- renderUI({                                
  selectInput("sctrCrtstAlt", "4. Select a one- or two-sided test.",        
              choices = c("two.sided", "less", "greater"), multiple=FALSE, selected="two.sided" ) 
})
#4A. Reactive function for alternative hypothesis test
scatter_cor_test_alternative <- reactive({
  input$sctrCrtstAlt
})
#5. 
output$scatter_cor_test_CI <- renderUI({                                #
  numericInput("sctrCrtstCI", "5. Select the confidence interval level.",       #
               value = 0.95, min=0, max=1, step=.01 )   #
})
#5A. Reactive function for confidence interval
Scatter_Cor_Test_Conf_Int <- reactive({
  input$sctrCrtstCI
})
#6. Exact method
output$scatter_cor_test_exct <- renderUI({  
  textInput("sctrCrtstEM", "6. Use the exact method?", 
  #            choices = c("",TRUE, FALSE), multiple=FALSE, selected="")     
  value = NULL)
})
#6A. Reactive function for Exact method
Scatter_Cor_Test_Exact_Method <- reactive({
  input$sctrCrtstEM
})
#7. Exact method
output$scatter_cor_test_cnt <- renderUI({  
  selectInput("sctrCrtstCC", "7. Use the continuity correction?", 
              choices = c(TRUE, FALSE), multiple=FALSE, selected=FALSE)     
})
#7A. Reactive function for Exact method
Scatter_Cor_Test_Cont_Correct <- reactive({
  input$sctrCrtstCC
})
#8. Line color
output$sctr_crtst_clr <- renderUI({                                
  selectInput("sctrCrtstClr", "8. Select the plot's line color.",        
              choices = xyplot_Line_Color_Names(), 
              multiple=FALSE, selected="red" ) 
})
#8A. Reactive function for alternative hypothesis test
scatter_cor_line_color <- reactive({
  input$sctrCrtstClr
})
#9. Exact method
output$scatter_cor_test_run_YN <- renderUI({  
  selectInput("sctrCrtstYN", "9. Run the correlation and plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#9A. Reactive function for Exact method
Scatter_Cor_Test_Run_Yes_No <- reactive({
  input$sctrCrtstYN
})
#10. Run the function below
scatter_cor_test_cor_run <- reactive({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {    
    fncSctrPltCr(DF=df(), X=scatter_cor_test_x(), Y=scatter_cor_test_y(), 
                 sct_plt_alt=scatter_cor_test_alternative(), 
                 sct_plt_meth=scatter_cor_test_method() , 
                 sct_plt_exct= eval(parse(text=Scatter_Cor_Test_Exact_Method() )) , 
                 
                 #sct_plt_exct=Scatter_Cor_Test_Exact_Method(), 
                 sct_plt_ci_lv=Scatter_Cor_Test_Conf_Int(), 
                 sct_plt_cont=Scatter_Cor_Test_Cont_Correct() 
    )
  }  
})
#10A.Correlation test output  
output$scatter_cor_test_cor_test_out <- renderPrint({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {
    scatter_cor_test_cor_run()
  }
})
#11. Run the function below
scatter_cor_test_plt_run <- reactive({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {    
    fncSctrPlt(DF=df(), X=scatter_cor_test_x(), Y=scatter_cor_test_y(), 
                 sct_plt_clr=scatter_cor_line_color(), CT=scatter_cor_test_cor_run()
    )
  }  
})
#11A.Scatter plot  
output$scatter_cor_test_plt_out <- renderPlot({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {
    scatter_cor_test_plt_run()
  }
})

######################################
## Function to get correlation test ##
######################################
fncSctrPltCr <- function(DF, X, Y, 
                         sct_plt_alt, 
                         sct_plt_meth , 
                         sct_plt_exct, 
                         sct_plt_ci_lv, 
                         sct_plt_cont
                         ) {
  #Correlation test
  CT <- cor.test(x=DF[, X], y=DF[, Y], alternative = sct_plt_alt,
                 method = sct_plt_meth, 
                 exact = sct_plt_exct, 
                 conf.level = sct_plt_ci_lv, 
                 continuity = sct_plt_cont, 
                 na.rm=TRUE)
  return("Correlation.Test"= CT)
}
##################################
## Function to get scatter plot ##
##################################
fncSctrPlt <- function(DF, X, Y, sct_plt_clr, CT) {
  #Scatter plot
  scatter.smooth(DF[, X], DF[, Y] , main= paste0("Correlation of ", Y, " on ", X, 
                                                 " (correlation= ", round(as.numeric(CT["estimate"]), 3), 
                                                 ", ", "p-value= ", try(round(as.numeric(CT["p.value"]), 4)), ")"),
                 xlab=X, ylab=Y,
                 lpars =list(col = "red", lwd = 3, lty = 3))
}

##########################################################################
## summaryRc plot of continuous Y by continuous X with a stratification ##
##########################################################################

#1. Y variable "Select the response variable"
output$smryRc_y <- renderUI({                                
  selectInput("smryrcY", "1. Select the continuous outcome variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
smryRc_outcome <- reactive({
  input$smryrcY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$smryRc_x <- renderUI({                                 #Same idea as output$vy
  selectInput("smryrcX", "2. Select the continuous predictor", 
              choices = setdiff(var(), smryRc_outcome()), multiple=FALSE, selected=var()[2]) 
})
#2A. Reactive function for the X variable
smryRc_X_var <- reactive({
  input$smryrcX
})
# stratify  
#3. Do you want to stratify by a factor
output$smryRc_strat_yes_no <- renderUI({  
  selectInput("smryRcStratYN", "3. Do you want to stratify?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#4. Select a stratification variable
output$smryRc_z <- renderUI({                                 #Same idea as output$vy
  selectInput("smryrcZ", "4. Select the stratification variable", 
              choices = setdiff(var(), c(smryRc_outcome(),smryRc_X_var )), multiple=FALSE, 
              selected=setdiff(var(), c(smryRc_outcome(),smryRc_X_var() ))[1]) 
})
#4A. Reactive function for the stratification variable
smryRc_Z_var <- reactive({
  input$smryrcZ
})
#5. Run the graph
output$smryrc_choice <- renderUI({  
  selectInput("smryRcChoice", "5. Run the summary plot now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#6. Run the function below
summaryRC_plot_function_run <- reactive({
  if(input$smryRcChoice == "Yes") {
    if(input$smryRcStratYN == "Yes") {    
    fncSumRcPlot(X=smryRc_X_var(), Y=smryRc_outcome(), Z=smryRc_Z_var(), DF=df())
  }  else {
    fncSumRcPlot(X=smryRc_X_var(), Y=smryRc_outcome(), Z=NULL, DF=df())
  }
} 
})
#6A. Summary plot output 
output$summaryRC_plot_function_out <- renderPlot({
  if(input$smryRcChoice == "Yes") {
    summaryRC_plot_function_run()
  }
})
## Function that creates summaryRc plot ##
fncSumRcPlot <- function(X, Y, Z=NULL, DF) {
  Vnms <- X
  #Create formula
  if(is.null(Z)) {
    fmla <- as.formula(paste(paste0(Y , "~"),   
                             paste(Vnms, collapse= "+") ))
  } else {
    fmla <- as.formula(paste(paste0(Y , "~"),   
                             paste(Vnms, collapse= "+"), "+ stratify(", Z,")" ))
  }
  #Create summary plot
  if(is.null(Z)) {
    summaryRc(fmla, data=DF, col=c(1,2), cex.quant=1.5, lwd=2, datadensity=TRUE, trim=.01,
              nloc=FALSE)
  } else {
    summaryRc(fmla, data=DF, col=c(1:length(levels(as.factor(DF[, Z] )))), cex.quant=1.5, lwd=2,
              datadensity=TRUE, trim=.01, label.curves=list(keys='lines'), nloc=FALSE)
  }
}


## Missing variable plots
#Select the outcome
output$miss_y <- renderUI({                                
  selectInput("missY", "1. Select the target variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#Select the predictors.
output$miss_x <- renderUI({                                 #Same idea as output$vy
  selectInput("missX", "2. Select the predictors", 
              choices = setdiff(var(), miss_outcome()), multiple=TRUE, selected=var()[2]) 
})
output$miss_choice <- renderUI({  
  selectInput("MissChoice", "3. Run the summary now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
## Reactive functions ##
#Formula for describe section
miss_outcome <- reactive({                  #Outcome is my reactive function name I will use below. 
  input$missY                      #variableY comes from the UI file drop down box.
})
#Predictors for missing section
miss_predictor <- reactive({ 
  input$missX  
})
#Formula for missing section
miss_fmla <- reactive({             #Spline terms 
  as.formula(paste(paste0("is.na(", miss_outcome(),")" , "~"),   
                   paste(miss_predictor(), collapse= "+")))
})
#Summary plot for outcome by predictors
miss_smry <- reactive({             #Spline terms 
  if (input$MissChoice == "Yes") {
    summary(miss_fmla(), data=df())
  }
})
#This renders the summary plot for missing values. Tried , pch=19 but didn't work
output$MissSmryPlt <- renderPlot({ 
  if (input$MissChoice == "Yes") {
    plot(miss_smry(), main="Proportion missing", col=2)
  }
}, height = 700, width = 1000  )
#
na.patterns <- reactive({             
  #naclus(df()[, c(miss_outcome(), miss_predictor())]) #Recusive and summary impacted, easier to use df() only
    naclus(df())
})
#This renders the wire plot for missing values. Tried , pch=19 but didn't work
output$naPlt <- renderPlot({ 
  if (input$MissChoice == "Yes") {
    naplot(na.patterns(), 'na per var', col=2)
  }
}, height = 700, width = 1000)

################################################################################
#                           CALCULATOR                                         #
################################################################################
#1. Textbox to enter a formula
output$calculator_box <- renderUI({  
  textInput("calcBox", "1. Enter a mathematical equation.")
})
#1A. Reactive function for textbox to enter a formula
Calculator_Box_Input <- reactive({
  input$calcBox
})                                                     
#2. Run the formula
output$calculator_yesno <- renderUI({                                 
  selectInput("calcYN", "2. Calculate results or create a graph?", 
              choices = c("No", "Results", "Graph"), multiple=FALSE, selected="No")  
})
#2A. Reactive function for textbox to enter a formula
Calculator_YN <- reactive({
  input$calcYN
})                                                     
#Print the calculation 
output$prnt_calculation <- renderPrint({
  if (Calculator_YN() == "Results") {
    eval(parse(text=Calculator_Box_Input() ))
  }
})
#Plot the calculation 
output$plt_calculation <- renderPlot({
  if (Calculator_YN() == "Graph") {
    eval(eval(parse(text=Calculator_Box_Input() )))
  }
})

################################################################################
#                           Summarize the data                                 #
################################################################################
#1. Select the variabes.
output$desc_summ_vars <- renderUI({
  selectInput("dscSmmVrs", "1. Select the variables",
              choices = var(), multiple=TRUE, 
              selected=var()[1])
})
#1A. Reactive function for textbox to enter a formula
descriptive_summary_variables <- reactive({
  input$dscSmmVrs
})                                                     
#2. Run the formula
output$des_summ_yesno <- renderUI({                                 
  selectInput("dscSmYN", "2. Do you want to summarize the data?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")  
})
#2A. Reactive function for textbox to enter a formula
Desc_Summary_YN <- reactive({
  input$dscSmYN
})                                                     
#3. Reactive function to get summary values
Desc_Summary_Central_Tendency <- reactive({
  summary(df()[, descriptive_summary_variables()])
})                                                     
#4. Reactive function to get summary values
Desc_Summary_SD <- reactive({
  apply(df()[, descriptive_summary_variables(), drop=FALSE], 2, sd, na.rm=TRUE)
})                                                     
#5. Reactive function for the coefficient of variation
Desc_Summary_COV <- reactive({
  #Desc_Summary_SD()/ colMeans(df()[, descriptive_summary_variables(), drop=FALSE], na.rm=TRUE)
  suppressWarnings(fncSmryCOV(DF=df(), X=descriptive_summary_variables(), 
                              SD= Desc_Summary_SD()))
})                                                     
#Print the summary 
output$prnt_desc_summ <- renderPrint({
  if (Desc_Summary_YN() == "Yes") {
    list("Summary"= Desc_Summary_Central_Tendency(),
         "Standard.Deviation"= Desc_Summary_SD(),
         "Coefficient.of.Variation"= try(Desc_Summary_COV()) )
  }
})

###############################################
##  Function to get coefficient of variation ##
###############################################
fncSmryCOV <- function(DF, X, SD) {
  #Make the SD a matrix
  aSD <- matrix(SD, nrow=1)
  #Get the means
  aMEAN <- lapply(DF[,X],  mean, na.rm=T)
  #Make the mean a matrix
  aMEAN <- matrix(unlist(aMEAN), nrow=1)
  COV <- as.vector(sweep(aSD, 1, unlist(aMEAN), "/") )
  names(COV) <- X
  return(COV)
}

################################################################################
##                        Summary X Histogram                                 ##
################################################################################
#1. Select the histogram variabe.
output$smry_var_hist_var <- renderUI({
  selectInput("smryVrHstVr", "1. Select the variable",
              choices = var(), multiple=FALSE, 
              selected=var()[1])
})
#1A. Reactive function for the variabe
descriptive_summary_histogram_variable <- reactive({
  input$smryVrHstVr
})
#1B. Summary mean
smry_var_hist_mean <- reactive({
  summary(df()[, descriptive_summary_histogram_variable()])["Mean"]
})
#1C. Summary median
smry_var_hist_median <- reactive({
  summary(df()[, descriptive_summary_histogram_variable()])["Median"]
})
#2. Select the approximate number of histogram bars
output$smry_var_hist_bars <- renderUI({                                 
  numericInput("smryVrHstBrs", "2. Select the approximate number of histogram bars.", 
               value = 15, step = 1, min=2)     
})
#2A. Object for histogram bars 
summary_variable_histogram_bars <- reactive({
  input$smryVrHstBrs
})
#3. Bar color
output$smry_var_hist_bar_clr <- renderUI({                                
  selectInput("smryVrHstBrClr", "3. Select the bar colors.",        
              choices = xyplot_Line_Color_Names(), 
              multiple=FALSE, selected="blue" ) 
})
#3A. Reactive function for Bar color
summary_var_histogram_bar_color <- reactive({
  input$smryVrHstBrClr
})
#4. Indicate if you want to show the mean and median
output$smry_hist_mn_med_yesno <- renderUI({                                 
  selectInput("smryHstMnMdYN", "4. Want to show the mean and median?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#4A. Object for the mean and median 
summary_hist_mean_median_yes_no <- reactive({
  input$smryHstMnMdYN
})
#5. Line colors
output$smry_var_hist_ln_clr <- renderUI({                                
  selectInput("smryVrHstLnClr", "5. Select the line colors.",        
              choices = xyplot_Line_Color_Names(), 
              multiple=TRUE, selected="black" ) 
})
#5A. Reactive function for the line color
summary_var_histogram_line_color <- reactive({
  input$smryVrHstLnClr
})
#6. Indicate if you want the histogram
output$smry_var_hist_yesno <- renderUI({                                 
  selectInput("smryVrHstYN", "6. Do you want to run the histogram?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#6A. Object for classification plot 
summary_var_hist_yes_no <- reactive({
  input$smryVrHstYN
})
#7. Run the histogram function below
summary_var_histogram_run <- reactive({
  if(summary_var_hist_yes_no() == "Yes") {    
    fncSmryHist(DF=df(), X=descriptive_summary_histogram_variable(), 
                BNS=summary_variable_histogram_bars(), CLR=summary_var_histogram_bar_color(), 
                LCLR=summary_var_histogram_line_color(), MN=smry_var_hist_mean(), 
                MED=smry_var_hist_median(), AddLine=summary_hist_mean_median_yes_no())
  }  
})
#7A.histogram  
output$summary_var_histogram_out <- renderPlot({
  if(summary_var_hist_yes_no() == "Yes") {
    summary_var_histogram_run()
  }
})

###############################
## Function to get histogram ##
###############################
fncSmryHist <- function(DF, X, BNS, CLR, LCLR, MN, MED, AddLine) {
  hist(x=DF[, X], breaks=BNS, col=CLR, xlab= X,
       main= paste0("Histogram of ", X, " (Mean= ", round(MN, 3),", Median= ", round(MED, 3), ")"))
  #Add mean and median lines
  if (AddLine== "Yes") {
    abline(v=MN,  col= head(LCLR, 1), lwd=3, lty=1)
    abline(v=MED, col= tail(LCLR, 1), lwd=3, lty=2)
  }
}

################################################################################
##           Density plot trend over time by groups                           ##
################################################################################
#1. Select the outcome variable.
output$dnsty_grp_trnd_Yvar <- renderUI({
  selectInput("dnsGrpTrnY", "1. Select the outcome.",
              choices = var(), multiple=FALSE, 
              selected=var()[1])
})
#1A. Reactive function for the outcome variable
density_group_trend_outcome <- reactive({
  input$dnsGrpTrnY
})
#1B. Reactive function for the density of the outcome variable
density_group_trend_Y_density <- reactive({
  density(df()[, density_group_trend_outcome()], na.rm=TRUE) 
})
#2. Select the grouping variable.
output$dnsty_grp_trnd_Xvar <- renderUI({
  selectInput("dnsGrpTrnX", "2. Select the group factor.",
              choices = setdiff(var(), density_group_trend_outcome()), multiple=FALSE, 
              selected=var()[2])
})
#2A. Reactive function for the variable
density_group_trend_group <- reactive({
  input$dnsGrpTrnX
})
#2B. Reactive function for the group levels
density_group_trend_grp_levels <- reactive({
  unique(df()[, density_group_trend_group()])
})
#3. Select the specific groups.
output$dnsty_grp_trnd_Xlevs <- renderUI({
  selectInput("dnsGrpTrXlev", "3. Select specific groups.",
              choices = sort(density_group_trend_grp_levels()), multiple=TRUE)
})
#3A. Reactive function for the variable
density_group_trend_grp_X_levs <- reactive({
  input$dnsGrpTrXlev
})
#4. Select the time indicator.
output$dnsty_grp_trnd_Zvar <- renderUI({ 
  selectInput("dnsGrpTrnZ", "4. Select the time indicator.", 
              choices = setdiff(var(), c(density_group_trend_outcome(), density_group_trend_group() ) ), 
              multiple=FALSE, selected= setdiff(var(), c(density_group_trend_outcome(), density_group_trend_group() ) )[1])
})
#4A. Reactive function for the variable
density_group_trend_time <- reactive({
  input$dnsGrpTrnZ
})
#4B. Reactive function for the range of time
density_group_trend_range_time <- reactive({
  range(as.numeric(df()[, density_group_trend_time()]), na.rm=TRUE)
})
#5. Select the rolling time period 
output$dnsty_grp_trnd_Z_inc <- renderUI({                                 
  numericInput("dnsGrpTrnZInc", "5. Select time increments (3= 3 months).", 
               value = 1, step = 1)     
})
#5A. Reactive function for the variable
density_group_trend_Time_Increment <- reactive({
  input$dnsGrpTrnZInc
})
#6. Line color
output$dnsty_grp_trnd_ln_clr <- renderUI({                                
  selectInput("dnsGrpTrnLClr", "6. Select the line color.",        
              choices = xyplot_Line_Color_Names(), 
              multiple=FALSE, selected="red" ) 
})
#6A. Reactive function for line color
density_group_trend_line_color <- reactive({
  input$dnsGrpTrnLClr
})
#7. Select the rolling time period 
output$dnsty_grp_trnd_trgt <- renderUI({                                 
  numericInput("dnsGrpTrnTrgt", "7. Set a target.", 
               value = NULL, step = .01)     
})
#7A. Reactive function for the variable
density_group_trend_Target <- reactive({
  input$dnsGrpTrnTrgt
})
#8. Legend location
output$dnsty_grp_trnd_lgd_loc <- renderUI({                                
  selectInput("dnsGrpTrnLgdLoc", "8. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#8A. Reactive function for legend location
density_group_trend_legend_location <- reactive({
  input$dnsGrpTrnLgdLoc
})
#9. Set the seed 
output$dnsty_grp_trnd_st_sed <- renderUI({                                 
  numericInput("dnsGrpTrnStSd", "9. Set (seed) group name randomness.", 
               value = 1, step = 1, min=1)     
})
#9A. Reactive function for the variable
density_group_trend_set_seed <- reactive({
  input$dnsGrpTrnStSd
})
#10. Indicate if you want to run the trend function
output$dnsty_grp_trnd_run_yesno <- renderUI({                                 
  selectInput("dnsGrpTrnRunYN", "10. Run the trend?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#10A. Object for yes/no running of the plot 
density_group_trend_run_yes_no <- reactive({
  input$dnsGrpTrnRunYN
})
#11. Speed of the animation play 
output$dnsty_grp_trnd_sec <- renderUI({                                 
  numericInput("dnsGrpTrnSec", "11. Play speed (1000= 1 second).", 
               value = 500, step = 100, min=0)     
})
#11A. Reactive function for the variable
density_group_trend_seconds <- reactive({
  input$dnsGrpTrnSec
})
#12. Play the trend graph
output$dnsty_grp_trnd_ply <- renderUI({
  sliderInput("dnsGrpTrnPly", "12. Play the time trend.",   
              min= density_group_trend_range_time()[1], 
              max= density_group_trend_range_time()[2] - (density_group_trend_Time_Increment() - 1), 
              value =1, step=1, animate=list(interval= density_group_trend_seconds() ))  
})
#12A. Reactive function for the variable
density_group_trend_play <- reactive({
  input$dnsGrpTrnPly
})
#13. Indicate lower limit of x-axis
output$dnsty_grp_trnd_Xlim1 <- renderUI({
  numericInput("dnsGrpTrnX1", "13. Lower X-axis limit.",
               value = min(density_group_trend_Y_density()[["x"]], na.rm=TRUE), step = .1)
})
#13A. Reactive function for the variable
density_group_trend_Xlim1 <- reactive({
  input$dnsGrpTrnX1
})
#14. Indicate upper limit of x-axis
output$dnsty_grp_trnd_Xlim2 <- renderUI({
  numericInput("dnsGrpTrnX2", "14. Upper X-axis limit.",
               value = max(density_group_trend_Y_density()[["x"]], na.rm=TRUE), step = .1)
})
#14A. Reactive function for the variable
density_group_trend_Xlim2 <- reactive({
  input$dnsGrpTrnX2
})
#15. Indicate lower limit of y-axis
output$dnsty_grp_trnd_Ylim1 <- renderUI({
  numericInput("dnsGrpTrnY1", "15. Lower Y-axis limit.",
               value = min(density_group_trend_Y_density()[["y"]], na.rm=TRUE), step = .1)
})
#15A. Reactive function for the variable
density_group_trend_Ylim1 <- reactive({
  input$dnsGrpTrnY1
})
#16. Indicate upper limit of Y-axis
output$dnsty_grp_trnd_Ylim2 <- renderUI({
  numericInput("dnsGrpTrnY2", "16. Upper Y-axis limit.",
               value = max(density_group_trend_Y_density()[["y"]], na.rm=TRUE), step = .1)
})
#16A. Reactive function for the variable
density_group_trend_Ylim2 <- reactive({
  input$dnsGrpTrnY2
})
#17. Run trend output function below  
density_group_trend_output <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnsty(DF=df(), X= density_group_trend_group(), Y= density_group_trend_outcome(), 
               Z= density_group_trend_time(), Increment= density_group_trend_Time_Increment(), 
               Seed.Multiplier= density_group_trend_set_seed() )
  }  
})
#18. Plot function below  
density_group_trend_plot <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnstPlot(TDList= density_group_trend_output(), X= density_group_trend_group(), 
                  Y= density_group_trend_outcome(), Z= density_group_trend_time(), 
                  Period= density_group_trend_play(), Lcol= density_group_trend_line_color(), 
                  Target= density_group_trend_Target(), Groups= density_group_trend_grp_X_levs(), 
                  Legend.Loc= density_group_trend_legend_location(), 
                  Xmin=density_group_trend_Xlim1(), Xmax=density_group_trend_Xlim2(), 
                  Ymin=density_group_trend_Ylim1(), Ymax= density_group_trend_Ylim2()) 
  }  
})
#18A. Trend plot  
output$dnsty_grp_trnd_plot <- renderPlot({
  if(density_group_trend_run_yes_no() == "Yes") {
    density_group_trend_plot()
  }
})
#19. Each period's output  
density_group_trend_by_time <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnstOut(TDList= density_group_trend_output(), 
                 Period=density_group_trend_play(), Target= density_group_trend_Target())  
  }  
})
#19A. Each period's output  
output$dnsty_grp_trnd_out_by_tm <- renderPrint({
  if(density_group_trend_run_yes_no() == "Yes") {    
    density_group_trend_by_time()
  }
})

################################################################################
## Function to create aggregated values and density plot over time increments ##
################################################################################
fncTmDnsty <- function(DF, X, Y, Z, Increment, Seed.Multiplier) {
  #Get summary of values
  Time.Period.Length <- length(unique(DF[, Z]))
  Increment.Length <- Time.Period.Length - (Increment - 1)
  Time.Period.Values <- unique(DF[, Z])
  oTPV <- order(Time.Period.Values)
  Time.Period.Values <- Time.Period.Values[oTPV]
  #Get time periods for aggregating rates  
  i <- 1
  Time.Periods <- list()
  while( i <= Increment.Length) {
    Time.Periods[[i]] <- Time.Period.Values[i:(i + (Increment -1))] 
    i = i + 1
  }
  #Aggregate values
  AggrY <- list()
  for (i in 1:length(Time.Periods )) {
    AggrY[[i]] <- aggregate(DF[DF[, Z] %in% Time.Periods[[i]], Y] ~ DF[DF[, Z] %in% Time.Periods[[i]], X], FUN=mean)
    colnames(AggrY[[i]]) <- c(X, Y)
  }
  
  #Overall density values
  D1 <- density(x = DF[, Y], na.rm=TRUE )
  XLim <- range(D1[["x"]])
  YLim <- range(D1[["y"]])
  
  #Main title: Get first and last time points
  Time.Start.Label <- lapply(Time.Periods, `[[`, 1)
  Time.Stop.Label <- lapply(Time.Periods, `[[`, Increment) 
  
  #Values: Lists with each value by time point and group names
  DXname <- lapply(AggrY, function(x) x[-length(x)])
  DYvals <- lapply(AggrY, function(x) x[length(x)])
  
  #Density values by groups and time
  DYvals <- lapply(DYvals, unlist)
  D2 <- lapply(DYvals,density, na.rm=TRUE)
  
  #Means and medians by groups
  YTmean <- lapply(DYvals, mean)
  YTmedian <- lapply(DYvals, median)
  
  
  #Get X and Y density values for the line to set as boundaries for text names
  DxDY <- list()
  for (j in 1:Increment.Length ) {
    DxDY[j] <- (lapply(D2[j], function(x) x[1:2]))
  }
  
  #I need to create a vector with the exact number of elements I need. Gets matching values within line
  Dx.Cord <- vector(mode = "list", length = Increment.Length)
  Dy.Cord <- vector(mode = "list", length = Increment.Length)
  Dy.Cord.Random <- vector(mode = "list", length = Increment.Length)
  for (i in 1:nrow(DXname[[i]]) ) {
    for (j in 1:Increment.Length ) {
      Dx.Cord[[j]][[i]] <- DxDY[[j]][[ 1]][which( min(abs(DxDY[[j]][[1]] - DYvals[[j]][i])) == abs(DxDY[[j]][[1]] - DYvals[[j]][i] ))]
      Dy.Cord[[j]][[i]] <- DxDY[[j]][[ 2]][which( min(abs(DxDY[[j]][[1]] - DYvals[[j]][i])) == abs(DxDY[[j]][[1]] - DYvals[[j]][i] ))]
      set.seed(i * Seed.Multiplier)
      Dy.Cord.Random[[j]][[i]] <- runif(1, min = 0.001, max = Dy.Cord[[j]][[i]])
      
    }
  }
  
  return(list("AggrY"=AggrY, "Time.Periods"=Time.Periods,
              "D1"=D1, "D2"=D2, "XLim"=XLim, "YLim"=YLim, "YTmean"=YTmean, "YTmedian"=YTmedian,  
              "Time.Start.Label"=Time.Start.Label, "Time.Stop.Label"=Time.Stop.Label,
              "DXname"=DXname, "DYvals"=DYvals, "DxDY"=DxDY, "Dx.Cord"=Dx.Cord, "Dy.Cord"=Dy.Cord, "Dy.Cord.Random"=Dy.Cord.Random,  
              "Increment"=Increment, "Time.Period.Length"=Time.Period.Length, 
              "Increment.Length"=Increment.Length, "Seed.Multiplier"=Seed.Multiplier))
}

################################################################################
##    Function to Plot density of aggregated values  over time increments     ##
################################################################################
fncTmDnstPlot <- function(TDList, X, Y, Z, Period, Lcol, Target, Groups,
                          Legend.Loc,Xmin, Xmax, Ymin, Ymax) {
  Increment.Length <- TDList[["Increment.Length"]]
  #Title
  if(TDList[["Increment"]] == 1) {
    Main.Title <- paste(Y," rate by ", Z, ": ", 
                        TDList[["Time.Start.Label"]][[Period]],  sep= "")
  } else {
    Main.Title <- paste(Y," rate by ", Z, ": ", 
                        TDList[["Time.Start.Label"]][[Period]], " - ",                      
                        TDList[["Time.Stop.Label"]][[Period]],  sep= "")
  }
  #Density plot
  plot(TDList[["D2"]][[ Period]], xlim=c(Xmin,Xmax), ylim= c(Ymin, Ymax),  
       col=Lcol, lwd=8  ,
       main=Main.Title, xlab= "Rate" 
  )
  #Add vertical lines
  abline(v=Target, col="blue", lwd=2)           
  abline(v= mean(TDList[["AggrY"]][[1]][[2]]), col=colors()[102], lwd=2)
  abline(v= mean(TDList[["AggrY"]][[Increment.Length]][[2]]), col=colors()[102], lwd=2, lty=2)
  ###Rug and text to identify the med centers.###
  rug(TDList[["DYvals"]][[Period]], side=1, col=Lcol)
  #Text for group values
  if ( is.null(Groups) ) {  
    text(TDList[["DYvals"]][[Period]], unlist(TDList[["Dy.Cord.Random"]][Period]),   
         labels= unlist(TDList[["DXname"]][[Period]][X]),
         cex=2)
  } else {
    non_groups <- setdiff(unlist(TDList[["DXname"]][[Period]][[X]]) , Groups)   #Get excluded groups
    Groups.Temp <- as.character(unlist(TDList[["DXname"]][[Period]][[X]]))      #Get all groups
    Groups.Temp[which(Groups.Temp %in% non_groups )] <- ""                      #Change non-groups to blanks
    text(TDList[["DYvals"]][[Period]], unlist(TDList[["Dy.Cord.Random"]][Period]),   
         labels= Groups.Temp, cex=2)
  }
  ###Legend###
  #This creates a legend for the ablines with and without targets. TDList[["AggrY"]][[Period]] YTmean
  if ( !is.numeric(Target) ) {  
    legend(x=Legend.Loc, legend=c(paste0("Starting pooled mean: ", round(mean(TDList[["AggrY"]][[1]][[2]], na.rm=TRUE), 3)),
                                  paste0("Ending pooled mean: ", round(mean(TDList[["AggrY"]][[Increment.Length]][[2]], na.rm=TRUE), 3) )),
           col=c(colors()[102], colors()[102], "blue"),
           lty= c(1,2,1), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  } else {  
    #legend(Legend.Loc, legend=c("Starting mean","Ending mean", "Target"),
           legend(Legend.Loc, legend=c(paste0("Starting pooled mean: ", round(mean(TDList[["AggrY"]][[1]][[2]], na.rm=TRUE), 3)),
                                       paste0("Ending pooled mean: ", round(mean(TDList[["AggrY"]][[Increment.Length]][[2]], na.rm=TRUE), 3) ) , "Target"),
           col=c(colors()[102], colors()[102], "blue"),
           lty= c(1,2,1), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  }
}

################################################################################
##           Function to get aggregated values over time increments           ##
################################################################################
fncTmDnstOut <- function(TDList, Period, Target) {
  out.by.period <- TDList[["AggrY"]][[Period]]
  
  #High Target
  HT.table <- vector(length=2)
  if ( is.numeric(Target) ) {  
    HT.table <- prop.table(table(out.by.period[, 2] < Target))
    if ( length(HT.table) == 1 ) {  
      names(HT.table) <- "All"
    } else {
      names(HT.table) <- c("At or above target", "Below target")
    }
  } else {
    HT.table <- NA
  }    
  #Low Target
  LT.table <- vector(length=2)
  if ( is.numeric(Target) ) {  
    LT.table <- prop.table(table(out.by.period[, 2] >= Target))
    if ( length(LT.table) == 1 ) {  
      names(LT.table) <- "All"
    } else {
      names(LT.table) <- c("At or below target", "Above target")
    }
  } else {
    LT.table <- NA
  }
  return(list("Period Rate"= out.by.period, "High Target"= HT.table, "Low Target"= LT.table, 
              "Pooled.Mean"= mean(as.numeric(unlist(out.by.period[[2]])) ,na.rm=TRUE),
              "Pooled.Median"= median(as.numeric(unlist(out.by.period[[2]])) ,na.rm=TRUE)))
}



################################################################################

################################################################################
## Testing section: Begin  ##
################################################################################
#output$testplot1 <- renderPlot({ 
##  plot(values$a, values$b)
##} )

#output$test1 <- renderPrint({
#list( #head(modifiedDf1()),
      #head(modifiedFacDf()), 
#      non_modified_vars() ,
#      modifiedNumDf()
#      head(modifiedTimeVarCrt()), head(modifiedYMmonthCrt())
#      modifiedDf()
#)  

#  }) 
 

################################################################################
## Testing section: End ##
################################################################################

  })   #This is the last line of code that closes out the entire server file
  
