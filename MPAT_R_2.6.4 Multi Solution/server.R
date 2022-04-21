server <- function(input, output, session){
  
  template <- read.csv(paste(path, "/", "Input_Data_Template_v2", ".csv", sep = ""),
                       as.is = T, header = T)
  
  #This function is used to download the template CSV file
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("Input_Data_Template", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    })
  
  #This function is repsonsible for loading in the selected file
  inputData_temp <- reactive({
    infile <- input$import
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath,
             as.is = T,
             skip = 1,
             header = F,
             na.strings=c("","NA")) ## JGD 20190221 replace blanks with na
  })
  
  # The event executes when the "Load Parameters" button is selected
  observeEvent(input$load,{
    
    if (is.null(input$import)){
      showNotification('Please upload CSV File', type = 'error')
    } else {
      
      
      withProgress(message = "Loading Parameters", value = 0, {
        
        
        disable <- c(paste0('budget',seq(1:5)), 'CBPLweight','Rankweight','runtime','load','downloadTemplate')
        
        for (i in 1:length(disable)){
          disable(disable[i])
        }
        
        hide <- c('solution_name','save','projectBox','fundedBox')
        
        for (i in 1:length(hide)){
          hide(hide[i])
        }
        
        setProgress(0.1, detail = "Initializing texts")
        Sys.sleep(0.25)
        
        # p <- 5 # Number of years
        output$statusUpdate <- renderText("text-warning")
        
        # Convert multiplier values to numeric
        b1 <- as.numeric(input$budget1)
        b2 <- as.numeric(input$budget2)
        b3 <- as.numeric(input$budget3)
        b4 <- as.numeric(input$budget4)
        b5 <- as.numeric(input$budget5)
        
        budget <<- matrix(c(b1,b2,b3,b4,b5),p,1) # Multiplier matrix
        
        w1 <- as.numeric(input$CBPLweight)
        w2 <- as.numeric(input$Rankweight)
        
        inputData <<- inputData_temp()
        inputData[is.na(inputData)] <<- 0 ## JGD 20190221 replace na with 0
        
        # y <- 21 # Must be updated each POM cycle to reflect first year of FYDP
        m <<- nrow(inputData) # Number of projects
        n <- m*p # Number of decision variables
        
        output$projects <- renderPrint({
          cat("Number of projects (m):", m)
        })
        
        # # Create an info box for the number of projects inputted to the model
        # output$projectBox <- renderInfoBox({
        #   infoBox("Projects", paste0(m), icon = icon("building", lib = "font-awesome"),
        #           color = "yellow")
        # })
        
        # Create a value box for the number of projects inputted to the model
        output$projectBox <- renderValueBox({
          valueBox(
            value = formatC(m, digits = 1, format = "d"),
            subtitle = "Projects",
            icon = icon("building", lib = "font-awesome"),
            color = "yellow"
          )
        })
        
        output$cols <- renderPrint({
          cat("Number of variables (n):", n)
        })  
        
        setProgress(0.2, detail = 'Grabbing data')
        Sys.sleep(0.25)
        
        projectNumber <- inputData[,1] # Read data from 1st column of inputData
        projectTitle <- inputData[,2] # Read data from 2nd column of inputData
        location <- inputData[,3] # Read data from 3rd column of inputData
        misProgram <- inputData[,4] # Read data from 4th column of inputData
        pomSponsor <- inputData[,5] # Read data from 5th column of inputData
        CBPL <- inputData[,6] # Read data from 6th column of inputData
        Rank <- inputData[,7] # Read data from 7th column of inputData
        tempScores <- (w1*CBPL) + (w2*Rank) # Add CBPL and Rank
        tempcosts <- inputData[,8] # Read data from 8th column of inputData
        costs <-t(tempcosts) # Transpose cost data
        must_fund_temp <- inputData[,9:13] # Read data from 9th-13th columns of inputData
        must_fund <- data.matrix(must_fund_temp) # Convert to a data matrix
        noEarlier_temp <- inputData[,14:18] # Read data from 14th-18th columns of inputData
        noEarlier <- data.matrix(noEarlier_temp) # Convert to a data matrix
        noEarlier[noEarlier == 0] <- -1 # Replace zeroes with -1
        desiredYear <- inputData[,19] # Read data from 19th column of inputData ## JGD 20190221 changed the business rule to desired year
        # businessRule_temp <- inputData[,19] # Read data from 19th column of inputData ## JGD 20192021 commented out
        # businessRule_temp <- replicate(m, 0) # Create a 0 vector of size m ## JGD 20190221 added
        # businessRule <- data.matrix(businessRule_temp) # Convert to a data matrix   ## Commented out MZ 02/22/2019
        previousProgram <- inputData[,20] # Read data from 20th column of inputData
        PDS	<- inputData[,21:22] # Read data from 21st-22nd columns of inputData
        PND1 <- inputData[,23:24]	# Read data from 23rd-24th columns of inputData
        PND2 <- inputData[,25:26] # Read data from 25th-26th columns of inputData
        CEQOM	<- inputData[,27:28] # Read data from 27th-28th columns of inputData
        CEQPROC	<- inputData[,29:30] # Read data from 29th-30th columns of inputData
        C4IOM	<- inputData[,31:32] # Read data from 31st-32nd columns of inputData
        C4IPROC	<- inputData[,33:34] # Read data from 33rd-34th columns of inputData
        
        
        ################################################
        ## Descriptive Statistics
        ################################################
        
        # Collapse must fund columns and convert to FY
        must_fund_fy <- matrix(NA,m,1)
        for (i in 1:m){
          for(j in 1:p){
            if(must_fund_temp[i,1]==1){
              must_fund_fy[i] = y
            }
            if(must_fund_temp[i,2]==1){
              must_fund_fy[i] = y+1
            }
            if(must_fund_temp[i,3]==1){
              must_fund_fy[i] = y+2
            }
            if(must_fund_temp[i,4]==1){
              must_fund_fy[i] = y+3
            }
            if(must_fund_temp[i,5]==1){
              must_fund_fy[i] = y+4
            }
          }
        }
        
        setProgress(0.3, detail = 'Preparing calculations')
        Sys.sleep(0.25)
        
        # Sum the rows of noEarlier_temp
        netSum <- rowSums(noEarlier_temp)
        
        descriptiveData <<- data.frame(projectNumber, projectTitle, pomSponsor, CBPL, Rank, tempcosts, tempScores, must_fund_fy, netSum)
        
        # Create table of just must fund projects
        mustFundProjects <<- subset(descriptiveData, !is.na(must_fund_fy))
        
        # Create a stability matrix 
        stabilityMatrix <- matrix(1,m,p)
        for (i in 1:m){
          if (previousProgram[i]==y){
            stabilityMatrix[i,1] = 5
            stabilityMatrix[i,2] = 4
            stabilityMatrix[i,3] = 3
            stabilityMatrix[i,4] = 2
            stabilityMatrix[i,5] = 1
          }
          if (previousProgram[i]==y+1){
            stabilityMatrix[i,1] = 4
            stabilityMatrix[i,2] = 5
            stabilityMatrix[i,3] = 4
            stabilityMatrix[i,4] = 3
            stabilityMatrix[i,5] = 2
          }
          if (previousProgram[i]==y+2){
            stabilityMatrix[i,1] = 3
            stabilityMatrix[i,2] = 4
            stabilityMatrix[i,3] = 5
            stabilityMatrix[i,4] = 4
            stabilityMatrix[i,5] = 3
          }
          if (previousProgram[i]==y+3){
            stabilityMatrix[i,1] = 2
            stabilityMatrix[i,2] = 3
            stabilityMatrix[i,3] = 4
            stabilityMatrix[i,4] = 5
            stabilityMatrix[i,5] = 4
          }
          if (previousProgram[i]==y+4){
            stabilityMatrix[i,1] = 1
            stabilityMatrix[i,2] = 2
            stabilityMatrix[i,3] = 3
            stabilityMatrix[i,4] = 4
            stabilityMatrix[i,5] = 5
          }
        }
        
        # Multiply value scores by stability matrix
        tempValueMatrix <- matrix(0,m,p)
        for (i in 1:m){
          for(j in 1:p){
            tempValueMatrix[i,j] = (tempScores[i] * stabilityMatrix[i,j])
          }
        }
        
        valueScores <- as.vector(t(tempValueMatrix)) # Put the value scores in a horizontal vector
        
        ################################################
        # Create Objective Function
        ################################################
        
        # Create (1xn) matrix of decision variables using values scores
        vectorVariables <<- data.matrix(valueScores)
        
        ################################################
        # Create contraints to limit number of projects
        ################################################
        
        # Each project can only be started once.
        # Create an m x n matrix of variables with diagonal of 5 "ones" in each row;
        variables <- matrix(0, m, n)
        count <- 0
        for (i in 1:m){
          for(j in 1:n){
            if (((j-count)<=5) && ((j-count)>0)){
              variables[i,j]=1
            }
            else  variables[i,j]=0
          }
          count <- count+5
        }
        M1_LHS <- variables
        
        variablesRHS <- matrix(1,m,1)
        M1_RHS <- variablesRHS
        
        #############################################
        # Create Must Fund contraints
        #############################################
        
        # Convert Must Fund data to m x n matrix
        mustfundRHS <- matrix(0, m, n)
        count <- 0
        for (i in 1:m){
          for(j in 1:n){
            if (((j-count)<=5) && ((j-count)>0)){
              mustfundRHS[i,j]=must_fund[i,j-(count)]
            }
          }
          count <- count+5
        }
        # Multiply -1 to convert to "<=" constraint
        M2_LHS <- mustfundRHS*-1 
        M2_RHS <- data.matrix(rowSums(M2_LHS))
        
        #############################################
        # Create No-Earlier contraints
        #############################################
        
        # Convert (m x p) NET matrix to (m x n) matrix
        noEarlierRHS <- matrix(0, m, n)
        count <- 0
        for (i in 1:m){
          for(j in 1:n){
            if (((j-count)<=5) && ((j-count)>0)){
              noEarlierRHS[i,j]=noEarlier[i,j-(count)]
            }
          }
          count <- count+5
        }
        
        M3_LHS <- noEarlierRHS*-1
        M3_RHS <- matrix(0,m,1)
        
        
        #############################################
        # Create Business-Rule constraints
        #############################################
        
        # # Multiply by -1 to convert to "<=" constraint      # Commented out MZ 02/22/2019
        # M4_LHS <- M1_LHS*-1 
        # M4_RHS <- businessRule*-1 
        
        #############################################
        # Create Budget constraints
        #############################################
        
        cost_temp1 <- matrix(0, n, p)
        projectCount <- 1
        counter2 <- 0
        for (i in 1:n){
          for (j in 1:p){
            if ((i-counter2) == j){
              
              cost_temp1[i,j] = costs[projectCount]
              
              # If you reach the end of the row
              if (j == p){
                projectCount <- projectCount + 1
                counter2 <- counter2 + 5}
            }}}
        
        # Duplicate each value 5 times for each year
        PDS_lead <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            PDS_lead[i,j] <- PDS[i,1]*-1}
        }
        # Convert matrix to a vector by rows
        PDS_lead <- as.vector(t(PDS_lead))
        
        # Duplicate each value 5 times for each year
        PDS_cost <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            PDS_cost[i,j] <- PDS[i,2]}
        }
        # Convert matrix to a vector by rows
        PDS_cost <- as.vector(t(PDS_cost))
        
        # Duplicate each value 5 times for each year
        PND1_lead <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            PND1_lead[i,j] <- PND1[i,1]*-1}
        }
        # Convert matrix to a vector by rows
        PND1_lead <- as.vector(t(PND1_lead))
        
        # Duplicate each value 5 times for each year
        PND1_cost <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            PND1_cost[i,j] <- PND1[i,2]}
        }
        # Convert matrix to a vector by rows
        PND1_cost <- as.vector(t(PND1_cost))
        
        # Duplicate each value 5 times for each year
        PND2_lead <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            PND2_lead[i,j] <- PND2[i,1]*-1}
        }
        # Convert matrix to a vector by rows
        PND2_lead <- as.vector(t(PND2_lead))
        
        # Duplicate each value 5 times for each year
        PND2_cost <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            PND2_cost[i,j] <- PND2[i,2]}
        }
        # Convert matrix to a vector by rows
        PND2_cost <- as.vector(t(PND2_cost))
        
        # Duplicate each value 5 times for each year
        CEQOM_lag <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            CEQOM_lag[i,j] <- CEQOM[i,1]}
        }
        # Convert matrix to a vector by rows
        CEQOM_lag <- as.vector(t(CEQOM_lag))
        
        # Duplicate each value 5 times for each year
        CEQOM_cost <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            CEQOM_cost[i,j] <- CEQOM[i,2]}
        }
        # Convert matrix to a vector by rows
        CEQOM_cost <- as.vector(t(CEQOM_cost))
        
        # Duplicate each value 5 times for each year
        CEQPROC_lag <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            CEQPROC_lag[i,j] <- CEQPROC[i,1]}
        }
        # Convert matrix to a vector by rows
        CEQPROC_lag <- as.vector(t(CEQPROC_lag))
        
        # Duplicate each value 5 times for each year
        CEQPROC_cost <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            CEQPROC_cost[i,j] <- CEQPROC[i,2]}
        }
        # Convert matrix to a vector by rows
        CEQPROC_cost <- as.vector(t(CEQPROC_cost))
        
        # Duplicate each value 5 times for each year
        C4IOM_lag <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            C4IOM_lag[i,j] <- C4IOM[i,1]}
        }
        # Convert matrix to a vector by rows
        CEQOM_lag <- as.vector(t(CEQOM_lag))
        
        # Duplicate each value 5 times for each year
        C4IOM_cost <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            C4IOM_cost[i,j] <- C4IOM[i,2]}
        }
        # Convert matrix to a vector by rows
        C4IOM_cost <- as.vector(t(C4IOM_cost))
        
        # Duplicate each value 5 times for each year
        C4IPROC_lag <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            C4IPROC_lag[i,j] <- C4IPROC[i,1]}
        }
        # Convert matrix to a vector by rows
        C4IPROC_lag <- as.vector(t(C4IPROC_lag))
        
        # Duplicate each value 5 times for each year
        C4IPROC_cost <- matrix(0, m, p)
        for (i in 1:m){
          for (j in 1:p){
            C4IPROC_cost[i,j] <- C4IPROC[i,2]}
        }
        # Convert matrix to a vector by rows
        C4IPROC_cost <- as.vector(t(C4IPROC_cost))
        
        cost_temp2 <- cost_temp1
        
        for (i in 1:n){
          for (j in 1:p){
            if (((j+PDS_lead[i])<=p) && (cost_temp1[i,j+PDS_lead[i]] > 0)){
              cost_temp2[i,j] <- cost_temp2[i,j] + PDS_cost[i]}
            
            if (((j+PND1_lead[i])<=p) && (cost_temp1[i,j+PND1_lead[i]] > 0)){
              cost_temp2[i,j] <- cost_temp2[i,j] + PND1_cost[i]}
            
            if (((j+PND2_lead[i])<=p) && (cost_temp1[i,j+PND2_lead[i]] > 0)){
              cost_temp2[i,j] <- cost_temp2[i,j] + PND2_cost[i]}
            
            if ((cost_temp1[i,j] > 0) && ((j+CEQOM_lag[i])<=p)){
              cost_temp2[i,j+CEQOM_lag[i]] <-  cost_temp2[i,j+CEQOM_lag[i]] + CEQOM_cost[i]}
            
            if ((cost_temp1[i,j] > 0) && ((j+CEQPROC_lag[i])<=p)){
              cost_temp2[i,j+CEQPROC_lag[i]] <-  cost_temp2[i,j+CEQPROC_lag[i]] + CEQPROC_cost[i]}
            
            if ((cost_temp1[i,j] > 0) && ((j+C4IOM_lag[i])<=p)){
              cost_temp2[i,j+C4IOM_lag[i]] <-  cost_temp2[i,j+C4IOM_lag[i]] + C4IOM_cost[i]}
            
            if ((cost_temp1[i,j] > 0) && ((j+C4IPROC_lag[i])<=p)){
              cost_temp2[i,j+C4IPROC_lag[i]] <-  cost_temp2[i,j+C4IPROC_lag[i]] + C4IPROC_cost[i]}
          }}
        
        M5_LHS <<- t(cost_temp2)
        M5_RHS <- budget
        
        #############################################
        # Create No-Later contraints                    # MZ 01/23/2019
        #############################################
        
        # Convert (m x p) NET matrix to (m x n) matrix
        noLaterRHS <- matrix(0, m, n)
        count <- 0
        for (i in 1:m){
          for(j in 1:n){
            if (((j-count)<=5) && ((j-count)>0)){
              noLaterRHS[i,j]=noLaterRHS[i,j-(count)]} # JGD 02/06/2019 added RHS to end of variable name on right hand side of equation
          }
          count <- count+5}
        
        M6_LHS <- noLaterRHS*-1
        M6_RHS <- matrix(0,m,1)
        
        ##################################################
        # Row-bind all constriants to form Ax <= b problem
        ##################################################
        
        list1 <- list(M1_LHS,M2_LHS,M3_LHS,M5_LHS)   # Deleted M4_LHS, MZ 02/22/2019
        Ax <- do.call(rbind,list1)
        list2 <- list(M1_RHS,M2_RHS,M3_RHS,M5_RHS)   # Deleted M4_RHS, MZ 02/22/2019
        b <- do.call(rbind,list2)
        
        #############################################
        # Make an LP
        #############################################
        lp <- make.lp(0, n)
        set.objfn(lp, vectorVariables) 
        lp.control(lp, sense = 'max') # Set to solve as maximize problem
        row.add.mode(lp,"on") 
        for (i in 1:nrow(Ax)){
          add.constraint(lp,xt=c(Ax[i,]),type="<=",rhs=b[i])
        } # Add constraitns and make all <= equations
        row.add.mode(lp,"off")
        # print("test1")
        set.type(lp, columns = 1:n, type = "binary")
        RowNames <- paste("r", 1:nrow(Ax))
        ColNames <- paste("c", 1:n)
        dimnames(lp) <- list(RowNames, ColNames)
        set.bounds(lp, lower = rep(0,n))
        
        # Print message that describes the number of variables and constraints in LP
        constraints <- nrow(Ax)
        output$lp_model <- renderPrint(cat("This is a linear program with", n, "variables and", constraints, "constraints." ))
        
        lp2 <- lp
        
        controls <- lp.control(lp2, presolve = c("rows", "cols", "lindep"), improve = c("dualfeas", "thetagap"), mip.gap = 1.0e-12,
                               obj.in.bas = FALSE,  verbose = "detailed", timeout = input$runtime)
        
        setProgress(0.4, message = "Looking for an optimal solution", detail = '')
        Sys.sleep(0.25)
        
        solveLP <- solve(lp2)
        
        output$solvedCheckmark <- renderImage({
          list(src = "./www/tick.png",
               contentType = "image/png",
               height = 20,
               alt = "tick")
        }, deleteFile = FALSE)
        
        
        objective <<- format(get.objective(lp2), scientific = FALSE, big.mark=",")
        output$objectiveValue <- renderPrint({
          cat("Objective value of solution:", objective)})
        
        iterationCount <<- format(get.total.iter(lp2), scientific = FALSE, big.mark=",")
        output$iterations <- renderPrint({
          cat("Number of iterations to reach solution:", iterationCount)})
        
        
        primal <- get.primal.solution(lp, orig = "TRUE") # Get the full primal solution
        primaltemp <- data.matrix(primal) # Put the primal soution into a data matrix 
        primalsolution <- tail(primaltemp,n) # Keep the last n entries of the primal solution
        projectCount <- sum(primalsolution)
        projectCount <<- projectCount
        output$projectCount <- renderPrint({
          cat("Projects funded:", projectCount)})
        
        output$summaryProjectCount <- renderPrint({
          cat("Number of projects funded:", projectCount)})
        
        # Create a value box for the number of projects inputted to the model
        output$fundedBox <- renderValueBox({
          valueBox(
            value = formatC(projectCount, digits = 1, format = "d"),
            subtitle = "Funded",
            icon = icon("dollar", lib = "font-awesome"),
            color = "green")})
        
        dim(primalsolution) <- c(p,m) # Convert the remaining solution to a (p x m) matrix
        exportSolution <- t(primalsolution) # Transpose the soltion to make (m x p) matrix
        
        
        setProgress(0.5, detail = "Cost vs Value")
        Sys.sleep(0.25)
        #######################################################
        # Create a matrix of costs
        #######################################################
        
        tempcosts <- inputData[,8]
        
        costTable <- data.frame(as.numeric(tempcosts)) # Table costs
        costTable <- data.matrix(costTable) # Convert to matrix
        
        colnames(costTable) <- NULL # Remove column names
        
        # Calculate costs for solution
        costMatrix <- matrix(0,m,p)
        for (i in 1:m){
          for (j in 1:p){
            costMatrix[i,j]=costTable[i,1] * exportSolution[i,j]}
        }
        totalcostMatrix <-  t(costTable) %*% exportSolution # Total cost by year
        
        costVector <- rowSums(costMatrix)
        
        #######################################################
        # Create cost vs value Chart
        #######################################################
        
        # (1xn) matrix of decision variables values scores
        v1 <- vectorVariables
        # Convert v1 to a mxp matrix
        dim(v1) <- c(m,p)
        # Multiply element-wise the value matrix and solution matrix 
        v2 <- v1 * exportSolution
        # Sum the values by row
        v3 <- rowSums(v2)
        # Get the cost matrix
        c1 <- costMatrix
        # Sum the costs by row
        c2 <- rowSums(c1)
        # Create a cost value data frame
        costValueTable <- data.frame(descriptiveData$projectNumber, descriptiveData$projectTitle, v3, c2, descriptiveData$pomSponsor)
        colnames(costValueTable) <- c("Project.Number", "Project.Title", "Value.Score", "Cost", "POM.Sponsor")
        
        # Remove projects (rows) that ar enot chosen in the solution
        costValueTable <<- costValueTable[!(costValueTable$Cost==0),]
        
        #######################################################
        # Format the solution for display in a Shiny dataTable
        #######################################################
        
        yearColumn <- matrix("-----",m,1)
        for (i in 1:m){
          for (j in 1:p){
            if (costMatrix[i,j]>0){ 
              yearColumn[i] = paste("FY",(y+j-1))}
          }}
        
        mustFund <- matrix("-----",m,1)
        for (i in 1:m){
          for (j in 1:p){
            if (must_fund[i,j]==1){ 
              mustFund[i] = paste("FY",(y+j-1))}
          }}
        
        NET <- matrix("-----",m,1)
        for (i in 1:m){
          netCount = 0
          for (j in 1:p){
            if (netCount == 0){
              if (noEarlier[i,j]==1){
                NET[i] = paste("FY",(y+j-1))
                netCount = 1}
            }}}
        
        oldProgram <- matrix("-----",m,1)
        for (i in 1:m){
          if (previousProgram[i]>0){ 
            oldProgram[i] = paste("FY",previousProgram[i])}
        }
        
        setProgress(0.6, detail = "Preparing Solutions")
        Sys.sleep(0.25)
        
        formatSolution <- data.frame(descriptiveData$projectNumber, descriptiveData$projectTitle, misProgram, descriptiveData$pomSponsor, exportSolution, costVector)
        colnames(formatSolution) <- c("Project Number", "Project Title", "MIS Program", "Capability Sponsor", paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "Cost")
        # rownames(formatSolution) <- projectNumber ## JGD 20190221 commented out, added as first column of table
        
        # This is the full solution in a table format
        tableSolution <<- data.frame(descriptiveData$projectNumber, descriptiveData$projectTitle, misProgram, location, descriptiveData$pomSponsor, mustFund, NET, oldProgram, yearColumn, costVector)
        colnames(tableSolution) <<- c("Project Number", "Project Title", "MIS Program", "Location", "Capability Sponsor", "Must Fund", "No Earlier", "Stability", "Current", "Cost")
        # rownames(tableSolution) <- projectNumber ## JGD 20190221 commented out, added as first column of table
        
        #Calculate Summary Statistics
        projectsAdded <- 0
        for (i in 1:m){
          if ((tableSolution[i,8] == "-----") && (tableSolution[i,9] != "-----")){  ## 02/22/19 MMZ  edited 7->8 8->9 
            projectsAdded <<- projectsAdded + 1}
        }
        
        projectsDropped <- 0
        for (i in 1:m){
          if ((tableSolution[i,8] != "-----") && (tableSolution[i,9] == "-----")){ ## 02/22/19 MMZ  edited 7->8 8->9
            projectsDropped <<- projectsDropped + 1}
        }
        
        # Count the number of projects moved.
        # Must convert to numeric since the columns have different factors.
        projectsMoved <- 0
        for (i in 1:m){
          if (tableSolution[i,8] != "-----"){
            a <- as.character(tableSolution[[i,8]])
            b <- as.character(tableSolution[[i,9]])
            if(a != b){
              projectsMoved <<- projectsMoved + 1}
          }}
        
        output$summaryProjectsAdded <- renderPrint({
          cat("Number of new projects added:",  projectsAdded)})
        
        output$summaryProjectsDropped <- renderPrint({
          cat("Number of", paste("POM",y-1), "projects dropped:",  projectsDropped)})
        
        output$summaryProjectsMoved <- renderPrint({
          cat("Number of", paste("POM",y-1), "projects moved:",  projectsMoved)})
        
        setProgress(0.7, detail = "Gathering Chart Data")
        Sys.sleep(0.25)
        
        plotData1 <- data.frame(misProgram, descriptiveData$pomSponsor, costVector, yearColumn)
        colnames(plotData1) <- c("MIS_Program","Capability_Sponsor", "Cost", "Current")
        
        geomplotData <<- plotData1[plotData1[,4] != "-----",] # Remove rows with no costs
        
        cropSolution <- formatSolution[,5:10]# Place costs in Year columns  ##MMZ 02/21/19 edited 
        costbyYear <- matrix(0,m,p)
        for (i in 1:m){
          for (j in 1:p){
            if (cropSolution[i,j]==1){ 
              costbyYear[i,j] = cropSolution[i,p+1]}
          }}  
        
        costSumData <- data.frame(descriptiveData$pomSponsor, costbyYear)
        colnames(costSumData) <- c("Capability_Sponsor",paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4))
        costSumData2 <- costSumData %>%
          group_by(Capability_Sponsor) %>%
          summarize_all(funs(sum))
        yearlyCosts <- costSumData2[,2:6]
        sponsors <- as.matrix(costSumData2[,1],6,1)
        pomSponsorSum <- rowSums(costSumData2[,2:6])
        costTable <<- data.frame(yearlyCosts, pomSponsorSum)
        colnames(costTable) <<- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
        rownames(costTable) <<- sponsors
        
        
        setProgress(0.8)
        Sys.sleep(0.25)
        
        sumYearlyCosts <- colSums(yearlyCosts)
        colnames(sumYearlyCosts)=NULL
        rownames(sumYearlyCosts)=NULL
        sumYearlyCosts2 <- data.frame(t(sumYearlyCosts))
        sumFYDP <- sum(pomSponsorSum)
        sumCostTable <- data.frame(sumYearlyCosts2, sumFYDP)
        colnames(sumCostTable) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
        rownames(sumCostTable) <- "PA Cost"
        
        # Calculate full costs for solution
        vectorSolution <- as.vector(t(exportSolution))
        fullCostMatrix_temp <- matrix(0,p,n)
        costLookup <- M5_LHS
        for (i in 1:p){
          for (j in 1:n){
            fullCostMatrix_temp[i,j] = vectorSolution[j] * costLookup[i,j]}
        }
        
        fullCostMatrix <- rowSums(fullCostMatrix_temp)
        sumFYDP2 <- sum(fullCostMatrix)
        sumFullCostMatrix <- data.frame(t(fullCostMatrix), sumFYDP2)
        colnames(sumFullCostMatrix) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
        rownames(sumFullCostMatrix) <- "Full Cost"
        
        sumBudget <- sum(budget)
        sumBudgetMatrix <- data.frame(t(budget), sumBudget)
        colnames(sumBudgetMatrix) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
        rownames(sumBudgetMatrix) <- "Available"
        
        slack <- (sumBudgetMatrix - sumFullCostMatrix)
        colnames(slack) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
        rownames(slack) <- "Slack"
        
        tempList <- list(sumCostTable, sumFullCostMatrix, sumBudgetMatrix, slack)
        table3Matrix <<- do.call(rbind,tempList)
        
        
        setProgress(0.9)
        Sys.sleep(0.25)
        
        output$solvedMSG <- renderText({"Solution Complete"})
        
        enable <- disable
        
        for (i in 1:length(enable)){
          enable(enable[i])}
        
        show <- hide
        
        for (i in 1:length(show)){
          show(show[i])}
        
        setProgress(1, detail = "")
        
      }) # end of Progress bar
      
    } # end of If Statement
    
  }) # end ObserveEvent load #solve
  
  
  observeEvent(input$save,{
    
    solution_name <- c(names(solution_list),input$solution_name)
    
    
    if (solution_name[length(solution_name)] == ''){
      
      solution_name <- solution_name[-length(solution_name)]
      
      showNotification('Enter Solution Name', type = 'error')} 
    
    else if (any(duplicated(solution_name)) == TRUE){
      
      solution_name <- solution_name[-length(solution_name)]
      
      showNotification('Enter a different solution name', type = 'error')}
    
    else {
      
      solution_list <<- c(solution_list,list(list(mustFundProjects=mustFundProjects,
                                                  descriptiveData=descriptiveData,
                                                  previousProgram = inputData_temp()[,20],
                                                  tableSolution=tableSolution,
                                                  costTable=costTable,
                                                  table3Matrix=table3Matrix,
                                                  geomplotData=geomplotData,
                                                  costValueTable=costValueTable,
                                                  iterationCount=iterationCount,
                                                  projectCount=projectCount,
                                                  projectsAdded=projectsAdded,
                                                  projectsDropped=projectsDropped,
                                                  projectsMoved=projectsMoved,
                                                  objective=objective)))
      
      names(solution_list) <<- solution_name
      
      hide <- c('solution_name','save')
      
      for (i in 1:length(hide)){
        hide(hide[i])}
      
      show('choice')
      
      output$solvedMSG <- renderText({'Solution Saved'})
      
      updateSelectInput(session, 'choice', choice = c('',names(solution_list)))
      
      showNotification('Solution Saved', type = 'message')}
    
  })  
  
  
  observeEvent(input$choice, {
    
    if (input$choice != ''){
      
      ##### Project Statistics #####  
      
      mustFundProjects <- solution_list[[input$choice]]$mustFundProjects
      
      brewer <- 'Set3'
      num <- length(unique(mustFundProjects$pomSponsor))
      
      output$descriptive2 <- renderPlot({
        ggplot(mustFundProjects, aes(x = must_fund_fy, y = tempcosts, fill = pomSponsor)) + 
          ggtitle("Must Fund Costs by Fiscal Year") +
          geom_bar(stat="identity", position = "stack") + scale_y_continuous(labels = dollar) +
          xlab("Must Fund (FY)") + ylab("Cost ($)") +
          scale_fill_manual(values = colorRampPalette(brewer.pal(num, brewer))(num))})
      
      output$descriptive1 <- renderPlot({
        ggplot(mustFundProjects, aes(as.character(must_fund_fy), fill = pomSponsor)) + # JGD 20190301 added as.character to make must_fund_fy categorical 
          ggtitle("Must Fund Project Counts by Fiscal Year") +
          geom_bar(stat="count", position = "stack") +
          xlab("Must Fund (FY)") + ylab("Count")+
          scale_fill_manual(values = colorRampPalette(brewer.pal(num, brewer))(num))})
      
      descriptiveData <- solution_list[[input$choice]]$descriptiveData
      
      feasibleProjects <- descriptiveData %>% filter(netSum > 0)
      
      num <- length(unique(feasibleProjects$pomSponsor))
      
      output$descriptive3 <- renderPlotly({
        
        g <- ggplot() +
          geom_point(data=feasibleProjects, aes(x=tempScores, y=tempcosts, color = pomSponsor,
                                                text = paste('Project: ', feasibleProjects$projectTitle,
                                                             '<br>Number: ', feasibleProjects$projectNumber,
                                                             '<br>Score: ', format(feasibleProjects$tempScores, digits = 3),
                                                             '<br>Cost: $', format(feasibleProjects$tempcosts, big.mark = ",")))) +
          xlab("Score") + ylab("Cost ($)") + labs(colour="") +
          theme(panel.background = element_rect(fill = "lightgray",
                                                colour = "black",
                                                size = 0.5, linetype = "solid")) +## JGD 20190219
          scale_color_manual(values = colorRampPalette(brewer.pal(num, brewer))(num))
        
        # ggplotly(g, tooltip = "text", dynamicTicks = TRUE) %>%
        ggplotly(g, tooltip = "text") %>%
          layout(title = 'Score vs. Cost',
                 yaxis = list(zeroline = FALSE, showgrid = FALSE),
                 xaxis = list(zeroline = FALSE, showgrid = FALSE),
                 font = list(
                   family = "arial",
                   size = 12,
                   color = 'black'),
                 legend = list(x = 1.0, y = 0.6,
                               font = list(
                                 family = "arial",
                                 size = 8,
                                 color = 'black')))}) ## JGD 20190219
      
      previousProgram <- solution_list[[input$choice]]$previousProgram
      
      output$descriptive4 <- renderPlotly({
        
        g <- ggplot() +
          geom_point(data = descriptiveData,
                     aes(x=jitter(previousProgram, factor = 1, amount = NULL),
                         y=jitter(must_fund_fy, factor = 1, amount = NULL),
                         color = pomSponsor
                         ,text = paste('Project: ', projectTitle,
                                       '<br>Number: ', projectNumber,
                                       '<br>Previous Program FY: ', previousProgram,
                                       '<br>Must Fund FY: ', must_fund_fy)
                     )) +
          xlab("Previously Programmed FY") + ylab("Must Fund FY") + labs(colour="") +
          scale_x_continuous(breaks = seq(y, y+4, by = 1)) + xlim(y, y+4) +
          scale_y_continuous(breaks = seq(y, y+4, by = 1)) + ylim(y, y+4) +
          theme(panel.background = element_rect(fill = "lightgray",
                                                colour = "black",
                                                size = 0.5, linetype = "solid"))+
          scale_color_manual(values = colorRampPalette(brewer.pal(num, brewer))(num))
        
        # ggplotly(g, tooltip = "text", dynamicTicks = FALSE)%>%
        ggplotly(g, tooltip = "text")%>%
          layout(title = 'Programmed (POM20) vs Must Fund (POM21)',
                 yaxis = list(zeroline = TRUE, showgrid = FALSE),
                 xaxis = list(zeroline = TRUE, showgrid = FALSE),
                 font = list(
                   family = "arial",
                   size = 12,
                   color = 'black'),
                 legend = list(x = 1.0, y = 0.6,
                               font = list(
                                 family = "arial",
                                 size = 8,
                                 color = 'black')))})
      
      tableSolution <- solution_list[[input$choice]]$tableSolution
      
      ##### MPAT Solution #####  
      
      output$fullSolution <- renderDataTable({
        datatable(tableSolution, options = list(
          order = list(9, 'desc'),  ## 02/22/19 MMZ  changed from 9 to 10 
          pageLength = 20,
          initComplete = JS( #Change table header background color and font color
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")
        ),
        rownames = FALSE, # JGD 20190221 added
        class = "compact",
        caption = "MILCON Projects"
        ) %>% formatCurrency(columns = 'Cost', digits = 0) %>% formatStyle(columns = 'Current', backgroundColor = "khaki")})
      
      #### Capability Sponsor Costs ####
      
      costTable <- solution_list[[input$choice]]$costTable
      table3Matrix <- solution_list[[input$choice]]$table3Matrix
      
      output$table2 <- renderDataTable({
        datatable(costTable, options = list(
          order = list(6, 'desc')), 
          # class = "compact",
          caption = "MILCON Costs (PA) by Capability Sponsor"
        ) %>% formatCurrency(columns = c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4),'FYDP'), digits = 0)})
      
      output$table3 <- renderDataTable({
        datatable(table3Matrix, 
                  # class = "compact",
                  caption = "MILCON Cost Summary"
        ) %>% formatCurrency(columns = c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4),'FYDP'), digits = 0)})
      
      #### Capability Sponsors ####
      geomplotData <- solution_list[[input$choice]]$geomplotData
      
      output$plot1 <- renderPlot({  
        
        num <- length(unique(geomplotData$Capability_Sponsor))
        
        ggplot(geomplotData, aes(x = Current, y = Cost, fill = Capability_Sponsor))+ 
          ggtitle("Total MILCON Costs by Fiscal Year")+
          geom_bar(stat="identity", position = "stack")+scale_y_continuous(labels = dollar) + 
          scale_fill_manual(values = colorRampPalette(brewer.pal(num,'Set3'))(num))})
      
      
      #### MIS Programs ####
      
      output$plot2 <- renderPlot({
        
        sponsor_list <- sort(unique(geomplotData$Capability_Sponsor))
        m_list <- c()
        mis_program_list <- c()
        # grabs the max Cost
        for (i in 1:length(sponsor_list)){
          
          m_list <- c(m_list,sum(geomplotData$Cost[geomplotData$Capability_Sponsor == sponsor_list[i]]))
          
          mis_program_list <- c(mis_program_list,length(unique(geomplotData$MIS_Program[geomplotData$Capability_Sponsor == sponsor_list[i]])))}
        
        m <- max(m_list)
        
        num <- max(mis_program_list)
        
        for (i in 1:length(sponsor_list)){
          
          chop <- geomplotData[geomplotData$Capability_Sponsor == sponsor_list[i],]
          
          
          plot <- ggplot(chop
                         , aes(x = Capability_Sponsor, y = Cost, fill = MIS_Program))+
            geom_bar(stat = "identity", position = "stack")+scale_y_continuous(labels = dollar)+coord_flip(ylim = c(0,m))+
            scale_fill_manual(values = colorRampPalette(brewer.pal(8, 'Accent'))(num)[mis_program_list[i]:1]) +
            theme(axis.title = element_blank(), axis.title.x = element_blank(), legend.position = 'bottom')
          
          if (i == 1){
            plot_list <- ggplotGrob(plot + ggtitle("Total MILCON Costs (PA) by Capability Sponsor"))}
          else {
            plot_list <- rbind(plot_list, ggplotGrob(plot), size = 'first')}
        }  
        
        grid::grid.draw(plot_list)})
      
      
      #### Project Counts ####
      
      fundedCount <- nrow(geomplotData)
      countColumn <- matrix(1,fundedCount,1)
      geomplotData2 <- data.frame(geomplotData, countColumn) # add count column
      colnames(geomplotData2) <- c("MIS_Program","Capability_Sponsor", "Cost", "Current", "Count")
      geomplotData3 <- ddply(geomplotData2, c("Capability_Sponsor", "Current"), .drop = FALSE, summarise, Count = sum(Count))
      geomplotData4 <- geomplotData3[geomplotData3[,2] != "-----",] 
      
      output$plot3 <- renderPlotly({
        
        num <- length(levels(geomplotData4$Current))
        
        plotly::plot_ly(geomplotData4, x = ~Capability_Sponsor, y = ~Count, type = 'bar'
                        , color = ~Current, colors = brewer.pal(num, 'Paired')) %>%
          layout(barmode = 'group')})
      
      #### Cost-Value ####
      
      costValuePlot <- solution_list[[input$choice]]$costValuePlot
      
      output$costValuePlot <- renderPlotly({
        plot_ly(data = costValueTable, x = ~Value.Score, y = ~Cost,
                text = ~paste("Number: ", Project.Number, "<br>Title: ", Project.Title, "<br>Sponsor: ", POM.Sponsor),
                marker = list(size = 10,
                              color = 'rgba(26, 83, 255, .8)',
                              line = list(color = 'rgba(0, 26, 102, .8)',
                                          width = 2))) %>%
          layout(title = 'Value Score vs. Cost of Selected Projects',
                 yaxis = list(zeroline = FALSE),
                 xaxis = list(zeroline = FALSE))})
      
      output$exportAll <- downloadHandler(
        filename = function(){
          paste0("solution_", Sys.Date(), '.zip')},
        
        content = function(file){
          
          files <- NULL
          
          for (i in 1:length(solution_list)){
            
            tableSolution <- solution_list[[as.numeric(i)]]$tableSolution
            
            fileName <- paste0(names(solution_list)[i],'_',Sys.Date(),'.csv')
            write.table(tableSolution, fileName, sep = ',', row.names = FALSE)
            files <- c(fileName, files)}
          
          zip(file, files)})
      
      output$export <- downloadHandler(
        filename = function(){
          paste(input$choice, "_", Sys.Date(), '.csv', sep = '')},
        content = function(file){
          write.csv(tableSolution, file, row.names = FALSE)})
      
      show <- c(showplot,showcontrols)
      
      for (i in 1:length(show)){
        show(show[i])}
      
    }
    
    
  })
  
  observeEvent(input$remove,{
    
    solution_list <<- solution_list[-which(names(solution_list) == input$choice)]
    
    updateSelectInput(session, 'choice', choice = c('',names(solution_list)))
    
    storeWarn <- getOption('warn')
    
    options(warn = -1)
    
    output$descriptive1 <- renderPlot({})
    output$descriptive2 <- renderPlot({})
    output$descriptive3 <- renderPlotly({plotly_empty()})
    output$descriptive4 <- renderPlotly({plotly_empty()})
    
    output$fullSolution <- renderDataTable({})
    output$table2 <- renderDataTable({})
    output$table3 <- renderDataTable({})
    
    output$plot1 <- renderPlot({})
    output$plot2 <- renderPlot({})
    output$plot3 <- renderPlotly({plotly_empty()})
    
    output$costValuePlot <- renderPlotly({plotly_empty()})
    
    shinyjs::delay(100, expr = ({options(warn = storeWarn)}))
    
    hide <- hide <- c(showplot[c('plot1','plot2','descriptive1','descriptive2')])
    
    for (i in 1:length(hide)){
      hide(hide[i])
    }
    
    if (length(solution_list) == 0){
      
      hide <- showcontrols
      
      for (i in 1:length(hide)){
        hide(hide[i])
      }
      
      showNotification('All Solutions Removed')}  
    
    else {showNotification(paste('Solution',input$choice,'removed'))}
    
  })
  
  observeEvent(input$removeAll,{
    
    solution_list <<- list()
    updateSelectInput(session, 'choice', choice = c('',names(solution_list)))
    
    storeWarn <- getOption('warn')
    
    options(warn = -1)
    
    output$descriptive1 <- renderPlot({})
    output$descriptive2 <- renderPlot({})
    output$descriptive3 <- renderPlotly({plotly_empty()})
    output$descriptive4 <- renderPlotly({plotly_empty()})
    
    output$fullSolution <- renderDataTable({})
    output$table2 <- renderDataTable({})
    output$table3 <- renderDataTable({})
    
    output$plot1 <- renderPlot({})
    output$plot2 <- renderPlot({})
    output$plot3 <- renderPlotly({plotly_empty()})
    
    output$costValuePlot <- renderPlotly({plotly_empty()})
    
    shinyjs::delay(100, expr = ({options(warn = storeWarn)}))
    
    hide <- c(showplot[c('plot1','plot2','descriptive1','descriptive2')], showcontrols)
    
    for (i in 1:length(hide)){
      hide(hide[i])}
    
    showNotification('All Solutions Removed')})
  
  # # This command resets the App when the reset button is pressed.
  # observeEvent(input$reset, {
  #   print("reseting")
  #   session$reload()
  # })
  
  # # Stop the App from running after the browser is closed
  session$onSessionEnded(function(){
    stopApp()
  })
  
  
  
} # end Server

# shinyApp(ui = ui, server = server)