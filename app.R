# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(seacarb)

ui <- fluidPage(
  tags$h1("Interactive error-space diagram for the marine CO2 system"),

  tags$p("Implements error function, allowing user to change various input values:"),

  tags$p('errors(flag, var1, var2, S=35, T=25, Patm=1, P=0, Pt=0, Sit=0, 
              evar1=0, evar2=0, eS=0.01, eT=0.01, ePt=0, eSit=0, 
              epK=c(0.002, 0.01, 0.02, 0.01, 0.01, 0.02, 0.02)'),

  sidebarLayout(
    sidebarPanel(
      # conditional menu to determine output var list given an input pair
      selectInput(inputId="flag", label="Input pair (var1, var2)", 
                  c("ALK and DIC" = "15",
                    "pH and ALK" = "8",
                    "pH and DIC" = "9",
                    "pCO2 and pH" = "21",
                    "pCO2 and ALK" = "24", #Error: f() values at end points not of opposite sign
                    "pCO2 and DIC" = "25"
                  ),
                  selected = "15", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),

      # CONDITIONAL CHECK FOR FLAG 15 (ALK and DIC)
      conditionalPanel(
        condition = "input.flag == '15'", #full output var list

        # Output variable list
        selectInput(inputId="outvar_flag15", label="Output variable", 
                  c("H+" = "H",
                    "pCO2" = "pCO2",
                    "CO3^2-" = "CO3",
                    "CO2*" = "CO2",
                    "HCO3-" = "HCO3",
                    "OmegaCalcite" = "OmegaCalcite",
                    "OmegaArgonite" = "OmegaAragonite"
                  ),
                  selected = "CO3", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
        
        # Input pair values
        fluidRow(
          column(5, 
            textInput(inputId = "var1_flag15",
              label = "Alkalinity [umol/kg]",
              value = 2295
            )
          ),

          column(6,
            textInput(inputId = "var2_flag15",
              label = "Dissolved inorganic C [umol/kg]",
              value = 2155
            )
          )
         ) #./fluidRow
      ), #./conditionalPanel
    
      # CONDITIONAL CHECK FOR FLAG 8 (pH and ALK)
      conditionalPanel(
        condition = "input.flag == '8'", #full output var list

        # Output variable list
        selectInput(inputId="outvar_flag8", label="Output variable", 
                  c("H+" = "H",
                    "pCO2" = "pCO2",
                    "CO3^2-" = "CO3",
                    "CO2*" = "CO2",
                    "HCO3-" = "HCO3",
                    "OmegaCalcite" = "OmegaCalcite",
                    "OmegaArgonite" = "OmegaAragonite"
                  ),
                  selected = "CO3", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
        
        # Input pair values
        fluidRow(
          column(5, 
            textInput(inputId = "var1_flag8",
              label = "pH",
              value = 8.1
            )
          ),

          column(6,
            textInput(inputId = "var2_flag8",
              label = "Alkalinity [umol/kg]",
              value = 2295
            )
          )
         ) #./fluidRow
      ), #./conditionalPanel
    
      # CONDITIONAL CHECK FOR FLAG 9 (pH and DIC)
      conditionalPanel(
        condition = "input.flag == '9'", #full output var list

        # Output variable list
        selectInput(inputId="outvar_flag9", label="Output variable", 
                  c("H+" = "H",
                    "pCO2" = "pCO2",
                    "CO3^2-" = "CO3",
                    "CO2*" = "CO2",
                    "HCO3-" = "HCO3",
                    "OmegaCalcite" = "OmegaCalcite",
                    "OmegaArgonite" = "OmegaAragonite"
                  ),
                  selected = "CO3", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
        
        # Input pair values
        fluidRow(
          column(5, 
            textInput(inputId = "var1_flag9",
              label = "pH",
              value = 8.1
            )
          ),

          column(6,
            textInput(inputId = "var2_flag9",
              label = "Dissolved inorganic C [umol/kg]",
              value = 2155
            )
          )
         ) #./fluidRow
      ), #./conditionalPanel

      # CONDITIONAL CHECK FOR FLAG 21 (pCO2 and pH)
      conditionalPanel(
        condition = "input.flag == '21'", #exclude pCO2

        # Output variable list
        selectInput(inputId="outvar_flag21", label="Output variable", 
                  c("H+" = "H",
                    "CO3^2-" = "CO3",
                    "CO2*" = "CO2",
                    "HCO3-" = "HCO3",
                    "OmegaCalcite" = "OmegaCalcite",
                    "OmegaArgonite" = "OmegaAragonite"
                  ),
                  selected = "CO3", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
        
        # Input pair values
        fluidRow(
          column(5, 
            textInput(inputId = "var1_flag21",
              label = "pCO2 [uatm]",
              value = 330.5
            )
          ),

          column(6,
            textInput(inputId = "var2_flag21",
              label = "pH",
              value = 8.1
            )
          )
         ) #./fluidRow
      ), #./conditionalPanel

      # CONDITIONAL CHECK FOR FLAG 24 (pCO2 and ALK)
      conditionalPanel(
        condition = "input.flag == '24'", #exclude pCO2

        # Output variable list
        selectInput(inputId="outvar_flag24", label="Output variable", 
                  c("H+" = "H",
                    "CO3^2-" = "CO3",
                    "CO2*" = "CO2",
                    "HCO3-" = "HCO3",
                    "OmegaCalcite" = "OmegaCalcite",
                    "OmegaArgonite" = "OmegaAragonite"
                  ),
                  selected = "CO3", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
        
        # Input pair values
        fluidRow(
          column(5, 
            textInput(inputId = "var1_flag24",
              label = "pCO2 [uatm]",
              value = 330.5
            )
          ),

          column(6,
            textInput(inputId = "var2_flag24",
              label = "Alkalinity [umol/kg]",
              value = 2295
            )
          )
         ) #./fluidRow
      ), #./conditionalPanel
                

      textOutput("result"),
      tags$br(),

     
      fluidRow(
        column(3,
          textInput(inputId = "salt",
            label = "Salinity (psu)",
            value = 35
          )
        ),

         column(3,
          textInput(inputId = "temp",
            label = "Temperature (C)",
            value = -0.49
          )
        )

      ), #./fluidRow

      fluidRow(
        column(3, 
          textInput(inputId = "pressure",
            label = "Pressure (dbars)",
            value = 0 #pass as dbar
          )
        ),

        column(4,
          textInput(inputId = "phos",
            label = "[Phosphate] (umol/kg)",
            value = 2  #2.e-6 mol/kg
          )
        ),

        column(4, 
          textInput(inputId = "sil",
            label = "[Silicon] (umol/kg)",
            value = 60 #60.e-6 mol/kg
          )
        )
      ) #./fluidRow

      # sliderInput(inputId = "evar1",
      #   label = "Max error in first var (must be > 0):",
      #   value = 0, min = 0, max = 20
      # )


    ), #./sidebarPanel

    mainPanel(
      fluidRow(
        column( 5, plotOutput("erspace", width = "500px", height = "500px") )
      )      
    ) #./mainPanel



  ) #./sidebarLayout

 


) # ./fluidPage

server <- function(input, output) {
  output$result <- renderText({
    paste("flag value:", input$flag)
  })

  # ===================================================================
  # Define function sources
  source("/homel/cnangini/PROJECTS/seacarb-git/R/errhalf.R")
  source("/homel/cnangini/PROJECTS/seacarb-git/R/errmid.R")
  source("/homel/cnangini/PROJECTS/seacarb-git/R/errors.R")
  source("/homel/cnangini/PROJECTS/seacarb-git/R/derivnum.R")



  # Default uncertainties in equilibrium constants 
  # (pK0, pK1, pK2, pKb, pKw, pKa, pKc, Bt)
  # ------------------------------------------------
  epKstd  <- c(0.004, 0.015,  0.03, 0.01,  0.01, 0.02, 0.02, 0.01)



  # ===================================================================
  # Functions

  # Function to strip NaNs and add a 0 at beginning of a vector
  zenon <- function(x){
    # Get rid of NaN's
    x <- c(0.0, x[!is.na(x)]) 
    return(x)
  }

  # Function to make plot
  plterrcontour <- function(sigcritXa, sigcritYa, xlab, ylab, subtitle, xlim, ylim,
                           sighpXa, sighpYa,
                           sigmidXa, sigmidYa,
                           sigsoaX, sigsoaY,
                           x, y, za, levels1,
                           methoda='flattest'){
    
    # Region 'a' is Southern Ocean  ;  Region 'b' is Tropics:
    # 'contour' command must come after any 'plot', 'lines', or 'points' commands 
    plot (sigcritXa, sigcritYa, xlab=xlab, ylab=ylab, plot.title=title(main=subtitle),
          xlim=xlim, ylim=ylim, col="black", type='l', lwd=4, lty='solid',xaxt='n',yaxt='n',
          cex.lab=1.4, ann=F, xaxs='r', yaxs='r')
    # Plot critical point
    if ( is.null(sighpXa) ) {
      points(sigcritXa[11], sigcritYa[11], col="black", cex=1.7, pch=19)
    } else {
      points(sighpXa, sighpYa, col="black", cex=1.7, pch=19)
    }    
    # Plot mid-line (where e1 = e2 on each isoline [and between isolines])
    lines(sigmidXa, sigmidYa, col="black", lwd=1, lty='solid')
  
    # Plot point(s) at state-of-the-art (soa) for absolute error in each member of the input pair
    # - pH has 2 points (random & overall error); other 3 input variables only have 1 point
    points(sigsoaX, sigsoaY, col="black", cex=1.7, pch=4)
    axis(1,labels=TRUE, tcl=-0.35, cex.axis=1.3) 
    axis(2, tcl=-0.35, labels=FALSE, las=0) 
    title(xlab = xlab, cex.lab = 1.4, line = 2.9)
    title(ylab = ylab, cex.lab = 1.4, line = 1.9)

    # Add 2 colored text strings near origin to indicate the propagated error at the origin (from constants only)
    a0 <- za[1,1]
    if (a0 < 1) {ndiga <- 2} else {ndiga <- 3}
    mtext( format(a0, digits=ndiga), side=1, line=2.5, at=0, srt=0,  cex=1.05, col='blue')

    # 'contour' command MUST come after any 'plot', 'lines', or 'points' commands 
    # For 'method', use 'simple, edge or flattest (flattest looks best but seems to fail often when lines are vertical)
    contour(x, y , za, levels=levels1, method=methoda, col="blue", lty=1, lwd=2, 
              yaxt='n', , labcex=1.0, add=TRUE)
    axis(2, tcl=-0.35, labels=FALSE) 

    axis(2, tcl=-0.35, labels=FALSE, las=0) ; 
    axis(2, lwd=0, line=-0.4, las=2, labels=TRUE, cex.axis=1.4)
    axis(3,labels=FALSE,tcl=-0.35) ; axis(4,labels=FALSE,tcl=-0.25)
    done <- c('error contour plot')
  
  }

  # ---------------------------------------------------------------------
  # Calculate and render plot based on user selections
  output$erspace <- renderPlot({

    # ===================================================================
    # Define input vars and their uncertainties
    # Specify flag & corresponding 2 input variables

    # Input variables:
    # ----------------
    # Approximate regional mean for Southern Ocean (c.f. Fig. 3.2 [Orr, 2011])
    menu_flag <- as.numeric(input$flag)
    menu_salt <- as.numeric(input$salt)
    menu_temp <- as.numeric(input$temp)
    menu_pressure <- as.numeric(input$pressure) /10  #convert dbars to bars
    menu_phos <- as.numeric(input$phos) * 1e-6 #convert umol/kg to mol/kg
    menu_sil <- as.numeric(input$sil) * 1e-6 #convert umol/kg to mol/kg

    # Define output variable based on conditional menu selection
    conditional_outvar <- reactive({switch(
      input$flag,
      "1" = as.character(input$outvar_flag1),
      "8" = as.character(input$outvar_flag8),  #full list
      "9" = as.character(input$outvar_flag9),  #full list
      "15" = as.character(input$outvar_flag15),  #full list
      "21" = as.character(input$outvar_flag21),  #exclude pCO2
      "24" = as.character(input$outvar_flag24),  #exclude pCO2
      "25" = as.character(input$outvar_flag25)  #exclude pCO2
    )})
    menu_outvar <- conditional_outvar()
    print("menu_outvar:")
    print(menu_outvar)

    # CONDITIONAL CHECK OVER ALL INPUT FLAGS TO DEFINE:
    # var1, var1_e, var1_e_soa, var1_e_soa2
    # var2, var2_e, var2_e_soa, var2_e_soa2
    if (input$flag == "15") { #var1=ALK, var2=DIC
      menu_var1 <- as.numeric(input$var1_flag15) * 1e-6 #convert umol/kg to mol/kg
      menu_var2 <- as.numeric(input$var2_flag15) * 1e-6 #convert umol/kg to mol/kg

      # uncertainties in input variables var1 and var2
      var1_e <- seq(0., 20., 1.0) * 1e-6
      var2_e <- var1_e

      # state-of-art errors for vars (c.f. Orr et al. 2017, Table 1)
      # (to be plotted as crosses in error-space diagram)
      var1_e_soa   <- 2 #umol/kg
      var2_e_soa   <- 2 #umol/kg

      var1_e_soa2  <- c(var1_e_soa, var1_e_soa)
      var2_e_soa2  <- c(var2_e_soa, var2_e_soa)


      # data arrays for plot
      xdata <- var2_e*1e+6  ;  ydata <- var1_e*1e+6
      xlim <- c(0,20)  ; ylim <- xlim
      levels1 <- c(1,seq(2,20,by=2))
      xlabel <- expression(paste(sigma[italic("C")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
      ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))

    } else if (input$flag == "8") { #var1=pH, var2=ALK
      menu_var1 <- as.numeric(input$var1_flag8)
      menu_var2 <- as.numeric(input$var2_flag8) * 1e-6 #convert umol/kg to mol/kg
      
      # Uncertainties in input variables
      var1_e <- seq(0,0.03,0.0015)
      var2_e <- seq(0., 20., 1.0) * 1e-6
      
      var1_e_soa   <- c(0.003, 0.01) #pH
      var2_e_soa   <- 2 #umol/kg
      
      var1_e_soa2  <- var1_e_soa
      var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      
      # for plot
      xdata <- var1_e           ;  ydata <- var2_e * 1e+6
      xlim <- c(0,0.03) ; ylim <- c(0,20) 
      levels1 <- c(4.2, seq(4,7,by=1))
      xlabel <- expression(paste(sigma[pH]," (total scale)",sep=""))
      ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
    
    } else if (input$flag == "9") { #var1=pH, var2=DIC
      menu_var1 <- as.numeric(input$var1_flag9)
      menu_var2 <- as.numeric(input$var2_flag9) * 1e-6 #convert umol/kg to mol/kg
      
      # Uncertainties in input variables
      var1_e <- seq(0,0.03,0.0015)
      var2_e <- seq(0., 20., 1.0) * 1e-6
      
      var1_e_soa   <- c(0.003, 0.01) #pH
      var2_e_soa   <- 2 #umol/kg
      
      var1_e_soa2  <- var1_e_soa
      var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      
      # for plot
      xdata <- var1_e           ;  ydata <- var2_e * 1e+6
      xlim <- c(0,0.03) ; ylim <- c(0,20) 
      levels1 <- c(4.5, seq(1,20,by=1))
      xlabel <- expression(paste(sigma[pH]," (total scale)",sep=""))
      ylabel <- expression(paste(sigma[italic("C")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
    
    } else if (input$flag == "21") { #var1=pCO2, var2=pH
      menu_var1 <- as.numeric(input$var1_flag21)
      menu_var2 <- as.numeric(input$var2_flag21)
      
      # Uncertainties in input variables
      var1_e <- seq(0,20,1)
      var2_e <- seq(0,0.03,0.0015)
      
      var1_e_soa   <- 2
      var2_e_soa   <- c(0.003, 0.01)
      
      var1_e_soa2  <- c(var1_e_soa, var1_e_soa)
      var2_e_soa2  <- var2_e_soa
      
      # for plot
      xdata <- var2_e           ;  ydata <- var1_e * 1e+6
      xlim <- c(0,0.03)  ; ylim <- c(0,20)
      levels1 <- c(5.4, 7, seq(0,20,by=2))
      xlabel <- expression(paste(sigma[pH]," (total scale)",sep=""))
      ylabel <- expression(paste(sigma[pCO[2]]," (",mu,"atm",")",sep=""))

    } else if (input$flag == "24") { #var1=pCO2, var2=ALK
      menu_var1 <- as.numeric(input$var1_flag24)
      menu_var2 <- as.numeric(input$var2_flag24)
      
      # Uncertainties in input variables
      var1_e <- seq(0,20,1)
      var2_e <- seq(0., 20., 1.0) * 1e-6
      
      var1_e_soa   <- 2
      var2_e_soa   <- 2
      
      var1_e_soa2  <- c(var1_e_soa, var1_e_soa)
      var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      
      # for plot
      xdata <- var1_e ; ydata <- var2_e*1e+6
      xlim <- c(0,20)  ; ylim <- xlim
      levels1 <- seq(3,7,by=0.5)
      xlabel <- expression(paste(sigma[pCO[2]]," (",mu,"atm",")",sep=""))
      ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
    }

    # Uncertainties in input variables
    # ---------------------------------
    # pCO2_e <- seq(0,20,1)
    # pH_e   <- seq(0,0.03,0.0015)

    # salt_e = 0.01   
    # temp_e = 0.01   

    # Pt_e = 0.1e-6
    # Sit_e = 4.0e-6

    # ===================================================================
    # Compute derived carbonate system vars with  seacarb routine carb
    # (Southern Ocean)
    print("Running carb function:")
    vars <- carb  (flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, 
                   T=menu_temp, Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil, 
                   k1k2='w14', kf='dg', ks="d", pHscale="T", 
                   b="u74", gas="potential", warn='n')

    pH <- vars$pH
    pCO2 <- vars$pCO2

    # Compute H+ from pH, i.e., pH = -log10[H+], where [H+] is the hydrogen ion concentration in mol/kg
    H = 10^(-1*vars$pH)

    # Keep only key columns to be consistent with output from 'errors.R' routine (called below)
    vars <- data.frame(H, vars[,c('pH','CO2','fCO2','pCO2','HCO3','CO3','OmegaAragonite','OmegaCalcite')] )

    # Duplicate rows in *vars* until same as number of members of error vector var1_e
    numerrs <- length(var1_e)
    vars <- vars[rep(row.names(vars), numerrs), ]

    # print(as.numeric(vars))
    print( dim(vars) )  #[1] 21  9 CORRECT
    
    # ===================================================================
    # Use 1-D error vectors to build 2-D error array (to plot contours in DIC-ALK space)
    if (menu_flag == 15) dat <- expand.grid(var2_e, var1_e)
    else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 21 || menu_flag == 24 || menu_flag == 25) {
        dat <- expand.grid(var1_e,  var2_e)
    }

    # ===================================================================
    # Compute derived vars and their errors
    # At-Ct pair only (flag=15)

    print("Running carb function again:")

    vars <- carb (flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp, 
                  Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil, 
                  k1k2='w14', kf='dg', ks="d", pHscale="T", 
                  b="u74", gas="potential", warn='n')

    print( dim(vars) )  #[1]  1 19 CORRECT

    H = 10^(-1*vars$pH)         # H+ concentration (mol/kg)
    vars <- data.frame(H, vars) # Add H+ as new column to vars data frame

    print("Calculating absEt:")

    # Absolute errors: propagated uncertainties
    if (menu_flag == 15) {
      dat_evar1 <- dat$Var2           ;  dat_evar2 <- dat$Var1
    } else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 21 || menu_flag == 24) {
      dat_evar1 <- dat$Var1           ;  dat_evar2 <- dat$Var2
    }
    absEt <- errors (flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp,
                    Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil, 
                    evar1=dat_evar1, evar2=dat_evar2, 
                    eS=0, eT=0, ePt=0, eSit=0, epK=epKstd,
                    k1k2='w14', kf='dg', ks="d", pHscale="T",
                    b="u74", gas="potential", warn='no')

    # Keep only key columns in vars for consistency with columns in absEt
    vars <- vars[,colnames(absEt)]

    # Duplicate rows in *vars* until same as number of members of error vector var1_e
    numerrs <- length(dat$Var1)
    vars <- vars[rep(row.names(vars), numerrs), ]

    print( dim(vars) ) #[1] 441   9 CORRECT

    #Relative errors (in percent)
    relEt <- 100* absEt / vars      #Total relative error (from constants and other input vars)

    # ===================================================================
    # Define simpler names for changes in variables

    er_outvar = relEt[[menu_outvar]]


   

    # ===================================================================
    # Compute other parts of error-space diagrams

    # Constants-pair CURVE (propagated error from constants = that from input pair)
    # (Southern Ocean)
    print("Calculating constants-pair curve (errhalf fn):")
    errcirc <- errhalf(flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp, 
                       Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil,
                       epK=epKstd,
                       k1k2='l', kf='dg', ks="d", pHscale="T", 
                       b="u74", gas="potential", warn="n")  

    # Balanced-pair LINE (input pair members contrbute equally to propagated error)
    # At-Ct pair (Southern Ocean)
    print("Calculating balanced-pair line (errmid fn):")
    sigyspct <- seq(0,20,by=0.1) # in percent
    errm <- errmid(flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp,
                  Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil,
                  sigyspct, epK=epKstd,
                  k1k2='l', kf='dg', ks="d", pHscale="T", 
                  b="u74", gas="potential", warn="n")
    # NB: Warning in sqrt(0.5 * (sigmay^2 - eKall^2)/dd1^2) : production de NaN
    # et: Warning in sqrt(0.5 * (sigmay^2 - eKall^2)/dd2^2) : production de NaN

    # Add scale factors if necessary
    if (menu_flag == 15 ) {
      sig1   <- data.frame(errcirc[1]) * 1e+6
      sig2   <- data.frame(errcirc[2]) * 1e+6

      sigm1   <- data.frame(errm[1]) * 1e+6
      sigm2   <- data.frame(errm[2]) * 1e+6
    } else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 24) {
      sig1   <- data.frame(errcirc[1]) #this is a pH
      sig2   <- data.frame(errcirc[2]) * 1e+6

      sigm1   <- data.frame(errm[1]) #this is a pH
      sigm2   <- data.frame(errm[2]) * 1e+6
    } else if (menu_flag == 21) {
      sig1   <- data.frame(errcirc[1]) #pCO2
      sig2   <- data.frame(errcirc[2]) #pH

      sigm1   <- data.frame(errm[1]) #pCO2
      sigm2   <- data.frame(errm[2]) #pH
    }

    

    # ===================================================================
    # Error-space diagram of relative error in CO3 for At-Ct input pair
    dim(er_outvar) <- c(length(var2_e), length(var1_e))
    
    subtitle <-paste("Output variable", menu_outvar, sep=" ")
    
    
    
    za <- er_outvar

    if (menu_flag == 15 || menu_flag == 21) {
      sigcritXa <- sig2[[menu_outvar]]  ;  sigcritYa <- sig1[[menu_outvar]]  #xdata; ydata

      plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,                     
                     NULL, NULL,
                     zenon(sigm2[[menu_outvar]]), zenon(sigm1[[menu_outvar]]),
                     var2_e_soa2, var1_e_soa2,
                     xdata, ydata, za, levels1,
                     'flattest')
    } else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 24) {
      sigcritXa <- sig1[[menu_outvar]]  ;  sigcritYa <- sig2[[menu_outvar]]  #xdata; ydata

      plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,                     
                     NULL, NULL,
                     zenon(sigm1[[menu_outvar]]), zenon(sigm2[[menu_outvar]]),
                     var1_e_soa2, var2_e_soa2,
                     xdata, ydata, za, levels1,
                     'flattest')

    }


  })



}

shinyApp(ui = ui, server = server)