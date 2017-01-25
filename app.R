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
      checkboxInput("smooth", "Smooth"),
      conditionalPanel(
        condition = "input.smooth == true",
        selectInput("smoothMethod", "Method",
                    list("lm", "glm", "gam", "loess", "rlm"))
      ),

      # conditional menu
      selectInput(inputId="invar", label="invar", 
                  c("A and B" = 15,
                    "C and D" = 1),
                  selected = "C and D", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),

      conditionalPanel(
        condition = "input.invar == 1",
        selectInput("pickOutputs_flag1", "Outvar_flag1",
                    list("a", "b", "c", "d", "e"))
      ),
      conditionalPanel(
        condition = "input.invar == 15",
        selectInput("pickOutputs_flag15", "Outvar_flag15",
                    list("asiago", "pecorino"))
      ),



      selectInput(inputId="flag", label="Input pair (var1, var2)", 
                  c("ALK and DIC" = 15,
                    "pH and CO2" = 1,
                    "CO2 and HCO3" = 2,
                    "CO2 and CO3" = 3,
                    "CO2 and ALK" = 4,
                    "CO2 and DIC" = 5,
                    "pH and HCO3" = 6,
                    "pH and CO3" = 7,
                    "pH and ALK" = 8,
                    "pH and DIC" = 9,
                    "HCO3 and CO3" = 10,
                    "HCO3 and ALK" = 11,
                    "HCO3 and DIC" = 12,
                    "CO3 and ALK" = 13,
                    "CO3 and DIC" = 14,
                    "pCO2 and pH" = 21,
                    "pCO2 and HCO3" = 22,
                    "pCO2 and CO3" = 23,
                    "pCO2 and ALK" = 24,
                    "pCO2 and DIC" = 25
                  ),
                  selected = "ALK and DIC", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL
      ),

        selectInput(inputId="outvar", label="Output variable", 
                  c("H+" = "H",
                    "pCO2" = "pCO2",
                    "CO3^2-" = "CO3",
                    "CO2*" = "CO2",                    
                    "HCO3-" = "HCO3",                    
                    "OmegaCalcite" = "OmegaCalcite",
                    "OmegaArgonite" = "OmegaAragonite"
                  ),
                  selected = "CO3", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL
      ),

      textOutput("result"),
      tags$br(),

      fluidRow(
        column(3, 
          textInput(inputId = "var1",
            label = "var1 (umol/kg)",
            value = 2295 #pass as umol/kg
          )
        ),

        column(3,
          textInput(inputId = "var2",
            label = "var2 (umol/kg)",
            value = 2155 #pass as umol/kg
          )
        ),

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
    menu_var1 <- as.numeric(input$var1) * 1e-6 #convert umol/kg to mol/kg
    menu_var2 <- as.numeric(input$var2) * 1e-6 #convert umol/kg to mol/kg
    menu_salt <- as.numeric(input$salt)
    menu_temp <- as.numeric(input$temp)
    menu_pressure <- as.numeric(input$pressure) /10  #convert dbars to bars
    menu_phos <- as.numeric(input$phos) * 1e-6 #convert umol/kg to mol/kg
    menu_sil <- as.numeric(input$sil) * 1e-6 #convert umol/kg to mol/kg
    menu_outvar <- as.character(input$outvar)

    # temp = -0.49    #C
    # salt = 33.96    #psu
    # press = 0       #bar
    # Phos = 2.e-6    #(mol/kg)
    # Sil = 60.e-6    #(mol/kg)

    # Uncertainties in input variables
    # ---------------------------------
    ALK_e <- seq(0., 20., 1.0) * 1e-6
    DIC_e <- ALK_e
    pCO2_e <- seq(0,20,1)
    pH_e   <- seq(0,0.03,0.0015)

    salt_e = 0.01   
    temp_e = 0.01   

    Pt_e = 0.1e-6
    Sit_e = 4.0e-6

    # ===================================================================
    # Compute derived carbonate system vars with  seacarb routine carb
    # (Southern Ocean)
    print("Running carb function:")
    vars <- carb  (flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp, Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil, 
                        k1k2='w14', kf='dg', ks="d", pHscale="T", 
                        b="u74", gas="potential", warn='n')

    pH <- vars$pH
    pCO2 <- vars$pCO2

    # Compute H+ from pH, i.e., pH = -log10[H+], where [H+] is the hydrogen ion concentration in mol/kg
    H = 10^(-1*vars$pH)

    # Keep only key columns to be consistent with output from 'errors.R' routine (called below)
    vars <- data.frame(H, vars[,c('pH','CO2','fCO2','pCO2','HCO3','CO3','OmegaAragonite','OmegaCalcite')] )

    # Duplicate rows in *vars* until same as number of members of error vector ALK_e
    numerrs <- length(ALK_e)
    vars <- vars[rep(row.names(vars), numerrs), ]

    # print(as.numeric(vars))
    print( dim(vars) )  #[1] 21  9 CORRECT
    
    # ===================================================================
    # Use 1-D error vectors to build 2-D error array (to plot contours in DIC-ALK space)
    dat <- expand.grid(DIC_e, ALK_e)

    # Define state-of-art errors for vars (c.f. Orr et al. 2017, Table 1)
    # (to be plotted as crosses in error-space diagram)
    ALK_e_soa   <- 2 #umol/kg
    DIC_e_soa   <- 2 #umol/kg

    ALK_e_soa2  <- c(ALK_e_soa, ALK_e_soa)
    DIC_e_soa2  <- c(DIC_e_soa, DIC_e_soa)

    ALK_e_soa2
    DIC_e_soa2

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

    print("Calculating absEt and absEk:")

    # Absolute errors: propagated uncertainties
    absEt <- errors (flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp,
                    Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil, 
                    evar1=dat$Var2, evar2=dat$Var1, 
                    eS=0, eT=0, ePt=0, eSit=0, epK=epKstd,
                    k1k2='w14', kf='dg', ks="d", pHscale="T",
                    b="u74", gas="potential", warn='no')

    # Aboslute error from constants only (all other input errors assumed to be zero)
    absEk <- errors  (flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp, 
                     Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil, 
                     evar1=0, evar2=0, 
                     eS=0, eT=0, ePt=0, eSit=0, epK=epKstd,
                     k1k2='w14', kf='dg', ks="d", pHscale="T",
                     b="u74", gas="potential", warn='n')

    # Keep only key columns in vars for consistency with columns in absEt
    vars <- vars[,colnames(absEt)]

    # Duplicate rows in *vars* until same as number of members of error vector ALK_e
    numerrs <- length(dat$Var1)
    vars <- vars[rep(row.names(vars), numerrs), ]

    print( dim(vars) ) #[1] 441   9 CORRECT

    #Relative errors (in percent)
    relEk <- 100* absEk / vars[1,]  #Relative error from constants only
    relEt <- 100* absEt / vars      #Total relative error (from constants and other input vars)

    # ===================================================================
    # Define simpler names for changes in variables

    # Absolute changes:
    pH       <- absEt$pH
    pCO2     <- absEt$pCO2
    CO3      <- absEt$CO3
    CO2      <- absEt$CO2
    OmegaA   <- absEt$OmegaAragonite
    OmegaC   <- absEt$OmegaCalcite
    HCO3     <- absEt$HCO3
    H        <- absEt$H

    # Relative changes:
    rCO3  <- relEt$CO3
    rH    <- relEt$H
    rpCO2 <- relEt$pCO2
    rCO2  <- relEt$CO2
    rOmegaA <- relEt$OmegaAragonite
    rOmegaC   <- relEt$OmegaCalcite
    rHCO3   <- relEt$HCO3
    


    rHCO3H <- sqrt(rHCO3^2 + rH^2)

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

    sig1_AtCt   <- data.frame(errcirc[1]) * 1e+6
    sig2_AtCt   <- data.frame(errcirc[2]) * 1e+6
    sigy_AtCt   <- data.frame(errcirc[3]) 
    sig1hp_AtCt <- data.frame(errcirc[4]) * 1e+6
    sig2hp_AtCt <- data.frame(errcirc[5]) * 1e+6

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

    sigm1_AtCt   <- data.frame(errm[1]) * 1e+6
    sigm2_AtCt   <- data.frame(errm[2]) * 1e+6
    

    # ===================================================================
    # Error-space diagram of relative error in CO3 for At-Ct input pair
    dim(er_outvar) <- c(length(DIC_e), length(ALK_e))
    
    subtitle <- NULL
    xlabel <- expression(paste(sigma[italic("C")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
    ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))

    # sigcritXa <- sig2_AtCt$CO3  ;  sigcritYa <- sig1_AtCt$CO3  #xdata; ydata
    sigcritXa <- sig2_AtCt[[menu_outvar]]  ;  sigcritYa <- sig1_AtCt[[menu_outvar]]  #xdata; ydata
    x <- DIC_e*1e+6  ;  y <- ALK_e*1e+6
    za <- er_outvar
    xlim <- c(0,20)  ; ylim <- xlim
    levels1 <- c(1,seq(2,20,by=2))

    plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                   sig1hp_AtCt[[menu_outvar]], sig2hp_AtCt[[menu_outvar]],
                   zenon(sigm2_AtCt[[menu_outvar]]), zenon(sigm1_AtCt[[menu_outvar]]),
                   DIC_e_soa2, ALK_e_soa2,
                   x, y, za, levels1,
                   'flattest')
  })


}

shinyApp(ui = ui, server = server)