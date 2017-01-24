# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(seacarb)

ui <- fluidPage(
  tags$h1("Interactive error-space diagram for the marine CO2 system"),
  tags$br(),
  tags$br(),

  selectInput(inputId="flag", label="Pair of carbonate system input variables", 
    c("pH and CO2" = 1,
      "Transmission" = "am",
      "Gears" = "gear"), 
    selected = "pH and CO2", multiple = FALSE,
    selectize = TRUE, width = NULL, size = NULL
  ),

  textOutput("result"),

  sliderInput(inputId = "num",
    label = "Choose a number",
    value = 25, min = 1, max = 100),


  fluidRow(
    column( 5, plotOutput("hist", width = "400px", height = "400px") )
  ),

  fluidRow(
    column( 5, verbatimTextOutput("stats") )
  ),

  fluidRow(
    column( 5, plotOutput("erspace", width = "400px", height = "400px") )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    paste("You chose", input$flag)
  })

  source("/homel/cnangini/PROJECTS/seacarb-git/R/errhalf.R")
  source("/homel/cnangini/PROJECTS/seacarb-git/R/errmid.R")
  source("/homel/cnangini/PROJECTS/seacarb-git/R/errors.R")
  source("/homel/cnangini/PROJECTS/seacarb-git/R/derivnum.R")

  # ===================================================================
  # Define input vars and their uncertainties
  # Specify flag & corresponding 2 input variables 
  flag = 15

  # Input variables:
  # ----------------
  # Approximate regional mean for Southern Ocean (c.f. Fig. 3.2 [Orr, 2011])
  # For ALK & DIC input pair
  ALK  = 2295e-6  #(umol/kg)
  DIC  = 2155e-6

  temp = -0.49    #C
  salt = 33.96    #psu
  press = 0       #bar

  Phos = 2.e-6    #(umol/kg)
  Sil = 60.e-6    #(umol/kg)

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

  # Default uncertainties in equilibrium constants 
  # (pK0, pK1, pK2, pKb, pKw, pKa, pKc, Bt)
  # ------------------------------------------------
  epKstd  <- c(0.004, 0.015,  0.03, 0.01,  0.01, 0.02, 0.02, 0.01)

  # ===================================================================
  # Compute derived carbonate system vars with  seacarb routine carb
  # (Southern Ocean)
  print("Running carb function:")
  vars <- carb  (flag=15, var1=ALK, DIC, S=salt, T=temp, Patm=1, P=press, Pt=Phos, Sit=Sil, 
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

  vars <- carb (flag=15, var1=ALK, var2=DIC, S=salt, T=temp, 
                Patm=1, P=press, Pt=Phos, Sit=Sil, 
                k1k2='w14', kf='dg', ks="d", pHscale="T", 
                b="u74", gas="potential", warn='n')

  print( dim(vars) )  #[1]  1 19 CORRECT

  H = 10^(-1*vars$pH)         # H+ concentration (mol/kg)
  vars <- data.frame(H, vars) # Add H+ as new column to vars data frame

  print("Calculating absEt and absEk:")
  # Absolute errors: propagated uncertainties
  absEt <- errors (flag=15, var1=ALK, var2=DIC, S=salt, T=temp,
                  Patm=1, P=press, Pt=Phos, Sit=Sil, 
                  evar1=dat$Var2, evar2=dat$Var1, 
                  eS=0, eT=0, ePt=0, eSit=0, epK=epKstd,
                  k1k2='w14', kf='dg', ks="d", pHscale="T",
                  b="u74", gas="potential", warn='no')

  # Aboslute error from constants only (all other input errors assumed to be zero)
  absEk <- errors  (flag=15, var1=ALK, var2=DIC, S=salt, T=temp, 
                   Patm=1, P=press, Pt=Phos, Sit=Sil, 
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
  pCO2socn <- absEt$pCO2
  OmegaA   <- absEt$OmegaAragonite
  HCO3     <- absEt$HCO3
  H        <- absEt$H

  # Relative changes:
  rCO3  <- relEt$CO3
  rH    <- relEt$H
  rpCO2 <- relEt$pCO2
  rCO2  <- relEt$CO2
  rpCO2   <- relEt$pCO2
  rOmegaA <- relEt$OmegaAragonite
  rHCO3   <- relEt$HCO3
  H       <- absEt$H

  rHCO3H <- sqrt(rHCO3^2 + rH^2)

  # ===================================================================
  # Compute other parts of error-space diagrams

  # Constants-pair CURVE (propagated error from constants = that from input pair)
  # (Southern Ocean)
  print("Calculating constants-pair curve (errhalf fn):")
  errcirc <- errhalf(flag=15, var1=ALK, var2=DIC, S=salt, T=temp, 
                     Patm=1, P=press, Pt=Phos, Sit=Sil,
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
  errm <- errmid(flag=15, var1=ALK, var2=DIC, S=salt, T=temp,
                Patm=1, P=press, Pt=Phos, Sit=Sil,
                sigyspct, epK=epKstd,
                k1k2='l', kf='dg', ks="d", pHscale="T", 
                b="u74", gas="potential", warn="n")
  # NB: Warning in sqrt(0.5 * (sigmay^2 - eKall^2)/dd1^2) : production de NaN
  # et: Warning in sqrt(0.5 * (sigmay^2 - eKall^2)/dd2^2) : production de NaN

  sigm1_AtCt   <- data.frame(errm[1]) * 1e+6
  sigm2_AtCt   <- data.frame(errm[2]) * 1e+6

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

  # ===================================================================
  # Error-space diagram of relative error in CO3 for At-Ct input pair
  dim(rCO3) <- c(length(DIC_e), length(ALK_e))
  subtitle <- NULL
  xlabel <- expression(paste(sigma[italic("C")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
  ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))

  sigcritXa <- sig2_AtCt$CO3  ;  sigcritYa <- sig1_AtCt$CO3  #xdata; ydata
  x <- DIC_e*1e+6  ;  y <- ALK_e*1e+6
  za <- rCO3
  xlim <- c(0,20)  ; ylim <- xlim
  levels1 <- c(1,seq(2,20,by=2))

  # xdata.numeric <- c(0.00000000, 0.08982232, 0.44856458, 0.89371531, 1.33206433, 1.76027554, 2.17508999, 2.57335068, 2.95202663, 3.30823587, 3.63926744, 3.94260198, 4.21593095, 4.45717413, 4.66449553, 4.83631730, 4.97133177, 5.06851141, 5.12711661, 5.14591750, 5.14670137)
  # ydata.numeric <- c(4.922858e+00, 4.922109e+00, 4.904125e+00, 4.848069e+00, 4.755116e+00, 4.625974e+00, 4.461625e+00, 4.263320e+00, 4.032570e+00, 3.771128e+00, 3.480987e+00, 3.164352e+00, 2.823636e+00, 2.461429e+00, 2.080490e+00, 1.683717e+00, 1.274130e+00, 8.548454e-01, 4.290554e-01, 8.591573e-02, 3.014381e-16)

  # reactive expression
  data <- reactive({
    rnorm( input$num )
  })

  output$hist <- renderPlot({
    print("dropdown menu: ", input$flag)
    hist( data(), main = input$title )
  })

  output$stats <- renderPrint({
    summary( data() )
  })

  output$erspace <- renderPlot({
    # hist( rnorm(100)  )

    plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                   sig1hp_AtCt$CO3, sig2hp_AtCt$CO3,
                   zenon(sigm2_AtCt$CO3), zenon(sigm1_AtCt$CO3),
                   DIC_e_soa2, ALK_e_soa2,
                   x, y, za, levels1,
                   'flattest')
  })


}

shinyApp(ui = ui, server = server)