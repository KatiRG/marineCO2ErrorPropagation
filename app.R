# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(seacarb)

# ui <- fluidPage(
ui <- navbarPage("Error propagation for the marine CO2 system",
  # includeCSS("style.css"),

  tabPanel("Error-space diagram",

    tags$h3("Interactive error-space diagram"),
    tags$em("Inputs"),

    fluidRow(
      column(4, 
        tags$p("Choose an input pair and change their default values if desired."),
        tags$p("Choose the output variable to be calculated based on the input 
                values and (modifiable) values for salinity, temperature, pressure, 
                phosphate and silicon."),
        tags$em("Display"),
        tags$p("The default contour level, the reference point (state-of-the-art measurement values
                marked as crosses on the plot), and the axes limits are also modifiable.")

      )
    ), #./fluidRow for intro text
    
    # tags$p("Implements error function, allowing user to change various input values:"),

    # tags$p('errors(flag, var1, var2, S=35, T=25, Patm=1, P=0, Pt=0, Sit=0, 
    #             evar1=0, evar2=0, eS=0.01, eT=0.01, ePt=0, eSit=0, 
    #             epK=c(0.002, 0.01, 0.02, 0.01, 0.01, 0.02, 0.02)'),

    sidebarLayout(
      sidebarPanel(
        # Conditional menu to determine output var list given an input pair
        selectInput(inputId="flag", label="Input pair", 
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
          
          # Input pair values
          fluidRow(
            column(5, 
              textInput(inputId = "var1_flag15",
                label = HTML("Alkalinity (umol kg<sup>-1</sup>)"),
                value = 2295
              )
            ),

            column(6,
              textInput(inputId = "var2_flag15",
                label = HTML("Dissolved inorganic C (umol kg<sup>-1</sup>)"),
                value = 2155
              )
            )
          ), #./fluidRow for input pair

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
                    selectize = TRUE, width = NULL, size = NULL
          ),

          # Contour level
          fluidRow(
            column(4,
              textInput(inputId = "level_flag15",
                label = "Contour level",
                value = "c(1,seq(2,20,by=1))"
              )
            ) #./column
          ), #./fluidRow

          # Axes limits
          fluidRow(
            column(3, checkboxInput("axes_flag15", "Axes limits ") ),
            column(4, conditionalPanel(
              condition = "input.axes_flag15 == true",
              textInput(inputId = "err1_flag15", #NB: ALK is y-axis!!!!
                label =  HTML("Max &sigma;<sub>A<sub>T</sub></sub> (umol kg<sup>-1</sup>)"),
                value = 20)
              ) #./inner conditionalPanel
            ), #./column

            column(4,conditionalPanel(
              condition = "input.axes_flag15 == true",
              textInput(inputId = "err2_flag15", #NB: DIC is x-axis!!!!
                label = HTML("Max &sigma;<sub>C<sub>T</sub></sub> (umol kg<sup>-1</sup>)"),
                value = 20)
              ) #./inner conditionalPanel
            ) #./column
          ), #./fluidRow axes

          # Reference point
          fluidRow(
            column(3, checkboxInput("refPt_flag15", "Edit ref. point ") ),
            column(4, conditionalPanel(
              condition = "input.refPt_flag15 == true",
              textInput(inputId = "refPt1_flag15",
                label = HTML("Alkalinity (umol kg<sup>-1</sup>)"),
                value = 2)
              ) #./inner conditionalPanel
            ), #./column

            column(5,conditionalPanel(
              condition = "input.refPt_flag15 == true",
              textInput(inputId = "refPt2_flag15",
                label = HTML("Dissolved inorg C (umol kg<sup>-1</sup>)"),
                value = 2)
              ) #./inner conditionalPanel
            ) #./column
          ) #./fluidRow refPt

        ), #./conditionalPanel
      
        # CONDITIONAL CHECK FOR FLAG 8 (pH and ALK)
        conditionalPanel(
          condition = "input.flag == '8'", #full output var list

          # Output variable list
          selectInput(inputId="outvar_flag8", label="Output variable",
                    c("pCO2" = "pCO2",
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
                label = HTML("Alkalinity (umol kg<sup>-1</sup>)"),
                value = 2295
              )
            )
           ), #./fluidRow
          
          # Plot level
          fluidRow(
            column(6,
              textInput(inputId = "level_flag8",
                label = "Plot level",
                value = "c(4.2, seq(4,7,by=1))"
              )
            )
          ), #./fluidRow

          # Axes limits
          fluidRow(
            column(3, checkboxInput("axes_flag8", "Axes limits ") ),
            column(4, conditionalPanel(
              condition = "input.axes_flag8 == true",
              textInput(inputId = "err1_flag8",
                label =  HTML("Max &sigma;<sub>pH</sub>"),
                value = 0.030)
              ) #./inner conditionalPanel
            ), #./column

            column(4,conditionalPanel(
              condition = "input.axes_flag8 == true",
              textInput(inputId = "err2_flag8",
                label = HTML("Max &sigma;<sub>A<sub>T</sub></sub> (umol kg<sup>-1</sup>)"),
                value = 20)
              ) #./inner conditionalPanel
            ) #./column
          ), #./fluidRow axes

          # Reference point
          fluidRow(
            column(3, checkboxInput("refPt_flag8", "Edit ref. point ") ),
            column(4, conditionalPanel(
              condition = "input.refPt_flag8 == true",
              textInput(inputId = "refPt1_flag8",
                label = "pH",
                value = "c(0.003, 0.010)")
              ) #./inner conditionalPanel
            ), #./column

            column(5,conditionalPanel(
              condition = "input.refPt_flag8 == true",
              textInput(inputId = "refPt2_flag8",
                label = HTML("Alkalinity (umol kg<sup>-1</sup>)"),
                value = 2)
              ) #./inner conditionalPanel
            ) #./column
          ) #./fluidRow refPt

        ), #./conditionalPanel
      
        # CONDITIONAL CHECK FOR FLAG 9 (pH and DIC)
        conditionalPanel(
          condition = "input.flag == '9'", #full output var list

          # Output variable list
          selectInput(inputId="outvar_flag9", label="Output variable",
                    c("pCO2" = "pCO2",
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
                label = HTML("Dissolved inorganic C (umol kg<sup>-1</sup>)"),
                value = 2155
              )
            )
           ), #./fluidRow

          # Plot level
          fluidRow(
            column(6,
              textInput(inputId = "level_flag9",
                label = "Plot level",
                value = "c(4.5, seq(1,20,by=1))"
              )
            )
          ), #./fluidRow

          # Axes limits
          fluidRow(
            column(3, checkboxInput("axes_flag9", "Axes limits ") ),
            column(4, conditionalPanel(
              condition = "input.axes_flag9 == true",
              textInput(inputId = "err1_flag9",
                label =  HTML("Max &sigma;<sub>pH</sub>"),
                value = 0.030)
              ) #./inner conditionalPanel
            ), #./column

            column(4,conditionalPanel(
              condition = "input.axes_flag9 == true",
              textInput(inputId = "err2_flag9",
                label = HTML("Max &sigma;<sub>C<sub>T</sub></sub> (umol kg<sup>-1</sup>)"),
                value = 20)
              ) #./inner conditionalPanel
            ) #./column
          ), #./fluidRow axes

          # Reference point
          fluidRow(
            column(3, checkboxInput("refPt_flag9", "Edit ref. point ") ),
            column(4, conditionalPanel(
              condition = "input.refPt_flag9 == true",
              textInput(inputId = "refPt1_flag9",
                label = "pH",
                value = "c(0.003, 0.010)")
              ) #./inner conditionalPanel
            ), #./column

            column(5,conditionalPanel(
              condition = "input.refPt_flag9 == true",
              textInput(inputId = "refPt2_flag9",
                label = HTML("Dissolved inorg C (umol kg<sup>-1</sup>)"),
                value = 2)
              ) #./inner conditionalPanel
            ) #./column
          ) #./fluidRow refPt

        ), #./conditionalPanel

        # CONDITIONAL CHECK FOR FLAG 21 (pCO2 and pH)
        conditionalPanel(
          condition = "input.flag == '21'", #exclude pCO2

          # Output variable list
          selectInput(inputId="outvar_flag21", label="Output variable",
                    c("CO3^2-" = "CO3",
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
           ), #./fluidRow
          
          # Plot level
          fluidRow(
            column(6,
              textInput(inputId = "level_flag21",
                label = "Plot level",
                value = "c(7,seq(0,20,by=2))"
              )
            )
          ), #./fluidRow

          # Axes limits
          fluidRow(
            column(3, checkboxInput("axes_flag21", "Axes limits ") ),
            column(4, conditionalPanel(
              condition = "input.axes_flag21 == true",
              textInput(inputId = "err1_flag21",                
                label =  HTML("Max &sigma;<sub>pCO<sub>2</sub></sub> (umol kg<sup>-1</sup>)"),
                value = 20)
              ) #./inner conditionalPanel
            ), #./column

            column(4,conditionalPanel(
              condition = "input.axes_flag21 == true",
              textInput(inputId = "err2_flag21",
                label = HTML("Max &sigma;<sub>pH</sub>"),
                value = 0.03)
              ) #./inner conditionalPanel
            ) #./column
          ), #./fluidRow axes

          # Reference point
          fluidRow(
            column(3, checkboxInput("refPt_flag21", "Edit ref. point ") ),
            column(4, conditionalPanel(
              condition = "input.refPt_flag21 == true",
              textInput(inputId = "refPt1_flag21",
                label = "pCO2 [uatm]",
                value = 2)
              ) #./inner conditionalPanel
            ), #./column

            column(5,conditionalPanel(
              condition = "input.refPt_flag21 == true",
              textInput(inputId = "refPt2_flag21",
                label = "pH",
                value = "c(0.003, 0.010)")
              ) #./inner conditionalPanel
            ) #./column
          ) #./fluidRow refPt

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
                label = HTML("Alkalinity (umol kg<sup>-1</sup>)"),
                value = 2295
              )
            )
           ), #./fluidRow

          # Plot level
          fluidRow(
            column(6,
              textInput(inputId = "level_flag24",
                label = "Plot level",
                value = "seq(3,7,by=0.5)"
              )
            )
          ), #./fluidRow

          # Axes limits
          fluidRow(
            column(3, checkboxInput("axes_flag24", "Axes limits ") ),
            column(4, conditionalPanel(
              condition = "input.axes_flag24 == true",
              textInput(inputId = "err1_flag24",                
                label =  HTML("Max &sigma;<sub>pCO<sub>2</sub></sub> (uatm)"),
                value = 20)
              ) #./inner conditionalPanel
            ), #./column

            column(4,conditionalPanel(
              condition = "input.axes_flag24 == true",
              textInput(inputId = "err2_flag24",
                label = HTML("Max &sigma;<sub>A<sub>T</sub></sub> (umol kg<sup>-1</sup>)"),
                value = 20)
              ) #./inner conditionalPanel
            ) #./column
          ), #./fluidRow axes

          # Reference point
          fluidRow(
            column(3, checkboxInput("refPt_flag24", "Edit ref. point ") ),
            column(4, conditionalPanel(
              condition = "input.refPt_flag24 == true",
              textInput(inputId = "refPt1_flag24",
                label = "pCO2 [uatm]",
                value = 2)
              ) #./inner conditionalPanel
            ), #./column

            column(5,conditionalPanel(
              condition = "input.refPt_flag24 == true",
              textInput(inputId = "refPt2_flag24",
                label = HTML("Alkalinity (umol kg<sup>-1</sup>)"),
                value = 2)
              ) #./inner conditionalPanel
            ) #./column
          ) #./fluidRow refPt

        ), #./conditionalPanel

        # CONDITIONAL CHECK FOR FLAG 25 (pCO2 and DIC)
        conditionalPanel(
          condition = "input.flag == '25'", #exclude pCO2

          # Output variable list
          selectInput(inputId="outvar_flag25", label="Output variable",
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
              textInput(inputId = "var1_flag25",
                label = "pCO2 [uatm]",
                value = 330.5
              )
            ),

            column(6,
              textInput(inputId = "var2_flag25",
                label = HTML("Dissolved inorganic C (umol kg<sup>-1</sup>)"),
                value = 2155
              )
            )
           ), #./fluidRow
          
          # Plot level
          fluidRow(
            column(6,
              textInput(inputId = "level_flag25",
                label = "Plot level",
                value = "c(4.7,seq(1,20,by=1))"
              )
            )
          ), #./fluidRow

          # Axes limits
          fluidRow(
            column(3, checkboxInput("axes_flag25", "Axes limits ") ),
            column(4, conditionalPanel(
              condition = "input.axes_flag25 == true",
              textInput(inputId = "err1_flag25",                
                label =  HTML("Max &sigma;<sub>pCO<sub>2</sub></sub> (uatm)"),
                value = 20)
              ) #./inner conditionalPanel
            ), #./column

            column(4,conditionalPanel(
              condition = "input.axes_flag25 == true",
              textInput(inputId = "err2_flag25",
                label = HTML("Max &sigma;<sub>C<sub>T</sub></sub> (umol kg<sup>-1</sup>)"),
                value = 20)
              ) #./inner conditionalPanel
            ) #./column
          ), #./fluidRow axes

          # Reference point
          fluidRow(
            column(3, checkboxInput("refPt_flag25", "Edit ref. point ") ),
            column(4, conditionalPanel(
              condition = "input.refPt_flag25 == true",
              textInput(inputId = "refPt1_flag25",
                label = "pCO2 [uatm]",
                value = 2)
              ) #./inner conditionalPanel
            ), #./column

            column(5,conditionalPanel(
              condition = "input.refPt_flag25 == true",
              textInput(inputId = "refPt2_flag25",
                label = HTML("Dissolved inorg C (umol kg<sup>-1</sup>)"),
                value = 2)
              ) #./inner conditionalPanel
            ) #./column
          ) #./fluidRow refPt

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
          column(4, 
            textInput(inputId = "pressure",
              label = "Pressure (dbars)",
              value = 0 #pass as dbar
            )
          ),

          column(4,
            textInput(inputId = "phos",
              label = HTML("[Phos] (umol kg<sup>-1</sup>)"),
              value = 2  #2.e-6 mol/kg
            )
          ),

          column(4, 
            textInput(inputId = "sil",
              label = HTML("[Si] (umol kg<sup>-1</sup>)"),
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
        ),
        downloadButton(outputId = "down", label = "Save plot")       
      ) #./mainPanel

    ), #./sidebarLayout


    HTML('<footer style="
            position:absolute; bottom:0; width:100%; height:50px; color: white;
            padding: 10px; background-color: #f5f5f5; z-index: 1000;
          ">
            <p class="text-muted credit" style="margin:5px 0;">Created by
            <a target="_blank" href="http://www.lsce.ipsl.fr/en">
            <span title="Climate and Environment Sciences Laboratory" style="font-weight:bold;">
            LSCE</span></a>
            &nbsp;<a target="_blank" href="http://www.lsce.ipsl.fr/en">
            <img src="LSCE_Icon.png" 
            title="Climate and Environment Sciences Laboratory"/></a> 
            
            and hosted by <a target="_blank" href="http://www.ipsl.fr/en">
            <span title="Institut Pierre Simon Laplace" style="font-weight:bold;">
            IPSL</span></a>&nbsp;<a target="_blank" href="http://www.ipsl.fr/en">
            <img src="IPSL_logo.png" title="Institut Pierre Simon Laplace"/></a>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Version 0.1 - 2017/01/31
            </p>

            <p class="text-muted credit">

        
          </footer>'
    )
   

  ),  #./tabPanel_1

  tabPanel("Help",

    fluidRow(
      column(5,
        tags$html(
          tags$body("To quantify errors more generally and assess the potential for improvement, 
                  this application uses the errors routine from the ",
                  a("seacarb package", href="https://github.com/jamesorr/seacarb-git", 
                    target="blank"), "to construct an error-space diagram showing how 
                  uncertainties in derived variables are affected by 
                  the range of possible uncertainties in input variables."
          ),
        tags$br(),
        tags$br(),
        tags$body("Based on the work published by ",
                  a("Orr et al.,", href="https://github.com/jamesorr/seacarb-git", 
                    target="blank"), " (submitted, 2017)."
          )
        )

      ) #./column
    ), #./fluidRow for intro help text

    tags$h3("Error-space diagram"),
    fluidRow(
      column(5,
        tags$html(
          tags$body("Error-space diagrams are contour plots that provide the propagated uncertainty 
                    in the computed variable as a function of the range of uncertainties in each 
                    member of the input pair, assuming total uncertainties from the equilibrium 
                    constants given in ",
                    a("Table 1", href="https://github.com/jamesorr/seacarb-git", 
                    target="blank"), " of the manuscript."
          )
        )
      )
    ), #./fluidRow for FAQ1

    tags$h3("Error calculation"),
    fluidRow(
      column(5,
             tags$html(
              tags$body("Estimates uncertainties in computed carbonate system variables by 
                    propagating standard error (uncertainty) in six input variables, 
                    including the input pair of carbonate system variables,  
                    the two input nutrients (silicate and phosphate concentrations),  
                    temperature and salinity, as well as the errors in the key dissociation 
                    constants pK0, pK1, pK2, pKb, pKw, pKspa and pKspc.  "
              ),
              tags$br(),
              tags$br(),
              tags$body("Uses the ",
                      a("'errors' function", href="https://rdrr.io/cran/seacarb/man/errors.html", 
                        target="blank"), " function of the seacarb package."
              )
            ) #./html
      ) #./column
    ), #./fluidRow for FAQ2

    HTML('<footer style="
            position:absolute; bottom:0; width:100%; height:50px; color: white;
            padding: 10px; background-color: #f5f5f5; z-index: 1000;
          ">
            <p class="text-muted credit" style="margin:5px 0;">Created by
            <a target="_blank" href="http://www.lsce.ipsl.fr/en">
            <span title="Climate and Environment Sciences Laboratory" style="font-weight:bold;">
            LSCE</span></a>
            &nbsp;<a target="_blank" href="http://www.lsce.ipsl.fr/en">
            <img src="LSCE_Icon.png" 
            title="Climate and Environment Sciences Laboratory"/></a> 
            
            and hosted by <a target="_blank" href="http://www.ipsl.fr/en">
            <span title="Institut Pierre Simon Laplace" style="font-weight:bold;">
            IPSL</span></a>&nbsp;<a target="_blank" href="http://www.ipsl.fr/en">
            <img src="IPSL_logo.png" title="Institut Pierre Simon Laplace"/></a>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Version 0.1 - 2017/01/31
            </p>

            <p class="text-muted credit">

        
          </footer>'
    )

  ),  #./tabPanel_2 

  tabPanel("Contact",

    fluidRow(
      column(5,
        tags$html(
          tags$strong("Principal Investigator"),
          tags$br(),
          tags$body(
                  a("James Orr", href="mailto:james.orr@lsce.ipsl.fr")
          ),
          tags$br(),
          tags$body("Research Director (Ocean carbon-cycle modeling), LSCE/CEA, Gif-sur-Yvette, France"),
          tags$br(),
          tags$br()
        )

      ) #./column
    ), #./fluidRow

    fluidRow(
      column(5,
        tags$html(
          tags$strong("Application Developer"),
          tags$br(),
          tags$body(a("Cathy Nangini", href="mailto:cnangini@lsce.ipsl.fr")),
          tags$br(),
          tags$body("GitHub ", a("@katiRG", href="www.github.com/katiRG", target="blank")),
          tags$br(),
          tags$body("Data scientist at LSCE/DRF/CEA, Gif-sur-Yvette, France")
        
        )

      ) #./column
    ), #./fluidRow
    

    HTML('<footer style="
            position:absolute; bottom:0; width:100%; height:50px; color: white;
            padding: 10px; background-color: #f5f5f5; z-index: 1000;
          ">
            <p class="text-muted credit" style="margin:5px 0;">Created by
            <a target="_blank" href="http://www.lsce.ipsl.fr/en">
            <span title="Climate and Environment Sciences Laboratory" style="font-weight:bold;">
            LSCE</span></a>
            &nbsp;<a target="_blank" href="http://www.lsce.ipsl.fr/en">
            <img src="LSCE_Icon.png" 
            title="Climate and Environment Sciences Laboratory"/></a> 
            
            and hosted by <a target="_blank" href="http://www.ipsl.fr/en">
            <span title="Institut Pierre Simon Laplace" style="font-weight:bold;">
            IPSL</span></a>&nbsp;<a target="_blank" href="http://www.ipsl.fr/en">
            <img src="IPSL_logo.png" title="Institut Pierre Simon Laplace"/></a>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Version 0.1 - 2017/01/31
            </p>

            <p class="text-muted credit">

        
          </footer>'
    )

  )  #./tabPanel_3
) #./navbarPage    ### ./fluidPage

server <- function(input, output) {
  # output$result <- renderText({
  #   paste("flag value:", input$flag)
  # })

  # ===================================================================
  # Define function sources
  source("errhalf.R")
  source("errmid.R")
  source("errors.R")
  source("derivnum.R")

  # Default uncertainties in equilibrium constants 
  # (pK0, pK1, pK2, pKb, pKw, pKa, pKc, Bt)
  # ------------------------------------------------
  epKstd  <- c(0.004, 0.015,  0.03, 0.01,  0.01, 0.02, 0.02, 0.01)
  # epKstd  <- c(0.002, 0.01,  0.02, 0.01,  0.01, 0.01, 0.01, 0.00) #agrees with draft ms c. dec 2016

  numpts = 20 #number of pts in plotted x, y vectors

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
  #----------------------------------------------------------------------
  #Reactive elements
  # Scales
  varScales <- reactive({
    maxlim = 20 #max of scale for all except pH
    redn = 1 #0.5 #reduce resolution by 50%

    list("maxlim" = maxlim,
         "redn" = redn
        )
  })

  # Get input vars and define the associated quantities
  varSet1 <- reactive({

    maxlim = varScales()[["maxlim"]]
    redn = varScales()[["redn"]]

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

    # CONDITIONAL CHECK OVER ALL INPUT FLAGS TO DEFINE:
    # var1, var1_e, var1_e_soa, var1_e_soa2
    # var2, var2_e, var2_e_soa, var2_e_soa2
    if (input$flag == "15") { #var1=ALK, var2=DIC
      menu_var1 <- as.numeric(input$var1_flag15) * 1e-6 #convert umol/kg to mol/kg
      menu_var2 <- as.numeric(input$var2_flag15) * 1e-6 #convert umol/kg to mol/kg

      # Scale factor for sig, sigm
      scalefactor1 = 1e+6 #ALK
      scalefactor2 = 1e+6 #DIC

      # Max error limits
      if(input$axes_flag15) { #Axes limits checkbox has been selected
        max_error1 <- as.numeric(input$err1_flag15)
        max_error2 <- as.numeric(input$err2_flag15)
      } else {
        max_error1 <- 20
        max_error2 <- 20
      }

      # Uncertainties in input variables var1 and var2
      # NB: both vectors must have same number of points
      var1_e <- seq(0., max_error1, (max_error1/numpts)*(1.0/redn) ) * 1e-6
      var2_e <- seq(0., max_error2, (max_error2/numpts)*(1.0/redn) ) * 1e-6

      # state-of-art errors for vars (c.f. Orr et al. 2017, Table 1)
      # (to be plotted as crosses in error-space diagram)
      var1_e_soa   <- 2 #umol/kg
      var2_e_soa   <- 2 #umol/kg

      # Edit soa2 value if modified by user
      if(input$refPt_flag15) { #Edit ref pt checkbox has been selected
        var1_e_soa2  <- as.numeric(input$refPt1_flag15)
        var2_e_soa2  <- as.numeric(input$refPt2_flag15)        
      } else {
        var1_e_soa2  <- c(var1_e_soa, var1_e_soa)
        var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      }

      # data arrays for plot
      xdata <- var2_e*1e+6  ;  ydata <- var1_e*1e+6
      xlim <- c(0,max_error2)  ; ylim <-c(0,max_error1)
      # levels1 <- c(1,seq(2,20,by=2))
      levels1 <- eval(parse(text = input$level_flag15))
      xlabel <- expression(paste(sigma[italic("C")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
      ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))

    } else if (input$flag == "8") { #var1=pH, var2=ALK
      menu_var1 <- as.numeric(input$var1_flag8)
      menu_var2 <- as.numeric(input$var2_flag8) * 1e-6 #convert umol/kg to mol/kg

      # Scale factor for sig, sigm
      scalefactor1 = 1 #pH
      scalefactor2 = 1e+6 #ALK

      # Max error limits
      if(input$axes_flag8) { #Axes limits checkbox has been selected
        max_error1 <- as.numeric(input$err1_flag8)
        max_error2 <- as.numeric(input$err2_flag8)
      } else {
        max_error1 <- 0.030
        max_error2 <- 20
      }

      # Uncertainties in input variables var1 and var2
      # NB: both vectors must have same number of points
      var1_e <- seq(0, max_error1, (max_error1/numpts) * (1/redn) )
      var2_e <- seq(0., max_error2, (max_error2/numpts)*(1.0/redn) ) * 1e-6
      
      var1_e_soa   <- c(0.003, 0.01) #pH
      var2_e_soa   <- 2 #umol/kg
      
      # Edit soa2 value if modified by user
      if(input$refPt_flag8) { #Edit ref pt checkbox has been selected
        var1_e_soa2  <- eval(parse(text = input$refPt1_flag8))
        var2_e_soa2  <- c( as.numeric(input$refPt2_flag8), as.numeric(input$refPt2_flag8) )
      } else {
        var1_e_soa2  <- var1_e_soa
        var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      }
      
      # for plot
      xdata <- var1_e           ;  ydata <- var2_e * 1e+6
      xlim <- c(0,max_error1) ; ylim <- c(0,max_error2) 
      # levels1 <- c(4.2, seq(4,7,by=1))
      levels1 <- eval(parse(text = input$level_flag8))
      xlabel <- expression(paste(sigma[pH]," (total scale)",sep=""))
      ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))

    } else if (input$flag == "9") { #var1=pH, var2=DIC
      menu_var1 <- as.numeric(input$var1_flag9)
      menu_var2 <- as.numeric(input$var2_flag9) * 1e-6 #convert umol/kg to mol/kg

      # Scale factor for sig, sigm
      scalefactor1 = 1 #pH
      scalefactor2 = 1e+6 #DIC

      # Max error limits
      if(input$axes_flag9) { #Axes limits checkbox has been selected
        max_error1 <- as.numeric(input$err1_flag9)
        max_error2 <- as.numeric(input$err2_flag9)
      } else {
        max_error1 <- 0.030
        max_error2 <- 20
      }

      # Uncertainties in input variables var1 and var2
      # NB: both vectors must have same number of points 
      var1_e <- seq(0, max_error1, (max_error1/numpts) * (1/redn) )
      var2_e <- seq(0., max_error2, (max_error2/numpts)*(1.0/redn) ) * 1e-6
      
      var1_e_soa   <- c(0.003, 0.01) #pH
      var2_e_soa   <- 2 #umol/kg
      
      # Edit soa2 value if modified by user
      if(input$refPt_flag9) { #Edit ref pt checkbox has been selected
        var1_e_soa2  <- eval(parse(text = input$refPt1_flag9))
        var2_e_soa2  <- c( as.numeric(input$refPt2_flag9), as.numeric(input$refPt2_flag9) )
      } else {
        var1_e_soa2  <- var1_e_soa
        var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      }
      
      # for plot
      xdata <- var1_e           ;  ydata <- var2_e * 1e+6
      xlim <- c(0,max_error1) ; ylim <- c(0,max_error2)
      # levels1 <- c(4.5, seq(1,20,by=1))
      levels1 <- eval(parse(text = input$level_flag9))
      xlabel <- expression(paste(sigma[pH]," (total scale)",sep=""))
      ylabel <- expression(paste(sigma[italic("C")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
    
    } else if (input$flag == "21") { #var1=pCO2, var2=pH
      menu_var1 <- as.numeric(input$var1_flag21)
      menu_var2 <- as.numeric(input$var2_flag21)
      
      # Scale factor for sig, sigm
      scalefactor1 = 1 #pCO2
      scalefactor2 = 1 #pH

      # Max error limits
      if(input$axes_flag21) { #Axes limits checkbox has been selected
        max_error1 <- as.numeric(input$err1_flag21)
        max_error2 <- as.numeric(input$err2_flag21)
      } else {
        max_error1 <- 20
        max_error2 <- 0.030
      }

      # Uncertainties in input variables var1 and var2
      # NB: both vectors must have same number of points 
      var1_e <- seq(0, max_error1, (max_error1/numpts) * (1/redn) )
      var2_e <- seq(0., max_error2, (max_error2/numpts)*(1.0/redn) )
      
      var1_e_soa   <- 2
      var2_e_soa   <- c(0.003, 0.01)
      
      # Edit soa2 value if modified by user
      if(input$refPt_flag21) { #Edit ref pt checkbox has been selected
        var1_e_soa2  <- c( as.numeric(input$refPt1_flag21), as.numeric(input$refPt1_flag21) )
        var2_e_soa2  <- eval(parse(text = input$refPt2_flag21))
      } else {
        var1_e_soa2  <- c(var1_e_soa, var1_e_soa)
        var2_e_soa2  <- var2_e_soa
      }
      
      # for plot
      xdata <- var2_e           ;  ydata <- var1_e
      xlim <- c(0,max_error2)  ; ylim <- c(0,max_error1)
      # levels1 <- c(7,seq(0,20,by=2))
      levels1 <- eval(parse(text = input$level_flag21))
      xlabel <- expression(paste(sigma[pH]," (total scale)",sep=""))
      ylabel <- expression(paste(sigma[pCO[2]]," (",mu,"atm",")",sep=""))

    } else if (input$flag == "24") { #var1=pCO2, var2=ALK
      menu_var1 <- as.numeric(input$var1_flag24)
      menu_var2 <- as.numeric(input$var2_flag24) * 1e-6 #convert umol/kg to mol/kg

      # Scale factor for sig, sigm
      scalefactor1 = 1 #pCO2
      scalefactor2 = 1e+6 #ALK

      # Max error limits
      if(input$axes_flag24) { #Axes limits checkbox has been selected
        max_error1 <- as.numeric(input$err1_flag24)
        max_error2 <- as.numeric(input$err2_flag24)
      } else {
        max_error1 <- 20
        max_error2 <- 20
      }

      # Uncertainties in input variables var1 and var2
      # NB: both vectors must have same number of points 
      var1_e <- seq(0, max_error1, (max_error1/numpts) * (1/redn) )
      var2_e <- seq(0., max_error2, (max_error2/numpts)*(1.0/redn) ) * 1e-6
      
      var1_e_soa   <- 2
      var2_e_soa   <- 2
      
      # Edit soa2 value if modified by user
      if(input$refPt_flag24) { #Edit ref pt checkbox has been selected    
        var1_e_soa2  <- as.numeric(input$refPt1_flag24)
        var2_e_soa2  <- as.numeric(input$refPt2_flag24)
      } else {
        var1_e_soa2  <- c(var1_e_soa, var1_e_soa)
        var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      }
      
      # for plot
      xdata <- var1_e ; ydata <- var2_e*1e+6
      xlim <- c(0,max_error1)  ; ylim <- c(0,max_error2)
      # levels1 <- seq(3,7,by=0.5)
      levels1 <- eval(parse(text = input$level_flag24))
      xlabel <- expression(paste(sigma[pCO[2]]," (",mu,"atm",")",sep=""))
      ylabel <- expression(paste(sigma[italic("A")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
    
    } else if (input$flag == "25") { #var1=pCO2, var2=DIC
      menu_var1 <- as.numeric(input$var1_flag25)
      menu_var2 <- as.numeric(input$var2_flag25)* 1e-6 #convert umol/kg to mol/kg

      # Scale factor for sig, sigm
      scalefactor1 = 1 #pCO2
      scalefactor2 = 1e+6 #DIC

      # Max error limits
      if(input$axes_flag25) { #Axes limits checkbox has been selected
        max_error1 <- as.numeric(input$err1_flag25)
        max_error2 <- as.numeric(input$err2_flag25)
      } else {
        max_error1 <- 20
        max_error2 <- 20
      }

      # Uncertainties in input variables var1 and var2
      # NB: both vectors must have same number of points 
      var1_e <- seq(0, max_error1, (max_error1/numpts) * (1/redn) )
      var2_e <- seq(0., max_error2, (max_error2/numpts)*(1.0/redn) ) * 1e-6          
      
      var1_e_soa   <- 2
      var2_e_soa   <- 2
      
      # Edit soa2 value if modified by user
      if(input$refPt_flag25) { #Edit ref pt checkbox has been selected    
        var1_e_soa2  <- as.numeric(input$refPt1_flag25)
        var2_e_soa2  <- as.numeric(input$refPt2_flag25)
      } else {
        var1_e_soa2  <- c(var1_e_soa, var1_e_soa)
        var2_e_soa2  <- c(var2_e_soa, var2_e_soa)
      }
      
      # for plot
      xdata <- var1_e ; ydata <- var2_e*1e+6
      xlim <- c(0,max_error1)  ; ylim <- c(0,max_error2)
      # levels1 <- c(4.7,seq(1,20,by=1))
      levels1 <- eval(parse(text = input$level_flag25))
      xlabel <- expression(paste(sigma[pCO[2]]," (",mu,"atm",")",sep=""))
      ylabel <- expression(paste(sigma[italic("C")[T]]," (",mu,"mol kg"^{-1},")",sep=""))
    }  

    list("menu_flag" = menu_flag,
          "menu_salt" = menu_salt,
          "menu_temp" = menu_temp,
          "menu_pressure" = menu_pressure,
          "menu_phos" = menu_phos,
          "menu_sil" = menu_sil,
          "menu_outvar" = menu_outvar,
          "menu_var1" = menu_var1,
          "menu_var2" = menu_var2,
          "scalefactor1" = scalefactor1,
          "scalefactor2" = scalefactor2,
          "var1_e" = var1_e,
          "var2_e" = var2_e,
          "var1_e_soa" = var1_e_soa,
          "var2_e_soa" = var2_e_soa,
          "var1_e_soa2" = var1_e_soa2,
          "var2_e_soa2" = var2_e_soa2,
          "xdata" = xdata,
          "ydata" = ydata,
          "xlim" = xlim,
          "ylim" = ylim,
          "levels1" = levels1,
          "xlabel" = xlabel,
          "ylabel" = ylabel
        )
  }) #./varSet1

  #----------------------------------------------------------------------
  # Compute derived carbonate system vars with  seacarb routine carb
  varCarb <- reactive({
    
    menu_flag <- varSet1()[["menu_flag"]]
    menu_var1 <- varSet1()[["menu_var1"]]
    menu_var2 <- varSet1()[["menu_var2"]]
    menu_outvar <- varSet1()[["menu_outvar"]]

    menu_salt <- varSet1()[["menu_salt"]]
    menu_temp <- varSet1()[["menu_temp"]]
    menu_pressure <- varSet1()[["menu_pressure"]]
    menu_phos <- varSet1()[["menu_phos"]]
    menu_sil <- varSet1()[["menu_sil"]]
    var1_e <- varSet1()[["var1_e"]]
    var2_e <- varSet1()[["var2_e"]]


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
    print("dim(vars):")
    print( dim(vars) )  #[1] 21  9 CORRECT
    
    # ===================================================================
    # Use 1-D error vectors to build 2-D error array (to plot contours in DIC-ALK space)
    if (menu_flag == 15) dat <- expand.grid(var2_e, var1_e)
    else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 21 || menu_flag == 24 || menu_flag == 25) {        
        dat <- expand.grid(var1_e,  var2_e)
    }

    # ===================================================================
    # Compute derived vars and their errors
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
    } else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 21 || menu_flag == 24 || menu_flag == 25) {
      dat_evar1 <- dat$Var1           ;  dat_evar2 <- dat$Var2
      print("defining order for absET!!")
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

    er_outvar = relEt[[menu_outvar]]

    # list("relEt" = relEt)
    list("er_outvar" = er_outvar)

  }) #./varCarb

  # Compute other parts of error-space diagrams
  varErr <- reactive({

    maxlim = varScales()[["maxlim"]]
    redn = varScales()[["redn"]]
    scalefactor1 = varSet1()[["scalefactor1"]]
    scalefactor2 = varSet1()[["scalefactor2"]]

    menu_flag <- varSet1()[["menu_flag"]]
    menu_var1 <- varSet1()[["menu_var1"]]
    menu_var2 <- varSet1()[["menu_var2"]]
    menu_outvar <- varSet1()[["menu_outvar"]]

    menu_salt <- varSet1()[["menu_salt"]]
    menu_temp <- varSet1()[["menu_temp"]]
    menu_pressure <- varSet1()[["menu_pressure"]]
    menu_phos <- varSet1()[["menu_phos"]]
    menu_sil <- varSet1()[["menu_sil"]]
    var1_e <- varSet1()[["var1_e"]]
    var2_e <- varSet1()[["var2_e"]]

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
    sigyspct <- seq(0,maxlim,by=1/redn) # in percent
    errm <- errmid(flag=menu_flag, var1=menu_var1, var2=menu_var2, S=menu_salt, T=menu_temp,
                  Patm=1, P=menu_pressure, Pt=menu_phos, Sit=menu_sil,
                  sigyspct, epK=epKstd,
                  k1k2='l', kf='dg', ks="d", pHscale="T", 
                  b="u74", gas="potential", warn="n")
    # NB: Warning in sqrt(0.5 * (sigmay^2 - eKall^2)/dd1^2) : production de NaN
    # et: Warning in sqrt(0.5 * (sigmay^2 - eKall^2)/dd2^2) : production de NaN

    # Add scale factors if necessary
    sig1   <- data.frame(errcirc[1]) * scalefactor1
    sig2   <- data.frame(errcirc[2]) * scalefactor2
        
    sigm1   <- data.frame(errm[1]) * scalefactor1
    sigm2   <- data.frame(errm[2]) * scalefactor2

    list("sig1" = sig1,
         "sig2" = sig2,
         "sigm1" = sigm1,
         "sigm2" = sigm2
        )

  }) #./varErr

  # ---------------------------------------------------------------------
  # Render plot
  output$erspace <- renderPlot({
    
    maxlim = varScales()[["maxlim"]]
    redn = varScales()[["redn"]]

    # Outputs from varSet1 (user inputs)
    menu_flag <- varSet1()[["menu_flag"]]
    
    menu_outvar <- varSet1()[["menu_outvar"]]

    var1_e <- varSet1()[["var1_e"]]
    var2_e <- varSet1()[["var2_e"]]

    var1_e_soa <- varSet1()[["var1_e_soa"]]
    var2_e_soa <- varSet1()[["var2_e_soa"]]

    var1_e_soa2 <- varSet1()[["var1_e_soa2"]]
    var2_e_soa2 <- varSet1()[["var2_e_soa2"]]

    xdata <- varSet1()[["xdata"]]
    ydata <- varSet1()[["ydata"]]
    xlim <- varSet1()[["xlim"]]
    ylim <- varSet1()[["ylim"]]
    levels1 <- varSet1()[["levels1"]]
    xlabel <- varSet1()[["xlabel"]]
    ylabel <- varSet1()[["ylabel"]]

    # Output from carb fn
    er_outvar <- varCarb()[["er_outvar"]]
   
    # Output from errhalf and errmid fns
    sig1 <- varErr()[["sig1"]]
    sig2 <- varErr()[["sig2"]]
    sigm1 <- varErr()[["sigm1"]]
    sigm2 <- varErr()[["sigm2"]]
    

    # ===================================================================
    # Error-space diagram of relative error in CO3 for At-Ct input pair
    dim(er_outvar) <- c(length(var2_e), length(var1_e))
    
    subtitle <-paste("Output variable", menu_outvar, sep=" ")      
    
    za <- er_outvar

    # pdf(file="halfway_CO3.pdf",  width=10.0, height=7.0)

    if (menu_flag == 15) {
      sigcritXa <- sig2[[menu_outvar]]  ;  sigcritYa <- sig1[[menu_outvar]]  #xdata; ydata

      plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                     NULL, NULL,
                     zenon(sigm2[[menu_outvar]]), zenon(sigm1[[menu_outvar]]),
                     var2_e_soa2, var1_e_soa2,
                     xdata, ydata, za, levels1,
                     'flattest')
    } else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 24 || menu_flag == 25) {
      sigcritXa <- sig1[[menu_outvar]]  ;  sigcritYa <- sig2[[menu_outvar]]  #xdata; ydata

      plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                     NULL, NULL,
                     zenon(sigm1[[menu_outvar]]), zenon(sigm2[[menu_outvar]]),
                     var1_e_soa2, var2_e_soa2,
                     xdata, ydata, za, levels1,
                     'flattest')
    } else if (menu_flag == 21) {
      za <- t(za)      
      sigcritXa <- sig2[[menu_outvar]]  ;  sigcritYa <- sig1[[menu_outvar]]  #xdata; ydata

      plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                     NULL, NULL,
                     zenon(sigm2[[menu_outvar]]), zenon(sigm1[[menu_outvar]]),
                     var2_e_soa2, var1_e_soa2,
                     xdata, ydata, za, levels1,
                     'flattest')
    }


  }) #./renderPlot


 # downloadHandler contains 2 arguments as functions, namely filename, content
  output$down <- downloadHandler(
    filename =  function() {
      paste("errorSpace", "pdf", sep=".")
    },


    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file) # open the pdf device
      print("file:")
      print(file)
        
      # Vars needed to make plot (must be reactive otherwise can't read them)
      maxlim = varScales()[["maxlim"]]
      redn = varScales()[["redn"]]

      # Outputs from varSet1 (user inputs)
      menu_flag <- varSet1()[["menu_flag"]]
      
      menu_outvar <- varSet1()[["menu_outvar"]]

      var1_e <- varSet1()[["var1_e"]]
      var2_e <- varSet1()[["var2_e"]]

      var1_e_soa <- varSet1()[["var1_e_soa"]]
      var2_e_soa <- varSet1()[["var2_e_soa"]]

      var1_e_soa2 <- varSet1()[["var1_e_soa2"]]
      var2_e_soa2 <- varSet1()[["var2_e_soa2"]]

      xdata <- varSet1()[["xdata"]]
      ydata <- varSet1()[["ydata"]]
      xlim <- varSet1()[["xlim"]]
      ylim <- varSet1()[["ylim"]]
      levels1 <- varSet1()[["levels1"]]
      xlabel <- varSet1()[["xlabel"]]
      ylabel <- varSet1()[["ylabel"]]

      # Output from carb fn
      er_outvar <- varCarb()[["er_outvar"]]
     
      # Output from errhalf and errmid fns
      sig1 <- varErr()[["sig1"]]
      sig2 <- varErr()[["sig2"]]
      sigm1 <- varErr()[["sigm1"]]
      sigm2 <- varErr()[["sigm2"]]
      

      # ===================================================================
      # Error-space diagram of relative error in CO3 for At-Ct input pair
      dim(er_outvar) <- c(length(var2_e), length(var1_e))
      
      subtitle <-paste("Output variable", menu_outvar, sep=" ")      
      
      za <- er_outvar    

      if (menu_flag == 15) {
        sigcritXa <- sig2[[menu_outvar]]  ;  sigcritYa <- sig1[[menu_outvar]]  #xdata; ydata

        plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                       NULL, NULL,
                       zenon(sigm2[[menu_outvar]]), zenon(sigm1[[menu_outvar]]),
                       var2_e_soa2, var1_e_soa2,
                       xdata, ydata, za, levels1,
                       'flattest')
      } else if (menu_flag == 8 || menu_flag == 9 || menu_flag == 24 || menu_flag == 25) {
        sigcritXa <- sig1[[menu_outvar]]  ;  sigcritYa <- sig2[[menu_outvar]]  #xdata; ydata

        plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                       NULL, NULL,
                       zenon(sigm1[[menu_outvar]]), zenon(sigm2[[menu_outvar]]),
                       var1_e_soa2, var2_e_soa2,
                       xdata, ydata, za, levels1,
                       'flattest')
      } else if (menu_flag == 21) {
        za <- t(za)      
        sigcritXa <- sig2[[menu_outvar]]  ;  sigcritYa <- sig1[[menu_outvar]]  #xdata; ydata

        plterrcontour (sigcritXa, sigcritYa, xlabel, ylabel, subtitle, xlim, ylim,
                       NULL, NULL,
                       zenon(sigm2[[menu_outvar]]), zenon(sigm1[[menu_outvar]]),
                       var2_e_soa2, var1_e_soa2,
                       xdata, ydata, za, levels1,
                       'flattest')
      }

      dev.off()  # turn the device off
    
    } #./content
  ) #./downloadHandler





} #./server

shinyApp(ui = ui, server = server)