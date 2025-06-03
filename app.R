
# last update - 20250603 - 3:55pm

# User Input -------------------------------------------------------------------

ProjectName <- ''

ExpressionMatrix_file <- ''

MetaData_file <- ''

human <- 'TRUE'


## Provided Input --------------------------------------------------------------
## User make sure paths are correct
GeneSet_HS_File <- "Genesets/GeneSet_List_HS_v6.RData"
GeneSetCat_HS_File <- "Genesets/GeneSet_CatTable_v6.txt"
GeneSet_MM_File <- "Genesets/GeneSet_List_MM.RData"
GeneSetCat_MM_File <- "Genesets/GeneSet_CatTable_MM.txt"
ExampleExpr_File <- "Example_Data/TCGA_CHOL_Expression_PatientID.txt"
ExampleClin_File <- "Example_Data/TCGA_CHOL_Clinical_PatientID.txt"

## Password Settings -----------------------------------------------------------
Password_Protected <- FALSE
PasswordSet <- ''




# Input Check ------------------------------------------------------------------

if (!file.exists(ExpressionMatrix_file)) {
  FileProvided <- FALSE
} else { FileProvided <- TRUE }

if (!isTruthy(ProjectName)) {
  ProjectName <- paste("{ UMAP Analysis }")
} else {
  ProjectName <- paste("{",ProjectName,"UMAP Analysis }")
}
if (!isTruthy(human)) {
  mouse <- FALSE
} else {
  if (!as.logical(human)) {
    mouse <- TRUE
  } else {
    mouse <- FALSE
  }
}

Path_Selec <- NULL

umap.mod <- umap.defaults

#increase file upload size
options(shiny.maxRequestSize=5000*1024^2)

# Password Table ---------------------------------------------------------------
# user database for login
if (Password_Protected) {
  user_base <- tibble::tibble(
    user = "user",
    password = PasswordSet,
    permissions = "admin",
    name = "User"
  )
}

# UI Tabs ----------------------------------------------------------------------
## Login Tab -------------------------------------------------------------------
login_tab <- tabPanel(
  title = icon("lock"),
  value = "login",
  loginUI("login")
)

## Data Input Tab --------------------------------------------------------------

DataInput_tab <- tabPanel("Data Input",
                          fluidPage(
                            sidebarPanel(
                              width = 3,
                              id = "DataInputPanel",
                              p(),
                              textInput("UserProjectName","Project Name:", value = "UMAP Analysis"),
                              uiOutput("rendExprFileInput"),
                              fluidRow(
                                column(5, style = 'padding-right:2px;margin-top:-30px;',
                                       checkboxInput("LogExprFile","Log2 Expression", value = F)
                                ),
                                column(7, style = 'padding-left:2px;margin-top:-30px;',
                                       checkboxInput("ScaleNormExprFile","Scale Normalize Expression", value = F)
                                )
                              ),
                              radioButtons("LowDemenReduc","Initial Low Dimensional Reduction:",
                                           choices = c("Base","NMF","PCA"), inline = T),
                              uiOutput("rendClinFileInput"),
                              fluidRow(
                                column(12, style = 'margin-top:-15px;',
                                       actionButton("UseExpData","Load Example Data"),
                                       tags$a(href="http://shawlab.science/shiny/PATH_SURVEYOR_ExampleData/PATH_SURVEYOR_App/", "Download example data", target='_blank'),
                                )
                              )
                            ),
                            mainPanel(
                              uiOutput("rendExprFilePrevHeader"),
                              div(DT::dataTableOutput("ExprFile_Preview"), style = "font-size:10px"),
                              uiOutput("rendClinFilePrevHeader"),
                              div(DT::dataTableOutput("ClinFile_Preview"), style = "font-size:10px")
                            )
                          )
)

## UMAP Tab --------------------------------------------------------------------

umap_tab <- tabPanel("UMAP",
                     fluidPage(
                       title = "UMAP",
                       sidebarLayout(
                         sidebarPanel(width = 3,
                                      
                                      ### Sidebar ------------------------------
                                      tabsetPanel(
                                        id = "mainSidebar",
                                        
                                        #### UMAP Params -----------------------
                                        
                                        tabPanel("UMAP Parameters",
                                                 tabsetPanel(
                                                   tabPanel("Data Input",
                                                            p(),
                                                            fluidRow(
                                                              column(4, style = 'padding-right:4px;',
                                                                     uiOutput("rendUMAPmetricSelec")
                                                              ),
                                                              column(4, style = 'padding-right:4px;padding-left:4px;',
                                                                     uiOutput("rendUMAPminDist")
                                                              ),
                                                              column(4, style = 'padding-left:4px;',
                                                                     uiOutput("rendUMAPnnb")
                                                              )
                                                            ),
                                                            h4("Annotate UMAP"),
                                                            uiOutput("rendUMAPsampSelect"),
                                                            tabsetPanel(
                                                              id = "AnnoUMAP",
                                                              tabPanel("Clinical",
                                                                       p(),
                                                                       #uiOutput("rendUMAPannotateSamps"),
                                                                       selectizeInput("UMAPannotateSamps","Annotate Samples By:",
                                                                                      choices = NULL, selected = 1,
                                                                                      options = list(
                                                                                        placeholder = 'Please select an option below',
                                                                                        onInitialize = I('function() { this.setValue(""); }')
                                                                                      )),
                                                                       fluidRow(
                                                                         column(6, style = 'margin-top:-20px;',
                                                                                #uiOutput("rendUMAPannotateSamps")
                                                                                uiOutput("rendRemoveNAsAnno")
                                                                         ),
                                                                         column(6, style = 'margin-top:-20px;',
                                                                                uiOutput("rendUMAPannoContCheck")
                                                                         )
                                                                       ),
                                                                       value = 1),
                                                              tabPanel("Feature",
                                                                       p(),
                                                                       fluidRow(
                                                                         column(7, style = 'padding-right:2px;',
                                                                                uiOutput("rendGeneSelection")
                                                                         ),
                                                                         column(2, style = 'padding-right:0px;padding-left:2px;',
                                                                                checkboxInput("LogGeneSelection","Log2",value = F)
                                                                                #uiOutput("rendLogGeneSelection"),
                                                                                #checkboxInput("LogGeneSelection","Log2",value = T)
                                                                         ),
                                                                         column(3, style = 'padding-left:0px;',
                                                                                uiOutput("rendGeneExprRange")
                                                                         )
                                                                       ),
                                                                       hr(),
                                                                       value = 2),
                                                              tabPanel("Clustering",
                                                                       p(),
                                                                       fluidRow(
                                                                         column(6, style = 'padding-right:4px;',
                                                                                selectInput("ClusterMethod", "Cluster Method:",
                                                                                            choices = c("ward.D", "complete", "ward.D2", "single",
                                                                                                        "average", "mcquitty", "median", "centroid")),
                                                                                checkboxInput("ViewUMAPclusterTab","View Cluster Table", value = F)
                                                                         ),
                                                                         column(6, style = 'padding-left:4px;',
                                                                                numericInput("ClusterNumber","Number of Clusters",value = 3),
                                                                                downloadButton("dnldUMAPclusterTab","Download Cluster Table")
                                                                         )
                                                                       ),
                                                                       div(DT::dataTableOutput("umapClusterTable"), style = "font-size:10px"),
                                                                       h4("Download Cluster Data"),
                                                                       fluidRow(
                                                                         column(3,
                                                                                numericInput("KmeansClusterSubsetnum","Cluster",value = 1,step = 1)
                                                                         ),
                                                                         column(9,
                                                                                downloadButton("dnldKmeansClusterSubsetExpr","Download Cluster Feature Matrix"),
                                                                                downloadButton("dnldKmeansClusterSubsetMeta","Download Cluster Meta")
                                                                         )
                                                                       ),
                                                                       hr(),
                                                                       value = 3)
                                                            ),
                                                            uiOutput("rendUMAPtypeHeader"),
                                                            uiOutput("rendRadioPathSelect"),
                                                            #textOutput("test_text"),
                                                            fluidRow(
                                                              column(8,
                                                                     uiOutput("rendUserGSupload")
                                                              ),
                                                              column(4,
                                                                     uiOutput("rendUserGSheaderCheck")
                                                              )
                                                            ),
                                                            uiOutput("rendGeneSetCatSelect"),
                                                            uiOutput("rendPathSelectTable"),
                                                            fluidRow(
                                                              column(4,
                                                                     uiOutput("rendTopNumMVG")
                                                              ),
                                                              column(4,
                                                                     uiOutput("rendVarMethodMVG")
                                                              ),
                                                              column(4,
                                                                     uiOutput("rendVeiwMVGTable")
                                                              )
                                                            ),
                                                            uiOutput("rendMVGlist"),
                                                            uiOutput("renddnldMVGtab")
                                                   ),
                                                   tabPanel("Figure Parameters",
                                                            p(),
                                                            selectInput("UMAPcolors","Continuous Variable Color Palette:",
                                                                        choices = c("Red" = "Reds","Blue" = "Blues","Purple" = "Purples",
                                                                                    "Green" = "Greens","Orange" = "Oranges","Grey" = "Greys",
                                                                                    "Blue/Green" = "BuGn","Yellow/Green" = "YlGn","Red/Purple" = "RdPu")),
                                                            selectInput("UMAPorientation","UMAP Plot Trio Orientation",choices = c("Side-by-Side","Stacked")),
                                                            fluidRow(
                                                              column(6,
                                                                     textInput("UMAPplotHeight","UMAP Plot Height:",value = "500px",
                                                                               placeholder = "e.g. '500px','auto','100%'")
                                                              ),
                                                              column(6,
                                                                     textInput("UMAPplotWidth","UMAP Plot Width:",value = "100%",
                                                                               placeholder = "e.g. '450px','auto','100%'")
                                                              )
                                                            ),
                                                            fluidRow(
                                                              column(6, style = 'padding-right:2px;',
                                                                     textInput("UMAPxAxisLim","X-Axis Limits: min,max",value = "")
                                                              ),
                                                              column(6, style = 'padding-right:2px;padding-left:2px;',
                                                                     textInput("UMAPyAxisLim","Y-Axis Limits: min,max",value = "")
                                                              )
                                                            ),
                                                            hr(),
                                                            h4("Font Sizes"),
                                                            fluidRow(
                                                              column(3,
                                                                     numericInput("UMAPtitleTextSize","Title:", value = 16)
                                                              ),
                                                              column(3,
                                                                     numericInput("UMAPaxisTextSize","Axis:", value = 12)
                                                              ),
                                                              column(3,
                                                                     numericInput("UMAPlegendTextSize","Legend:", value = 11)
                                                              ),
                                                              column(3,
                                                                     numericInput("UMAPdotSize","Dot size:", value = 2)
                                                              )
                                                            )
                                                   )
                                                 ),
                                                 value = 1),
                                        
                                        #### Feature Plot Params ---------------
                                        
                                        tabPanel("Feature Plot Parameters",
                                                 tabsetPanel(
                                                   tabPanel("Data Input",
                                                            p(),
                                                            uiOutput("rendBPsampSubset"),
                                                            uiOutput("rendBPsampCriteria"),
                                                            uiOutput("rendBPgroupCriteria"),
                                                            fluidRow(
                                                              column(8, style = 'padding-right:4px;',
                                                                     uiOutput("rendBPFeatSelection")
                                                              ),
                                                              column(4, style = 'padding-left:4px;',
                                                                     checkboxInput("log2Vplot","Log2(value+1) Feature",value = F)
                                                              )
                                                            ),
                                                            fluidRow(
                                                              column(5, style = 'padding-right:4px;',
                                                                     uiOutput("rendVplotstatComp")
                                                                     #selectInput("VplotstatComp","Stat Test Method:",
                                                                     #            choices = c("none","wilcox.test","t.test","kruskal.test","anova"))
                                                              ),
                                                              column(4, style = 'padding-right:4px;padding-left:4px;',
                                                                     uiOutput("rendVplotsampledots")
                                                                     #checkboxInput("Vplotsampledots","Include Dot Annotation", value = F)
                                                              ),
                                                              column(3, style = 'padding-left:4px;',
                                                                     uiOutput("rendVplotDotSize")
                                                                     #numericInput("VplotDotSize","Dot Size:", value =1, step = 0.25)
                                                              )
                                                            ),
                                                            radioButtons("ViolinOrBoxP","View As:",choices = c("Violin Plot","Box Plot","Stacked Barplot"), inline = T),
                                                            uiOutput("rendBPfeatFill")
                                                   ),
                                                   tabPanel("Figure Parameters",
                                                            p(),
                                                            textInput("VplotColoCodes","Color Code(s):",value = "",
                                                                      placeholder = "HEX or R Color Code(s) (Space Delim)"),
                                                            h4("Plot Axis Settings"),
                                                            fluidRow(
                                                              column(6,
                                                                     textInput("VPlotYlim","Y-Axis Limits", value = "", placeholder = "min,max"),
                                                                     selectInput("VxAxisOrient","X-Axis Label Angle",
                                                                                 choices = c(45,90,0))
                                                              ),
                                                              column(6,
                                                                     numericInput("VplotYbreaks","Y-Axis Breaks",value = "", step = 0.5),
                                                                     selectInput("VplotXaxOrder","X-Axis Group Order",
                                                                                 choices = c("Not Specificed","Ascending","Descending"))
                                                              )
                                                            ),
                                                            h4("Font Sizes"),
                                                            fluidRow(
                                                              column(3,
                                                                     numericInput("Vplot1TitleSize","Title:",
                                                                                  value = 20, step = 1)
                                                              ),
                                                              column(3,
                                                                     numericInput("Vplot1XAxisSize","X-Axis:",
                                                                                  value = 16, step = 1)
                                                              ),
                                                              column(3,
                                                                     numericInput("Vplot1YAxisSize","Y-Axis:",
                                                                                  value = 16, step = 1)
                                                              ),
                                                              column(3,
                                                                     numericInput("Vplot1aAnnoSize","Annotation:",
                                                                                  value = 7, step = 1)
                                                              )
                                                            )
                                                   )
                                                 ),
                                                 value = 3),
                                        
                                        #### Enrich Plot Params ----------------
                                        
                                        tabPanel("Enrichment Test",
                                                 p(),
                                                 tags$head(
                                                   tags$style(HTML('.selectize-input {
                                                                              white-space: nowrap;
                                                                              }
                                                                              .selectize-dropdown {
                                                                              width: 400px !important;
                                                                              }'
                                                   )
                                                   )
                                                 ),
                                                 uiOutput("rendBPsampSubsetEnrich"),
                                                 uiOutput("rendBPsampCriteriaEnrich"),
                                                 fluidRow(
                                                   column(6,
                                                          uiOutput("rendEnrichFeat1"),
                                                          uiOutput("rendEnrichVar1")
                                                   ),
                                                   column(6,
                                                          uiOutput("rendEnrichFeat2"),
                                                          uiOutput("rendEnrichVar2")
                                                   )
                                                 ),
                                                 radioButtons("FisherTailChoice","",choices = c("Two-Tailed" = "two.sided","One-Tailed - Greater" = "greater","One-Tailed - Less" = "less"), inline = T)
                                        )
                                      )
                         ),
                         
                         ### Main Panel ----------------------------------------
                         
                         mainPanel(
                           tabsetPanel(
                             id = "MainUMAPpan",
                             
                             #### MVG UMAP -------------------------------------
                             
                             tabPanel("Most Variable Features UMAP",
                                      tabsetPanel(
                                        tabPanel("UMAP",
                                                 p(),
                                                 fluidRow(
                                                   column(8,
                                                          uiOutput("rendUMAPmvgMainTitle")
                                                   ),
                                                   column(4,
                                                          jqui_draggable(jqui_resizable(plotOutput('UMAPmvgLegend', width = "100%", height = "110px")))
                                                   )
                                                 ),
                                                 p(),
                                                 uiOutput("rendUMAPplot_MVG_ALL"),
                                                 p(),
                                                 downloadButton("dnldUMAP_SVG_MVG_clin","Download Clinical Annotation UMAP"),
                                                 downloadButton("dnldUMAP_SVG_MVG_expr","Download Feature Range UMAP"),
                                                 downloadButton("dnldUMAP_SVG_MVG_kmean","Download Clustering UMAP"),
                                                 p(),
                                                 div(DT::dataTableOutput("UMAP_MVG_CoordTable"), style = "font-size:12px"),
                                                 downloadButton("dnldUMAPcoords_MVG","Download UMAP Coordinates")
                                        ),
                                        tabPanel("Feature Plot",
                                                 p(),
                                                 jqui_resizable(plotOutput('violin_MVG', width = "100%", height = "500px")),
                                                 div(DT::dataTableOutput("ViolinPlotTable"), style = "font-size:12px")
                                                 #downloadButton("dnldViolinPlotTable_MVG","Download Table")
                                        ),
                                        tabPanel("Enrichment Test",
                                                 p(),
                                                 withSpinner(jqui_resizable(plotOutput("EnrichVennPlotMVG", height = "450px", width = "100%"))),
                                                 hr(),
                                                 htmlOutput("EnrichFisherTextMVG", style = "font-size:14px;"),
                                                 hr(),
                                                 fluidRow(
                                                   column(6,
                                                          tableOutput("EnrichFisherTabMVG")
                                                   ),
                                                   column(6,
                                                          verbatimTextOutput("EnrichFisherOutMVG")
                                                   )
                                                 )
                                        )
                                      ),
                                      value = 4),
                             
                             #### Path UMAP ------------------------------------
                             
                             tabPanel("Pathway UMAP",
                                      tabsetPanel(
                                        tabPanel("UMAP",
                                                 p(),
                                                 fluidRow(
                                                   column(8,
                                                          uiOutput("rendUMAPpathMainTitle")
                                                   ),
                                                   column(4,
                                                          jqui_draggable(jqui_resizable(plotOutput('UMAPpathLegend', width = "100%", height = "110px")))
                                                   )
                                                 ),
                                                 p(),
                                                 uiOutput("rendUMAPplot_PATH_ALL"),
                                                 p(),
                                                 downloadButton("dnldUMAP_PATH_SVG_clin","Download Clinical Annotation UMAP"),
                                                 downloadButton("dnldUMAP_PATH_SVG_expr","Download Feature Range UMAP"),
                                                 downloadButton("dnldUMAP_PATH_SVG_kmean","Download Clustering UMAP"),
                                                 p(),
                                                 div(DT::dataTableOutput("UMAP_PATH_CoordTable"), style = "font-size:12px"),
                                                 downloadButton("dnldUMAPcoords_PATH","Download UMAP Coordinates")
                                        ),
                                        tabPanel("Feature Plot",
                                                 p(),
                                                 jqui_resizable(plotOutput('violin_PATH', width = "100%", height = "500px")),
                                                 div(DT::dataTableOutput("ViolinPlotTable_PATH"), style = "font-size:12px")
                                                 #downloadButton("dnldViolinPlotTable_PATH","Download Table")
                                        ),
                                        tabPanel("Enrichment Test",
                                                 p(),
                                                 withSpinner(jqui_resizable(plotOutput("EnrichVennPlotPATH", height = "450px", width = "100%"))),
                                                 hr(),
                                                 htmlOutput("EnrichFisherTextPATH", style = "font-size:14px;"),
                                                 hr(),
                                                 fluidRow(
                                                   column(6,
                                                          tableOutput("EnrichFisherTabPATH")
                                                   ),
                                                   column(6,
                                                          verbatimTextOutput("EnrichFisherOutPATH")
                                                   )
                                                 )
                                        )
                                      ),
                                      value = 3),
                             
                             #### Base UMAP ------------------------------------
                             
                             tabPanel("All Features UMAP",
                                      tabsetPanel(
                                        tabPanel("UMAP",
                                                 p(),
                                                 fluidRow(
                                                   column(8,
                                                          h3("UMAP of All Feaures from Matrix")
                                                   ),
                                                   column(4,
                                                          jqui_draggable(jqui_resizable(plotOutput('UMAPallLegend', width = "100%", height = "110px")))
                                                   )
                                                 ),
                                                 p(),
                                                 uiOutput("rendUMAPplot_ALL"),
                                                 p(),
                                                 downloadButton("dnldUMAP_SVG_clin","Download Clinical Annotation UMAP"),
                                                 downloadButton("dnldUMAP_SVG_expr","Download Feature Range UMAP"),
                                                 downloadButton("dnldUMAP_SVG_kmean","Download Clustering UMAP"),
                                                 p(),
                                                 div(DT::dataTableOutput("UMAP_CoordTable"), style = "font-size:12px"),
                                                 downloadButton("dnldUMAPcoords","Download UMAP Coordinates")),
                                        tabPanel("Feature Plot",
                                                 p(),
                                                 jqui_resizable(plotOutput('violin_BASE', width = "100%", height = "500px")),
                                                 div(DT::dataTableOutput("ViolinPlotTable_BASE"), style = "font-size:12px")
                                                 #downloadButton("dnldViolinPlotTable_PATH","Download Table")
                                        ),
                                        tabPanel("Enrichment Test",
                                                 p(),
                                                 withSpinner(jqui_resizable(plotOutput("EnrichVennPlotBASE", height = "450px", width = "100%"))),
                                                 hr(),
                                                 htmlOutput("EnrichFisherTextBASE", style = "font-size:14px;"),
                                                 hr(),
                                                 fluidRow(
                                                   column(6,
                                                          tableOutput("EnrichFisherTabBASE")
                                                   ),
                                                   column(6,
                                                          verbatimTextOutput("EnrichFisherOutBASE")
                                                   )
                                                 )
                                        )
                                      ),
                                      value = 2),
                             
                             #### PreC UMAP ------------------------------------
                             
                             tabPanel("Pre-Calculated UMAP",
                                      tabsetPanel(
                                        tabPanel("UMAP",
                                                 p(),
                                                 fluidRow(
                                                   column(3,
                                                          uiOutput("rendSelectPreCalc1")
                                                   ),
                                                   column(3,
                                                          uiOutput("rendSelectPreCalc2")
                                                   ),
                                                   column(6, style = 'padding-top:2px;padding-bottom:2px;',
                                                          jqui_draggable(jqui_resizable(plotOutput('UMAPprecLegend', width = "100%", height = "110px")))
                                                   )
                                                 ),
                                                 uiOutput("rendUMAPplot_PreC_ALL"),
                                                 p(),
                                                 downloadButton("dnldUMAP_SVG_PreC_clin","Download Clinical Annotation UMAP"),
                                                 downloadButton("dnldUMAP_SVG_PreC_expr","Download Feature Range UMAP"),
                                                 downloadButton("dnldUMAP_SVG_PreC_kmean","Download Clustering UMAP"),
                                                 p(),
                                                 div(DT::dataTableOutput("UMAP_PreC_CoordTable"), style = "font-size:12px")
                                        ),
                                        tabPanel("Feature Plot",
                                                 p(),
                                                 jqui_resizable(plotOutput('violin_PreC', width = "100%", height = "500px")),
                                                 div(DT::dataTableOutput("ViolinPlotTable_PreC"), style = "font-size:12px")
                                                 #downloadButton("dnldViolinPlotTable_PATH","Download Table")
                                        ),
                                        tabPanel("Enrichment Test",
                                                 p(),
                                                 withSpinner(jqui_resizable(plotOutput("EnrichVennPlotPreC", height = "450px", width = "100%"))),
                                                 hr(),
                                                 htmlOutput("EnrichFisherTextPreC", style = "font-size:14px;"),
                                                 hr(),
                                                 fluidRow(
                                                   column(6,
                                                          tableOutput("EnrichFisherTabPreC")
                                                   ),
                                                   column(6,
                                                          verbatimTextOutput("EnrichFisherOutPreC")
                                                   )
                                                 )
                                        )
                                      ),
                                      value = 6)
                           )
                         )
                       )
                     )
)

# Render UI --------------------------------------------------------------------

if (Password_Protected) {
  ui <- navbarPage(paste(ProjectName),
                   id = "tabs",
                   collapsible = TRUE,
                   login_tab)
} else {
  ui <- navbarPage(paste(ProjectName),
                   id = "tabs",
                   collapsible = TRUE,
                   DataInput_tab,
                   umap_tab)
}


server <- function(input, output, session) {
  
  # hack to add the logout button to the navbar on app launch
  if (Password_Protected) {
    insertUI(
      selector = ".navbar .container-fluid .navbar-collapse",
      ui = tags$ul(
        class="nav navbar-nav navbar-right",
        tags$li(
          div(
            style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
            logoutUI("logout")
          )
        )
      )
    )
  }
  
  # call the shinyauthr login and logout server modules
  if (Password_Protected) {
    credentials <- loginServer(
      id = "login",
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      sodium_hashed = FALSE,
      reload_on_logout = TRUE,
      log_out = reactive(logout_init())
    )
  } else {
    credentials <- reactive({
      list(user_auth = TRUE)
    })
  }
  
  if (Password_Protected) {
    logout_init <- logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
  }
  
  
  observeEvent(credentials()$user_auth, {
    
    if (Password_Protected) {
      if (credentials()$user_auth) {
        # remove the login tab
        removeTab("tabs", "login")
        # add home tab
        if (!FileProvided) {
          appendTab("tabs", DataInput_tab, select = TRUE)
          appendTab("tabs", umap_tab, select = FALSE)
        } else {
          appendTab("tabs", umap_tab, select = TRUE)
        }
      }
    }
    
    if (credentials()$user_auth) {
      
      # Reactive Val Start -----------------------------------------------------
      
      ## Set User Inputs
      ProjectName_react <- reactiveVal(ProjectName)
      ExpressionMatrix_file_react <- reactiveVal(ExpressionMatrix_file)
      MetaData_file_react <- reactiveVal(MetaData_file)
      MM_react <- reactiveVal(mouse)
      
      ## Set file reactives
      expr_raw <- reactiveVal()
      expr_react <- reactiveVal()
      meta_react <- reactiveVal()
      
      ## Geneset values
      gs <- reactiveVal()
      genesetCats <- reactiveVal()
      gs_cat2 <- reactiveVal()
      
      # Data Input Tab ---------------------------------------------------------
      
      output$rendExprFileInput <- renderUI({
        refresh <- input$UseExpData
        fileInput("ExprFileInput","Expression Matrix")
      })
      output$rendClinFileInput <- renderUI({
        refresh <- input$UseExpData
        fileInput("ClinFileInput","Clinical Data")
      })
      output$rendExprFilePrevHeader <- renderUI({
        req(ExpressionMatrix_file_react())
        h3("Expression File Preview")
      })
      output$rendClinFilePrevHeader <- renderUI({
        req(MetaData_file_react())
        h3("Clinical File Preview")
      })
      
      ## Update Inputs ---------------------------------------------------------
      
      ### URL Input ------------------------------------------------------------
      observe({
        query <- parseQueryString(session$clientData$url_search)
        print(query)
        if (isTruthy(query[['expr']]) && isTruthy(query[['meta']])) {
          ExpressionMatrix_file_react(query[['expr']])
          MetaData_file_react(query[['meta']])
        }
        if (isTruthy(query[['proj']])) {
          ProjectName_react(query[['proj']])
        }
        if ('mouse' %in% names(query)) {
          if (query[['mouse']] == "") {
            MM_react(TRUE)
          } else {
            MM_react(query[['mouse']])
          }
        } else {
          MM_react(FALSE)
        }
      })
      
      ### Example Data ---------------------------------------------------------
      observeEvent(input$UseExpData, {
        ProjectName_react("TCGA_CHOL")
        ExpressionMatrix_file_react(ExampleExpr_File)
        MetaData_file_react(ExampleClin_File)
        MM_react(FALSE)
        updateTextInput(session,"UserProjectName",value = "TCGA_CHOL")
      })
      
      ### User Upload ----------------------------------------------------------
      observe({
        if (isTruthy(input$ExprFileInput$datapath) & isTruthy(input$ClinFileInput$datapath)) {
          ProjectName_react(input$UserProjectName)
          ExpressionMatrix_file_react(input$ExprFileInput$datapath)
          MetaData_file_react(input$ClinFileInput$datapath)
        }
      })
      
      ## Load Data -------------------------------------------------------------
      
      observe({
        
        if (isTruthy(ExpressionMatrix_file_react()) & isTruthy(MetaData_file_react())) {
          # Expression file
          # If expression is file
          if (file.exists(ExpressionMatrix_file_react())) {
            print(paste0("Loading in local file: ",ExpressionMatrix_file_react()))
            if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("txt","tsv","TXT","TSV")) {
              expr <- as.data.frame(fread(ExpressionMatrix_file_react(), sep = '\t', header = T))
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("zip","gz","ZIP","GZ")) {
              expr <- as.data.frame(read_delim(ExpressionMatrix_file_react(), delim = '\t', col_names = T))
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("csv","CSV")) {
              expr <- as.data.frame(fread(ExpressionMatrix_file_react(), sep = ',', header = T))
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("RData","rdata")) {
              expr <- loadRData(ExpressionMatrix_file_react())
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("rds","RDS")) {
              expr <- readRDS(ExpressionMatrix_file_react())
            }
            # If expression is URL
          } else {
            print(paste0("Loading in url file: ",ExpressionMatrix_file_react()))
            if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("txt","tsv","TXT","TSV")) {
              expr <- as.data.frame(read_delim(url(ExpressionMatrix_file_react()), delim = '\t', col_names = T))
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("zip","gz","ZIP","GZ")) {
              expr <- as.data.frame(read_delim(Hmisc::getZip(ExpressionMatrix_file_react()), delim = '\t', col_names = T))
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("csv","CSV")) {
              expr <- as.data.frame(read_delim(url(ExpressionMatrix_file_react()), delim = ',', col_names = T))
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("RData","rdata")) {
              expr <- loadRData(url(ExpressionMatrix_file_react()))
            } else if (tools::file_ext(ExpressionMatrix_file_react()) %in% c("rds","RDS")) {
              expr <- readRDS(url(ExpressionMatrix_file_react()))
            }
          }
          # Remove Expression with NA
          expr <- expr %>%
            drop_na() %>%
            as.data.frame()
          # Check that expression data is numeric
          isChar <- unname(which(sapply(expr, function(x) is.character(x))))
          isChar <-  isChar[-1]
          if (length(isChar) > 0) {
            expr[isChar] <- sapply(expr[isChar],as.numeric)
          }
          colnames(expr)[1] <- "Gene"
          # Remove Duplicate genes
          if (TRUE %in% duplicated(expr[,1])) {
            expr <- expr %>%
              group_by(Gene) %>%
              summarise_all(max) %>%
              as.data.frame()
          }
          row.names(expr) <- expr[,1]
          expr <- expr[,-1]
          
          expr <- as.matrix(expr[order(rownames(expr)),])
          
          # Meta file
          # If Meta is file
          if (file.exists(MetaData_file_react())) {
            print(paste0("Loading in local file: ",MetaData_file_react()))
            if (tools::file_ext(MetaData_file_react()) %in% c("txt","tsv","TXT","TSV")) {
              meta <- as.data.frame(fread(MetaData_file_react(), sep = '\t', header = T))
            } else if (tools::file_ext(MetaData_file_react()) %in% c("zip","gz","ZIP","GZ")) {
              expr <- as.data.frame(read_delim(MetaData_file_react(), delim = '\t', col_names = T))
            } else if (tools::file_ext(MetaData_file_react()) %in% c("csv","CSV")) {
              meta <- as.data.frame(fread(MetaData_file_react(), sep = ',', header = T))
            } else if (tools::file_ext(MetaData_file_react()) %in% c("RData","rdata")) {
              meta <- loadRData(MetaData_file_react())
            } else if (tools::file_ext(MetaData_file_react()) %in% c("rds","RDS")) {
              meta <- readRDS(MetaData_file_react())
            }
            # If Meta is URL
          } else {
            print(paste0("Loading in url file: ",MetaData_file_react()))
            if (tools::file_ext(MetaData_file_react()) %in% c("txt","tsv","TXT","TSV")) {
              meta <- as.data.frame(read_delim(url(MetaData_file_react()), delim = '\t', col_names = T))
            } else if (tools::file_ext(MetaData_file_react()) %in% c("zip","gz","ZIP","GZ")) {
              meta <- as.data.frame(read_delim(Hmisc::getZip(MetaData_file_react()), delim = '\t', col_names = T))
            } else if (tools::file_ext(MetaData_file_react()) %in% c("csv","CSV")) {
              meta <- as.data.frame(read_delim(url(MetaData_file_react()), delim = ',', col_names = T))
            } else if (tools::file_ext(MetaData_file_react()) %in% c("RData","rdata")) {
              meta <- loadRData(url(MetaData_file_react()))
            } else if (tools::file_ext(MetaData_file_react()) %in% c("rds","RDS")) {
              meta <- readRDS(url(MetaData_file_react()))
            }
          }
          #colnames(meta_react())[1] <- "SampleName"
          
          # Ensure expression samples and meta are exact
          sampsames <- intersect(colnames(expr),meta[,1])
          expr <- expr[,sampsames]
          meta <- meta[which(meta[,1] %in% sampsames),]
          
          expr_raw(expr)
          expr_react(expr)
          meta_react(meta)
          
        }
        
      })
      
      observe({
        req(expr_raw())
        expr <- expr_raw()
        expr_col <- colnames(expr)
        if (isTruthy(input$LogExprFile)) {
          if (input$LogExprFile == T) {
            expr <- log2(expr + 0.00001)
          }
        }
        if (isTruthy(input$ScaleNormExprFile)) {
          if (input$ScaleNormExprFile == T) {
            expr = apply(expr, 1, scale)
            expr = apply(expr, 1, rev)
            colnames(expr) <- expr_col
          }
        }
        expr <- as.matrix(expr[sort(rownames(expr)),])
        expr_react(expr)
      })
      
      observe({
        
        if (isTruthy(MM_react())) {
          if (as.logical(MM_react())) {
            GeneSet_File <- GeneSet_MM_File
            GeneSetCat_File <- GeneSetCat_MM_File
          } else {
            GeneSet_File <- GeneSet_HS_File
            GeneSetCat_File <- GeneSetCat_HS_File
          }
        } else {
          GeneSet_File <- GeneSet_HS_File
          GeneSetCat_File <- GeneSetCat_HS_File
        }
        gs <- loadRData(GeneSet_File)
        gs_cat <- as.data.frame(read_delim(GeneSetCat_File, delim = '\t', col_names = T))
        genesetCats <- unique(gs_cat[,1])
        gs_cat2 <- gs_cat[,-2]
        colnames(gs_cat2)[c(2,3)] <- c("Gene Set Category","Gene Set Name")
        gs(gs)
        genesetCats(genesetCats)
        gs_cat2(gs_cat2)
      })
      
      ## Data Preview ----------------------------------------------------------
      output$ExprFile_Preview <- DT::renderDataTable({
        req(expr_react())
        expr <- expr_react()
        expr <- head(expr,c(100,100))
        DT::datatable(expr,
                      options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                     pageLength = 10,
                                     scrollX = T),
                      rownames = T)
      })
      
      output$ClinFile_Preview <- DT::renderDataTable({
        req(meta_react())
        clin <- meta_react()
        clin <- head(clin,c(100,100))
        DT::datatable(clin,
                      options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                     pageLength = 10,
                                     scrollX = T),
                      rownames = F)
      })
      
      # UMAP -------------------------------------------------------------------
      
      output$rendProjectNameHeader <- renderUI({
        
        h3(paste0(ProjectName))
        
      })
      
      output$rendUMAPsampSelect <- renderUI({
        
        expr <- expr_react()
        meta <- meta_react()
        samples <- c("Select All Samples",union(colnames(expr),meta[,1]))
        selectizeInput(
          "UMAPsampSelect",
          label = "Select Samples:",
          choices = samples,
          multiple = T,
          selected = "",
          options = list(delimiter = " ", create = T)
        )
      })
      
      #output$rendUMAPannotateSamps <- renderUI({
      #  meta <- meta_react()
      #  anno_options <- colnames(meta)[2:ncol(meta)]
      #  anno_options <- c(" ",anno_options)
      #  selectInput("UMAPannotateSamps","Annotate Samples By:", choices = anno_options, multiple = F)
      #})
      
      observe({
        req(meta_react())
        meta <- meta_react()
        anno_options <- colnames(meta)[-1]
        updateSelectizeInput(session,"UMAPannotateSamps","Annotate Samples By:", choices = anno_options,
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
      })
      
      output$rendUMAPannoContCheck <- renderUI({
        checkboxInput("UMAPannoContCheck","Continuous Variable")
      })
      
      output$rendRemoveNAsAnno <- renderUI({
        checkboxInput("RemoveNAsAnno","Remove NAs")
      })
      
      output$rendGeneSelection <- renderUI({
        
        expr <- expr_react()
        meta <- meta_react()
        anno_options <- colnames(meta)[2:ncol(meta)]
        genes <- rownames(expr)
        FeatureChoices <- c(genes,anno_options)
        selectizeInput("GeneSelection","Feature:", choices = FeatureChoices)
        
        
      })
      
      output$rendLogGeneSelection <- renderUI({
        
        geneSelec <- input$GeneSelection
        expr <- expr_raw()
        meta <- meta_react()
        if (!is.null(input$GeneSelection)) {
          if (geneSelec %in% rownames(expr)) {
            checkboxInput("LogGeneSelection","Log2",value = F)
          }
          else if (geneSelec %in% colnames(meta)) {
            checkboxInput("LogGeneSelection","Log2",value = F)
          }
        }
        
        
      })
      
      output$rendGeneExprRange <- renderUI({
        
        if (!is.null(input$GeneSelection)) {
          geneSelec <- input$GeneSelection
          expr <- expr_raw()
          meta <- meta_react()
          if (is.null(input$LogGeneSelection)) {
            LogChoice <- TRUE
          }
          else if (!is.null(input$LogGeneSelection)) {
            LogChoice <- input$LogGeneSelection
          }
          if (geneSelec %in% rownames(expr)) {
            if (LogChoice == TRUE) {
              expr <- log2(as.matrix(expr) + 0.00001)
            }
            exprRange <- round(range(expr[geneSelec,]),2)
            exprRangeText <- paste(exprRange[1],",",exprRange[2],sep = "")
            textInput("GeneExprRange","Range:",value = exprRangeText, placeholder = "min,max")
          }
          else if (geneSelec %in% colnames(meta)) {
            #if (is.numeric((meta[,geneSelec, drop = F]))) {
            meta_geneselec <- meta[,geneSelec, drop = F]
            if (LogChoice == TRUE) {
              meta_geneselec <- log2(as.matrix(meta[,geneSelec, drop = F]) + 0.00001)
            }
            exprRange <- round(range(meta_geneselec),2)
            exprRangeText <- paste(exprRange[1],",",exprRange[2],sep = "")
            textInput("GeneExprRange","Range:",value = exprRangeText, placeholder = "min,max")
            #}
            
          }
        }
        
      })
      
      output$rendUMAPtypeHeader <- renderUI({
        
        if (input$MainUMAPpan == 3) {
          h4("Pathway Selection")
        }
        else if (input$MainUMAPpan == 4) {
          h4("Most Variable Features")
        }
        
      })
      
      #selec_input <- reactiveValues(seleced_opt = 0)
      #observeEvent(input$RadioPathSelect, {
      #  selec_input$seleced_opt <- input$RadioPathSelect
      #})
      output$rendRadioPathSelect <- renderUI({
        
        if (input$MainUMAPpan == 3) {
          #if (selec_input$seleced_opt == 0) {
          radioButtons("RadioPathSelect","",choices = c("Provided Genesets","Upload Genesets"), inline = T)
          #}
          #if (selec_input$seleced_opt != 0) {
          #  radioButtons("RadioPathSelect","",choices = c("Provided Genesets","Upload Genesets"), inline = T, selected = selec_input$seleced_opt)
          #}
        }
        
      })
      
      #observeEvent(input$RadioPathSelect, {
      #
      #  #selec_input <- as.character(input$RadioPathSelect)
      #
      #  updateRadioButtons(session, "RadioPathSelect",
      #                     label = "",
      #                     choices = c("Provided Genesets","Upload Genesets"),
      #                     inline = T,
      #                     selected = input$RadioPathSelect)
      #  })
      
      output$rendUserGSupload <- renderUI({
        
        if (input$MainUMAPpan == 3) {
          if (input$RadioPathSelect == "Upload Genesets") {
            fileInput("UserGSupload","Upload Genesets", accept = c(".txt",".tsv",".gmt"))
          }
        }
        
      })
      
      output$rendUserGSheaderCheck <- renderUI({
        
        if (input$MainUMAPpan == 3) {
          if (input$RadioPathSelect == "Upload Genesets") {
            checkboxInput("UserGSheaderCheck","Header")
          }
        }
        
      })
      
      GS_CatSelec <- reactiveValues(GS_Cat = genesetCats()[1])
      observeEvent(input$GeneSetCatSelect, {
        GS_CatSelec$GS_Cat <- input$GeneSetCatSelect
      })
      output$rendGeneSetCatSelect <- renderUI({
        
        if (input$MainUMAPpan == 3) {
          if (input$RadioPathSelect == "Provided Genesets"){
            selectInput("GeneSetCatSelect","Select Gene Set Database:",choices = genesetCats(), selected = GS_CatSelec$GS_Cat)
          }
        }
        
      })
      
      output$rendPathSelectTable <- renderUI({
        
        if (input$MainUMAPpan == 3) {
          div(DT::dataTableOutput("PathSelectTable"), style = "font-size:10px")
        }
        
      })
      
      MVG_TopNum <- reactiveValues(MVG_num = 1000)
      observeEvent(input$TopNumMVG, {
        MVG_TopNum$MVG_num <- input$TopNumMVG
      })
      output$rendTopNumMVG <- renderUI({
        
        if (input$MainUMAPpan == 4) {
          numericInput("TopNumMVG","Top Features", value = MVG_TopNum$MVG_num, step = 1)
        }
        
      })
      
      MVG_VarMethod <- reactiveValues(MVG_Var = "MAD")
      observeEvent(input$VarMethodMVG, {
        MVG_VarMethod$MVG_Var <- input$VarMethodMVG
      })
      output$rendVarMethodMVG <- renderUI({
        
        if (input$MainUMAPpan == 4) {
          selectInput("VarMethodMVG","Variance Method", choices = c("MAD","VAR","CV"), selected = MVG_VarMethod$MVG_Var)
        }
        
      })
      
      output$rendMVGlist <- renderUI({
        
        if (input$MainUMAPpan == 4) {
          if (isTruthy(input$VeiwMVGTable)) {
            if (input$VeiwMVGTable == T) {
              div(DT::dataTableOutput("MVGlist"), style = "font-size:12px; height:350px; overflow-Y: scroll")
            }
          }
        }
        
      })
      
      output$renddnldMVGtab <- renderUI({
        
        if (input$MainUMAPpan == 4) {
          downloadButton("dnldMVGtab","Download Most Variable Features List")
        }
        
      })
      
      output$rendSelectPreCalc1 <- renderUI({
        
        meta <- meta_react()
        selectInput("SelectPreCalc1","Select X-Axis Coordinate Column:",
                    choices = colnames(meta)[2:ncol(meta)], selected = colnames(meta)[2])
        
      })
      
      output$rendSelectPreCalc2 <- renderUI({
        
        meta <- meta_react()
        selectInput("SelectPreCalc2","Select Y-Axis Coordinate Column:",
                    choices = colnames(meta)[2:ncol(meta)], selected = colnames(meta)[3])
        
      })
      
      output$rendVeiwMVGTable <- renderUI({
        
        if (input$MainUMAPpan == 4) {
          checkboxInput("VeiwMVGTable","View Most Variable Features", value = F)
        }
        
      })
      
      
      #output$renddnldUMAP_SVG_PreC_clin <- renderUI({
      #  req(input$UMAPmetaFile)
      #  downloadButton("dnldUMAP_SVG_PreC_clin","Download SVG")
      #})
      #output$renddnldUMAP_PDF_PreC_clin <- renderUI({
      #  req(input$UMAPmetaFile)
      #  downloadButton("dnldUMAP_PDF_PreC_clin","Download PDF")
      #})
      #
      #output$rendUMAPplot_PreC_expr <- renderUI({
      #  req(input$UMAPmetaFile)
      #  withSpinner(jqui_resizable(plotOutput('UMAPplot_PreC_expr', width = "100%", height = "400px")), type = 6)
      #})
      #output$renddnldUMAP_SVG_PreC_expr <- renderUI({
      #  req(input$UMAPmatrixFile)
      #  downloadButton("dnldUMAP_SVG_PreC_expr","Download SVG")
      #})
      #output$renddnldUMAP_PDF_PreC_expr <- renderUI({
      #  req(input$UMAPmatrixFile)
      #  downloadButton("dnldUMAP_PDF_PreC_expr","Download PDF")
      #})
      #
      #output$rendUMAPplot_PreC_kmean <- renderUI({
      #  req(input$UMAPmetaFile)
      #  withSpinner(jqui_resizable(plotOutput('UMAPplot_PreC_kmean', width = "100%", height = "400px")), type = 6)
      #})
      #output$renddnldUMAP_SVG_PreC_kmean <- renderUI({
      #  req(input$UMAPmatrixFile)
      #  downloadButton("dnldUMAP_SVG_PreC_kmean","Download SVG")
      #})
      #output$renddnldUMAP_PDF_PreC_kmean <- renderUI({
      #  req(input$UMAPmatrixFile)
      #  downloadButton("dnldUMAP_PDF_PreC_kmean","Download PDF")
      #})
      
      output$rendUMAPplot_MVG_ALL <- renderUI({
        
        ph <- input$UMAPplotHeight
        pw <- input$UMAPplotWidth
        withSpinner(jqui_resizable(plotlyOutput('UMAPplot_MVG_ALL', width = pw, height = ph)), type = 6)
        
      })
      output$rendUMAPplot_PATH_ALL <- renderUI({
        
        ph <- input$UMAPplotHeight
        pw <- input$UMAPplotWidth
        withSpinner(jqui_resizable(plotlyOutput('UMAPplot_PATH_ALL', width = pw, height = ph)), type = 6)
        
      })
      output$rendUMAPplot_ALL <- renderUI({
        
        ph <- input$UMAPplotHeight
        pw <- input$UMAPplotWidth
        withSpinner(jqui_resizable(plotlyOutput('UMAPplot_ALL', width = pw, height = ph)), type = 6)
        #withSpinner(jqui_resizable(plotOutput('UMAPplot_ALL', width = pw, height = ph)), type = 6)
        
      })
      output$rendUMAPplot_PreC_ALL <- renderUI({
        
        ph <- input$UMAPplotHeight
        pw <- input$UMAPplotWidth
        withSpinner(jqui_resizable(plotlyOutput('UMAPplot_PreC_ALL', width = pw, height = ph)), type = 6)
        #withSpinner(jqui_resizable(plotOutput('UMAPplot_PreC_ALL', width = pw, height = ph)), type = 6)
        
      })
      
      output$rendUMAPmvgMainTitle <- renderUI({
        
        if (!is.null(input$TopNumMVG)) {
          topMVGnum <- input$TopNumMVG
        }
        if (is.null(input$TopNumMVG)) {
          topMVGnum <- 1000
        }
        if (!is.null(input$VarMethodMVG)) {
          topMVGMethod <- input$VarMethodMVG
        }
        if (is.null(input$VarMethodMVG)) {
          topMVGMethod <- "MAD"
        }
        
        h3(paste("UMAP of Top",topMVGnum,"Most Variable Features by",topMVGMethod))
        
      })
      
      output$rendUMAPpathMainTitle <- renderUI({
        
        if (input$RadioPathSelect == "Provided Genesets") {
          if (input$PathSelectTable_rows_selected > 0) {
            rowsSelected <- input$PathSelectTable_rows_selected
            GeneSet <- gs_cat2()[rowsSelected,3]
          }
        }
        else if (input$RadioPathSelect == "Upload Genesets") {
          req(input$UserGSupload)
          gs_tab <- user_gs_upload()
          gs_tab2 <- data.frame(GeneSet = unique(gs_tab[,1]))
          gs_list <- user_gs_upload_asList()
          if (input$PathSelectTable_rows_selected > 0) {
            rowsSelected <- input$PathSelectTable_rows_selected
            GeneSet <- gs_tab2[rowsSelected,1]
          }
        }
        
        h3(paste("UMAP of",GeneSet,"Genes"))
        
      })
      
      #output$rendLowlyFilterNum <- renderUI({
      #
      #  if (input$FilterLowlyExpr == T) {
      #    numericInput("LowlyFilterNum", "Filter rowSums Below:", value = 1)
      #  }
      #
      #})
      
      #output$rendHighlyFilterNum <- renderUI({
      #
      #  if (input$FilterLowlyExpr == T) {
      #    numericInput("HighlyFilterNum", "Filter rowSums Above:", value = "")
      #  }
      #
      #})
      
      output$rendBPsampSubset <- renderUI({
        
        meta <- meta_react()
        anno_options <- colnames(meta)[2:ncol(meta)]
        #if (!is.null(input$UMAPmatrixFile)) {
        anno_options <- c("Select All","Cluster",anno_options)
        #}
        #if (is.null(input$UMAPmatrixFile)) {
        #  anno_options <- c("Select All",anno_options)
        #}
        selectInput("BPsampSubset","Subset Samples By:", choices = anno_options)
        
      })
      
      output$rendBPsampCriteria <- renderUI({
        
        SampSubset <- input$BPsampSubset
        if (!is.null(SampSubset)) {
          if (SampSubset != "Select All") {
            if (SampSubset == "Cluster") {
              ClusterNum <- input$ClusterNumber
              SubsetOptions <- seq(ClusterNum)
              selectInput("BPsampCriteria",paste(SampSubset,"Criteria:"), choices = SubsetOptions)
            }
            else {
              meta <- meta_react()
              SubsetOptions <- unique(meta[,SampSubset])
              selectInput("BPsampCriteria",paste(SampSubset,"Criteria:"), choices = SubsetOptions)
            }
          }
        }
        
      })
      
      BPgroupSelec <- reactiveValues(BPgroupSelec_Var = "Cluster")
      observeEvent(input$BPgroupCriteria, {
        BPgroupSelec$BPgroupSelec_Var <- input$BPgroupCriteria
      })
      output$rendBPgroupCriteria <- renderUI({
        
        SampSubset <- input$BPsampSubset
        SampCrit <- input$BPsampCriteria
        meta <- meta_react()
        anno_options <- colnames(meta)[2:ncol(meta)]
        #if (!is.null(input$UMAPmatrixFile)) {
        if (SampSubset != "Cluster") {
          anno_options <- c("Cluster",anno_options)
        }
        #}
        
        selectInput("BPgroupCriteria","Plot Grouping Criteria:", choices = anno_options, selected = BPgroupSelec$BPgroupSelec_Var)
        
      })
      output$rendBPFeatSelection <- renderUI({
        
        meta <- meta_react()
        features <- c()
        #if (!is.null(input$UMAPmatrixFile)) {
        features <- rownames(expr_raw())
        features <- c(features,colnames(meta)[-1])
        #}
        #features <- rownames(expr_raw())
        features <- c(features,colnames(meta)[-1])
        selectizeInput("BPFeatSelection","Select Feature:", choices = features)
        
      })
      
      UMAP_MetricSelec<- reactiveValues(metricSelec_Var = 1)
      observeEvent(input$UMAPmetricSelec, {
        UMAP_MetricSelec$metricSelec_Var <- input$UMAPmetricSelec
      })
      output$rendUMAPmetricSelec <- renderUI({
        
        if (input$MainUMAPpan %in% c(4,3,2)) {
          selectInput("UMAPmetricSelec","Metric:",
                      choices = c("euclidean","manhattan","cosine","pearson","pearson2","hamming",
                                  "correlation"
                                  ## Below required python integration with 'umap-learn'
                                  #"chebyshev","minkowski","canberra","braycurtis",
                                  #"mahalanobis","wminkowski","seuclidean","haversine","jaccard",
                                  #"dice","russelrao","kulsinski","rogerstanimoto","sokalmichener",
                                  #"sokalsneath","yule"
                      ),
                      selected = UMAP_MetricSelec$metricSelec_Var)
        }
        
      })
      
      UMAP_minDistSelec<- reactiveValues(minDistSelec_Var = 0.1)
      observeEvent(input$UMAPminDist, {
        UMAP_minDistSelec$minDistSelec_Var <- input$UMAPminDist
      })
      output$rendUMAPminDist <- renderUI({
        
        if (input$MainUMAPpan %in% c(4,3,2)) {
          numericInput("UMAPminDist","Min Distance:",
                       #value = 0.1,
                       value = UMAP_minDistSelec$minDistSelec_Var,
                       step = 0.05, min = 0)
        }
        
      })
      
      UMAP_nnbSelec<- reactiveValues(nnbSelec_Var = 15)
      observeEvent(input$UMAPnnb, {
        UMAP_nnbSelec$nnbSelec_Var <- input$UMAPnnb
      })
      output$rendUMAPnnb <- renderUI({
        
        if (input$MainUMAPpan %in% c(4,3,2)) {
          numericInput("UMAPnnb","N Neighbors:",
                       #value = 15,
                       value = UMAP_nnbSelec$nnbSelec_Var,
                       step = 1, min = 5, max = 50)
        }
        
      })
      
      output$rendBPfeatFill <- renderUI({
        
        if (input$ViolinOrBoxP == "Stacked Barplot") {
          
          checkboxInput("BPfeatFill","View Barplot Fill as Percentage", value = F)
          
        }
        
      })
      
      output$rendVplotstatComp <- renderUI({
        
        if (input$ViolinOrBoxP != "Stacked Barplot") {
          selectInput("VplotstatComp","Stat Test Method:",
                      choices = c("none","wilcox.test","t.test","kruskal.test","anova"))
        }
        
      })
      
      output$rendVplotsampledots <- renderUI({
        
        if (input$ViolinOrBoxP != "Stacked Barplot") {
          checkboxInput("Vplotsampledots","Include Dot Annotation", value = F)
        }
        
      })
      
      output$rendVplotDotSize <- renderUI({
        
        if (input$ViolinOrBoxP != "Stacked Barplot") {
          numericInput("VplotDotSize","Dot Size:", value =1, step = 0.25)
        }
        
      })
      
      output$rendBPsampSubsetEnrich <- renderUI({
        
        meta <- meta_react()
        anno_options <- colnames(meta)[2:ncol(meta)]
        anno_options <- c("Select All","Cluster",anno_options)
        selectInput("BPsampSubsetEnrich","Subset Samples By:", choices = anno_options)
        
      })
      
      output$rendBPsampCriteriaEnrich <- renderUI({
        
        SampSubset <- input$BPsampSubsetEnrich
        if (!is.null(SampSubset)) {
          if (SampSubset != "Select All") {
            if (SampSubset == "Cluster") {
              ClusterNum <- input$ClusterNumber
              SubsetOptions <- seq(ClusterNum)
              selectInput("BPsampCriteriaEnrich",paste(SampSubset,"Criteria:"), choices = SubsetOptions)
            }
            else {
              meta <- meta_react()
              SubsetOptions <- unique(meta[,SampSubset])
              selectInput("BPsampCriteriaEnrich",paste(SampSubset,"Criteria:"), choices = SubsetOptions)
            }
          }
        }
        
      })
      
      output$rendEnrichFeat1 <- renderUI({
        
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        anno_options <- colnames(meta)[2:ncol(meta)]
        anno_select <- "Cluster"
        #SampSubset <- input$BPsampSubsetEnrich
        #if (!is.null(SampSubset)) {
        #  if (SampSubset == "Cluster") {
        #    #anno_options <- anno_options[grep("^Cluster$",anno_options,value = T, invert = T)]
        #    anno_options <- anno_options[!anno_options == "Cluster"]
        #    anno_select <- anno_options[2]
        #  }
        #}
        selectInput("EnrichFeat1","Feature One:", choices = anno_options, selected = anno_select)
        
      })
      
      output$rendEnrichFeat2 <- renderUI({
        
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        anno_options <- colnames(meta)[2:ncol(meta)]
        #SampSubset <- input$BPsampSubsetEnrich
        #if (!is.null(SampSubset)) {
        #  if (SampSubset == "Cluster") {
        #    anno_options <- anno_options[!anno_options == "Cluster"]
        #    #anno_options <- anno_options[grep("^Cluster$",anno_options,value = T, invert = T)]
        #  }
        #}
        selectInput("EnrichFeat2","Feature Two:", choices = anno_options, selected = anno_options[1])
        
      })
      
      output$rendEnrichVar1 <- renderUI({
        
        Feat1 <- input$EnrichFeat1
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        VarChoices <- unique(meta[,Feat1])
        selectInput("EnrichVar1", "Variable from Feature One:", choices = VarChoices, selected = VarChoices[1])
        
      })
      
      output$rendEnrichVar2 <- renderUI({
        
        Feat2 <- input$EnrichFeat2
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        VarChoices <- unique(meta[,Feat2])
        selectInput("EnrichVar2", "Variable from Feature Two:", choices = VarChoices, selected = VarChoices[1])
        
      })
      
      
      ####----Reactives----####
      
      ### User Matrix Upload
      #expr_raw <- reactive({
      #
      #  #gs.u <- input$UMAPmatrixFile
      #  #ext <- tools::file_ext(gs.u$datapath)
      #  #req(gs.u)
      #  #validate(need(ext == c("tsv","txt","csv","zip"), "Please upload .tsv, .txt, or .csv file"))
      #  #
      #  #if (ext == "csv") {
      #  #  matrix.u <- as.data.frame(read_delim(gs.u$datapath, delim = ',', col_names = T))
      #  #}
      #  #else {
      #  #  matrix.u <- as.data.frame(read_delim(gs.u$datapath, delim = '\t', col_names = T))
      #  #}
      #
      #  matrix.u <- exprIn
      #
      #  #if (input$FeatRowOrCols == "Cols as Feat. - Rows as Samples") {
      #  #  rownames(matrix.u) <- matrix.u[,1]
      #  #  matrix.u <- matrix.u[,-1]
      #  #  matrix.u <- as.data.frame(t(matrix.u))
      #  #  matrix.u$Name <- rownames(matrix.u)
      #  #  matrix.u <- matrix.u %>%
      #  #    relocate(Name)
      #  #  rownames(matrix.u) <- 1:nrow(matrix.u)
      #  #}
      #  isChar <- unname(which(sapply(matrix.u, function(x) is.character(x))))
      #  isChar <-  isChar[-1]
      #  if (length(isChar) > 0) {
      #    matrix.u[isChar] <- sapply(matrix.u[isChar],as.numeric)
      #  }
      #  colnames(matrix.u)[1] <- "Symbol"
      #  if (TRUE %in% duplicated(matrix.u[,1])) {
      #    matrix.u <- matrix.u %>%
      #      group_by(Symbol) %>%
      #      summarise_all(max)
      #  }
      #  matrix.u <- as.data.frame(matrix.u)
      #  rownames(matrix.u) <- matrix.u[,1]
      #  matrix.u <- matrix.u[,-1]
      #  colnames(matrix.u) <- gsub("[[:punct:]]",".",colnames(matrix.u))
      #
      #  #expr_raw <- matrix.u
      #  matrix.u
      #
      #})
      #
      #umap_matrix_loaded <- reactive({
      #
      #  matrix.u <- expr_raw()
      #  matrix.u2 <- matrix.u
      #
      #  if (input$FilterLowlyExpr == T) {
      #    if (!is.na(input$LowlyFilterNum)) {
      #      matrix.u <- matrix.u[which(rowSums(matrix.u, na.rm = T) > input$LowlyFilterNum),]
      #    }
      #    if (!is.na(input$HighlyFilterNum)) {
      #      matrix.u <- matrix.u[which(rowSums(matrix.u, na.rm = T) < input$HighlyFilterNum),]
      #    }
      #  }
      #
      #  if (input$LogUMAPmatrix == T) {
      #    matrix.u <- log2(matrix.u + 0.00001)
      #  }
      #  if (input$NormUMAPmatrix == T) {
      #    matrix.u = apply(matrix.u, 1, scale)
      #    matrix.u = apply(matrix.u, 1, rev)
      #    colnames(matrix.u) <- colnames(matrix.u2)
      #  }
      #
      #  matrix.u <- as.matrix(matrix.u[sort(rownames(matrix.u)),])
      #
      #  #expr_reac <- matrix.u
      #  matrix.u
      #
      #})
      #
      ### User Meta Upload
      #umap_meta_loaded <- reactive({
      #
      #  #gs.u <- input$UMAPmetaFile
      #  #ext <- tools::file_ext(gs.u$datapath)
      #  #req(gs.u)
      #  #validate(need(ext == c("tsv","txt","csv"), "Please upload .tsv, .txt, or .csv file"))
      #  #
      #  #if (ext == "csv") {
      #  #  meta.u <- as.data.frame(read_delim(gs.u$datapath, delim = ',', col_names = T))
      #  #}
      #  #else {
      #  #  meta.u <- as.data.frame(read_delim(gs.u$datapath, delim = '\t', col_names = T))
      #  #}
      #  meta.u <- metaIn
      #
      #  colnames(meta.u)[1] <- "SampleName"
      #  meta.u$SampleName <- gsub("[[:punct:]]",".",meta.u$SampleName)
      #
      #  #umap_meta_loaded <- meta.u
      #  meta.u
      #
      #})
      
      ## User Geneset Upload
      user_gs_upload <- reactive({
        
        header_check <- input$UserGSheaderCheck
        gs.u <- input$UserGSupload
        ext <- tools::file_ext(gs.u$datapath)
        req(gs.u)
        validate(need(ext == c("tsv","txt","gmt"), "Please upload .tsv, .txt, or .gmt file"))
        if (ext == "gmt") {
          ranked_file <- read.gmt(gs.u$datapath)
        }
        else {
          ranked_file <- as.data.frame(read_delim(gs.u$datapath, delim = '\t', col_names = header_check, comment = "#"))
        }
        
        colnames(ranked_file) <- c("term","gene")
        ranked_file
        
      })
      
      ## Convert Uploaded GS to list
      user_gs_upload_asList <- reactive({
        
        user_gs <- user_gs_upload()
        gsDataList <- list()
        for (i in unique(user_gs[,1])){
          gsDataList[[i]] <- user_gs[user_gs[,1] == i,]$gene
        }
        gsDataList
        
      })
      
      ## Get MVGs from matrix
      umap_mvg <- reactive({
        
        expr <- expr_react()
        top_probes <- input$TopNumMVG
        var_type <- input$VarMethodMVG
        
        exp <- expr
        mad <- NULL
        var <- NULL
        cv <- NULL
        
        if (!is.null(var_type)) {
          
          if (var_type == "MAD"){
            mad <- apply(exp, 1, mad)
            mad <- sort(mad, decreasing = T)
            mad <- head(mad, n = (top_probes +1))
            out <- cbind(names(mad), mad[names(mad)], exp[names(mad),])
            colnames(out) <- c("Gene", "MAD", colnames(exp))
            dataset <- exp[names(mad),]
            variable_gene_list <- names(mad)
          }
          else if (var_type == "VAR"){
            var <- apply(exp, 1, var)
            var <- sort(var, decreasing = T)
            var <- head(var, n = (top_probes +1))
            out <- cbind(names(var), var[names(var)], exp[names(var),])
            colnames(out) <- c("Gene", "VAR", colnames(exp))
            dataset <- exp[names(var),]
            variable_gene_list <- names(var)
          }
          else if (var_type == "CV"){
            cv <- apply(exp, 1, cv)
            cv <- sort(cv, decreasing = T)
            cv <- head(cv, n = (top_probes +1))
            out <- cbind(names(cv), cv[names(cv)], exp[names(cv),])
            colnames(out) <- c("Gene", "CV", colnames(exp))
            dataset <- exp[names(cv),]
            variable_gene_list <- names(cv)
          }
          
          #umap_mvg <- variable_gene_list
          variable_gene_list
          
        }
        
      })
      
      ## Get Genes from chosen pathway
      path_specific_umap_genes <- reactive({
        
        if (input$RadioPathSelect == "Provided Genesets") {
          
          if (input$PathSelectTable_rows_selected > 0) {
            
            rowsSelected <- input$PathSelectTable_rows_selected
            GeneSet <- gs_cat2()[rowsSelected,3]
            gs_sub <- gs()[GeneSet]
            genesSelected <- unique(unname(unlist(gs_sub)))
            
          }
          
        }
        else if (input$RadioPathSelect == "Upload Genesets") {
          
          req(input$UserGSupload)
          gs_tab <- user_gs_upload()
          gs_tab2 <- data.frame(GeneSet = unique(gs_tab[,1]))
          gs_list <- user_gs_upload_asList()
          if (input$PathSelectTable_rows_selected > 0) {
            
            rowsSelected <- input$PathSelectTable_rows_selected
            GeneSet <- gs_tab2[rowsSelected,1]
            gs_sub <- gs_list[GeneSet]
            genesSelected <- unique(unname(unlist(gs_sub)))
            
          }
          
        }
        
        genesSelected
        
      })
      
      ## Get Cluster information on matrix
      umapClusterTable_react <- reactive({
        
        if (input$MainUMAPpan == 2 | input$MainUMAPpan == 6) {
          df <- expr_react()
        }
        else if (input$MainUMAPpan == 4) {
          df <- expr_react()
          genesSelected <- umap_mvg()
          df <- df[which(rownames(df) %in% genesSelected),]
        }
        else if (input$MainUMAPpan == 3) {
          df <- expr_react()
          if (length(input$PathSelectTable_rows_selected) > 0) {
            genesSelected <- path_specific_umap_genes()
            df <- df[which(rownames(df) %in% genesSelected),]
          }
        }
        
        clust_me <- input$ClusterMethod
        cut_k <- input$ClusterNumber
        
        results2 = hclust(dist(as.matrix(t(df))), method = clust_me)
        m = sort(cutree(results2, k=cut_k))
        output = as.data.frame(cbind(colnames(m), as.matrix(m)))
        output[,colnames(meta_react())[1]] <- rownames(output)
        output2 <- output[,c(2,1)]
        colnames(output2)[2] <- "Cluster"
        output2$Cluster <- as.factor(output2$Cluster)
        
        #umapClusterTable_react <- output2
        output2
        
      })
      
      ## Matrix containing only MVGs
      umap_matrix_mvg <- reactive({
        
        df <- expr_react()
        df <- as.data.frame(df)
        meta <- meta_react()
        if (isTruthy(input$UMAPannotateSamps)) {
          if (input$UMAPannotateSamps != "") {
            if (input$RemoveNAsAnno == TRUE) {
              NAsamps <- meta[which(is.na(meta[,input$UMAPannotateSamps])),1]
              df <- df[,which(!colnames(df) %in% NAsamps)]
            }
          }
        }
        df2 <- df
        #if (input$FilterLowlyExpr == T) {
        #  isexpr <- rowSums(df>input$LowlyFilterNum) >= 1
        #  if (!is.na(input$HighlyFilterNum)) {
        #    isexpr <- rowSums(df<input$HighlyFilterNum) >= 1
        #  }
        #  isexpr_t <- names(isexpr)[which(isexpr == T)]
        #  df2 <- df[rownames(df) %in% isexpr_t,]
        #}
        genesSelected <- umap_mvg()
        df2 <- df2[which(rownames(df2) %in% genesSelected),]
        df3 = as.matrix(df2)
        df4 = apply(df3, 1, rev)
        
        if (input$LowDemenReduc == "NMF") {
          df <- expr_raw()
          df <- as.data.frame(df)
          df2 <- df
          #if (input$FilterLowlyExpr == T) {
          #  isexpr <- rowSums(df>input$LowlyFilterNum) >= 1
          #  isexpr_t <- names(isexpr)[which(isexpr == T)]
          #  df2 <- df[rownames(df) %in% isexpr_t,]
          #}
          genesSelected <- umap_mvg()
          df2 <- df2[which(rownames(df2) %in% genesSelected),]
          df3 = as.matrix(df2)
          df4 = apply(df3, 1, rev)
          res <- nmf(df4, 2, method = "lee")
          W <- basis(res)
          df4 <- W
        }
        if (input$LowDemenReduc == "PCA") {
          pca_res <- prcomp(df4)
          scores <- as.matrix(pca_res$x)
          df4 <- scores
        }
        
        #umap_matrix_mvg <- df4
        df4
        
      })
      
      ## UMAP Coordinate Table  - MVG
      umap_plot_table_MVG_react <- reactive({
        
        tdata <- umap_matrix_mvg()
        set.seed(input$UserSetSeed)
        
        umapMetric <- input$UMAPmetricSelec
        umapMinDist <- input$UMAPminDist
        umapNN <- input$UMAPnnb
        
        
        if (!is.null(umapMetric) && !is.null(umapMinDist) && !is.null(umapNN)) {
          
          if (umapNN >= nrow(tdata)) {
            umapNN <- nrow(tdata) - 1
          }
          
          otherMets <- c("euclidean","manhattan","cosine","pearson","pearson2")
          
          if (umapMetric == "hamming" || umapMetric == "correlation") {
            tdata_fit_df <- as.data.frame(uwot::umap(tdata,metric = umapMetric, n_neighbors = umapNN, min_dist = umapMinDist))
            
          }
          else if (umapMetric %in% otherMets) {
            umap.mod$metric <- umapMetric
            umap.mod$min_dist <- umapMinDist
            umap.mod$n_neighbors <- umapNN
            tdata_fit <- umap::umap(tdata,config = umap.mod)
            tdata_fit_df <- as.data.frame(tdata_fit$layout)
          }
          
          tdata_fit_df <- tdata_fit_df %>%
            rename(UMAP1="V1",
                   UMAP2="V2") %>%
            mutate(ID=row_number())
          tdata_fit_df[,colnames(meta_react())[1]] <- rownames(tdata_fit_df)
          tdata_fit_df <- tdata_fit_df %>%
            relocate(any_of(colnames(meta_react())[1]))
          
          
          #umap_plot_table_MVG_react <- tdata_fit_df
          tdata_fit_df
          
        }
        
      })
      
      ## Matrix containing only genes in chosen pathway
      umap_matrix_pathway <- reactive({
        
        df <- expr_react()
        df <- as.data.frame(df)
        meta <- meta_react()
        if (input$UMAPannotateSamps != "") {
          if (input$RemoveNAsAnno == TRUE) {
            NAsamps <- meta[which(is.na(meta[,input$UMAPannotateSamps])),1]
            df <- df[,which(!colnames(df) %in% NAsamps)]
          }
        }
        df2 <- df
        #if (input$FilterLowlyExpr == T) {
        #  isexpr <- rowSums(df>input$LowlyFilterNum) >= 1
        #  isexpr_t <- names(isexpr)[which(isexpr == T)]
        #  df2 <- df[rownames(df) %in% isexpr_t,]
        #}
        genesSelected <- path_specific_umap_genes()
        df2 <- df2[which(rownames(df2) %in% genesSelected),]
        df3 = as.matrix(df2)
        df4 = apply(df3, 1, rev)
        
        if (input$LowDemenReduc == "NMF") {
          df <- expr_raw()
          df <- as.data.frame(df)
          df2 <- df
          #if (input$FilterLowlyExpr == T) {
          #  isexpr <- rowSums(df>input$LowlyFilterNum) >= 1
          #  isexpr_t <- names(isexpr)[which(isexpr == T)]
          #  df2 <- df[rownames(df) %in% isexpr_t,]
          #}
          genesSelected <- path_specific_umap_genes()
          df2 <- df2[which(rownames(df2) %in% genesSelected),]
          df3 = as.matrix(df2)
          df4 = apply(df3, 1, rev)
          res <- nmf(df4, 2, method = "lee")
          W <- basis(res)
          df4 <- W
        }
        if (input$LowDemenReduc == "PCA") {
          pca_res <- prcomp(df4)
          scores <- as.matrix(pca_res$x)
          df4 <- scores
        }
        
        df4
        
      })
      
      ## UMAP Coordinate Table  - Pathway
      umap_plot_table_PATH_react <- reactive({
        
        tdata <- umap_matrix_pathway()
        set.seed(input$UserSetSeed)
        
        umapMetric <- input$UMAPmetricSelec
        umapMinDist <- input$UMAPminDist
        umapNN <- input$UMAPnnb
        if (umapNN >= nrow(tdata)) {
          umapNN <- nrow(tdata) - 1
        }
        otherMets <- c("euclidean","manhattan","cosine","pearson","pearson2")
        
        if (umapMetric == "hamming" || umapMetric == "correlation") {
          tdata_fit_df <- uwot::umap(tdata,metric = umapMetric, n_neighbors = umapNN, min_dist = umapMinDist)
          
        }
        else if (umapMetric %in% otherMets) {
          umap.mod$metric <- umapMetric
          umap.mod$min_dist <- umapMinDist
          umap.mod$n_neighbors <- umapNN
          tdata_fit <- umap::umap(tdata,config = umap.mod)
          tdata_fit_df <- as.data.frame(tdata_fit$layout)
        }
        
        tdata_fit_df <- tdata_fit_df %>%
          rename(UMAP1="V1",
                 UMAP2="V2") %>%
          mutate(ID=row_number())
        tdata_fit_df[,colnames(meta_react())[1]] <- rownames(tdata_fit_df)
        tdata_fit_df <- tdata_fit_df %>%
          relocate(any_of(colnames(meta_react())[1]))
        
        
        tdata_fit_df
        
      })
      
      ## Base Matrix
      umap_matrix <- reactive({
        
        df <- expr_react()
        df <- as.data.frame(df)
        meta <- meta_react()
        if (input$UMAPannotateSamps != "") {
          if (input$RemoveNAsAnno == TRUE) {
            NAsamps <- meta[which(is.na(meta[,input$UMAPannotateSamps])),1]
            df <- df[,which(!colnames(df) %in% NAsamps)]
          }
        }
        df2 <- df
        #if (input$FilterLowlyExpr == T) {
        #  isexpr <- rowSums(df>input$LowlyFilterNum) >= 1
        #  isexpr_t <- names(isexpr)[which(isexpr == T)]
        #  df2 <- df[rownames(df) %in% isexpr_t,]
        #}
        df3 = as.matrix(df2)
        df4 = apply(df3, 1, rev)
        
        if (input$LowDemenReduc == "NMF") {
          df <- expr_raw()
          df <- as.data.frame(df)
          df2 <- df
          #if (input$FilterLowlyExpr == T) {
          #  isexpr <- rowSums(df>input$LowlyFilterNum) >= 1
          #  isexpr_t <- names(isexpr)[which(isexpr == T)]
          #  df2 <- df[rownames(df) %in% isexpr_t,]
          #}
          df3 = as.matrix(df2)
          df4 = apply(df3, 1, rev)
          df4[df4 < 0] <- 0
          res <- nmf(df4, 2, method = "lee")
          W <- basis(res)
          df4 <- W
        }
        if (input$LowDemenReduc == "PCA") {
          pca_res <- prcomp(df4)
          scores <- as.matrix(pca_res$x)
          df4 <- scores
        }
        
        df4
        
      })
      
      ## UMAP Coordinate Table  - Base
      umap_plot_table_react <- reactive({
        
        tdata <- umap_matrix()
        set.seed(input$UserSetSeed)
        
        umapMetric <- input$UMAPmetricSelec
        umapMinDist <- input$UMAPminDist
        umapNN <- input$UMAPnnb
        if (umapNN >= nrow(tdata)) {
          umapNN <- nrow(tdata) - 1
        }
        otherMets <- c("euclidean","manhattan","cosine","pearson","pearson2")
        
        if (umapMetric == "hamming" || umapMetric == "correlation") {
          tdata_fit_df <- uwot::umap(tdata,metric = umapMetric, n_neighbors = umapNN, min_dist = umapMinDist)
          
        }
        else if (umapMetric %in% otherMets) {
          umap.mod$metric <- umapMetric
          umap.mod$min_dist <- umapMinDist
          umap.mod$n_neighbors <- umapNN
          tdata_fit <- umap::umap(tdata,config = umap.mod)
          tdata_fit_df <- as.data.frame(tdata_fit$layout)
        }
        
        tdata_fit_df <- tdata_fit_df %>%
          rename(UMAP1="V1",
                 UMAP2="V2") %>%
          mutate(ID=row_number())
        tdata_fit_df[,colnames(meta_react())[1]] <- rownames(tdata_fit_df)
        tdata_fit_df <- tdata_fit_df %>%
          relocate(any_of(colnames(meta_react())[1]))
        
        
        tdata_fit_df
        
      })
      
      ## UMAP Coordinate Table  - Pre-Calculated
      umap_plot_table_PreC_react <- reactive({
        
        #req(input$UMAPmetaFile)
        meta <- meta_react()
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        tdata_fit_df <- meta[,c(colnames(meta_react())[1],umap1,umap2)]
        if (ncol(tdata_fit_df) >= 3) {
          colnames(tdata_fit_df)[c(2,3)] <- c("UMAP1","UMAP2")
        }
        
        tdata_fit_df
        
      })
      
      ####----Data Tables----####
      
      output$UMAPMatrixPreview <- DT::renderDataTable({
        
        df <- expr_react()
        
        DT::datatable(df,
                      extensions = "FixedColumns",
                      options = list(lengthMenu = c(10,20,50,100,1000,5000,10000),
                                     pageLength = 20,
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     fixedColumns = list(leftColumns = 1)),
                      selection=list(mode = "multiple")) %>%
          formatRound(columns = c(1:ncol(df)), digits = 4)
        
      })
      
      output$UMAPMetaPreview <- DT::renderDataTable({
        
        df <- umap_meta_loaded()
        
        DT::datatable(df,
                      extensions = "FixedColumns",
                      options = list(lengthMenu = c(10,20,50,100,1000,5000,10000),
                                     pageLength = 20,
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     fixedColumns = list(leftColumns = 1)),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
        
      })
      
      output$umapClusterTable <- DT::renderDataTable({
        
        if (input$ViewUMAPclusterTab == T) {
          
          output2 <- umapClusterTable_react()
          DT::datatable(output2,
                        options = list(keys = TRUE,
                                       searchHighlight = TRUE,
                                       pageLength = 5,
                                       lengthMenu = c("5","10", "25", "50", "100")),
                        rownames = F)
        }
        
      })
      
      #Path_Selec <- reactiveValues(row_selec = 1)
      #observeEvent(input$PathSelectTable_rows_selected, {
      #  Path_Selec$row_selec <- input$PathSelectTable_rows_selected
      #})
      
      output$PathSelectTable <- DT::renderDataTable({
        
        if (input$RadioPathSelect == "Provided Genesets") {
          chosen_cat <- input$GeneSetCatSelect
          gs_cat_sub <- gs_cat2()[which(gs_cat2()[,1] == chosen_cat),-1]
          DT::datatable(gs_cat_sub,
                        selection = list(mode = 'single', selected = 1),
                        options = list(keys = TRUE,
                                       searchHighlight = TRUE,
                                       pageLength = 10,
                                       scrollX = TRUE,
                                       lengthMenu = c("10", "25", "50", "100")),
                        rownames = F)
        }
        else if (input$RadioPathSelect == "Upload Genesets") {
          req(input$UserGSupload)
          gs_tab <- user_gs_upload()
          GeneSets <- unique(gs_tab[,1])
          gs_tab2 <- data.frame(GeneSet = GeneSets)
          DT::datatable(gs_tab2,
                        selection = list(mode = 'single', selected = 1),
                        options = list(keys = TRUE,
                                       searchHighlight = TRUE,
                                       pageLength = 10,
                                       scrollX = TRUE,
                                       lengthMenu = c("10", "25", "50", "100")),
                        rownames = F)
        }
        
      })
      
      #output$PathSelectTable <- DT::renderDataTable({
      #
      #  if (input$RadioPathSelect == "Provided Genesets") {
      #    if (Path_Selec$row_selec == 0) {
      #      chosen_cat <- input$GeneSetCatSelect
      #      gs_cat_sub <- gs_cat2()[which(gs_cat2()[,1] == chosen_cat),-1]
      #      DT::datatable(gs_cat_sub,
      #                    selection = list(mode = 'single', selected = 1),
      #                    options = list(keys = TRUE,
      #                                   searchHighlight = TRUE,
      #                                   pageLength = 10,
      #                                   scrollX = TRUE,
      #                                   lengthMenu = c("10", "25", "50", "100")),
      #                    rownames = F)
      #    }
      #    else if (Path_Selec$row_selec > 0) {
      #      chosen_cat <- input$GeneSetCatSelect
      #      gs_cat_sub <- gs_cat2()[which(gs_cat2()[,1] == chosen_cat),-1]
      #      DT::datatable(gs_cat_sub,
      #                    selection = list(mode = 'single', selected = Path_Selec$row_selec),
      #                    options = list(keys = TRUE,
      #                                   searchHighlight = TRUE,
      #                                   pageLength = 10,
      #                                   scrollX = TRUE,
      #                                   lengthMenu = c("10", "25", "50", "100")),
      #                    rownames = F)
      #    }
      #  }
      #  else if (input$RadioPathSelect == "Upload Genesets") {
      #    req(input$UserGSupload)
      #    if (Path_Selec$row_selec == 0) {
      #      gs_tab <- user_gs_upload()
      #      GeneSets <- unique(gs_tab[,1])
      #      gs_tab2 <- data.frame(GeneSet = GeneSets)
      #      DT::datatable(gs_tab2,
      #                    selection = list(mode = 'single', selected = 1),
      #                    options = list(keys = TRUE,
      #                                   searchHighlight = TRUE,
      #                                   pageLength = 10,
      #                                   scrollX = TRUE,
      #                                   lengthMenu = c("10", "25", "50", "100")),
      #                    rownames = F)
      #    }
      #    else if (Path_Selec$row_selec > 0) {
      #      gs_tab <- user_gs_upload()
      #      GeneSets <- unique(gs_tab[,1])
      #      gs_tab2 <- data.frame(GeneSet = GeneSets)
      #      DT::datatable(gs_tab2,
      #                    selection = list(mode = 'single', selected = Path_Selec$row_selec),
      #                    options = list(keys = TRUE,
      #                                   searchHighlight = TRUE,
      #                                   pageLength = 10,
      #                                   scrollX = TRUE,
      #                                   lengthMenu = c("10", "25", "50", "100")),
      #                    rownames = F)
      #    }
      #  }
      #
      #})
      
      output$MVGlist <- renderDataTable({
        
        if (input$MainUMAPpan == 4) {
          
          if (input$VeiwMVGTable == T) {
            
            gene_list <- umap_mvg()
            top_num <- input$TopNumMVG
            gene_list_df <- data.frame(Feature = gene_list)
            DT::datatable(gene_list_df, options = list(paging = F,scrollY = TRUE), rownames = F)
            
          }
          
        }
        
      })
      
      FeatExpreAnnotation <- reactive({
        
        expr <- expr_raw()
        meta <- meta_react()
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        if (!is.null(input$GeneSelection)) {
          feat <- input$GeneSelection
          if (feat %in% rownames(expr)) {
            if (LogChoice == TRUE) {
              expr <- log2(as.matrix(expr) + 0.00001)
            }
            exprRange <- round(range(expr[feat,]),2)
            expr2 <- as.data.frame(expr)
            expr_g <- expr2[feat,]
            expr_g_t <- as.data.frame(t(expr_g))
            expr_g_t[,colnames(meta_react())[1]] <- rownames(expr_g_t)
            expr_g_t
          }
          else if (feat %in% colnames(meta)) {
            expr_g_t <- meta[,c(colnames(meta_react())[1],feat)]
            expr_g_t
          }
        }
        else if (is.null(input$GeneSelection)) {
          feat <- rownames(expr)[1]
          if (LogChoice == TRUE) {
            expr <- log2(as.matrix(expr) + 0.00001)
          }
          exprRange <- round(range(expr[feat,]),2)
          expr2 <- as.data.frame(expr)
          expr_g <- expr2[feat,]
          expr_g_t <- as.data.frame(t(expr_g))
          expr_g_t[,colnames(meta_react())[1]] <- rownames(expr_g_t)
          expr_g_t
        }
        
      })
      
      UMAP_MVG_CoordTable_react <- reactive({
        
        tdata_fit_df <- umap_plot_table_MVG_react()
        tdata_fit_df <- tdata_fit_df[,-4]
        
        UMAPannotateSamps <- input$UMAPannotateSamps
        metaColanno <- input$UMAPannotateSamps
        meta <- meta_react()
        #save(list = ls(), file = "UMAP_MVG_CoordTable_react.RData", envir = environment())
        
        ## Add Annotation column
        #if (is.null(input$UMAPmetaFile) == F) {
        if (!is.null(metaColanno)) {
          if (metaColanno != "") {
            tdata_fit_df <- merge(tdata_fit_df,
                                  meta[,c(colnames(meta)[1],metaColanno)],
                                  by.x = colnames(tdata_fit_df)[1],
                                  by.y = colnames(meta)[1])
          }
          
        }
        #}
        
        ## Add Gene Expression Column
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        else if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        
        
        
        ## Add Cluster Annotation Column
        cluster_tab <- umapClusterTable_react()
        metaColkmean <- "Cluster"
        tdata_fit_df <- merge(tdata_fit_df,cluster_tab, by = colnames(meta_react())[1])
        
        tdata_fit_df
        
        
      })
      
      output$UMAP_MVG_CoordTable <- DT::renderDataTable({
        
        tdata_fit_df <- UMAP_MVG_CoordTable_react()
        
        #table output,
        DT::datatable(tdata_fit_df,
                      options = list(keys = TRUE,
                                     searchHighlight = TRUE,
                                     pageLength = 20,
                                     scrollY = T,
                                     lengthMenu = c("10", "20", "50", "100")
                      ),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      UMAP_PATH_CoordTable_react <- reactive({
        
        tdata_fit_df <- umap_plot_table_PATH_react()
        tdata_fit_df <- tdata_fit_df[,-4]
        
        ## Add Annotation column
        metaColanno <- input$UMAPannotateSamps
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          meta <- meta_react()
          metaColanno <- input$UMAPannotateSamps
          tdata_fit_df <- merge(tdata_fit_df,meta[,c(colnames(meta_react())[1],metaColanno)])
        }
        #}
        
        ## Add Gene Expression Column
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        else if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        ## Add Cluster Annotation Column
        cluter_tab <- umapClusterTable_react()
        metaColkmean <- "Cluster"
        tdata_fit_df <- merge(tdata_fit_df,cluter_tab, by = colnames(meta_react())[1])
        
        tdata_fit_df
        
      })
      
      output$UMAP_PATH_CoordTable <- DT::renderDataTable({
        
        tdata_fit_df <- UMAP_PATH_CoordTable_react()
        #table output,
        DT::datatable(tdata_fit_df,
                      options = list(keys = TRUE,
                                     searchHighlight = TRUE,
                                     pageLength = 20,
                                     scrollY = T,
                                     lengthMenu = c("10", "20", "50", "100")
                      ),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      UMAP_CoordTable_react <- reactive({
        
        tdata_fit_df <- umap_plot_table_react()
        tdata_fit_df <- tdata_fit_df[,-4]
        
        ## Add Annotation column
        metaColanno <- input$UMAPannotateSamps
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          meta <- meta_react()
          metaColanno <- input$UMAPannotateSamps
          tdata_fit_df <- merge(tdata_fit_df,meta[,c(colnames(meta_react())[1],metaColanno)])
        }
        #}
        
        ## Add Gene Expression Column
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        else if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        
        ## Add Cluster Annotation Column
        cluter_tab <- umapClusterTable_react()
        metaColkmean <- "Cluster"
        tdata_fit_df <- merge(tdata_fit_df,cluter_tab, by = colnames(meta_react())[1])
        
        tdata_fit_df
        
        
      })
      
      output$UMAP_CoordTable <- DT::renderDataTable({
        
        tdata_fit_df <- UMAP_CoordTable_react()
        #table output,
        DT::datatable(tdata_fit_df,
                      options = list(keys = TRUE,
                                     searchHighlight = TRUE,
                                     pageLength = 20,
                                     scrollY = T,
                                     lengthMenu = c("10", "20", "50", "100")
                      ),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      UMAP_PreC_CoordTable_react <- reactive({
        
        
        
        tdata_fit_df <- umap_plot_table_PreC_react()
        
        ## Add Annotation column
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          meta <- meta_react()
          metaColanno <- input$UMAPannotateSamps
          tdata_fit_df <- merge(tdata_fit_df,meta[,c(colnames(meta_react())[1],metaColanno)], by = colnames(meta_react())[1])
        }
        #}
        
        ## Add Gene Expression Column
        #if (!is.null(input$UMAPmatrixFile)) {
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        else if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          tdata_fit_df <- merge(tdata_fit_df,expr_g_t, by = colnames(meta_react())[1])
        }
        
        ## Add Cluster Annotation Column
        cluter_tab <- umapClusterTable_react()
        metaColkmean <- "Cluster"
        tdata_fit_df <- merge(tdata_fit_df,cluter_tab, by = colnames(meta_react())[1])
        #}
        
        tdata_fit_df
        
        
      })
      
      output$UMAP_PreC_CoordTable <- DT::renderDataTable({
        
        tdata_fit_df <- UMAP_PreC_CoordTable_react()
        #table output,
        DT::datatable(tdata_fit_df,
                      options = list(keys = TRUE,
                                     searchHighlight = TRUE,
                                     pageLength = 20,
                                     scrollY = T,
                                     lengthMenu = c("10", "20", "50", "100")
                      ),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      ####----Plots----####
      
      ####----MVG UMAP----####
      
      ## UMAP Plot  - MVG - Clin
      umap_plot_MVG_react_clin_base <- reactive({
        req(UMAP_MVG_CoordTable_react())
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot z
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2",
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2")
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  k <- k + labs(x = "UMAP1",
        #                y = "UMAP2")
        #}
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          if (input$UMAPannoContCheck == T) {
            myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
            k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
          }
        }
        #}
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        k
        
      })
      
      ## UMAP Plot  - MVG - Clin
      umap_plot_MVG_react_clin_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot z
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2",
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2")
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  k <- k + labs(x = "UMAP1",
        #                y = "UMAP2")
        #}
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          if (input$UMAPannoContCheck == T) {
            myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
            k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
          }
        }
        #}
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            min.segment.length = 0,
                            color = "black",
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        k
        
      })
      
      umap_plot_MVG_react_clin <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        k <- umap_plot_MVG_react_clin_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        k2 <- ggplotly(k,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F))
        
        k2 <- k2 %>%
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          k2 <- k2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        k2
        
      })
      
      umap_plot_MVG_react_expr_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        m <- m +
          geom_point(shape = 19,
                     size = UMAPdotSize) +
          
          theme_minimal()
        
        m <- m + labs(x = "UMAP1",
                      y = "UMAP2",
                      color = metaColgene)
        
        m <-  m + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        m <- m + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          m <- m + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        m
        
      })
      
      umap_plot_MVG_react_expr_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        m <- m +
          geom_point(shape = 19,
                     size = UMAPdotSize) +
          
          theme_minimal()
        
        m <- m + labs(x = "UMAP1",
                      y = "UMAP2",
                      color = metaColgene)
        
        m <-  m + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        m <- m + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          m <- m + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        m
        
      })
      
      ## UMAP Plot  - Base - expr
      umap_plot_MVG_react_expr <- reactive({
        
        
        ## Variables}## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        m <- umap_plot_MVG_react_expr_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        m2 <- ggplotly(m,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F)) %>%
          
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          m2 <- m2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        
        m2
        
      })
      
      umap_plot_MVG_react_kmean_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        n <- n + geom_point(shape = 19,
                            size = UMAPdotSize)+
          
          labs(x = "UMAP1",
               y = "UMAP2",
               color = "Cluster")  +
          
          theme_minimal()
        
        n <- n + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          n <- n + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        n
        
      })
      
      umap_plot_MVG_react_kmean_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        n <- n + geom_point(shape = 19,
                            size = UMAPdotSize)+
          
          labs(x = "UMAP1",
               y = "UMAP2",
               color = "Cluster")  +
          
          theme_minimal()
        
        n <- n + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          n <- n + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        n
        
      })
      
      ## UMAP Plot  - Base - kmean
      umap_plot_MVG_react_kmean <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_MVG_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        n <- umap_plot_MVG_react_kmean_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        n2 <- ggplotly(n,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F)) %>%
          
          hide_legend() %>%
          hide_colorbar()
        if (length(sampSelected) > 0) {
          n2 <- n2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        
        n2
        
      })
      
      output$UMAPmvgLegend <- renderPlot({
        
        k <- umap_plot_MVG_react_clin_base()
        m <- umap_plot_MVG_react_expr_base()
        n <- umap_plot_MVG_react_kmean_base()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          legendk <- g_legend(k)
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendk,legendm,legendn,nrow = 1)
        }
        else if (input$UMAPannotateSamps == "") {
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendm,legendn,nrow = 1)
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  legendm <- g_legend(m)
        #  legendn <- g_legend(n)
        #  legend_grid <- grid.arrange(legendm,legendn,nrow = 1)
        #}
        
        legend_grid
        
      })
      
      UMAPplot_MVG_ALL_react <- reactive({
        
        k2 <- umap_plot_MVG_react_clin()
        m2 <- umap_plot_MVG_react_expr()
        n2 <- umap_plot_MVG_react_kmean()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          UMAPtitlek <- paste("Annotated by",input$UMAPannotateSamps)
        }
        else if (input$UMAPannotateSamps == "") {
          UMAPtitlek <- ""
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  UMAPtitlek <- ""
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          GeneSelec <- colnames(expr_g_t)[1]
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          GeneSelec <- colnames(expr_g_t)[1]
        }
        #if (!is.null(input$GeneSelection)) {
        #  GeneSelec <- input$GeneSelection
        #}
        #if (is.null(input$GeneSelection)) {
        #  GeneSelec <- rownames(expr)[1]
        #}
        
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        if (LogChoice == T) {
          GeneSelecExpr <- paste(GeneSelec,"(Log2)")
        }
        if (LogChoice == F) {
          GeneSelecExpr <- paste(GeneSelec)
        }
        
        UMAPtitlem <- paste("Annotated by",GeneSelecExpr)
        UMAPtitlen <- paste("Annotated by",input$ClusterMethod,input$ClusterNumber,"Clusters")
        
        
        if (input$UMAPorientation == "Side-by-Side") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 1, shareY = TRUE, shareX = TRUE)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlem,
              xref = "x2",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlen,
              xref = "x3",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        else if (input$UMAPorientation == "Stacked") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 3, shareY = TRUE, shareX = TRUE, margin = 0.04)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.65,
              text = UMAPtitlem,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.3,
              text = UMAPtitlen,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        
        subplot_all_sbs <- subplot_all_sbs %>% layout(annotations = plot_titles)
        
        subplot_all_sbs
      })
      
      output$UMAPplot_MVG_ALL <- renderPlotly({
        p_all <- UMAPplot_MVG_ALL_react()
        p_all
      })
      
      ####----MVG Violin Plot----####
      
      VI_meta_subset <- reactive({
        
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        if (sampSubset == "Select All") {
          metaSub <- meta
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),all_of(sampGroupCriteria))
          metaSub
        }
        else if (sampSubset != "Select All") {
          metaSub <- meta[which(meta[,sampSubset] == sampGroupCriteria),]
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),sampGroupCriteria,sampSubset)
          metaSub
        }
        
      })
      
      violin_MVG_react <- reactive({
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          groupCriteria <- "Cluster"
        } else {groupCriteria <- input$BPgroupCriteria}
        
        title_font <- input$Vplot1TitleSize            # Title font size
        Xaxis_font <- input$Vplot1XAxisSize              # Axis font size
        Yaxis_font <- input$Vplot1YAxisSize              # Axis font size
        anno_size <- input$Vplot1aAnnoSize
        hjust_orient <- 1                                # Initial hjust
        axis_orient <- as.numeric(input$VxAxisOrient)  # X-axis label orientation
        if (axis_orient == 0) {                          # Adjust hjust if orientation is 0
          hjust_orient <- 0.5
        }
        logchoice <- input$log2Vplot                   # Log expression data option
        colorin <- input$VplotColoCodes                # Bar plot Color codes
        Vplotylim <- input$VPlotYlim                      # Y-limit
        Vplotybreaks <- input$VplotYbreaks                # Y-axis breaks
        if (is.null(input$VplotDotSize)) {
          dotsizein <- 1
        } else {dotsizein <- input$VplotDotSize}
        #dotsizein <- input$VplotDotSize
        if (is.null(input$VplotstatComp)) {
          StatMethod <- "None"
        } else {StatMethod <- input$VplotstatComp}
        #StatMethod <- input$VplotstatComp
        if (is.null(input$Vplotsampledots)) {
          dotChoice <- FALSE
        } else {dotChoice <- input$Vplotsampledots}
        #dotChoice <- input$Vplotsampledots
        VilOrBP <- input$ViolinOrBoxP
        
        FeatMat <- expr_raw()
        metaSub <- VI_meta_subset()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        #sampSubset <- input$BPsampSubset
        sampCriteria <- input$BPsampCriteria
        #groupCriteria <- input$BPgroupCriteria
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        sampSelected <- input$UMAPsampSelect
        metaSub2 <- metaSub[,c(colnames(meta_react())[1],groupCriteria)]
        metaSub2[,groupCriteria] <- as.factor(metaSub2[,groupCriteria])
        
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          feat_gene2 <- merge(feat_gene,metaSub2)
          #feat_gene2 <- merge(feat_gene,metaSub2,by = "SampleName", all = T)
          feature_lab <- feature
          feattitle <- feature_lab
          if (logchoice == T) {
            feature_lab <- paste(feature_lab,"(Log2)")
            feattitle <- feature_lab
          }
          
          if (sampSubset == "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab, "Across Top", input$TopNumMVG, "Most Variable Features\nClustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab, "Across Top", input$TopNumMVG, "Most Variable Features\nGrouped by",groupCriteria)
            }
          }
          if (sampSubset != "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nAcross Top", input$TopNumMVG, "Most Variable Features\nClustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nAcross Top", input$TopNumMVG, "Most Variable Features\nGrouped by",groupCriteria)
            }
          }
          
          
          
          colnames(feat_gene2) <- c(colnames(meta_react())[1],"FeatureName","Type")
          
          #if (!is.numeric(feat_gene2$FeatureName)) {
          if (VilOrBP == "Stacked Barplot") {
            feat_gene2[,"FeatureName"] <- as.factor(feat_gene2[,"FeatureName"])
            #if (input$VplotXaxOrder == "Descending"){
            #  feat_gene2$Type <- fct_rev(feat_gene2$Type)
            #  feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            #}
            #if (input$VplotXaxOrder == "Ascending"){
            #  feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            #}
            barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            if (is.null(input$BPfeatFill)) {
              FillChoice <- FALSE
            } else {FillChoice <- input$BPfeatFill}
            #FillChoice <- input$BPfeatFill
            if (FillChoice == FALSE) {
              barp <- barp + geom_bar() +
                theme_minimal()
            }
            if (FillChoice == TRUE) {
              barp <- barp + geom_bar(position = "fill") +
                theme_minimal()
            }
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)))
            barp
            
          }
          #}
          
          else if(!VilOrBP == "Stacked Barplot") {
            #if (is.numeric(feat_gene2$FeatureName)) {
            
            if (logchoice == T) {
              #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
              feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
            }
            
            y_min <- 0
            y_max <- max(feat_gene2$FeatureName, na.rm = T) * 1.03
            if (Vplotylim != "") {
              y_min <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][1]))
              y_max <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][2]))
            }
            
            if (input$VplotXaxOrder == "Descending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            }
            if (input$VplotXaxOrder == "Ascending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            }
            
            barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #if (VilOrBP == "Stacked Barplot") {
            #  barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            #}
            #if (VilOrBP != "Stacked Barplot") {
            #  barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #}
            if (StatMethod != "None") {
              barp <- barp + stat_compare_means(data = feat_gene2,
                                                aes(x=Type,y=FeatureName),
                                                method = StatMethod,
                                                label.x = 1,
                                                label.y = max(feat_gene2$FeatureName, na.rm = T) * 1.02)
            }
            if (VilOrBP == "Box Plot") {
              barp <- barp + geom_boxplot() +
                theme_minimal()
            }
            if (VilOrBP == "Violin Plot") {
              barp <- barp + geom_violin() +
                theme_minimal()
              barp <- barp + stat_summary(fun=median, geom="point", shape=23, size=dotsizein, color="black", bg = "black")
            }
            #if (VilOrBP == "Stacked Barplot") {
            #  FillChoice <- input$BPfeatFill
            #  if (FillChoice == FALSE) {
            #    barp <- barp + geom_bar() +
            #      theme_minimal()
            #  }
            #  else if (FillChoice == TRUE) {
            #    barp <- barp + geom_bar() +
            #      theme_minimal(position = "fill")
            #  }
            #}
            
            if (Vplotylim == "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(expansion(mult = c(0, 0.1))) ## expansion function causing issue with y-axis title
                barp <- barp + scale_y_continuous()
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(0,y_max),breaks=seq(0,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(0,y_max),expansion(mult = c(0, 0.1)),breaks=seq(0,y_max,Vplotybreaks))
              }
            }
            else if (Vplotylim != "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max))
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)),breaks=seq(y_min,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max),breaks=seq(y_min,y_max,Vplotybreaks))
              }
            }
            
            if (dotChoice == T) {
              feat_gene3 <- feat_gene2
              feat_gene3$xj <- jitter(as.numeric(factor(feat_gene3$Type)))
              barp <- barp + geom_point(data = feat_gene3, aes(x=xj), col="gray44", size=dotsizein)
              if (length(sampSelected) > 0) {
                feat_gene3$SampLabel <- ifelse(feat_gene3[,1] %in% sampSelected, feat_gene3[,1], NA)
                barp <- barp + geom_text_repel(data = feat_gene3,
                                               aes(x = xj, y = FeatureName, label = SampLabel),
                                               size = anno_size,
                                               max.overlaps = Inf,
                                               color = "black",
                                               fontface = "bold",
                                               min.segment.length = unit(0, 'lines'),
                                               box.padding = unit(0.35, "lines"),
                                               point.padding = unit(0.3, "lines"),
                                               arrow = arrow(length = unit(0.015, "npc"))
                )
                feat_gene4 <- feat_gene3[!is.na(feat_gene3$SampLabel),]
                barp <- barp + geom_point(data = feat_gene4, aes(x=xj), col="darkred", size=dotsizein)
              }
            }
            
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)),
                                 legend.position = 'none')
            barp
            
          }
          
        }
        
        #  }
        #}
        
      })
      
      output$violin_MVG <- renderPlot({
        
        plot <- violin_MVG_react()
        plot
        
      })
      
      ViolinPlotTable_react <- reactive({
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        metaSub <- VI_meta_subset()
        FeatMat <- expr_raw()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        logchoice <- input$log2Vplot
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          feat_gene2 <- merge(feat_gene,metaSub,by = colnames(meta_react())[1], all = T)
          if (logchoice == T) {
            #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
            feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
          }
          feat_gene2
        }
        #  }
        #}
        
      })
      
      output$ViolinPlotTable <- DT::renderDataTable({
        
        df <- ViolinPlotTable_react()
        DT::datatable(df,
                      extensions = "FixedColumns",
                      options = list(lengthMenu = c(10,20,50,100,1000,5000,10000),
                                     pageLength = 20,
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     fixedColumns = list(leftColumns = 1)),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      
      ####----MVG Enrichment Test----####
      
      EnrichMeta_Sub <- reactive({
        
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        
        if (is.null(input$BPsampSubsetEnrich)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubsetEnrich}
        if (is.null(input$BPsampCriteriaEnrich)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPsampCriteriaEnrich}
        
        if (sampSubset == "Select All") {
          metaSub <- meta
          metaSub
        }
        else if (sampSubset != "Select All") {
          metaSub <- meta[which(meta[,sampSubset] == sampGroupCriteria),]
          metaSub
        }
        
        
      })
      
      FisherMatrix_MVG_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        FishMat <- matrix(c(inBoth,inVar1,inVar2,inNone), nrow = 2)
        FishMat
        
      })
      
      output$EnrichFisherTabMVG <- renderTable({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        FishMat <- FisherMatrix_MVG_react()
        
        FishMat <- as.data.frame(FishMat)
        colnames(FishMat) <- c(paste0(Feat1," - ",Var1),paste0(Feat1," - Not ",Var1))
        rownames(FishMat) <- c(paste0(Feat2," - ",Var2),paste0(Feat2," - Not ",Var2))
        
        FishMat
        
      }, rownames = TRUE)
      
      Fisher_MVG_react <- reactive({
        
        FishMat <- FisherMatrix_MVG_react()
        TailChoice <- input$FisherTailChoice
        
        Fisher_react <- fisher.test(FishMat, alternative = TailChoice)
        Fisher_react
        
      })
      
      output$EnrichFisherOutMVG <- renderText({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        text <- capture.output(Fisher_MVG_react())
        text[grep("data:  FishMat",text)] <- paste0("data:  ",Feat1," - ",Var1," vs. ",Feat2," - ",Var2)
        textOut <- paste(text,collapse = '\n')
        textOut
        
      })
      
      FisherVenn_MVG_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inVar1 <- meta[which(meta[,Feat1] == Var1),1]
        inVar2 <- meta[which(meta[,Feat2] == Var2),1]
        
        VennList <- list(inVar1 = inVar1,
                         inVar2 = inVar2)
        names(VennList)[1] <- paste0(Feat1,":\n",Var1)
        names(VennList)[2] <- paste0(Feat2,":\n",Var2)
        VennObj <- Venn(VennList)
        VennData <- process_data(VennObj)
        
        if (packageVersion("ggVennDiagram") < package_version("1.4")) {
          ggplot() +
            # 1. region count layer
            geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
            # 2. set edge layer
            geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
            # 3. set label layer
            geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
            # 4. region label layer
            geom_sf_label(aes(label = paste0(count)),
                          data = venn_region(VennData),
                          size = 6,
                          alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            theme_void()
        } else {
          ggplot() +
            # 1. region count layer
            geom_polygon(aes(X, Y, fill = count, group = id),
                         data = venn_regionedge(VennData), show.legend = F) +
            # 2. set edge layer
            geom_path(aes(X, Y, color = id, group = id),
                      data = venn_setedge(VennData),
                      show.legend = FALSE, linewidth = 1) +
            # 3. set label layer
            geom_text(aes(X, Y, label = name),
                      data = venn_setlabel(VennData), size = 4) +
            # 4. region label layer
            geom_label(aes(X, Y, label = count),
                       data = venn_regionlabel(VennData),
                       size = 6,
                       alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            coord_flip() +
            theme_void()
        }
        
        #ggplot() +
        #  # 1. region count layer
        #  geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
        #  # 2. set edge layer
        #  geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
        #  # 3. set label layer
        #  geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
        #  # 4. region label layer
        #  geom_sf_label(aes(label = paste0(count)),
        #                data = venn_region(VennData),
        #                size = 6,
        #                alpha = 0.5) +
        #  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
        #  scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
        #  theme_void()
        
      })
      
      output$EnrichVennPlotMVG <- renderPlot({
        
        p <- FisherVenn_MVG_react()
        p
        
      })
      
      output$EnrichFisherTextMVG <- renderUI({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        TailChoice <- input$FisherTailChoice
        FisherOut <- capture.output(Fisher_MVG_react())
        pval <- gsub("p-value [[:punct:]] ","",grep("^p-value ",FisherOut, value = T))
        if (as.numeric(pval) < 0.05) {
          SigWord <- "significantly"
        } else {SigWord <- "not significantly"}
        OR <- trimws(FisherOut[grep("^odds ratio",FisherOut)+1])
        if (OR < 1) {
          ORWord <- "depleted"
        } else {ORWord <- "enriched"}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        HTML(paste("Fishers Exact test found that <b>",Feat1,"</b> - <b>",Var1,"</b> is <b>",SigWord,"</b> <b>",ORWord,
                   "</b> in <b>",Feat2,"</b> - <b>",Var2,"</b>, based on a P.value of <b>",pval,"</b> and an odds ratio of <b>",OR,"</b>.", sep = ""))
        
      })
      
      
      ####----PATH UMAP----####
      
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_PATH_react_clin_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2",
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2")
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  k <- k + labs(x = "UMAP1",
        #                y = "UMAP2")
        #}
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          if (input$UMAPannoContCheck == T) {
            myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
            k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
          }
        }
        #}
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        k
        
      })
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_PATH_react_clin_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2",
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2")
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  k <- k + labs(x = "UMAP1",
        #                y = "UMAP2")
        #}
        
        if (is.null(input$UMAPmetaFile) == F) {
          if (input$UMAPannotateSamps != "") {
            if (input$UMAPannoContCheck == T) {
              myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
              k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
            }
          }
        }
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        k
        
      })
      
      
      umap_plot_PATH_react_clin <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        k <- umap_plot_PATH_react_clin_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        k2 <- ggplotly(k,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F))
        
        k2 <- k2 %>%
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          k2 <- k2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        k2
        
      })
      
      umap_plot_PATH_react_expr_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        m <- m +
          geom_point(shape = 19,
                     size = UMAPdotSize) +
          
          theme_minimal()
        
        m <- m + labs(x = "UMAP1",
                      y = "UMAP2",
                      color = metaColgene)
        
        m <-  m + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        m <- m + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          m <- m + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        m
        
      })
      
      umap_plot_PATH_react_expr_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        m <- m +
          geom_point(shape = 19,
                     size = UMAPdotSize) +
          
          theme_minimal()
        
        m <- m + labs(x = "UMAP1",
                      y = "UMAP2",
                      color = metaColgene)
        
        m <-  m + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        m <- m + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          m <- m + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        m
        
      })
      
      ## UMAP Plot  - Base - expr
      umap_plot_PATH_react_expr <- reactive({
        
        
        ## Variables}## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        m <- umap_plot_PATH_react_expr_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        m2 <- ggplotly(m,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F)) %>%
          
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          m2 <- m2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        
        m2
        
      })
      
      umap_plot_PATH_react_kmean_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        n <- n + geom_point(shape = 19,
                            size = UMAPdotSize)+
          
          labs(x = "UMAP1",
               y = "UMAP2",
               color = "Cluster")  +
          
          theme_minimal()
        
        n <- n + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          n <- n + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        n
        
      })
      
      umap_plot_PATH_react_kmean_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        n <- n + geom_point(shape = 19,
                            size = UMAPdotSize)+
          
          labs(x = "UMAP1",
               y = "UMAP2",
               color = "Cluster")  +
          
          theme_minimal()
        
        n <- n + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          n <- n + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        n
        
      })
      
      ## UMAP Plot  - Base - kmean
      umap_plot_PATH_react_kmean <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_PATH_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        n <- umap_plot_PATH_react_kmean_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        n2 <- ggplotly(n,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F)) %>%
          
          hide_legend() %>%
          hide_colorbar()
        if (length(sampSelected) > 0) {
          n2 <- n2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        
        n2
        
      })
      
      output$UMAPpathLegend <- renderPlot({
        
        k <- umap_plot_PATH_react_clin_base()
        m <- umap_plot_PATH_react_expr_base()
        n <- umap_plot_PATH_react_kmean_base()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          legendk <- g_legend(k)
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendk,legendm,legendn,nrow = 1)
        }
        else if (input$UMAPannotateSamps == "") {
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendm,legendn,nrow = 1)
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  legendm <- g_legend(m)
        #  legendn <- g_legend(n)
        #  legend_grid <- grid.arrange(legendm,legendn,nrow = 1)
        #}
        
        legend_grid
        
      })
      
      UMAPplot_PATH_ALL_react <- reactive({
        
        k2 <- umap_plot_PATH_react_clin()
        m2 <- umap_plot_PATH_react_expr()
        n2 <- umap_plot_PATH_react_kmean()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          UMAPtitlek <- paste("Annotated by",input$UMAPannotateSamps)
        }
        else if (input$UMAPannotateSamps == "") {
          UMAPtitlek <- ""
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  UMAPtitlek <- ""
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          GeneSelec <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          GeneSelec <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        if (LogChoice == T) {
          GeneSelecExpr <- paste(GeneSelec,"(Log2)")
        }
        if (LogChoice == F) {
          GeneSelecExpr <- paste(GeneSelec)
        }
        
        UMAPtitlem <- paste("Annotated by",GeneSelecExpr)
        UMAPtitlen <- paste("Annotated by",input$ClusterMethod,input$ClusterNumber,"Clusters")
        
        
        if (input$UMAPorientation == "Side-by-Side") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 1, shareY = TRUE, shareX = TRUE)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlem,
              xref = "x2",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlen,
              xref = "x3",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        else if (input$UMAPorientation == "Stacked") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 3, shareY = TRUE, shareX = TRUE, margin = 0.04)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.65,
              text = UMAPtitlem,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.3,
              text = UMAPtitlen,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        
        subplot_all_sbs <- subplot_all_sbs %>% layout(annotations = plot_titles)
        
        subplot_all_sbs
      })
      
      output$UMAPplot_PATH_ALL <- renderPlotly({
        p_all <- UMAPplot_PATH_ALL_react()
        p_all
      })
      
      ####----PATH Violin Plot----####
      
      VI_meta_subset_PATH <- reactive({
        
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        if (sampSubset == "Select All") {
          metaSub <- meta
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),all_of(sampGroupCriteria))
          metaSub
        }
        else if (sampSubset != "Select All") {
          metaSub <- meta[which(meta[,sampSubset] == sampGroupCriteria),]
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),sampGroupCriteria,sampSubset)
          metaSub
        }
        
      })
      
      violin_PATH_react <- reactive({
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          groupCriteria <- "Cluster"
        } else {groupCriteria <- input$BPgroupCriteria}
        
        title_font <- input$Vplot1TitleSize            # Title font size
        Xaxis_font <- input$Vplot1XAxisSize              # Axis font size
        Yaxis_font <- input$Vplot1YAxisSize              # Axis font size
        anno_size <- input$Vplot1aAnnoSize
        hjust_orient <- 1                                # Initial hjust
        axis_orient <- as.numeric(input$VxAxisOrient)  # X-axis label orientation
        if (axis_orient == 0) {                          # Adjust hjust if orientation is 0
          hjust_orient <- 0.5
        }
        logchoice <- input$log2Vplot                   # Log expression data option
        colorin <- input$VplotColoCodes                # Bar plot Color codes
        Vplotylim <- input$VPlotYlim                      # Y-limit
        Vplotybreaks <- input$VplotYbreaks                # Y-axis breaks
        if (is.null(input$VplotDotSize)) {
          dotsizein <- 1
        } else {dotsizein <- input$VplotDotSize}
        #dotsizein <- input$VplotDotSize
        if (is.null(input$VplotstatComp)) {
          StatMethod <- "None"
        } else {StatMethod <- input$VplotstatComp}
        #StatMethod <- input$VplotstatComp
        if (is.null(input$Vplotsampledots)) {
          dotChoice <- FALSE
        } else {dotChoice <- input$Vplotsampledots}
        #dotChoice <- input$Vplotsampledots
        VilOrBP <- input$ViolinOrBoxP
        rowsSelected <- input$PathSelectTable_rows_selected
        GeneSet <- gs_cat2()[rowsSelected,3]
        
        FeatMat <- expr_raw()
        metaSub <- VI_meta_subset_PATH()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        #sampSubset <- input$BPsampSubset
        sampCriteria <- input$BPsampCriteria
        #groupCriteria <- input$BPgroupCriteria
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        sampSelected <- input$UMAPsampSelect
        metaSub2 <- metaSub[,c(colnames(meta_react())[1],groupCriteria)]
        metaSub2[,groupCriteria] <- as.factor(metaSub2[,groupCriteria])
        
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          feat_gene2 <- merge(feat_gene,metaSub2,by = colnames(meta_react())[1], all = T)
          feature_lab <- feature
          feattitle <- feature_lab
          if (logchoice == T) {
            feature_lab <- paste(feature_lab,"(Log2)")
            feattitle <- feature_lab
          }
          
          if (sampSubset == "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab, "Across", GeneSet, "Gene Set\nClustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab, "Across", GeneSet, "Gene Set\nGrouped by",groupCriteria)
            }
          }
          if (sampSubset != "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nAcross", GeneSet, "Gene Set\nClustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nAcross", GeneSet, "Gene Set\nGrouped by",groupCriteria)
            }
          }
          
          
          if (logchoice == T) {
            #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
            feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
          }
          
          colnames(feat_gene2) <- c(colnames(meta_react())[1],"FeatureName","Type")
          
          #if (!is.numeric(feat_gene2$FeatureName)) {
          if (VilOrBP == "Stacked Barplot") {
            feat_gene2[,"FeatureName"] <- as.factor(feat_gene2[,"FeatureName"])
            #if (input$VplotXaxOrder == "Descending"){
            #  feat_gene2$Type <- fct_rev(feat_gene2$Type)
            #  feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            #}
            #if (input$VplotXaxOrder == "Ascending"){
            #  feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            #}
            barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            if (is.null(input$BPfeatFill)) {
              FillChoice <- FALSE
            } else {FillChoice <- input$BPfeatFill}
            #FillChoice <- input$BPfeatFill
            if (FillChoice == FALSE) {
              barp <- barp + geom_bar() +
                theme_minimal()
            }
            if (FillChoice == TRUE) {
              barp <- barp + geom_bar(position = "fill") +
                theme_minimal()
            }
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)))
            barp
            
          }
          #}
          
          else if(!VilOrBP == "Stacked Barplot") {
            #if (is.numeric(feat_gene2$FeatureName)) {
            
            if (logchoice == T) {
              #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
              feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
            }
            
            y_min <- 0
            y_max <- max(feat_gene2$FeatureName, na.rm = T) * 1.03
            if (Vplotylim != "") {
              y_min <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][1]))
              y_max <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][2]))
            }
            
            if (input$VplotXaxOrder == "Descending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            }
            if (input$VplotXaxOrder == "Ascending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            }
            
            barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #if (VilOrBP == "Stacked Barplot") {
            #  barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            #}
            #if (VilOrBP != "Stacked Barplot") {
            #  barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #}
            if (StatMethod != "None") {
              barp <- barp + stat_compare_means(data = feat_gene2,
                                                aes(x=Type,y=FeatureName),
                                                method = StatMethod,
                                                label.x = 1,
                                                label.y = max(feat_gene2$FeatureName, na.rm = T) * 1.02)
            }
            if (VilOrBP == "Box Plot") {
              barp <- barp + geom_boxplot() +
                theme_minimal()
            }
            if (VilOrBP == "Violin Plot") {
              barp <- barp + geom_violin() +
                theme_minimal()
              barp <- barp + stat_summary(fun=median, geom="point", shape=23, size=dotsizein, color="black", bg = "black")
            }
            #if (VilOrBP == "Stacked Barplot") {
            #  FillChoice <- input$BPfeatFill
            #  if (FillChoice == FALSE) {
            #    barp <- barp + geom_bar() +
            #      theme_minimal()
            #  }
            #  else if (FillChoice == TRUE) {
            #    barp <- barp + geom_bar() +
            #      theme_minimal(position = "fill")
            #  }
            #}
            
            if (Vplotylim == "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(expansion(mult = c(0, 0.1))) ## expansion function causing issue with y-axis title
                barp <- barp + scale_y_continuous()
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(0,y_max),breaks=seq(0,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(0,y_max),expansion(mult = c(0, 0.1)),breaks=seq(0,y_max,Vplotybreaks))
              }
            }
            else if (Vplotylim != "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max))
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)),breaks=seq(y_min,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max),breaks=seq(y_min,y_max,Vplotybreaks))
              }
            }
            
            if (dotChoice == T) {
              feat_gene3 <- feat_gene2
              feat_gene3$xj <- jitter(as.numeric(factor(feat_gene3$Type)))
              barp <- barp + geom_point(data = feat_gene3, aes(x=xj), col="gray44", size=dotsizein)
              if (length(sampSelected) > 0) {
                feat_gene3$SampLabel <- ifelse(feat_gene3[,1] %in% sampSelected, feat_gene3[,1], NA)
                barp <- barp + geom_text_repel(data = feat_gene3,
                                               aes(x = xj, y = FeatureName, label = SampLabel),
                                               size = anno_size,
                                               max.overlaps = Inf,
                                               color = "black",
                                               fontface = "bold",
                                               min.segment.length = unit(0, 'lines'),
                                               box.padding = unit(0.35, "lines"),
                                               point.padding = unit(0.3, "lines"),
                                               arrow = arrow(length = unit(0.015, "npc"))
                )
                feat_gene4 <- feat_gene3[!is.na(feat_gene3$SampLabel),]
                barp <- barp + geom_point(data = feat_gene4, aes(x=xj), col="darkred", size=dotsizein)
              }
            }
            
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)),
                                 legend.position = 'none')
            barp
            
          }
          
          #y_min <- 0
          #y_max <- max(feat_gene2$FeatureName, na.rm = T) * 1.03
          #if (Vplotylim != "") {
          #  y_min <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][1]))
          #  y_max <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][2]))
          #}
          #
          #if (input$VplotXaxOrder == "Descending"){
          #  feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
          #}
          #if (input$VplotXaxOrder == "Ascending"){
          #  feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
          #}
          #
          #barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
          #if (StatMethod != "None") {
          #  barp <- barp + stat_compare_means(data = feat_gene2,
          #                                    aes(x=Type,y=FeatureName),
          #                                    method = StatMethod,
          #                                    label.x = 1,
          #                                    label.y = max(feat_gene2$FeatureName, na.rm = T) * 1.02)
          #}
          #if (VilOrBP == "Box Plot") {
          #  barp <- barp + geom_boxplot() +
          #    theme_minimal()
          #}
          #if (VilOrBP == "Violin Plot") {
          #  barp <- barp + geom_violin() +
          #    theme_minimal()
          #  barp <- barp + stat_summary(fun=median, geom="point", shape=23, size=dotsizein, color="black", bg = "black")
          #}
          #
          #if (Vplotylim == "") {
          #  if (is.na(Vplotybreaks)) {
          #    #barp <- barp + scale_y_continuous(expansion(mult = c(0, 0.1))) ## expansion function causing issue with y-axis title
          #    barp <- barp + scale_y_continuous()
          #  }
          #  else if (!is.na(Vplotybreaks)) {
          #    #barp <- barp + scale_y_continuous(limits=c(0,y_max),breaks=seq(0,y_max,Vplotybreaks))
          #    barp <- barp + scale_y_continuous(limits=c(0,y_max),expansion(mult = c(0, 0.1)),breaks=seq(0,y_max,Vplotybreaks))
          #  }
          #}
          #else if (Vplotylim != "") {
          #  if (is.na(Vplotybreaks)) {
          #    #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)))
          #    barp <- barp + scale_y_continuous(limits=c(y_min,y_max))
          #  }
          #  else if (!is.na(Vplotybreaks)) {
          #    #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)),breaks=seq(y_min,y_max,Vplotybreaks))
          #    barp <- barp + scale_y_continuous(limits=c(y_min,y_max),breaks=seq(y_min,y_max,Vplotybreaks))
          #  }
          #}
          #
          #if (dotChoice == T) {
          #  feat_gene3 <- feat_gene2
          #  feat_gene3$xj <- jitter(as.numeric(factor(feat_gene3$Type)))
          #  barp <- barp + geom_point(data = feat_gene3, aes(x=xj), col="gray44", size=dotsizein)
          #  if (length(sampSelected) > 0) {
          #    feat_gene3$SampLabel <- ifelse(feat_gene3[,1] %in% sampSelected, feat_gene3[,1], NA)
          #    barp <- barp + geom_text_repel(data = feat_gene3,
          #                                   aes(x = xj, y = FeatureName, label = SampLabel),
          #                                   size = anno_size,
          #                                   max.overlaps = Inf,
          #                                   color = "black",
          #                                   fontface = "bold",
          #                                   min.segment.length = unit(0, 'lines'),
          #                                   box.padding = unit(0.35, "lines"),
          #                                   point.padding = unit(0.3, "lines"),
          #                                   arrow = arrow(length = unit(0.015, "npc"))
          #    )
          #    feat_gene4 <- feat_gene3[!is.na(feat_gene3$SampLabel),]
          #    barp <- barp + geom_point(data = feat_gene4, aes(x=xj), col="darkred", size=dotsizein)
          #  }
          #}
          #
          #if (colorin != "") {
          #  colorin <- strsplit(colorin," ")[[1]]
          #  if (length(colorin) == 1) {
          #    colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
          #  }
          #  barp <- barp + scale_fill_manual(values=colorin)
          #}
          #barp <- barp + labs(x = groupCriteria,
          #                    y = feattitle,
          #                    title = plottitle)
          #barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
          #                     axis.title.x = element_text(size = Xaxis_font),
          #                     axis.text.y = element_text(size = Yaxis_font),
          #                     axis.title.y = element_text(size = Yaxis_font),
          #                     plot.title = element_text(size = title_font, margin=margin(0,0,30,0)),
          #                     legend.position = 'none')
          #barp
        }
        
        #  }
        #}
        
      })
      
      output$violin_PATH <- renderPlot({
        
        plot <- violin_PATH_react()
        plot
        
      })
      
      ViolinPlotTable_PATH_react <- reactive({
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        metaSub <- VI_meta_subset_PATH()
        FeatMat <- expr_raw()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        logchoice <- input$log2Vplot
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          feat_gene2 <- merge(feat_gene,metaSub,by = colnames(meta_react())[1], all = T)
          if (logchoice == T) {
            #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
            feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
          }
          feat_gene2
        }
        #  }
        #}
        
      })
      
      output$ViolinPlotTable_PATH <- DT::renderDataTable({
        
        df <- ViolinPlotTable_PATH_react()
        DT::datatable(df,
                      extensions = "FixedColumns",
                      options = list(lengthMenu = c(10,20,50,100,1000,5000,10000),
                                     pageLength = 20,
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     fixedColumns = list(leftColumns = 1)),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      ####----PATH Enrichment Test----####
      
      FisherMatrix_PATH_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        FishMat <- matrix(c(inBoth,inVar1,inVar2,inNone), nrow = 2)
        FishMat
        
      })
      
      output$EnrichFisherTabPATH <- renderTable({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        FishMat <- FisherMatrix_PATH_react()
        
        FishMat <- as.data.frame(FishMat)
        colnames(FishMat) <- c(paste0(Feat1," - ",Var1),paste0(Feat1," - Not ",Var1))
        rownames(FishMat) <- c(paste0(Feat2," - ",Var2),paste0(Feat2," - Not ",Var2))
        
        FishMat
        
      }, rownames = TRUE)
      
      Fisher_PATH_react <- reactive({
        
        FishMat <- FisherMatrix_PATH_react()
        TailChoice <- input$FisherTailChoice
        
        Fisher_react <- fisher.test(FishMat, alternative = TailChoice)
        Fisher_react
        
      })
      
      output$EnrichFisherOutPATH <- renderText({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        text <- capture.output(Fisher_PATH_react())
        text[grep("data:  FishMat",text)] <- paste0("data:  ",Feat1," - ",Var1," vs. ",Feat2," - ",Var2)
        textOut <- paste(text,collapse = '\n')
        textOut
        
      })
      
      FisherVenn_PATH_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inVar1 <- meta[which(meta[,Feat1] == Var1),1]
        inVar2 <- meta[which(meta[,Feat2] == Var2),1]
        
        VennList <- list(inVar1 = inVar1,
                         inVar2 = inVar2)
        names(VennList)[1] <- paste0(Feat1,":\n",Var1)
        names(VennList)[2] <- paste0(Feat2,":\n",Var2)
        VennObj <- Venn(VennList)
        VennData <- process_data(VennObj)
        
        if (packageVersion("ggVennDiagram") < package_version("1.4")) {
          ggplot() +
            # 1. region count layer
            geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
            # 2. set edge layer
            geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
            # 3. set label layer
            geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
            # 4. region label layer
            geom_sf_label(aes(label = paste0(count)),
                          data = venn_region(VennData),
                          size = 6,
                          alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            theme_void()
        } else {
          ggplot() +
            # 1. region count layer
            geom_polygon(aes(X, Y, fill = count, group = id),
                         data = venn_regionedge(VennData), show.legend = F) +
            # 2. set edge layer
            geom_path(aes(X, Y, color = id, group = id),
                      data = venn_setedge(VennData),
                      show.legend = FALSE, linewidth = 1) +
            # 3. set label layer
            geom_text(aes(X, Y, label = name),
                      data = venn_setlabel(VennData), size = 4) +
            # 4. region label layer
            geom_label(aes(X, Y, label = count),
                       data = venn_regionlabel(VennData),
                       size = 6,
                       alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            coord_flip() +
            theme_void()
        }
        
        #ggplot() +
        #  # 1. region count layer
        #  geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
        #  # 2. set edge layer
        #  geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
        #  # 3. set label layer
        #  geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
        #  # 4. region label layer
        #  geom_sf_label(aes(label = paste0(count)),
        #                data = venn_region(VennData),
        #                size = 6,
        #                alpha = 0.5) +
        #  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
        #  scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
        #  theme_void()
        
      })
      
      output$EnrichVennPlotPATH <- renderPlot({
        
        p <- FisherVenn_PATH_react()
        p
        
      })
      
      output$EnrichFisherTextPATH <- renderUI({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        TailChoice <- input$FisherTailChoice
        FisherOut <- capture.output(Fisher_PATH_react())
        pval <- gsub("p-value [[:punct:]] ","",grep("^p-value ",FisherOut, value = T))
        if (as.numeric(pval) < 0.05) {
          SigWord <- "significantly"
        } else {SigWord <- "not significantly"}
        OR <- trimws(FisherOut[grep("^odds ratio",FisherOut)+1])
        if (OR < 1) {
          ORWord <- "depleted"
        } else {ORWord <- "enriched"}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        HTML(paste("Fishers Exact test found that <b>",Feat1,"</b> - <b>",Var1,"</b> is <b>",SigWord,"</b> <b>",ORWord,
                   "</b> in <b>",Feat2,"</b> - <b>",Var2,"</b>, based on a P.value of <b>",pval,"</b> and an odds ratio of <b>",OR,"</b>.", sep = ""))
        
      })
      
      ####----BASE UMAP----####
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_clin_react_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2",
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2")
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  k <- k + labs(x = "UMAP1",
        #                y = "UMAP2")
        #}
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          if (input$UMAPannoContCheck == T) {
            myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
            k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
          }
        }
        #}
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        k
        
      })
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_clin_react_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2",
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = "UMAP1",
                        y = "UMAP2")
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  k <- k + labs(x = "UMAP1",
        #                y = "UMAP2")
        #}
        
        if (is.null(input$UMAPmetaFile) == F) {
          if (input$UMAPannotateSamps != "") {
            if (input$UMAPannoContCheck == T) {
              myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
              k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
            }
          }
        }
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        k
        
      })
      
      umap_plot_clin_react <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        k <- umap_plot_clin_react_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        k2 <- ggplotly(k,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F))
        
        k2 <- k2 %>%
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          k2 <- k2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        k2
        
      })
      
      umap_plot_expr_react_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        m <- m +
          geom_point(shape = 19,
                     size = UMAPdotSize) +
          
          theme_minimal()
        
        m <- m + labs(x = "UMAP1",
                      y = "UMAP2",
                      color = metaColgene)
        
        m <-  m + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        m <- m + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          m <- m + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        m
        
      })
      
      umap_plot_expr_react_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          m <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        m <- m +
          geom_point(shape = 19,
                     size = UMAPdotSize) +
          
          theme_minimal()
        
        m <- m + labs(x = "UMAP1",
                      y = "UMAP2",
                      color = metaColgene)
        
        m <-  m + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        m <- m + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          m <- m + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        m
        
      })
      
      ## UMAP Plot  - Base - expr
      umap_plot_expr_react <- reactive({
        
        
        ## Variables}## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        metaColkmean <- "Cluster"
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        m <- umap_plot_expr_react_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        m2 <- ggplotly(m,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F)) %>%
          
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          m2 <- m2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        
        m2
        
      })
      
      umap_plot_kmean_react_base <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        n <- n + geom_point(shape = 19,
                            size = UMAPdotSize)+
          
          labs(x = "UMAP1",
               y = "UMAP2",
               color = "Cluster")  +
          
          theme_minimal()
        
        n <- n + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          n <- n + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        n
        
      })
      
      umap_plot_kmean_react_base_anno <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        ## Meta Annotation Plot
        if (!is.null(metaColanno)) {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        if (is.null(metaColanno)) {
          colnames(plot_df)[4] <- "GeneName"
          n <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        n <- n + geom_point(shape = 19,
                            size = UMAPdotSize)+
          
          labs(x = "UMAP1",
               y = "UMAP2",
               color = "Cluster")  +
          
          theme_minimal()
        
        n <- n + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
          n <- n + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            min.segment.length = 0,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        n
        
      })
      
      ## UMAP Plot  - Base - kmean
      umap_plot_kmean_react <- reactive({
        
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        n <- umap_plot_kmean_react_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        ## Make it plotly
        n2 <- ggplotly(n,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title="UMAP1",zeroline=F),
                 yaxis=list(title="UMAP2",zeroline=F)) %>%
          
          hide_legend() %>%
          hide_colorbar()
        if (length(sampSelected) > 0) {
          n2 <- n2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        
        n2
        
      })
      
      output$UMAPallLegend <- renderPlot({
        
        k <- umap_plot_clin_react_base()
        m <- umap_plot_expr_react_base()
        n <- umap_plot_kmean_react_base()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          legendk <- g_legend(k)
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendk,legendm,legendn,nrow = 1)
        }
        else if (input$UMAPannotateSamps == "") {
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendm,legendn,nrow = 1)
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  legendm <- g_legend(m)
        #  legendn <- g_legend(n)
        #  legend_grid <- grid.arrange(legendm,legendn,nrow = 1)
        #}
        
        legend_grid
        
      })
      
      UMAPplot_ALL_react <- reactive({
        
        k2 <- umap_plot_clin_react()
        m2 <- umap_plot_expr_react()
        n2 <- umap_plot_kmean_react()
        
        #if (is.null(input$UMAPmetaFile) == F) {
        if (input$UMAPannotateSamps != "") {
          UMAPtitlek <- paste("Annotated by",input$UMAPannotateSamps)
        }
        else if (input$UMAPannotateSamps == "") {
          UMAPtitlek <- ""
        }
        #}
        #if (is.null(input$UMAPmetaFile) == T) {
        #  UMAPtitlek <- ""
        #}
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          GeneSelec <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          GeneSelec <- rownames(expr)[1]
        }
        
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        if (LogChoice == T) {
          GeneSelecExpr <- paste(GeneSelec,"(Log2)")
        }
        if (LogChoice == F) {
          GeneSelecExpr <- paste(GeneSelec)
        }
        
        UMAPtitlem <- paste("Annotated by",GeneSelecExpr)
        UMAPtitlen <- paste("Annotated by",input$ClusterMethod,input$ClusterNumber,"Clusters")
        
        
        if (input$UMAPorientation == "Side-by-Side") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 1, shareY = TRUE, shareX = TRUE)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlem,
              xref = "x2",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlen,
              xref = "x3",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        else if (input$UMAPorientation == "Stacked") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 3, shareY = TRUE, shareX = TRUE, margin = 0.04)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.65,
              text = UMAPtitlem,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.3,
              text = UMAPtitlen,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        
        subplot_all_sbs <- subplot_all_sbs %>% layout(annotations = plot_titles)
        
        subplot_all_sbs
      })
      
      output$UMAPplot_ALL <- renderPlotly({
        p_all <- UMAPplot_ALL_react()
        p_all
      })
      
      ####----BASE Violin Plot----####
      
      VI_meta_subset_BASE <- reactive({
        
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        if (sampSubset == "Select All") {
          metaSub <- meta
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),all_of(sampGroupCriteria))
          metaSub
        }
        else if (sampSubset != "Select All") {
          metaSub <- meta[which(meta[,sampSubset] == sampGroupCriteria),]
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),sampGroupCriteria,sampSubset)
          metaSub
        }
        
      })
      
      violin_BASE_react <- reactive({
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          groupCriteria <- "Cluster"
        } else {groupCriteria <- input$BPgroupCriteria}
        
        title_font <- input$Vplot1TitleSize            # Title font size
        Xaxis_font <- input$Vplot1XAxisSize              # Axis font size
        Yaxis_font <- input$Vplot1YAxisSize              # Axis font size
        anno_size <- input$Vplot1aAnnoSize
        hjust_orient <- 1                                # Initial hjust
        axis_orient <- as.numeric(input$VxAxisOrient)  # X-axis label orientation
        if (axis_orient == 0) {                          # Adjust hjust if orientation is 0
          hjust_orient <- 0.5
        }
        logchoice <- input$log2Vplot                   # Log expression data option
        colorin <- input$VplotColoCodes                # Bar plot Color codes
        Vplotylim <- input$VPlotYlim                      # Y-limit
        Vplotybreaks <- input$VplotYbreaks                # Y-axis breaks
        if (is.null(input$VplotDotSize)) {
          dotsizein <- 1
        } else {dotsizein <- input$VplotDotSize}
        #dotsizein <- input$VplotDotSize
        if (is.null(input$VplotstatComp)) {
          StatMethod <- "None"
        } else {StatMethod <- input$VplotstatComp}
        #StatMethod <- input$VplotstatComp
        if (is.null(input$Vplotsampledots)) {
          dotChoice <- FALSE
        } else {dotChoice <- input$Vplotsampledots}
        #dotChoice <- input$Vplotsampledots
        VilOrBP <- input$ViolinOrBoxP
        
        FeatMat <- expr_raw()
        metaSub <- VI_meta_subset_BASE()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        #sampSubset <- input$BPsampSubset
        sampCriteria <- input$BPsampCriteria
        #groupCriteria <- input$BPgroupCriteria
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        sampSelected <- input$UMAPsampSelect
        metaSub2 <- metaSub[,c(colnames(meta_react())[1],groupCriteria)]
        metaSub2[,groupCriteria] <- as.factor(metaSub2[,groupCriteria])
        
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          feat_gene2 <- merge(feat_gene,metaSub2,by = colnames(meta_react())[1], all = T)
          feature_lab <- feature
          feattitle <- feature_lab
          if (logchoice == T) {
            feature_lab <- paste(feature_lab,"(Log2)")
            feattitle <- feature_lab
          }
          
          if (sampSubset == "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab, "Clustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab, "Grouped by",groupCriteria)
            }
          }
          if (sampSubset != "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nClustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nGrouped by",groupCriteria)
            }
          }
          
          
          if (logchoice == T) {
            #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
            feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
          }
          
          colnames(feat_gene2) <- c(colnames(meta_react())[1],"FeatureName","Type")
          
          #if (!is.numeric(feat_gene2$FeatureName)) {
          if (VilOrBP == "Stacked Barplot") {
            feat_gene2[,"FeatureName"] <- as.factor(feat_gene2[,"FeatureName"])
            #if (input$VplotXaxOrder == "Descending"){
            #  feat_gene2$Type <- fct_rev(feat_gene2$Type)
            #  feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            #}
            #if (input$VplotXaxOrder == "Ascending"){
            #  feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            #}
            barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            if (is.null(input$BPfeatFill)) {
              FillChoice <- FALSE
            } else {FillChoice <- input$BPfeatFill}
            #FillChoice <- input$BPfeatFill
            if (FillChoice == FALSE) {
              barp <- barp + geom_bar() +
                theme_minimal()
            }
            if (FillChoice == TRUE) {
              barp <- barp + geom_bar(position = "fill") +
                theme_minimal()
            }
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)))
            barp
            
          }
          #}
          
          else if(!VilOrBP == "Stacked Barplot") {
            #if (is.numeric(feat_gene2$FeatureName)) {
            
            if (logchoice == T) {
              #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
              feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
            }
            
            y_min <- 0
            y_max <- max(feat_gene2$FeatureName, na.rm = T) * 1.03
            if (Vplotylim != "") {
              y_min <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][1]))
              y_max <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][2]))
            }
            
            if (input$VplotXaxOrder == "Descending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            }
            if (input$VplotXaxOrder == "Ascending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            }
            
            barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #if (VilOrBP == "Stacked Barplot") {
            #  barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            #}
            #if (VilOrBP != "Stacked Barplot") {
            #  barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #}
            if (StatMethod != "None") {
              barp <- barp + stat_compare_means(data = feat_gene2,
                                                aes(x=Type,y=FeatureName),
                                                method = StatMethod,
                                                label.x = 1,
                                                label.y = max(feat_gene2$FeatureName, na.rm = T) * 1.02)
            }
            if (VilOrBP == "Box Plot") {
              barp <- barp + geom_boxplot() +
                theme_minimal()
            }
            if (VilOrBP == "Violin Plot") {
              barp <- barp + geom_violin() +
                theme_minimal()
              barp <- barp + stat_summary(fun=median, geom="point", shape=23, size=dotsizein, color="black", bg = "black")
            }
            #if (VilOrBP == "Stacked Barplot") {
            #  FillChoice <- input$BPfeatFill
            #  if (FillChoice == FALSE) {
            #    barp <- barp + geom_bar() +
            #      theme_minimal()
            #  }
            #  else if (FillChoice == TRUE) {
            #    barp <- barp + geom_bar() +
            #      theme_minimal(position = "fill")
            #  }
            #}
            
            if (Vplotylim == "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(expansion(mult = c(0, 0.1))) ## expansion function causing issue with y-axis title
                barp <- barp + scale_y_continuous()
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(0,y_max),breaks=seq(0,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(0,y_max),expansion(mult = c(0, 0.1)),breaks=seq(0,y_max,Vplotybreaks))
              }
            }
            else if (Vplotylim != "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max))
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)),breaks=seq(y_min,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max),breaks=seq(y_min,y_max,Vplotybreaks))
              }
            }
            
            if (dotChoice == T) {
              feat_gene3 <- feat_gene2
              feat_gene3$xj <- jitter(as.numeric(factor(feat_gene3$Type)))
              barp <- barp + geom_point(data = feat_gene3, aes(x=xj), col="gray44", size=dotsizein)
              if (length(sampSelected) > 0) {
                feat_gene3$SampLabel <- ifelse(feat_gene3[,1] %in% sampSelected, feat_gene3[,1], NA)
                barp <- barp + geom_text_repel(data = feat_gene3,
                                               aes(x = xj, y = FeatureName, label = SampLabel),
                                               size = anno_size,
                                               max.overlaps = Inf,
                                               color = "black",
                                               fontface = "bold",
                                               min.segment.length = unit(0, 'lines'),
                                               box.padding = unit(0.35, "lines"),
                                               point.padding = unit(0.3, "lines"),
                                               arrow = arrow(length = unit(0.015, "npc"))
                )
                feat_gene4 <- feat_gene3[!is.na(feat_gene3$SampLabel),]
                barp <- barp + geom_point(data = feat_gene4, aes(x=xj), col="darkred", size=dotsizein)
              }
            }
            
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)),
                                 legend.position = 'none')
            barp
            
          }
        }
        
        #  }
        #}
        
      })
      
      output$violin_BASE <- renderPlot({
        
        plot <- violin_BASE_react()
        plot
        
      })
      
      ViolinPlotTable_BASE_react <- reactive({
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        metaSub <- VI_meta_subset_BASE()
        FeatMat <- expr_raw()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        logchoice <- input$log2Vplot
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          feat_gene2 <- merge(feat_gene,metaSub,by = colnames(meta_react())[1], all = T)
          if (logchoice == T) {
            #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
            feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
          }
          feat_gene2
        }
        #  }
        #}
        
      })
      
      output$ViolinPlotTable_BASE <- DT::renderDataTable({
        
        df <- ViolinPlotTable_BASE_react()
        DT::datatable(df,
                      extensions = "FixedColumns",
                      options = list(lengthMenu = c(10,20,50,100,1000,5000,10000),
                                     pageLength = 20,
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     fixedColumns = list(leftColumns = 1)),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      ####----BASE Enrichment Test----####
      
      FisherMatrix_BASE_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        FishMat <- matrix(c(inBoth,inVar1,inVar2,inNone), nrow = 2)
        FishMat
        
      })
      
      output$EnrichFisherTabBASE <- renderTable({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        FishMat <- FisherMatrix_BASE_react()
        
        FishMat <- as.data.frame(FishMat)
        colnames(FishMat) <- c(paste0(Feat1," - ",Var1),paste0(Feat1," - Not ",Var1))
        rownames(FishMat) <- c(paste0(Feat2," - ",Var2),paste0(Feat2," - Not ",Var2))
        
        FishMat
        
      }, rownames = TRUE)
      
      Fisher_BASE_react <- reactive({
        
        FishMat <- FisherMatrix_BASE_react()
        TailChoice <- input$FisherTailChoice
        
        Fisher_react <- fisher.test(FishMat, alternative = TailChoice)
        Fisher_react
        
      })
      
      output$EnrichFisherOutBASE <- renderText({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        text <- capture.output(Fisher_BASE_react())
        text[grep("data:  FishMat",text)] <- paste0("data:  ",Feat1," - ",Var1," vs. ",Feat2," - ",Var2)
        textOut <- paste(text,collapse = '\n')
        textOut
        
      })
      
      FisherVenn_BASE_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inVar1 <- meta[which(meta[,Feat1] == Var1),1]
        inVar2 <- meta[which(meta[,Feat2] == Var2),1]
        
        VennList <- list(inVar1 = inVar1,
                         inVar2 = inVar2)
        names(VennList)[1] <- paste0(Feat1,":\n",Var1)
        names(VennList)[2] <- paste0(Feat2,":\n",Var2)
        VennObj <- Venn(VennList)
        VennData <- process_data(VennObj)
        
        if (packageVersion("ggVennDiagram") < package_version("1.4")) {
          ggplot() +
            # 1. region count layer
            geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
            # 2. set edge layer
            geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
            # 3. set label layer
            geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
            # 4. region label layer
            geom_sf_label(aes(label = paste0(count)),
                          data = venn_region(VennData),
                          size = 6,
                          alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            theme_void()
        } else {
          ggplot() +
            # 1. region count layer
            geom_polygon(aes(X, Y, fill = count, group = id),
                         data = venn_regionedge(VennData), show.legend = F) +
            # 2. set edge layer
            geom_path(aes(X, Y, color = id, group = id),
                      data = venn_setedge(VennData),
                      show.legend = FALSE, linewidth = 1) +
            # 3. set label layer
            geom_text(aes(X, Y, label = name),
                      data = venn_setlabel(VennData), size = 4) +
            # 4. region label layer
            geom_label(aes(X, Y, label = count),
                       data = venn_regionlabel(VennData),
                       size = 6,
                       alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            coord_flip() +
            theme_void()
        }
        
        #ggplot() +
        #  # 1. region count layer
        #  geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
        #  # 2. set edge layer
        #  geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
        #  # 3. set label layer
        #  geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
        #  # 4. region label layer
        #  geom_sf_label(aes(label = paste0(count)),
        #                data = venn_region(VennData),
        #                size = 6,
        #                alpha = 0.5) +
        #  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
        #  scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
        #  theme_void()
        
      })
      
      output$EnrichVennPlotBASE <- renderPlot({
        
        p <- FisherVenn_BASE_react()
        p
        
      })
      
      output$EnrichFisherTextBASE <- renderUI({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        TailChoice <- input$FisherTailChoice
        FisherOut <- capture.output(Fisher_BASE_react())
        pval <- gsub("p-value [[:punct:]] ","",grep("^p-value ",FisherOut, value = T))
        if (as.numeric(pval) < 0.05) {
          SigWord <- "significantly"
        } else {SigWord <- "not significantly"}
        OR <- trimws(FisherOut[grep("^odds ratio",FisherOut)+1])
        if (OR < 1) {
          ORWord <- "depleted"
        } else {ORWord <- "enriched"}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        HTML(paste("Fishers Exact test found that <b>",Feat1,"</b> - <b>",Var1,"</b> is <b>",SigWord,"</b> <b>",ORWord,
                   "</b> in <b>",Feat2,"</b> - <b>",Var2,"</b>, based on a P.value of <b>",pval,"</b> and an odds ratio of <b>",OR,"</b>.", sep = ""))
        
      })
      
      ####----PreC UMAP----####
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_PreC_clin_react_base <- reactive({
        
        #req(input$UMAPmetaFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #if (!is.null(input$UMAPmatrixFile)) {
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        #}
        
        #print(head(as_tibble(plot_df)))
        
        ## Meta Annotation Plot
        #if (!is.null(input$UMAPmatrixFile)) {
        if (input$UMAPannotateSamps != "") {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        else if (input$UMAPannotateSamps == "") {
          colnames(plot_df)[4] <- "GeneName"
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        #}
        #else if (is.null(input$UMAPmatrixFile)) {
        #  if (input$UMAPannotateSamps != "") {
        #    colnames(plot_df)[4] <- c("AnnoName")
        #    #print(head(as_tibble(plot_df)))
        #    k <- plot_df %>%
        #      ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
        #                 text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
        #                              "</br> <b>",metaColanno,":</b> ", AnnoName,
        #                              sep = "")))
        #  }
        #  else if (input$UMAPannotateSamps == "") {
        #    k <- plot_df %>%
        #      ggplot(aes(UMAP1, UMAP2,
        #                 text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
        #                              sep = "")))
        #  }
        #
        #}
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = umap1,
                        y = umap2,
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = umap1,
                        y = umap2,)
        }
        
        if (input$UMAPannotateSamps != "") {
          if (input$UMAPannoContCheck == T) {
            myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
            k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
          }
        }
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        k
        
      })
      
      umap_plot_PreC_clin_react_base_anno <- reactive({
        
        #req(input$UMAPmetaFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #if (!is.null(input$UMAPmatrixFile)) {
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        #}
        
        #print(head(as_tibble(plot_df)))
        
        ## Meta Annotation Plot
        #if (!is.null(input$UMAPmatrixFile)) {
        if (input$UMAPannotateSamps != "") {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        else if (input$UMAPannotateSamps == "") {
          colnames(plot_df)[4] <- "GeneName"
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        #}
        #else if (is.null(input$UMAPmatrixFile)) {
        #  if (input$UMAPannotateSamps != "") {
        #    colnames(plot_df)[4] <- c("AnnoName")
        #    #print(head(as_tibble(plot_df)))
        #    k <- plot_df %>%
        #      ggplot(aes(UMAP1, UMAP2, colour=AnnoName,
        #                 text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
        #                              "</br> <b>",metaColanno,":</b> ", AnnoName,
        #                              sep = "")))
        #  }
        #  else if (input$UMAPannotateSamps == "") {
        #    k <- plot_df %>%
        #      ggplot(aes(UMAP1, UMAP2,
        #                 text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
        #                              sep = "")))
        #  }
        #
        #}
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = umap1,
                        y = umap2,
                        color = metaColanno)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = umap1,
                        y = umap2,)
        }
        
        if (input$UMAPannotateSamps != "") {
          if (input$UMAPannoContCheck == T) {
            myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
            k <- k + scale_colour_gradientn(colours = rev(myPalette(100)))
          }
        }
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines")
            )
        }
        k
        
      })
      
      umap_plot_PreC_clin_react <- reactive({
        
        #req(input$UMAPmetaFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        #if (!is.null(input$UMAPmatrixFile)) {
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        #}
        
        
        k <- umap_plot_PreC_clin_react_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        #print("anno df")
        #print(head(as_tibble(k[["data"]])))
        
        ## Make it plotly
        k2 <- ggplotly(k,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title=umap1,zeroline=F),
                 yaxis=list(title=umap2,zeroline=F))
        
        k2 <- k2 %>%
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          k2 <- k2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        k2
        
      })
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_PreC_expr_react_base <- reactive({
        
        #req(input$UMAPmetaFile)
        #req(input$UMAPmatrixFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Expr Annotation Plot
        #if (!is.null(input$UMAPmatrixFile)) {
        if (input$UMAPannotateSamps != "") {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        else if (input$UMAPannotateSamps == "") {
          colnames(plot_df)[4] <- "GeneName"
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        #}
        
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = umap1,
                        y = umap2,
                        color = metaColgene)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = umap1,
                        y = umap2,)
        }
        k <- k + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText),
                       legend.spacing.y = unit(0,"cm"))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        k
        
      })
      
      umap_plot_PreC_expr_react_base_anno <- reactive({
        
        #req(input$UMAPmetaFile)
        #req(input$UMAPmatrixFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        #if (LogChoice == TRUE) {
        #  expr2 <- log2(as.matrix(expr) + 0.00001)
        #}
        #exprRange <- round(range(expr2[metaColgene,]),2)
        exprRange <- round(range(as.numeric(expr_g_t[,metaColgene])),2)
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Expr Annotation Plot
        #if (!is.null(input$UMAPmatrixFile)) {
        if (input$UMAPannotateSamps != "") {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        else if (input$UMAPannotateSamps == "") {
          colnames(plot_df)[4] <- "GeneName"
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=GeneName,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        #}
        
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = umap1,
                        y = umap2,
                        color = metaColgene)
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = umap1,
                        y = umap2,)
        }
        k <- k + scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(exprMin, exprMax), oob = scales::squish)
        
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText),
                       legend.spacing.y = unit(0,"cm"))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        k
        
      })
      
      umap_plot_PreC_expr_react <- reactive({
        
        #req(input$UMAPmetaFile)
        #req(input$UMAPmatrixFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        
        k <- umap_plot_PreC_expr_react_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        
        #print("expr df")
        #print(head(as_tibble(k[["data"]])))
        
        ## Make it plotly
        k2 <- ggplotly(k,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title=umap1,zeroline=F),
                 yaxis=list(title=umap2,zeroline=F))
        
        k2 <- k2 %>%
          hide_legend() %>%
          hide_colorbar()
        
        if (length(sampSelected) > 0) {
          k2 <- k2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        k2
        
      })
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_PreC_kmean_react_base <- reactive({
        
        #req(input$UMAPmetaFile)
        #req(input$UMAPmatrixFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        if (LogChoice == TRUE) {
          expr2 <- log2(as.matrix(expr) + 0.00001)
        }
        exprRange <- round(range(expr2[metaColgene,]),2)
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Cluster Annotation Plot
        #if (!is.null(input$UMAPmatrixFile)) {
        if (input$UMAPannotateSamps != "") {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        else if (input$UMAPannotateSamps == "") {
          colnames(plot_df)[4] <- "GeneName"
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        #}
        
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = umap1,
                        y = umap2,
                        color = "Cluster")
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = umap1,
                        y = umap2,)
        }
        
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3)
        }
        k
        
      })
      
      ## UMAP Plot  - PATH - Clin
      umap_plot_PreC_kmean_react_base_anno <- reactive({
        
        #req(input$UMAPmetaFile)
        #req(input$UMAPmatrixFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        UMAPtitleText <- input$UMAPtitleTextSize     # Title text size
        UMAPaxisText <- input$UMAPaxisTextSize       # Axis text size
        UMAPlegendText <- input$UMAPlegendTextSize   # Legend text size
        UMAPdotSize <- input$UMAPdotSize             # Dot size
        metaColanno <- NULL
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        #LogChoice <- input$LogGeneSelection
        expr2 <- expr
        if (LogChoice == TRUE) {
          expr2 <- log2(as.matrix(expr) + 0.00001)
        }
        exprRange <- round(range(expr2[metaColgene,]),2)
        
        if (!is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(input$GeneExprRange),",")[[1]][2]))
        }
        else if (is.null(input$GeneExprRange)) {
          exprMin <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][1]))
          exprMax <- as.numeric(gsub(" ","",strsplit(as.character(exprRange),",")[[1]][2]))
        }
        myPalette <- colorRampPalette(rev(brewer.pal(9, input$UMAPcolors)))
        
        ## Cluster Annotation Plot
        #if (!is.null(input$UMAPmatrixFile)) {
        if (input$UMAPannotateSamps != "") {
          colnames(plot_df)[c(4,5)] <- c("AnnoName","GeneName")
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColanno,":</b> ", AnnoName,
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        else if (input$UMAPannotateSamps == "") {
          colnames(plot_df)[4] <- "GeneName"
          #print(head(as_tibble(plot_df)))
          k <- plot_df %>%
            ggplot(aes(UMAP1, UMAP2, colour=Cluster,
                       text = paste("</br> <b>Sample Name:</b> ", colnames(meta_react())[1],
                                    "</br> <b>",metaColgene," Gene Expression:</b> ", round(GeneName,4),
                                    "</br> <b>Cluster:</b> ", Cluster,
                                    sep = "")))
        }
        
        #}
        
        
        k <- k + geom_point(shape = 19,
                            size = UMAPdotSize) +
          
          theme_minimal()
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          k <- k + labs(x = umap1,
                        y = umap2,
                        color = "Cluster")
        }
        if (input$UMAPannotateSamps == "") {
          k <- k + labs(x = umap1,
                        y = umap2,)
        }
        
        k <- k + theme(axis.text = element_text(size = UMAPaxisText),
                       axis.title = element_text(size = UMAPaxisText),
                       plot.title = element_text(size = UMAPtitleText),
                       legend.text=element_text(size=UMAPlegendText))
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plot_df$SampLabel <- ifelse(rownames(plot_df) %in% sampSelected, rownames(plot_df), NA)
          plotlabel <- rownames(plot_df[sampSelected,])
          k <- k + geom_point(data = plotdata,
                              aes(x = UMAP1, y = UMAP2),
                              pch = 1,
                              color = "black",
                              size = UMAPdotSize,
                              stroke = .3) +
            geom_text_repel(data = plot_df, aes(label = SampLabel),
                            max.overlaps = Inf,
                            color = "black",
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines"))
        }
        k
        
      })
      
      
      
      umap_plot_PreC_kmean_react <- reactive({
        
        #req(input$UMAPmetaFile)
        #req(input$UMAPmatrixFile)
        ## Variables
        sampSelected <- input$UMAPsampSelect         # Sample Names to annotate
        metaColanno <- NULL
        
        plot_df <- UMAP_PreC_CoordTable_react()
        rownames(plot_df) <- plot_df[,1]
        
        umap1 <- input$SelectPreCalc1
        umap2 <- input$SelectPreCalc2
        
        if (input$UMAPannotateSamps != "") {
          metaColanno <- input$UMAPannotateSamps
          if (input$UMAPannoContCheck != T) {
            plot_df[,metaColanno] <- as.factor(plot_df[,metaColanno])
          }
        }
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          expr_g_t <- FeatExpreAnnotation()
          metaColgene <- colnames(expr_g_t)[1]
          #metaColgene <- rownames(expr)[1]
        }
        metaColkmean <- "Cluster"
        
        
        k <- umap_plot_PreC_kmean_react_base()
        
        if (length(sampSelected) > 0) {
          plotdata <- plot_df[sampSelected,]
          plotlabel <- rownames(plot_df[sampSelected,])
          if (!is.null(metaColanno)) {
            colnames(plotdata)[c(4,5)] <- c("AnnoName","GeneName")
          }
          if (is.null(metaColanno)) {
            colnames(plotdata)[4] <- "GeneName"
          }
        }
        #print("Kmeans df")
        #print(head(as_tibble(k[["data"]])))
        
        ## Make it plotly
        k2 <- ggplotly(k,
                       tooltip = "text") %>%
          
          config(displayModeBar = F)  %>%
          
          layout(font=list(color="#black"),
                 xaxis=list(title=umap1,zeroline=F),
                 yaxis=list(title=umap2,zeroline=F))
        
        k2 <- k2 %>%
          hide_legend()
        
        if (length(sampSelected) > 0) {
          k2 <- k2 %>%
            add_annotations(x = plotdata$UMAP1,
                            y = plotdata$UMAP2,
                            text = rownames(plotdata),
                            showarrow = TRUE,
                            arrowhead = 4,
                            arrowsize = .5)
        }
        
        k2
        
      })
      
      output$UMAPprecLegend <- renderPlot({
        
        #req(input$UMAPmetaFile)
        #if (is.null(input$UMAPmatrixFile)) {
        #  if (!is.null(input$UMAPannotateSamps)) {
        #    if (input$UMAPannotateSamps != "") {
        #      k <- umap_plot_PreC_clin_react_base()
        #      legendk <- g_legend(k)
        #      legend_grid <- legendk
        #    }
        #    else if (input$UMAPannotateSamps == "") {
        #      legend_grid <- NULL
        #    }
        #  }
        #  legend_grid
        #}
        #else if (!is.null(input$UMAPmatrixFile)) {
        if (input$UMAPannotateSamps != "") {
          k <- umap_plot_PreC_clin_react_base()
          m <- umap_plot_PreC_expr_react_base()
          n <- umap_plot_PreC_kmean_react_base()
          legendk <- g_legend(k)
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendk,legendm,legendn,nrow = 1)
        }
        else if (input$UMAPannotateSamps == "") {
          m <- umap_plot_PreC_expr_react_base()
          n <- umap_plot_PreC_kmean_react_base()
          legendm <- g_legend(m)
          legendn <- g_legend(n)
          legend_grid <- grid.arrange(legendm,legendn,nrow = 1)
        }
        legend_grid
        #}
        
        
        
      })
      
      UMAPplot_PreC_ALL_react <- reactive({
        
        #req(input$UMAPmetaFile)
        
        #if (!is.null(input$UMAPmatrixFile)) {
        k2 <- umap_plot_PreC_clin_react()
        m2 <- umap_plot_PreC_expr_react()
        n2 <- umap_plot_PreC_kmean_react()
        
        if (input$UMAPannotateSamps != "") {
          UMAPtitlek <- paste("Annotated by",input$UMAPannotateSamps)
        }
        else if (input$UMAPannotateSamps == "") {
          UMAPtitlek <- ""
        }
        
        if (is.null(input$UMAPmetaFile) == T) {
          UMAPtitlek <- ""
        }
        expr <- expr_raw()
        if (!is.null(input$GeneSelection)) {
          GeneSelec <- input$GeneSelection
        }
        if (is.null(input$GeneSelection)) {
          GeneSelec <- rownames(expr)[1]
        }
        
        if (is.null(input$LogGeneSelection)) {
          LogChoice <- TRUE
        }
        else if (!is.null(input$LogGeneSelection)) {
          LogChoice <- input$LogGeneSelection
        }
        if (LogChoice == T) {
          GeneSelecExpr <- paste(GeneSelec,"(Log2)")
        }
        if (LogChoice == F) {
          GeneSelecExpr <- paste(GeneSelec)
        }
        
        
        UMAPtitlem <- paste("Annotated by",GeneSelecExpr)
        UMAPtitlen <- paste("Annotated by",input$ClusterMethod,input$ClusterNumber,"Clusters")
        
        
        if (input$UMAPorientation == "Side-by-Side") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 1, shareY = TRUE, shareX = TRUE)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlem,
              xref = "x2",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 1,
              text = UMAPtitlen,
              xref = "x3",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        else if (input$UMAPorientation == "Stacked") {
          subplot_all_sbs <- plotly::subplot(k2, m2, n2, nrows = 3, shareY = TRUE, shareX = TRUE, margin = 0.04)
          plot_titles = list(
            list(
              x = 0,
              y = 1,
              text = UMAPtitlek,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.65,
              text = UMAPtitlem,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ),
            list(
              x = 0,
              y = 0.3,
              text = UMAPtitlen,
              xref = "x",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              showarrow = FALSE
            ))
        }
        
        subplot_all_sbs <- subplot_all_sbs %>% layout(annotations = plot_titles)
        #}
        
        #if (is.null(input$UMAPmatrixFile)) {
        #  k2 <- umap_plot_PreC_clin_react()
        #  if (input$UMAPannotateSamps != "") {
        #    UMAPtitlek <- paste("Annotated by",input$UMAPannotateSamps)
        #  }
        #  else if (input$UMAPannotateSamps == "") {
        #    UMAPtitlek <- ""
        #  }
        #
        #  if (is.null(input$UMAPmetaFile) == T) {
        #    UMAPtitlek <- ""
        #  }
        #  k2 <- k2 %>%
        #    layout(title = UMAPtitlek)
        #  subplot_all_sbs <- k2
        #}
        
        subplot_all_sbs
      })
      
      output$UMAPplot_PreC_ALL <- renderPlotly({
        #req(input$UMAPmetaFile)
        p_all <- UMAPplot_PreC_ALL_react()
        p_all
      })
      
      ####----PreC Violin Plot----####
      
      VI_meta_subset_PreC <- reactive({
        
        ClusterTab <- umapClusterTable_react()
        meta <- meta_react()
        meta <- merge(meta,ClusterTab, all.y = T)
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        if (sampSubset == "Select All") {
          metaSub <- meta
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),all_of(sampGroupCriteria))
          metaSub
        }
        else if (sampSubset != "Select All") {
          metaSub <- meta[which(meta[,sampSubset] == sampGroupCriteria),]
          metaSub <- metaSub %>%
            relocate(any_of(colnames(meta_react())[1]),sampGroupCriteria,sampSubset)
          metaSub
        }
        
      })
      
      violin_PreC_react <- reactive({
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          groupCriteria <- "Cluster"
        } else {groupCriteria <- input$BPgroupCriteria}
        
        title_font <- input$Vplot1TitleSize              # Title font size
        Xaxis_font <- input$Vplot1XAxisSize              # Axis font size
        Yaxis_font <- input$Vplot1YAxisSize              # Axis font size
        anno_size <- input$Vplot1aAnnoSize
        hjust_orient <- 1                                # Initial hjust
        axis_orient <- as.numeric(input$VxAxisOrient)    # X-axis label orientation
        if (axis_orient == 0) {                          # Adjust hjust if orientation is 0
          hjust_orient <- 0.5
        }
        logchoice <- input$log2Vplot                     # Log expression data option
        colorin <- input$VplotColoCodes                  # Bar plot Color codes
        Vplotylim <- input$VPlotYlim                     # Y-limit
        Vplotybreaks <- input$VplotYbreaks               # Y-axis breaks
        if (is.null(input$VplotDotSize)) {
          dotsizein <- 1
        } else {dotsizein <- input$VplotDotSize}
        #dotsizein <- input$VplotDotSize
        if (is.null(input$VplotstatComp)) {
          StatMethod <- "None"
        } else {StatMethod <- input$VplotstatComp}
        #StatMethod <- input$VplotstatComp
        if (is.null(input$Vplotsampledots)) {
          dotChoice <- FALSE
        } else {dotChoice <- input$Vplotsampledots}
        #dotChoice <- input$Vplotsampledots
        VilOrBP <- input$ViolinOrBoxP
        
        metaSub <- VI_meta_subset_PreC()
        #if (!is.null(input$UMAPmatrixFile)) {
        FeatMat <- expr_raw()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        #sampSubset <- input$BPsampSubset
        sampCriteria <- input$BPsampCriteria
        #groupCriteria <- input$BPgroupCriteria
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        sampSelected <- input$UMAPsampSelect
        metaSub2 <- metaSub[,c(colnames(meta_react())[1],groupCriteria)]
        metaSub2[,groupCriteria] <- as.factor(metaSub2[,groupCriteria])
        
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          #if (!is.null(input$UMAPmatrixFile)) {
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          #}
          #else if (is.null(input$UMAPmatrixFile)) {
          #  feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          #}
          
          feat_gene2 <- merge(feat_gene,metaSub2,by = colnames(meta_react())[1], all = T)
          feature_lab <- feature
          feattitle <- feature_lab
          if (logchoice == T) {
            feature_lab <- paste(feature_lab,"(Log2)")
            feattitle <- feature_lab
          }
          
          if (sampSubset == "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab, "Clustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab, "Grouped by",groupCriteria)
            }
          }
          if (sampSubset != "Select All") {
            if (groupCriteria == "Cluster") {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nClustered by",input$ClusterMethod)
            }
            else {
              plottitle <- paste(feature_lab,"in",sampSubset,"-",sampCriteria, "Samples\nGrouped by",groupCriteria)
            }
          }
          
          
          if (logchoice == T) {
            #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
            feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
          }
          
          colnames(feat_gene2) <- c(colnames(meta_react())[1],"FeatureName","Type")
          
          #if (!is.numeric(feat_gene2$FeatureName)) {
          if (VilOrBP == "Stacked Barplot") {
            feat_gene2[,"FeatureName"] <- as.factor(feat_gene2[,"FeatureName"])
            #if (input$VplotXaxOrder == "Descending"){
            #  feat_gene2$Type <- fct_rev(feat_gene2$Type)
            #  feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            #}
            #if (input$VplotXaxOrder == "Ascending"){
            #  feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            #}
            barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            if (is.null(input$BPfeatFill)) {
              FillChoice <- FALSE
            } else {FillChoice <- input$BPfeatFill}
            #FillChoice <- input$BPfeatFill
            if (FillChoice == FALSE) {
              barp <- barp + geom_bar() +
                theme_minimal()
            }
            if (FillChoice == TRUE) {
              barp <- barp + geom_bar(position = "fill") +
                theme_minimal()
            }
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)))
            barp
            
          }
          #}
          
          else if(!VilOrBP == "Stacked Barplot") {
            #if (is.numeric(feat_gene2$FeatureName)) {
            
            if (logchoice == T) {
              #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
              feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
            }
            
            y_min <- 0
            y_max <- max(feat_gene2$FeatureName, na.rm = T) * 1.03
            if (Vplotylim != "") {
              y_min <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][1]))
              y_max <- as.numeric(gsub(" ","",strsplit(Vplotylim,",")[[1]][2]))
            }
            
            if (input$VplotXaxOrder == "Descending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,-feat_gene2$FeatureName)
            }
            if (input$VplotXaxOrder == "Ascending"){
              feat_gene2$Type <- reorder(feat_gene2$Type,feat_gene2$FeatureName)
            }
            
            barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #if (VilOrBP == "Stacked Barplot") {
            #  barp <- ggplot(feat_gene2, aes(fill = FeatureName, x = Type))
            #}
            #if (VilOrBP != "Stacked Barplot") {
            #  barp <- ggplot(data = feat_gene2, aes(x=Type,y=FeatureName, fill=Type))
            #}
            if (StatMethod != "None") {
              barp <- barp + stat_compare_means(data = feat_gene2,
                                                aes(x=Type,y=FeatureName),
                                                method = StatMethod,
                                                label.x = 1,
                                                label.y = max(feat_gene2$FeatureName, na.rm = T) * 1.02)
            }
            if (VilOrBP == "Box Plot") {
              barp <- barp + geom_boxplot() +
                theme_minimal()
            }
            if (VilOrBP == "Violin Plot") {
              barp <- barp + geom_violin() +
                theme_minimal()
              barp <- barp + stat_summary(fun=median, geom="point", shape=23, size=dotsizein, color="black", bg = "black")
            }
            
            if (Vplotylim == "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(expansion(mult = c(0, 0.1))) ## expansion function causing issue with y-axis title
                barp <- barp + scale_y_continuous()
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(0,y_max),breaks=seq(0,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(0,y_max),expansion(mult = c(0, 0.1)),breaks=seq(0,y_max,Vplotybreaks))
              }
            }
            else if (Vplotylim != "") {
              if (is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max))
              }
              else if (!is.na(Vplotybreaks)) {
                #barp <- barp + scale_y_continuous(limits=c(y_min,y_max),expansion(mult = c(0, 0.1)),breaks=seq(y_min,y_max,Vplotybreaks))
                barp <- barp + scale_y_continuous(limits=c(y_min,y_max),breaks=seq(y_min,y_max,Vplotybreaks))
              }
            }
            
            if (dotChoice == T) {
              feat_gene3 <- feat_gene2
              feat_gene3$xj <- jitter(as.numeric(factor(feat_gene3$Type)))
              barp <- barp + geom_point(data = feat_gene3, aes(x=xj), col="gray44", size=dotsizein)
              if (length(sampSelected) > 0) {
                feat_gene3$SampLabel <- ifelse(feat_gene3[,1] %in% sampSelected, feat_gene3[,1], NA)
                barp <- barp + geom_text_repel(data = feat_gene3,
                                               aes(x = xj, y = FeatureName, label = SampLabel),
                                               size = anno_size,
                                               max.overlaps = Inf,
                                               color = "black",
                                               fontface = "bold",
                                               min.segment.length = unit(0, 'lines'),
                                               box.padding = unit(0.35, "lines"),
                                               point.padding = unit(0.3, "lines"),
                                               arrow = arrow(length = unit(0.015, "npc"))
                )
                feat_gene4 <- feat_gene3[!is.na(feat_gene3$SampLabel),]
                barp <- barp + geom_point(data = feat_gene4, aes(x=xj), col="darkred", size=dotsizein)
              }
            }
            
            if (colorin != "") {
              colorin <- strsplit(colorin," ")[[1]]
              if (length(colorin) == 1) {
                colorin <- rep(colorin,length(unique(metaSub[,groupCriteria])))
              }
              barp <- barp + scale_fill_manual(values=colorin)
            }
            barp <- barp + labs(x = groupCriteria,
                                y = feattitle,
                                title = plottitle)
            barp <- barp + theme(axis.text.x = element_text(angle = axis_orient, hjust = hjust_orient,size = Xaxis_font),
                                 axis.title.x = element_text(size = Xaxis_font),
                                 axis.text.y = element_text(size = Yaxis_font),
                                 axis.title.y = element_text(size = Yaxis_font),
                                 plot.title = element_text(size = title_font, margin=margin(0,0,30,0)),
                                 legend.position = 'none')
            barp
            
          }
        }
        
        #  }
        #}
        
      })
      
      output$violin_PreC <- renderPlot({
        
        plot <- violin_PreC_react()
        plot
        
      })
      
      ViolinPlotTable_PreC_react <- reactive({
        
        if (is.null(input$BPsampSubset)) {
          sampSubset <- "Select All"
        } else {sampSubset <- input$BPsampSubset}
        if (is.null(input$BPgroupCriteria)) {
          sampGroupCriteria <- "Cluster"
        } else {sampGroupCriteria <- input$BPgroupCriteria}
        
        #if (!is.null(input$BPsampSubset)) {
        #  if (!is.null(input$BPgroupCriteria)) {
        metaSub <- VI_meta_subset_PreC()
        FeatMat <- expr_raw()
        FeatMat <- FeatMat[,metaSub[,colnames(meta_react())[1]]]
        logchoice <- input$log2Vplot
        if (is.null(input$BPFeatSelection)) {
          featSelected <- rownames(FeatMat)[1]
        } else {featSelected <- input$BPFeatSelection}
        #featSelected <- input$BPFeatSelection
        if (length(featSelected) > 0){
          
          feature <- featSelected
          FeatMat <- as.data.frame(FeatMat)
          if (feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(t(FeatMat[feature,]))
            feat_gene[,colnames(meta_react())[1]] <- rownames(feat_gene)
          }
          if (!feature %in% rownames(FeatMat)) {
            feat_gene <- as.data.frame(metaSub[,c(colnames(meta_react())[1],feature)])
          }
          feat_gene2 <- merge(feat_gene,metaSub,by = colnames(meta_react())[1], all = T)
          if (logchoice == T) {
            #feat_gene2[which(feat_gene2[,2] < 0),2] <- 0
            feat_gene2[,2] <- log2(feat_gene2[,2] + 1)
          }
          feat_gene2
        }
        #  }
        #}
        
      })
      
      output$ViolinPlotTable_PreC <- DT::renderDataTable({
        
        df <- ViolinPlotTable_PreC_react()
        DT::datatable(df,
                      extensions = "FixedColumns",
                      options = list(lengthMenu = c(10,20,50,100,1000,5000,10000),
                                     pageLength = 20,
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     fixedColumns = list(leftColumns = 1)),
                      rownames = F,
                      selection=list(mode = "multiple"))
        
      })
      
      
      
      ####----PreC Enrichment Test----####
      
      FisherMatrix_PreC_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        FishMat <- matrix(c(inBoth,inVar1,inVar2,inNone), nrow = 2)
        FishMat
        
      })
      
      output$EnrichFisherTabPreC <- renderTable({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        FishMat <- FisherMatrix_PreC_react()
        
        FishMat <- as.data.frame(FishMat)
        colnames(FishMat) <- c(paste0(Feat1," - ",Var1),paste0(Feat1," - Not ",Var1))
        rownames(FishMat) <- c(paste0(Feat2," - ",Var2),paste0(Feat2," - Not ",Var2))
        
        FishMat
        
      }, rownames = TRUE)
      
      Fisher_PreC_react <- reactive({
        
        FishMat <- FisherMatrix_PreC_react()
        TailChoice <- input$FisherTailChoice
        
        Fisher_react <- fisher.test(FishMat, alternative = TailChoice)
        Fisher_react
        
      })
      
      output$EnrichFisherOutPreC <- renderText({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        text <- capture.output(Fisher_PreC_react())
        text[grep("data:  FishMat",text)] <- paste0("data:  ",Feat1," - ",Var1," vs. ",Feat2," - ",Var2)
        textOut <- paste(text,collapse = '\n')
        textOut
        
      })
      
      FisherVenn_PreC_react <- reactive({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        inVar1 <- meta[which(meta[,Feat1] == Var1),1]
        inVar2 <- meta[which(meta[,Feat2] == Var2),1]
        
        VennList <- list(inVar1 = inVar1,
                         inVar2 = inVar2)
        names(VennList)[1] <- paste0(Feat1,":\n",Var1)
        names(VennList)[2] <- paste0(Feat2,":\n",Var2)
        VennObj <- Venn(VennList)
        VennData <- process_data(VennObj)
        
        if (packageVersion("ggVennDiagram") < package_version("1.4")) {
          ggplot() +
            # 1. region count layer
            geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
            # 2. set edge layer
            geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
            # 3. set label layer
            geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
            # 4. region label layer
            geom_sf_label(aes(label = paste0(count)),
                          data = venn_region(VennData),
                          size = 6,
                          alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            theme_void()
        } else {
          ggplot() +
            # 1. region count layer
            geom_polygon(aes(X, Y, fill = count, group = id),
                         data = venn_regionedge(VennData), show.legend = F) +
            # 2. set edge layer
            geom_path(aes(X, Y, color = id, group = id),
                      data = venn_setedge(VennData),
                      show.legend = FALSE, linewidth = 1) +
            # 3. set label layer
            geom_text(aes(X, Y, label = name),
                      data = venn_setlabel(VennData), size = 4) +
            # 4. region label layer
            geom_label(aes(X, Y, label = count),
                       data = venn_regionlabel(VennData),
                       size = 6,
                       alpha = 0.5) +
            scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
            scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
            coord_flip() +
            theme_void()
        }
        
        #ggplot() +
        #  # 1. region count layer
        #  geom_sf(aes(fill = count), data = venn_region(VennData), show.legend = F) +
        #  # 2. set edge layer
        #  geom_sf(aes(color = name), data = venn_setedge(VennData), show.legend = F, size = 1) +
        #  # 3. set label layer
        #  geom_sf_text(aes(label = name), data = venn_setlabel(VennData), size = 6) +
        #  # 4. region label layer
        #  geom_sf_label(aes(label = paste0(count)),
        #                data = venn_region(VennData),
        #                size = 6,
        #                alpha = 0.5) +
        #  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
        #  scale_color_manual(values = c("#F4FAFE","#F4FAFE"))+
        #  theme_void()
        
      })
      
      output$EnrichVennPlotPreC <- renderPlot({
        
        p <- FisherVenn_PreC_react()
        p
        
      })
      
      output$EnrichFisherTextPreC <- renderUI({
        
        meta <- EnrichMeta_Sub()
        
        if (is.null(input$EnrichFeat1)) {
          Feat1 <- "Cluster"
        } else {Feat1 <- input$EnrichFeat1}
        if (is.null(input$EnrichFeat2)) {
          Feat2 <- colnames(meta_react())[1]
        } else {Feat2 <- input$EnrichFeat2}
        if (is.null(input$EnrichVar1)) {
          Var1 <- levels(as.factor(meta[,Feat1]))[1]
        } else {Var1 <- input$EnrichVar1}
        if (is.null(input$EnrichVar2)) {
          Var2 <- levels(as.factor(meta[,Feat2]))[1]
        } else {Var2 <- input$EnrichVar2}
        
        TailChoice <- input$FisherTailChoice
        FisherOut <- capture.output(Fisher_PreC_react())
        pval <- gsub("p-value [[:punct:]] ","",grep("^p-value ",FisherOut, value = T))
        if (as.numeric(pval) < 0.05) {
          SigWord <- "significantly"
        } else {SigWord <- "not significantly"}
        OR <- trimws(FisherOut[grep("^odds ratio",FisherOut)+1])
        if (OR < 1) {
          ORWord <- "depleted"
        } else {ORWord <- "enriched"}
        
        inBoth <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] == Var2),])
        inNone <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] != Var2),])
        inVar1 <- nrow(meta[which(meta[,Feat1] == Var1 & meta[,Feat2] != Var2),])
        inVar2 <- nrow(meta[which(meta[,Feat1] != Var1 & meta[,Feat2] == Var2),])
        
        HTML(paste("Fishers Exact test found that <b>",Feat1,"</b> - <b>",Var1,"</b> is <b>",SigWord,"</b> <b>",ORWord,
                   "</b> in <b>",Feat2,"</b> - <b>",Var2,"</b>, based on a P.value of <b>",pval,"</b> and an odds ratio of <b>",OR,"</b>.", sep = ""))
        
      })
      
      ####----Downloads----####
      
      ## Cluster Table
      
      output$dnldUMAPclusterTab <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #n_clust <- input$ClusterNumber
          #cluster_method <- input$ClusterMethod
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          paste(file_name,"_",cluster_method,"_",n_clust,"_Clusters.txt", sep = '')
        },
        content = function(file) {
          tab <- umapClusterTable_react()
          write_delim(tab,file,delim = '\t')
        }
      )
      
      ## MVG Downloads
      
      output$dnldUMAPcoords_MVG <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          topNum <- input$TopNumMVG
          paste(file_name,"_",topNum,"MVG_UMAP_Coordinates.txt", sep = '')
        },
        content = function(file) {
          tdata_fit_df <- as.data.frame(UMAP_MVG_CoordTable_react())
          write_delim(tdata_fit_df,file,delim = '\t')
        }
      )
      
      output$dnldUMAP_SVG_MVG_clin <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          topNum <- input$TopNumMVG
          metaCol <- input$UMAPannotateSamps
          metaCol <- gsub(" ","",input$UMAPannotateSamps)
          metaCol <- gsub("[[:punct:]]","_",metaCol)
          paste(file_name,"_",topNum,"MVG_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_MVG_react_clin_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_SVG_MVG_expr <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          topNum <- input$TopNumMVG
          metaCol <- paste(input$GeneSelection,"Expr",sep = "")
          paste(file_name,"_",topNum,"MVG_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_MVG_react_expr_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_SVG_MVG_kmean <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          topNum <- input$TopNumMVG
          clusterMethod <- input$ClusterMethod
          ClusterNum <- input$ClusterNumber
          metaCol <- paste(clusterMethod,ClusterNum,"Clusters",sep = "")
          paste(file_name,"_",topNum,"MVG_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_MVG_react_kmean_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      ## Pathway downloads
      
      output$dnldUMAPcoords_PATH <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          paste(file_name,"_",GeneSet,"_UMAP_Coordinates.txt", sep = '')
        },
        content = function(file) {
          tdata_fit_df <- as.data.frame(UMAP_PATH_CoordTable_react())
          write_delim(tdata_fit_df,file,delim = '\t')
        }
      )
      
      output$dnldUMAP_PATH_SVG_clin <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          metaCol <- input$UMAPannotateSamps
          metaCol <- gsub(" ","",input$UMAPannotateSamps)
          metaCol <- gsub("[[:punct:]]","_",metaCol)
          paste(file_name,"_",GeneSet,"_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_PATH_react_clin_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_PATH_SVG_expr <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          metaCol <- paste(input$GeneSelection,"Expr",sep = "")
          paste(file_name,"_",GeneSet,"_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_PATH_react_expr_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_PATH_SVG_kmean <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          clusterMethod <- input$ClusterMethod
          ClusterNum <- input$ClusterNumber
          metaCol <- paste(clusterMethod,ClusterNum,"Clusters",sep = "")
          paste(file_name,"_",GeneSet,"_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_PATH_react_kmean_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      ## Base downloads
      
      output$dnldUMAPcoords <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          paste(file_name,"_AllGenes_UMAP_Coordinates.txt", sep = '')
        },
        content = function(file) {
          tdata_fit_df <- as.data.frame(UMAP_CoordTable_react())
          write_delim(tdata_fit_df,file,delim = '\t')
        }
      )
      
      output$dnldUMAP_SVG_clin <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          metaCol <- input$UMAPannotateSamps
          metaCol <- gsub(" ","",input$UMAPannotateSamps)
          metaCol <- gsub("[[:punct:]]","_",metaCol)
          paste(file_name,"_AllGenes_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_clin_react_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_SVG_expr <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          metaCol <- paste(input$GeneSelection,"Expr",sep = "")
          paste(file_name,"_AllGenes_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_expr_react_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_SVG_kmean <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          clusterMethod <- input$ClusterMethod
          ClusterNum <- input$ClusterNumber
          metaCol <- paste(clusterMethod,ClusterNum,"Clusters",sep = "")
          paste(file_name,"_AllGenes_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_kmean_react_base_anno()
          #plot <- umap_plot_kmean_react()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      ## Pre-calculated downloads
      
      output$dnldUMAP_SVG_PreC_clin <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          umap1 <- input$SelectPreCalc1
          umap2 <- input$SelectPreCalc2
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          metaCol <- input$UMAPannotateSamps
          metaCol <- gsub(" ","",input$UMAPannotateSamps)
          metaCol <- gsub("[[:punct:]]","_",metaCol)
          paste(file_name,"_",umap1,"_",umap2,"_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_PreC_clin_react_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_SVG_PreC_expr <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          umap1 <- input$SelectPreCalc1
          umap2 <- input$SelectPreCalc2
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          metaCol <- paste(input$GeneSelection,"Expr",sep = "")
          paste(file_name,"_",umap1,"_",umap2,"_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          plot <- umap_plot_PreC_expr_react_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldUMAP_SVG_PreC_kmean <- downloadHandler(
        filename = function() {
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          umap1 <- input$SelectPreCalc1
          umap2 <- input$SelectPreCalc2
          rowsSelected <- input$PathSelectTable_rows_selected
          GeneSet <- gs_cat2()[rowsSelected,3]
          clusterMethod <- input$ClusterMethod
          ClusterNum <- input$ClusterNumber
          metaCol <- paste(clusterMethod,ClusterNum,"Clusters",sep = "")
          paste(file_name,"_",umap1,"_",umap2,"_",metaCol,"Anno_UMAP.svg", sep = '')
        },
        content = function(file) {
          #plot <- umap_plot_PreC_kmean_react_base()
          plot <- umap_plot_PreC_kmean_react_base_anno()
          ggsave(file,plot, width = 8, height = 8)
        }
      )
      
      output$dnldKmeansClusterSubsetExpr <- downloadHandler(
        filename = function() {
          
          #gs.u <- input$UMAPmatrixFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          cluster_choice <- input$KmeansClusterSubsetnum
          
          paste(file_name,"_UMAPCluster",cluster_choice,"Subset.txt",sep = "")
        },
        content = function(file) {
          #gs.u <- input$UMAPmatrixFile
          #ext <- tools::file_ext(gs.u$datapath)
          #req(gs.u)
          #validate(need(ext == c("tsv","txt","csv","zip"), "Please upload .tsv, .txt, or .csv file"))
          #
          #if (ext == "csv") {
          #  expr <- as.data.frame(read_delim(gs.u$datapath, delim = ',', col_names = T))
          #}
          #else {
          #  expr <- as.data.frame(read_delim(gs.u$datapath, delim = '\t', col_names = T))
          #}
          expr <- expr_react()
          clusterTab <- umapClusterTable_react()
          cluster_choice <- input$KmeansClusterSubsetnum
          clusterTab_sub <- clusterTab[which(clusterTab$Cluster == cluster_choice),]
          samples <- clusterTab_sub[,colnames(meta_react())[1]]
          expr_sub <- expr[,c(colnames(expr)[1],samples)]
          
          write_delim(expr_sub,file,delim = '\t')
        }
      )
      
      output$dnldKmeansClusterSubsetMeta <- downloadHandler(
        filename = function() {
          
          #gs.u <- input$UMAPmetaFile
          #file_name <- gs.u$name
          #file_name <- tools::file_path_sans_ext(gs.u)
          file_name <- ProjectName
          cluster_choice <- input$KmeansClusterSubsetnum
          
          paste(file_name,"_UMAPCluster",cluster_choice,"MetaSubset.txt",sep = "")
        },
        content = function(file) {
          
          meta <- meta_react()
          
          clusterTab <- umapClusterTable_react()
          cluster_choice <- input$KmeansClusterSubsetnum
          clusterTab_sub <- clusterTab[which(clusterTab$Cluster == cluster_choice),]
          samples <- clusterTab_sub[,colnames(meta_react())[1]]
          meta_sub <- meta[which(meta[,colnames(meta_react())[1]] %in% samples),]
          
          write_delim(meta_sub,file,delim = '\t')
        }
      )
      
      
      
      
    }
  })
}



# Run the application
shinyApp(ui = ui, server = server)


















