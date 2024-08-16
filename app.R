pacman::p_load(shiny,shinyBS,shinydashboard,stm,tidygraph,ggraph,
               stringr,ggplot2,ggrepel,tibble,dplyr,readr,fresh)



ui <- dashboardPage(
  header = dashboardHeader(title = 'STM HR Insights'),

# 1. Sidebar -----------------------------------------------------------------

  sidebar = dashboardSidebar(
    use_googlefont("Noto Serif KR"),
    use_googlefont("Kanit"),
   
    tags$style(
      "#sidebarItemExpanded {
            overflow: auto;
            height: calc(100vh - 50px) !important;
            position:fixed;width:220px;
        }"
    ),

    
    conditionalPanel(
      condition = "input.tabvals == 0", 

      br(),br(),br(),br(),br(),br(),
      p(HTML(paste0(strong('안녕하세요'),br(), strong('STM HR Insights'),
                    br(),strong('활용방법 안내드립니다'))), 
      style = "color:white; text-align:center; font-size:18px;
      font-family:'Noto Serif KR';line-height:40px;")

      ),
    
    
    conditionalPanel(
      condition = "input.tabvals == 10", 
      
      br(),br(),br(),br(),br(),br(),
      p(HTML(paste0(strong('HR 텍스트 데이터 기반'),br(),
                    strong('구조적 토픽모델링을'),br(), 
                    strong('진행합니다'))), 
        style = "color:white; text-align:center; font-size:18px;
        font-family:'Noto Serif KR';line-height:40px;")
    ),
    
    conditionalPanel(
      condition = "input.tabvals == 1",
      br(),br(),
      p(HTML(paste0(strong('① 토픽 선택')
      )), 
      style = "color:white; text-align:center; font-size:16px;
        font-family:'Noto Serif KR';line-height:20px;"),
      
      selectInput(
        "topic",
        label = NULL,
        choices = c(1),
        selected = 1
      ),
      
      checkboxGroupInput(
        "labtypes",
        "② 단어 가중치 선택",
        c(
          "Probability" = 1,
          "FREX" = 2,
          "Lift" = 3,
          "Score" = 4
        ),
        selected = c(1, 2)
      ),
      
      sliderInput(
        'nrwords',
        '③단어 개수 선택',
        min = 1,
        max = 50,
        value = 10
      ),
 
      tags$hr(),
      # p(HTML(paste0(strong('문서선택')
      # )), 
      # style = "color:white; text-align:center; font-size:16px;
      #   font-family:'Noto Serif KR';line-height:20px;"),
      uiOutput('doccol'),
    
      numericInput(
        'number_articles',
        label = "⑤ 문서 개수 선택",
        value = 100,
        min = 1
      ),
      
      sliderInput(
        'mintheta',
        '⑥ 문서-토픽 간 관계의 정도 선택',
        min = 0,
        max = 1,
        value = 0.2

      ),

      checkboxGroupInput(
        "include_doc_theta",
        "⑦ 표에 나타낼 열 선택",
        c(
          "STM document ID" = 1,
          "Row index" = 2,
          "Theta" = 3
        ),
        selected = NULL
      )
    ),

    conditionalPanel(
      condition = "input.tabvals == 6", 
      
      br(),br(),br(),br(),br(),br(),
      p(HTML(paste0(strong('분석 결과를 통해'),br(),
                    strong('토픽을 아우루는 주제를 '),br(), 
                    strong('선정합니다'))), 
        style = "color:white; text-align:center; font-size:18px;
        font-family:'Noto Serif KR';line-height:40px;")
      
    ),


    conditionalPanel(
      condition = "input.tabvals == 7",
      
      selectInput(
        'plotType',
        # type "difference" is not implemented at the moment
        label = "Type",
        choices = c("wordcloud", "perspectives", "pointestimate", "continuous"),
        selected = "wordcloud"
      )

    ),


    conditionalPanel(
      condition = "input.tabvals == 12",
      width = 2,
      br(),
      br(),
      p(HTML(paste0('① 문서 ID 선택 ')),
        style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:white;
                 text-align:justify;font-size:16px;line-height:20px;"),
      
      numericInput(
        'articleID',
        label = "STM 문서 ID",
        value = 1,
        min = 1
      ),
      
      br(),
      p(HTML(paste0('② Scatter Plot 그리기')),
        style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:white;
                 text-align:justify;font-size:16px;line-height:20px;"),
      h5("x축 토픽 선택"),
      selectInput(
        "topic_graph_1",
        label = NULL,
        choices = c(1),
        selected = 1
      ),
      
      h5("y축 토픽선택"),
      selectInput(
        "topic_graph_2",
        label = NULL,
        choices = c(1),
        selected = 2
      ),
      bsTooltip(
        'topic_graph_2',
        "Select the topic that will be on the y-axis.",
        placement = "right"
      ),
      br(),
      p(HTML(paste0('③ 문서 정보 확인하기')),
        style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:white;
                 text-align:justify;font-size:16px;line-height:20px;"),

      uiOutput('doccol2'),

      checkboxGroupInput(
        "include_doc_theta2",
        "Display:",
        c("STM document ID" = 1),
        selected = NULL
      ),
      bsTooltip(
        'include_doc_theta',
        "Check to include STM document ID",
        placement = "right"
      )


    ),


    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType != 'perspectives'",

      htmlOutput("effectTopic"),


      bsTooltip(
        'effectTopic',
        "Select the topic for the plot creation.",
        placement = "right"
      )
    ),




    conditionalPanel(
      condition = "input.tabvals == 7 && (input.plotType == 'continuous' || input.plotType == 'pointestimate')",

      htmlOutput("plotVar"),
      htmlOutput("plotLabel"),
      htmlOutput("plotLabel2"),

      sliderInput(
        'effectci',
        'Confidence Intervall',
        min = 0.55,
        max = 0.99,
        value = 0.95
      ),
      bsTooltip(
        'effectci',
        "Width of confidence intervalls can be adjusted here.",
        placement = "right"
      )



    ),


    conditionalPanel(
      condition = "input.tabvals == 7 && (input.plotType == 'continuous' || input.plotType == 'pointestimate')",
      #|| input.plotType == 'pointestimate' not implemented for now
      checkboxInput('moderator',
                    label = "Interaction Effect"),
      bsTooltip(
        'moderator',
        "Plot an interaction effect for a categorical variable.",
        placement = "right"
      )
    ),





    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'wordcloud'",
      sliderInput(
        'cloud_words',
        'Maximum nr. of words',
        min = 10,
        max = 100,
        value = 50
      ),
      bsTooltip(
        'cloud_words',
        "The maximum number of words to be included in the cloud.",
        placement = "right"
      ),

      sliderInput(
        'scalemax',
        'Maximum word scaling',
        min = 1,
        max = 10,
        value = 3
      ),
      bsTooltip('scalemax', "Adjust the maximum word size.",
                placement = "right"),

      sliderInput(
        'scalemin',
        'Minimum word scaling',
        min = .2,
        max = 2,
        value = .5
      ),
      bsTooltip('scalemin', "Adjust the minimum word size.",
                placement = "right")
    ),




    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'perspectives'",



      htmlOutput("perspTopic1"),
      bsTooltip(
        'perspTopic1',
        "Choose the first topic for the perspective plot.",
        placement = "right"
      ),

      htmlOutput("perspTopic2"),
      bsTooltip(
        'perspTopic2',
        "Choose the second topic for the perspective plot.",
        placement = "right"
      ),

      sliderInput(
        'persp_words',
        'Number of words',
        min = 5,
        max = 50,
        value = 25
      ),
      bsTooltip(
        'persp_words',
        "Number of words to be displayed in the perspective plot.",
        placement = "right"
      ),

      sliderInput(
        'persp_cex',
        'Text Scaling',
        min = 0.1,
        max = 2,
        value = 1
      ),
      bsTooltip(
        'persp_cex',
        "Controls the scaling constant on text size.",
        placement = "right"
      ),
      checkboxInput('contLabels',
                    'Change content categories',
                    value = FALSE),
      bsTooltip(
        'contLabels',
        "Check if you want to change the categories to plot. Only affects content models.",
        placement = "right"
      )


    ),



    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'perspectives' && input.contLabels == true",

      htmlOutput("perspCat1"),
      bsTooltip(
        'perspCat1',
        "Choose the first category for the content perspective plot.",
        placement = "right"
      ),

      htmlOutput("perspCat2"),
      bsTooltip(
        'perspCat2',
        "Choose the second category for the content perspective plot.",
        placement = "right"
      )
    ),



    conditionalPanel(
      condition = "input.tabvals == 7 && input.moderator == true && (input.plotType == 'continuous' || input.plotType == 'pointestimate')",
      # || input.plotType == 'pointestimate') not implemented for now

      htmlOutput("modvar"),
      bsTooltip('modvar',
                "Select a categorical moderator variable.",
                placement = "right"),

      selectInput("modval1",
                  choices = "None",
                  label = "Interaction value 1"),

      selectInput("modval2",
                  choices = "None",
                  label = "Interaction value 2")

    ),



    conditionalPanel(
      condition = "input.tabvals == 4",

      selectInput(
        "graphmethod",
        label = "방법 선택",
        choices = c("cutoff", "huge")
      ),
      bsTooltip(
        'graphmethod',
        'Choose edges are determined algorithmically\\
        or by a correlation cutoff criterium.',
        placement = "right"
      )
    ),

    conditionalPanel(
      condition = "input.tabvals == 4 && input.graphmethod == 'cutoff'",

      sliderInput(
        "cutoff 값 설정",
        label = h4("cutoff"),
        min = 0.01,
        max = 0.5 ,
        value = 0.05
      ),
      bsTooltip('cutoff', 'Minimum correlation between topic pairs.',
                placement = "right")
    ),

    conditionalPanel(
      condition = "input.tabvals == 4",
      checkboxInput(
        "cutiso",
        label = h4("Remove isolated Nodes"),
        value = FALSE
      ),
      bsTooltip(
        'cutiso',
        'Based on the correlation criteria isolated topics have no connections to other topics.',
        placement = "right"
      ),
      checkboxInput("eLabels", label = h4("Show weights"),
                    value = FALSE),
      bsTooltip(
        'eLabels',
        'Edge labels display correlations between topics.',
        placement = "right"
      ),

      checkboxInput(
        "includelegend",
        label = h4("Include legend"),
        value = FALSE
      ),
      bsTooltip(
        'includelegend',
        'Include scaling guides for the plot.',
        placement = "right"
      )
    )
  ),


#2. Body --------------------------------------------------------------------

  body = dashboardBody(
    use_googlefont("Noto Serif KR"),
    use_googlefont("Kanit"),
    
    tags$head(
      tags$style(
        HTML('.content-wrapper {
               overflow: auto;
           }'
        )
      )
    ),
    
    fluidRow(column(
    width = 12,
    tabBox(
      id = 'tabvals',
      width = NULL,
      
      tabPanel(strong('사용방법 안내'),
               
               fluidRow(width = 12,
               box(width = 12, title=strong('대시보드 활용방법 안내'), status = "primary",
                   solidHeader = FALSE,  collapsible = TRUE,
                   
               imageOutput("picture", height = "auto"),
               )
               ),
        value = 0,
      ),

      tabPanel(strong('Ⅰ. 토픽모델링 실행'),

# -------------------------------------------------------------------------

fluidRow(width=12,
         box(width = 12, title=strong('[Ⅰ. 토픽모델링 실행] 진행 안내'), status = "primary",
             solidHeader = FALSE,  collapsible = TRUE,
             
             p(HTML(paste0('아래 순서에 따라 진행합니다',br(),br(),
                           '1. 4stminsights.R 스크립트를 활용하여 전처리를 진행합니다.',br(),br(),
                           '2. 4stminsights.R에 안내된 것처럼 확인하고자 하는 토픽 개수를 정합니다.',br(), br(),
                           '3. 정해진 토픽 수 만큼 stm하여 RData로 저장합니다.',br(),br(),
                           '4. 확인하려고 하는 토픽 개수에 해당하는 모델을 선택합니다. '
             )),style="font-family:'Noto Serif KR';
                          list-style-position: inside;color:black;font-weight:bold;
                   text-align:justify;font-size:16px;line-height:20px;")),
),
tags$br(),
tags$br(),

fluidRow(width=12,
         box(width = 12, title=strong('파일 업로드 및 모델 선택'), status = "primary",
             solidHeader = FALSE, height = 200, collapsible = FALSE,
             br(),
             p(HTML(paste0('① 4stm insights.R 코드로 텍스트 데이터를 전처리 한 뒤 업로드 하세요')),
               style="font-family:'Noto Serif KR';list-style-position: inside;
            color:black;text-align:justify;font-size:16px;font-weight:bold"),
             fileInput('rfile', 'RData 파일 포맷만 가능합니다', accept = c('.RData', '.Rda', '.tsv','.csv')),
             
             br(),
             br(),
             
             uiOutput('modelchoice'),
             uiOutput('effectschoice'),
             uiOutput('effectsexplain')
             
         ),style = "height:380px;"),
# -------------------------------------------------------------------------
               value = 10
        
      ),
      
      tabPanel(
        strong('Ⅱ. 토픽 기본 정보 확인'),
        tags$head(tags$style("tfoot {display: table-header-group;}")),
        shinyjs::useShinyjs(),
        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }",
        ),
        
        fluidRow(width=12,
                 box(width = 12, title=strong('[Ⅱ. 토픽 기본 정보 확인] 진행 안내'), status = "primary",
                     solidHeader = FALSE, height = 400, collapsible = TRUE,
                     p(HTML(paste0('왼쪽 슬라이드바에서'
                       ,br(),br(),'1. [토픽]을 선택합니다',br(),br(),                                   '2. 단어 가중치를 선택합니다',br(), br(),
                                   '3. 표시되는 단어 개수를 선택합니다',br(), br(),
                                   '4. 원문서가 들어있는 열을 선택하고, 보여지는 문서의 개수를 선택합니다 ',br(), br(),
                                   '5. 선택한 토픽과 문서가 관계 있는 정도를 선택합니다',br(),br(),'  ※ 1에 가까울수록 토픽-문서 관계가 강함',br(),br(),
                                   
                                   '6. 선택한 토픽에서 나온 단어와 문서를 보면서 주제를 정합니다'
                     )),style="font-family:'Noto Serif KR';
                          list-style-position: inside;color:black;
                   text-align:justify;font-size:16px;line-height:20px;")),
        ),
        
        br(),
        br(),
        
        fluidRow(width = 12,
        
         box(width = 12, title=strong('단어로 토픽 톺아보기'), 
             status = "primary",solidHeader = FALSE, height = 500, collapsible = TRUE,
        
             p(HTML(paste0('단어를 기준으로 토픽의 주제를 유추합니다'
                           ,br(),br(), 
                           "&nbsp","&nbsp",strong('Highest Prob: '),'각 주제에서 가장 높은 확률을 가진 단어',
              br(), br(), 
              "&nbsp","&nbsp",strong('FREX: '), '빈번하게 등장하면서 다른 document와 exclusive한 단어',
                     br(),br(), 
              "&nbsp","&nbsp",strong('Score: '),'깁스샘플링 기반 토픽모델링 점수가 높은 단어',
                     br(),br(), 
              "&nbsp","&nbsp",strong('Lift: '),'주제-단어분포를 경험적 단어수 확률분포로 계산한 값이 높은 단어'))
              ,style="font-family:'Noto Serif KR';
                          list-style-position: inside;color:black;
                   text-align:justify;font-size:16px;line-height:20px;"),
             br(),
              dataTableOutput('tterms')
             
             ),
        ),#fluidRow end
        
        br(),
        
        
        fluidRow(width = 10,offset = 1,
        box(width = 12, title=strong('문서로 토픽 톺아보기'), 
            status = "primary",solidHeader = FALSE,  collapsible = TRUE,
            
        
            p(HTML(paste0('각 토픽의 비중이 가장 높은 문서들을 확인할 수 있습니다')),
          style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:black;
                 text-align:justify;font-size:16px;line-height:20px;"),
          br(),
          dataTableOutput('tlabel'),
        )
        ),

        tags$hr(),
        
        value = 1
      ),

      tabPanel(
        strong('Ⅲ. 토픽 주제 선정'),
        

# -------------------------------------------------------------------------
fluidRow(width=12,
         box(width = 12, title=strong('[Ⅲ. 토픽 주제 선정] 진행 안내'), status = "primary",
             solidHeader = FALSE, collapsible = TRUE,
             p(HTML(paste0('1. 전체 문서에서 토픽의 비중을 확인합니다 ',
                           br(),br(),                                   
                           '2.[Ⅱ. 토픽 기본 정보 확인]의 결과를 보면서 토픽의 중심 주제를 정합니다 ',br(), br(),
                           '3. 토픽별로 중심주제를 잘 표현할 수 있는 제목을 정합니다',br(), br(),
                           '4. 아래 토픽 제목 붙이기에서 토픽 제목을 입력해줍니다'
             )),style="font-family:'Noto Serif KR';
                          list-style-position: inside;color:black;
                   text-align:justify;font-size:16px;line-height:20px;")),
),



# -------------------------------------------------------------------------
fluidRow(width=12,       
        box(width = 12, title=strong('토픽 비중 확인하기'), 
            status = "primary",solidHeader = FALSE,  collapsible = TRUE,
            
            
            p(HTML(paste0('문서 전체에서, 각 토픽들의 비중(Proportion)을 보여주는 그래프입니다')),
              style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:black;
                 text-align:justify;font-size:16px;line-height:20px;"),
            br(),
        
        plotOutput('topicprops',
                   height = '600px',
                   width = "70%"),
        downloadButton("download_prop", "Download plot"),
        )
        ),
        
        fluidRow(width = 12,
                 box(width = 12, title=strong('토픽 제목 붙이기'), 
                     status = "primary",solidHeader = FALSE, collapsible = TRUE,
                  
                         p(HTML(paste0('[Ⅱ.토픽 기본 정보 확인]에서 단어와 문서를 통해 토픽을 살펴본 뒤',
                                br(),'토픽별로 주제를 정하고 이름을 붙여줍니다 ')),
                           style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:black;
                 text-align:justify;font-size:16px;line-height:20px;"),
                         br(),
                     uiOutput("textInputs")
                 )),

        value = 6
      ),


      tabPanel(
        strong('Ⅳ. 문서별 토픽 비중 확인'),
        
        
        # -------------------------------------------------------------------------
        fluidRow(width=12,
                 box(width = 12, title=strong('[Ⅳ. 문서별 토픽 비중 확인] 진행 안내'), status = "primary",
                     solidHeader = FALSE, collapsible = TRUE,
                     p(HTML(paste0('1. 왼쪽 슬라이드에서 확인하고 싶은 [문서ID]를 선택합니다',br(),br(),                                   '2. 연관성이 있을 것 같은 토픽을 선택하여 x축과 y축으로 선택합니다. ',br(), br(),
                                   '3. Scatter Plot을 보면서,문서를 선택하여 관계를 살펴봅니다')),style="font-family:'Noto Serif KR';
                          list-style-position: inside;color:black;
                   text-align:justify;font-size:16px;line-height:20px;")),
        ),
        
        
        
        # -------------------------------------------------------------------------
        fluidRow(width=12,
                         box(width = 12, title=strong('Proportion Plot 그리기'), status = "primary",
                     solidHeader = FALSE, collapsible = FALSE,
                     
                     p(HTML(paste0('개별 문서의 토픽별 비중을 나타내는 그래프입니다')),
                       style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:black;
                 text-align:justify;font-size:16px;line-height:16px;"),
                     
        plotOutput('topicpropsperdoc',
                   height = '600px',
                   #             height = paste(as.character((ncol(props()) * 25), 'px')),
                   width = "70%"),
        #downloadButton("download_prop_docs", "Download plot"),
        
                         )
        ),
        fluidRow(width=12,
        box(width = 12, title=strong('Scatter Plot 그리기'), status = "primary",
            solidHeader = FALSE,collapsible = FALSE,
            
            p(HTML(paste0('두 가지 토픽에 포함된 정도로 문서의 정보가 보여집니다')),
              style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:black;
                 text-align:justify;font-size:16px;line-height:16px;"),
    
        plotOutput(
          'doc_topic_scatter',
          height = '600px',
          width = "600px",
          click = "plot_click",
          brush = brushOpts(id = "plot_brush")
        ),
        #downloadButton("download_scatter", "Download plot"),
        
        )
        ),

# -------------------------------------------------------------------------
fluidRow(width=12,
    box(width = 12, title=strong('단일 문서 정보 확인하기'), status = "primary",
      solidHeader = FALSE, collapsible = FALSE,
      
      p(HTML(paste0('Scatter Plot 상의 객체를 클릭하면, 해당 문서의 정보를 확인할 수 있습니다', br(),br(), '※ 각 문서의 토픽 비중과 메타 데이터를 보여줍니다')),
        style="font-family:'Noto Serif KR';
                   list-style-position: inside;color:black;
                   text-align:justify;font-size:16px;line-height:16px;"),   
          
          dataTableOutput("click_info"),
          
    )
    ),
        
        
        
        

# -------------------------------------------------------------------------
fluidRow(width=12,
box(width = 12, title=strong('다수 문서 정보 확인하기'), status = "primary",
    solidHeader = FALSE,  collapsible = FALSE,
    
    p(HTML(paste0('Scatter Plot 상의 객체를 드래그하면, 드래그한 영역 안의 문서 정보를 확인할 수 있습니다', br(),br(), '※ 50개까지 문서의 토픽 비중과 메타 데이터를 한번에 보여줍니다')),
      style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:black;
                 text-align:justify;font-size:16px;line-height:16px;"),   
        dataTableOutput("brush_info"),
)
),
        value = 12
      ),



      tabPanel(
        strong('Ⅴ. 다양한 그래프 생성'),
        
# -------------------------------------------------------------------------
        fluidRow(width=12,
                 box(width = 12, title=strong('[Ⅴ. 다양한 그래프 생성] 진행 안내'), status = "primary",
                     solidHeader = FALSE, collapsible = TRUE,
                     p(HTML(paste0('1. 왼쪽 슬라이드바에서 출력하고 싶은 그래프 Type을 선택합니다',br(),br(),                                   '2. 그래프 형태별로, 옵션을 조정하며 원하는 그래프 형태를 얻습니다',br(), br(),
                                   strong('wordcloud: 단어 빈도수 기준으로 단어 크기를 정하여 wordcloud를 생성합니다'),br(), br(),
                                   strong('perspectives: 두개의 토픽을 메타 데이터를 기준으로 비교하는 그래프를 생성합니다'),br(), br(),
                                   strong('pointestimate: 점 추정 기반, 토픽 메타 데이터에 의한 변화를 확인합니다'),br(), br(),
                                   strong('continuous: 구간 추정 기반, 토픽 메타 데이터에 의한 변화를 확인합니다'),br(), br())),style="font-family:'Noto Serif KR';
                          list-style-position: inside;color:black;
                   text-align:justify;font-size:16px;line-height:20px;")),
        ),

# -------------------------------------------------------------------------
fluidRow(width=12,
         box(width = 12, title=strong('그래프 생성'), status = "primary",
             solidHeader = FALSE, collapsible = TRUE,
        
        p(HTML(paste0('토픽모델링 결과와 메타데이터를 기반으로  슬라이드바의 옵션을 조정하여 다양한 그래프를 생성합니다')), style="font-family:'Noto Serif KR';
                 list-style-position: inside;color:black;
                 text-align:justify;font-size:16px;line-height:16px;"),  
        actionButton("effectsubmit", label = "Generate plot") ,
        plotOutput('effectplot',
                   height = "500px",
                   width = "70%") ,
        downloadButton("download_plot", "Download plot"),
        
         )),

        value = 7
      ),



      tabPanel(
        strong('Ⅵ. 토픽간 상관 그래프'), 
        
        fluidRow(width=12,
                 box(width = 12, title=strong('[Ⅵ. 토픽간 상관 그래프] 진행 안내'), status = "primary",
                     solidHeader = FALSE,  collapsible = TRUE,
                     
                     p(HTML(paste0('토픽간 상관관계가 있을 때 네트워크 그래프를 그릴 수 있습니다',br(),br(),
                                   '1. 네트워크 그래프를 그릴 방법을 선택합니다',br(),br(),
                                   '2. cutoff를 선택한 경우, 슬라이드바를 이동하여 상관계수의 임계값을 조절합니다', br(), br(), 
                                   '3. Generate Plot을 클릭하여 그래프를 생성합니다')),style="font-family:'Noto Serif KR';
                          list-style-position: inside;color:black;font-weight:bold;
                   text-align:justify;font-size:16px;line-height:20px;")
        
        )), 
        fluidRow(width=12,
                 box(width = 12, title=strong('토픽간 상관 그래프'), status = "primary",
                     solidHeader = FALSE,  collapsible = FALSE,
        
      actionButton("graphsubmit", label = "Generate plot") ,
        plotOutput('graphplot',
                   height = "600px",
                   width = "80%"),
        downloadButton("download_graph", "Download plot")
        
      )), 
        
        value = 4
      ),



      tabPanel(
        strong('Ⅶ. 토픽 모델 정보 확인'),
        h2('Model Info'),
        p(
          "Parameters for the selected model and effect estimates are listed below."
        ),
        dataTableOutput('modelinfo'),

        h2('Model Diagnostics'),
        p(
          'Plot Semantic Coherence and Exclusivity for all models in your environment without a content variable.'
        ),
        actionButton("diagsubmit", label = "Generate plot") ,
        plotOutput("stm_diag", width = "70%"),
        downloadButton("download_diag", "Download plot"),
        h2('Topic Labels'),
        p(
          "If you assigned labels to your topics you can download a corresponding table here."
        ),
        dataTableOutput('labelframe'),
        downloadButton('downloadLabels', 'Download topic labels'),


        value = 2
      )
    )
  )))

)


#3. Server ------------------------------------------------------------------



server <- function(input, output, session) {
  # The environment needs to include three kinds of objects:
  # 1) one or multiple stm models
  # 2) an object"out" -> stm meta data generated with stm.prepDocuments
  # 3) one or multiple effect estimates for all topics generated with stm.estimateEffect


  options(shiny.maxRequestSize = 5000 * 1024 ^ 2)
  options(shiny.reactlog = TRUE)

  #### read and parse input file ####

  stm_data <- reactive({
    infile <- input$rfile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }

    load(infile$datapath, .GlobalEnv)
    rm(infile)

    stm_data <- list(
      model_names = NULL,
      effect_names = NULL,
      columns = NULL,
      models = NULL,
      effects = NULL,
      out = NULL
    )


    stm_data$model_names <-
      Filter(function(x)
        'STM' %in% class(get(x)), ls(envir = .GlobalEnv))

    stm_data$models <- purrr::map(stm_data$model_names, get)
    names(stm_data$models) <- stm_data$model_names
    stm_data$effect_names <-
      Filter(function(x)
        'estimateEffect' %in% class(get(x)),
        ls(envir =  .GlobalEnv))
    stm_data$effects <- purrr::map(stm_data$effect_names, get)
    names(stm_data$effects) <- stm_data$effect_names
    stm_data$out <- out
    stm_data$columns <- names(stm_data$out$meta)
    return(stm_data)
  })


  #### update ui inputs ####

  observeEvent(stm_data(), {
    output$doccol = renderUI({
      selectInput(
        "doccol",
        label = "④ 문서가 들어있는 열 이름 선택",
        choices = stm_data()$columns,
        multiple = TRUE,
        selected = stm_data()$columns[1]
      )
    })

    output$doccol2 = renderUI({
      selectInput(
        "doccol2",
        label = "확인하고자 하는 정보(열) 선택",
        choices = stm_data()$columns,
        multiple = TRUE,
        selected = stm_data()$columns[1]
      )
    })

    output$modelchoice = renderUI({
      selectInput("modelchoice",
                  label = p(HTML(paste0('② 모델을 선택해주세요',br(),
                                        '※ 모델이 하나인 경우, 선택하지 않으셔도 됩니다.')),
                            style="font-family:'Noto Serif KR';list-style-position: inside;
            color:black;text-align:justify;font-size:16px;line-height:20px;"),
                  choices = stm_data()$model_names)
    })
    output$effectschoice = renderUI({
      
      selectInput("effectschoice",
                  label = p(HTML(paste0('③ 메타 효과를 선택해주세요',br(),
                                        '※ 위에서 선택한 모델과 일치하도록 선택해주세요.')),
                            style="font-family:'Noto Serif KR';list-style-position: inside;
            color:black;text-align:justify;font-size:16px;line-height:20px;"),
                  choices = stm_data()$effect_names)
      
    })
    
    
  output$effectsexplain = renderUI({
    
             p(HTML(paste0('선택을 완료하신 경우, [Ⅱ. 토픽 기본 정보 확인] 탭으로 이동하신 뒤 30초정도 기다려주세요.')),
               style="font-family:'Noto Serif KR';list-style-position: inside;
            color:black;text-align:justify;font-size:16px;line-height:20px;font-weight:bold")
    
  })
    

  })

  observeEvent(input$rfile, {
    shinyjs::delay(800, shinyjs::hide('rfile'))
  }, priority = 10)


  observeEvent(input$plotType, {
    if (input$plotType %in% c('wordcloud', 'perspectives')) {
      shinyjs::hide('download_plot')
    }
    else {
      shinyjs::show('download_plot')
    }

  }, priority = 10)



  model <- reactive({
    req(stm_data())
    req(input$modelchoice)
    model <- stm_data()$models[[input$modelchoice]]

    return(model)
  })
  stm_effect_estimates <- reactive({
    req(stm_data()$effects)
    stm_effect_estimates <-
      stm_data()$effects[[input$effectschoice]]
    return(stm_effect_estimates)
  })

  props <-  reactive({
    req(model())
    props <- model()[['theta']]
    colnames(props) <- 1:ncol(props)
    return(props)
  })





  observeEvent(model(), {
    removeUI(
      selector = "div.form-group shiny-input-container",
      multiple = TRUE,
      immediate = TRUE,
      session = session
    )
  }, priority = -2)


  # Dynamically create text inputs for topic labels
  observeEvent(model(), {
    output$textInputs <- renderUI({
      w <- 1:ncol(props())
      uis <- vector("list", length(w))
      for (i in seq(w)) {
        if (nchar(as.character(i)) == 1) {
          uis[[i]] <-
            textInput(paste("TL", "0", i, sep = ""),
                      paste("Topic ", i, sep = ""),
                      value = as.character(paste0('0', w[i])))
        }
        else
          uis[[i]] <-
            textInput(paste("TL", i, sep = ""),
                      paste("Topic ", i, sep = ""),
                      value = as.character(w[i]))

      }
      return(uis)
    })
  }, priority = 5) # was -1 before

  # dynamically update topic labels

  tlabels <- reactive({
    req(model())
    req(!any(is.na(str_subset(
      names(input), '^TL'
    )[1:ncol(props())])))
    nr_topics <- ncol(props())
    tnames <- sort(str_subset(names(input), '^TL'))[1:nr_topics]


    topiclabels <-  list()
    for (i in tnames) {
      topiclabels[[i]] <- input[[i]]
    }
    to_return <- unlist(topiclabels, use.names = FALSE)

    return(to_return)

  })





  plot_pointestimate <- function(estobj,
                                 variable,
                                 topic,
                                 xlab = input$plotLabel,
                                 ylab = input$plotLabel2,
                                 ci = 0.95) {
    # function for plotting point estimates for estimated effects
    data <- plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = 'pointestimate',
      ci.level = ci,
      omit.plot = TRUE
    )
    means <- tibble(
      mean  = data$means[[1]],
      value = data$uvals,
      labels = data$labels
    )
    cis <- t(data$cis[[1]])
    colnames(cis) <- c('lower', 'upper')
    comb <- cbind(means, cis)
    comb$value <- as.factor(comb$value)

    plot <-
      ggplot(comb, aes(y = reorder(value, mean), x = mean)) + geom_point() +
      geom_line(aes(group = 1)) +  guides(fill = F,
                                          color = F,
                                          group = F)  +
      geom_errorbarh(aes(xmin = lower, xmax = upper),
                     height = 0.1) +
      scale_x_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) +
      labs(x = ylab , y  = xlab) + theme_bw(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      )
    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)
  }


  plot_continuous <- function(estobj,
                              variable,
                              topic,
                              xlab = input$plotLabel,
                              ylab = input$plotLabel2,
                              ci = 0.95) {
    # function for plotting estimated effects of continuous variables
    data <- plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = 'continuous',
      ci.level = ci,
      omit.plot = TRUE
    )
    means <- tibble(mean  = data$means[[1]], value = data$x)
    cis <- t(data$ci[[1]])
    colnames(cis) <- c('lower', 'upper')
    comb <- cbind(means, cis)
    comb$lower <- ifelse(comb$lower < 0, 0, comb$lower)
    comb$mean <- ifelse(comb$mean < 0, 0, comb$mean)
    #comb$value <- get_date(comb$value)

    plot <- ggplot(comb, aes(x = value, y = mean)) +
      geom_line(color = '#778899') +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  alpha = 0.2,
                  fill = '#778899') +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                         expand = c(0.00, 0)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0.00, 0),
        labels = scales::percent
      ) +
      guides(fill = F,
             color = F,
             group = F)  +
      coord_cartesian(ylim = c(-0.001, max(comb$upper))) +
      labs(x = xlab , y  = ylab) + theme_bw(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      )
    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)
  }


  plot_continuous_int <- function(estobj,
                                  variable,
                                  topic,
                                  xlab = input$plotLabel,
                                  ylab =  input$plotLabel2,
                                  ci = 0.95,
                                  modvar = modvar,
                                  modval1 =  modval1,
                                  modval2 =  modval2) {
    dat1 <-  plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = "continuous",
      moderator = modvar,
      moderator.value = modval1,
      ci.level = ci,
      omit.plot = TRUE,
      printlegend = FALSE
    )

    dat2 <-  plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = "continuous",
      moderator = modvar,
      moderator.value = modval2,
      ci.level = ci,
      omit.plot = TRUE,
      printlegend = FALSE
    )

    get_int_data <- function(data, label) {
      means <- tibble(mean  = data$means[[1]], value = data$x)
      cis <- t(data$ci[[1]])
      colnames(cis) <- c('lower', 'upper')
      comb <- cbind(means, cis)
      comb$lower <- ifelse(comb$lower < 0, 0, comb$lower)
      comb$mean <- ifelse(comb$mean < 0, 0, comb$mean)
      comb$Moderator <- label
      return(comb)

    }

    mod1 <- get_int_data(dat1, modval1)
    mod2 <- get_int_data(dat2, modval2)
    both <- bind_rows(mod1, mod2)

    plot <-
      ggplot(both, aes(x = value, y = mean, group = Moderator)) +
      geom_line(aes(color = Moderator)) +
      geom_ribbon(aes(
        ymin = lower,
        ymax = upper,
        fill = Moderator
      ),
      alpha = 0.2) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                         expand = c(0.00, 0)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0.00, 0),
        labels = scales::percent
      ) +
      guides(fill = FALSE, group = FALSE)  +
      coord_cartesian(ylim = c(-0.001, max(both$upper))) +
      theme_bw(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      )   +
      scale_fill_brewer(palette = 'Set1')  +
      scale_color_brewer(palette = 'Set1') +
      labs(x = xlab , y  = ylab)

    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)

  }



  plot_pointestimate_int <- function(estobj,
                                     variable,
                                     topic,
                                     xlab = input$plotLabel,
                                     ylab =  input$plotLabel2,
                                     ci = 0.95,
                                     modvar = modvar,
                                     modval1 =  modval1,
                                     modval2 =  modval2) {
    dat1 <-  plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = "pointestimate",
      moderator = modvar,
      moderator.value = modval1,
      ci.level = ci,
      omit.plot = TRUE,
      printlegend = FALSE
    )

    dat2 <-  plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = "pointestimate",
      moderator = modvar,
      moderator.value = modval2,
      ci.level = ci,
      omit.plot = TRUE,
      printlegend = FALSE
    )

    get_int_data <- function(data, label) {
      means <- tibble(
        mean  = data$means[[1]],
        value = data$uvals,
        labels = data$labels
      )
      cis <- t(data$cis[[1]])
      colnames(cis) <- c('lower', 'upper')
      comb <- cbind(means, cis)
      comb$value <- as.factor(comb$value)
      comb$Moderator <- label
      return(comb)

    }

    mod1 <- get_int_data(dat1, modval1)
    mod2 <- get_int_data(dat2, modval2)
    both <- bind_rows(mod1, mod2)

    plot <-
      ggplot(both, aes(x = value, y = mean, group = Moderator)) +
      geom_point(aes(color = Moderator), size = 3) +
      geom_errorbar(
        aes(
          ymin = lower,
          ymax = upper,
          group = Moderator,
          color = Moderator,
          linetype = Moderator
        ),
        alpha = 0.5,
        width = 0.1,
        size = 1
      ) +
      #  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), expand = c(0.00, 0)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8),
                         labels = scales::percent) +
      guides(fill = FALSE, group = FALSE)  +
      #     coord_cartesian(ylim = c(-0.001, max(both$upper))) +
      theme_bw(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      )   +
      scale_fill_brewer(palette = 'Set1')  +
      scale_color_brewer(palette = 'Set1') +
      labs(x = xlab , y  = ylab)

    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)

  }

  #### diagnostics graph ####


  get_diags <- function() {
    len_models <- length(stm_data()$models)

    df <- tibble(
      exclusivity = numeric(),
      coherence = numeric(),
      statistic = character(),
      topics = numeric()
    )
    withProgress(message = 'Diagnostic calculations in progress...', {
      for (model in seq_along(1:len_models)) {
        incProgress(1 / len_models)

        modelcall <- stm_data()$models[[model]]$settings$call[2:8]
        if ('content' %in% names(modelcall)) {
          next()
        }

        stm_obj <- stm_data()$models[[model]]
        obj_name <- stm_data()$model_names[[model]]

        exclusivity_mod <- exclusivity(stm_obj)
        coherence_mod <- semanticCoherence(stm_obj,  out$documents)
        nr_topics_m <- ncol(stm_obj$theta)

        model_df_mean <- tibble(
          model_name = obj_name,
          exclusivity = mean(exclusivity_mod),
          coherence = mean(coherence_mod),
          statistic = 'mean',
          topics = nr_topics_m
        )

        model_df_median <- tibble(
          model_name = obj_name,
          exclusivity = median(exclusivity_mod),
          coherence = median(coherence_mod),
          statistic = 'median',
          topics = nr_topics_m
        )

        df <- bind_rows(df, model_df_mean, model_df_median)

      }
    })
    df$topics <- as.factor(df$topics)

    dplot <- ggplot(df,
                    aes(
                      y = exclusivity,
                      x = coherence,
                      color = statistic,
                      shape = topics
                    )) +
      geom_point(size = 3) +  geom_text_repel(
        aes(
          x = coherence,
          label = model_name,
          size = 3
        ),
        show.legend = FALSE,
        box.padding = 0.5
      ) + theme_bw(base_size = 15) +
      labs(
        x = 'Semantic Coherence',
        y = 'Exclusivity',
        color = 'Measure',
        shape = 'Nr. topics'
      ) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      ) +
      scale_color_brewer(palette = 'Set1') +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

    # ggsave(
    #   "plot.png",
    #   dplot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )

    return(dplot)
  }

  plot_stm_diag <- eventReactive(input$diagsubmit, {
    get_diags()

  })




  output$stm_diag <- renderPlot({
    plot_stm_diag()
  })


  #### update plot input UI ####

  output$perspTopic2 <- renderUI({
    selectInput(
      "perspTopic2",
      label = "Perspectives - Topic 2",
      choices = tlabels(),
      selected = tlabels()[2]
    )
  })

  output$perspTopic1 <- renderUI({
    selectInput(
      "perspTopic1",
      label = "Perspectives - Topic 1",
      choices = tlabels(),
      selected = tlabels()[1]
    )
  })

  output$perspCat1 <- renderUI({
    selectInput(
      "perspCat1",
      label = "Content - Category 1",
      choices = model()$settings$covariates$yvarlevels,
      selected = model()$settings$covariates$yvarlevels[1]
    )
  })

  output$perspCat2 <- renderUI({
    selectInput(
      "perspCat2",
      label = "Content - Category 2",
      choices = model()$settings$covariates$yvarlevels,
      selected = model()$settings$covariates$yvarlevels[2]
    )
  })




  output$effectTopic <- renderUI({
    selectInput(
      "effectTopic",
      label = "Topic",
      choices = tlabels(),
      selected = tlabels()[1]
    )

  })
  output$plotVar <- renderUI({
    selectInput(
      "plotVar",
      label = "Plot variable",
      choices = stm_effect_estimates()$varlist,
      selected = stm_effect_estimates()$varlist[3]
    )
  })

  output$plotLabel <- renderUI({
    textInput("plotLabel",
              label = "Axis label",
              value = input$plotVar)
  })
  output$plotLabel2 <- renderUI({
    textInput("plotLabel2",
              label = "Axis label 2",
              value = 'Topic Proportion')
  })

  output$modvar <- renderUI({
    selectInput(
      "modvar",
      label = "Interaction variable",
      choices = stm_effect_estimates()$varlist,
      selected = stm_effect_estimates()$varlist[1]
    )
  })



  # moderator values for plots
  modvalues <- reactive({
    req(input$modvar)
    modvar <- out$meta[[input$modvar]]
    modvar <- as.factor(modvar)
    levels <- levels(modvar)

    return(levels)

  })
  #
  #### update plotting inputs ####

  observe({
    updateSelectInput(session,
                      "topic",
                      choices = tlabels(),
                      selected = tlabels()[1])
    updateSelectInput(session,
                      "topic_graph_1",
                      choices = tlabels(),
                      selected = tlabels()[1])
    updateSelectInput(session,
                      "topic_graph_2",
                      choices = tlabels(),
                      selected = tlabels()[2])
    #    updateSelectInput(session,
    #                      "topic_selected_condition",
    #                      choices = tlabels(),
    #                      selected = tlabels()[1])
    updateSelectInput(session,
                      "effectTopic",
                      choices = tlabels(),
                      selected = tlabels()[1])
    updateSelectInput(session,
                      "perspTopic1",
                      choices = tlabels(),
                      selected = tlabels()[1])
    updateSelectInput(session,
                      "perspTopic2",
                      choices = tlabels(),
                      selected = tlabels()[2])
    updateSelectInput(session,
                      "modval1",
                      choices = modvalues(),
                      selected = modvalues()[1])
    updateSelectInput(session,
                      "modval2",
                      choices = modvalues(),
                      selected = modvalues()[2])
  }, priority = -10)


  #### find topic documents ####

  topicDocs <- reactive({
    validate(
      need(!is.null(model()), "Loading, please wait.."),
      need(!any(is.na(
        str_subset(names(input), '^TL')[1:ncol(props())]
      )),
      "Loading, please wait.."),
      need(!is.null(tlabels()), "Loading, please wait.."),
      need(!is.null(stm_data()), "Loading, please wait.."),
      need(length(tlabels()) == ncol(props()), "Loading, please wait..")
    )

    t <- which(input$topic == tlabels())
    validate(need(length(t) > 0, "Loading, please wait.."))



    #nrt <- as.numeric(input$nrthoughts)
    # if(is.null(model())) {return(tibble(docs = ''))}
    #  else {
    thoughts <- reactive({
      thought_texts <- stm_data()$out$meta[[input$doccol[1]]] %>%
        as.character()

      findThoughts(
        model(),
        n = input$number_articles,
        # set to 100
        topics = c(t),
        texts = thought_texts,
        thresh = input$mintheta

      )

    })

    # slice and select meta data according to findThoughts indices
    topicIndices <- thoughts()$index[[1]]
    thoughtdf <- stm_data()$out$meta %>%
      slice(topicIndices) %>% select(input$doccol)

    if (2 %in% input$include_doc_theta) {
      thoughtdf <- cbind(seq.int(nrow(thoughtdf)), thoughtdf)
      colnames(thoughtdf)[1] <- "Row Index"
    }
    if (1 %in% input$include_doc_theta) {
      thoughtdf <- cbind(topicIndices, thoughtdf)
      colnames(thoughtdf)[1] <- "STM document ID"
    }
    if (3 %in% input$include_doc_theta) {
      thoughtdf$theta <- model()$theta[c(topicIndices), t]
    }

    #topicThoughts <- thoughts()$docs[[1]][1:100]
    #thoughtdf <- data.frame(topicThoughts, stringsAsFactors = FALSE)
    #names(thoughtdf) <- " "

    return(thoughtdf)
    # }


  })

  #### Find topic terms ####

  topicTerms <- reactive({
    req(stm_data())
    req(model())
    req(input$topic)
    req(tlabels())
    req(!any(is.na(str_subset(
      names(input), '^TL'
    )[1:ncol(props())]))
    , cancelOutput = TRUE)

    t <- which(input$topic == tlabels())
    nrterms <- as.numeric(input$nrwords)
    labels <- labelTopics(model(), n = nrterms)

    labList <- list()


    if ("content" %in% modelcall()$Attribute) {
      labels <- sageLabels(model(), n = nrterms)
      contentList <- list()

      if (1 %in% input$labtypes) {
        prob <- str_c(labels$marginal$prob[t, ], collapse = ', ')
        labList$Probability <- prob

        names(labels$cov.betas) <-
          str_c('Prob_', labels$covnames)
        contentList <-
          c(contentList,
            labels$cov.betas %>% purrr::map(function (x) {
              x$problabels[t, ] %>% str_c(collapse = ', ')
            }))

      }
      if (2 %in% input$labtypes) {
        frex <- str_c(labels$marginal$frex[t, ], collapse = ', ')
        labList$FREX <- frex

        names(labels$cov.betas) <-
          str_c('FREX_', labels$covnames)
        contentList <-
          c(contentList,
            labels$cov.betas %>% purrr::map(function (x) {
              x$frexlabels[t, ] %>% str_c(collapse = ', ')
            }))
      }

      if (3 %in% input$labtypes) {
        lift <- str_c(labels$marginal$lift[t, ], collapse = ', ')
        labList$Lift <- lift

        names(labels$cov.betas) <-
          str_c('Lift_', labels$covnames)
        contentList <-
          c(contentList,
            labels$cov.betas %>% purrr::map(function (x) {
              x$liftlabels[t, ] %>% str_c(collapse = ', ')
            }))
      }

      if (4 %in% input$labtypes) {
        score <- str_c(labels$marginal$score[t, ], collapse = ', ')
        labList$Score <- score

        names(labels$cov.betas) <-
          str_c('Score_', labels$covnames)
        contentList <-
          c(contentList,
            labels$cov.betas %>% purrr::map(function (x) {
              x$scorelabels[t, ] %>% str_c(collapse = ', ')
            }))
      }



      labList <- c(labList, contentList)

      labDf <- data.frame(labList)
      return(labDf)

    }

    else{
      if (1 %in% input$labtypes) {
        prob <- str_c(labels$prob[t, ][1:nrterms], collapse = ', ')
        labList$Probability <- prob

      }
      if (2 %in% input$labtypes) {
        frex <- str_c(labels$frex[t, ][1:nrterms], collapse = ', ')
        labList$FREX <- frex
      }

      if (3 %in% input$labtypes) {
        lift <- str_c(labels$lift[t, ][1:nrterms], collapse = ', ')
        labList$Lift <- lift
      }

      if (4 %in% input$labtypes) {
        score <- str_c(labels$score[t, ][1:nrterms], collapse = ', ')
        labList$Score <- score
      }

      labDf <- data.frame(labList)


      return(labDf)

    }


  })

  #### dataframes for docs and terms ####

  # Dataframe for topic docs
  output$tlabel <- renderDataTable(
    topicDocs(),
    # escape = FALSE,
    options = list(
      pageLength = 1,
      searching = TRUE,
      autoWidth = TRUE,
      scrollX = TRUE,
      info = FALSE,
      lengthMenu = c(5, 10, 50, 100),
      lengthChange = T,
      dom  = '<"top">iplrt<"bottom">'
    )
  )

  # Dataframe for topic terms
  output$tterms <- renderDataTable(
    topicTerms(),
    options = list(
      pageLength = 1,
      searching = FALSE,
      autoWidth = TRUE,
      scrollX = TRUE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    )
  )

  #### Topic proportions plot  ####

  plotTopicProps <- function(proportions) {
    req(input$modelchoice)
    req(stm_data())
    if (length(tlabels()) > 0) {
      names_ <- tlabels()
    }
    else {
      names_ <- colnames(proportions)
    }
    frequency <- colMeans(proportions)
    order <- order(frequency, decreasing = F)
    percentage <- frequency[order]
    names_ <- names_[order]
    topic <- factor(names_, levels = names_)
    combined <- data.frame(percentage, topic)
    p <- ggplot(combined, aes(x = topic, y = percentage)) +
      geom_bar(stat = "identity",
               fill = "#377eb8",
               color = "#000000") +
      coord_flip() + scale_y_continuous(labels = scales::percent,
                                        expand = c(0, 0)) +
      labs(y = "Proportion", x = "Topic") +
      theme_light(base_size = 14) +
      theme(axis.text = element_text(size = 14),
            panel.grid.major.y = element_blank())

    # ggsave("plot.png",
    #        p,
    #        dpi = 300,
    #        width = 9,
    #        height = 6)
    return(p)
  }

  #### Topic proportions plot  ####

  plotTopicPropsPerDoc <- function(proportions) {
    req(input$modelchoice)
    req(stm_data())
    if (length(tlabels()) > 0) {
      names_ <- tlabels()
    }
    else {
      names_ <- colnames(proportions)
    }
    frequency <- (proportions[input$articleID, ])
    order <- order(frequency, decreasing = F)
    percentage <- frequency[order]
    names_ <- names_[order]
    topic <- factor(names_, levels = names_)
    combined <- data.frame(percentage, topic)
    p <- ggplot(combined, aes(x = topic, y = percentage)) +
      geom_bar(stat = "identity",
               fill = "#377eb8",
               color = "#000000") +
      coord_flip() + scale_y_continuous(labels = scales::percent,
                                        expand = c(0, 0)) +
      labs(y = "Proportion", x = "Topic") +
      theme_light(base_size = 14) +
      theme(axis.text = element_text(size = 14),
            panel.grid.major.y = element_blank())

    # ggsave("plot.png",
    #        p,
    #        dpi = 300,
    #        width = 9,
    #        height = 6)
    return(p)
  }

  plotScatterDoc <- function(df_scatter, t1, t2) {
    req(input$modelchoice)
    req(stm_data())
    req(tlabels())

    p <-
      ggplot(df_scatter, aes_string(x = names(df_scatter)[1], y = names(df_scatter)[2])) +
      geom_point() +
      xlim(0, 1) +
      ylim(0, 1) +
      xlab(paste0("Topic ", t1)) +
      ylab(paste0("Topic ", t2)) +
      theme_bw()
    return(p)
  }




  output$topicprops <- renderPlot({
    plotTopicProps(props())
  })

  output$topicpropsperdoc <- renderPlot({
    plotTopicPropsPerDoc(props())
  })


  output$doc_topic_scatter <- renderPlot({
    t1 <- which(input$topic_graph_1 == tlabels())
    t2 <- which(input$topic_graph_2 == tlabels())

    df_scatter <- data.frame(model()$theta)[, c(t1, t2)]
    plotScatterDoc(df_scatter, t1, t2)
  })

  output$click_info <- renderDataTable({
    t1 <- which(input$topic_graph_1 == tlabels())
    t2 <- which(input$topic_graph_2 == tlabels())

    df_scatter <- data.frame(model()$theta)[, c(t1, t2)]
    if (1 %in% input$include_doc_theta2) {
      df_scatter$STM_doc_ID <- 1:nrow(df_scatter)
    }
    df_scatterjoin <- stm_data()$out$meta %>% select(input$doccol2)
    df_scatter <- cbind(df_scatter, df_scatterjoin)

    nearclickdata <-
      nearPoints(df_scatter,
                 input$plot_click,
                 addDist = FALSE,
                 maxpoints = 1)
    if (1 %in% input$include_doc_theta2) {
      colnames(nearclickdata)[3] <- "STM document ID"
    }
    colnames(nearclickdata)[1:2] <-
      c(paste0("Topic ", t1), paste0("Topic ", t2))
    return(nearclickdata)

  },
  options = list(
    pageLength = 1,
    searching = FALSE,
    autoWidth = TRUE,
    scrollX = FALSE,
    lengthChange = FALSE,
    info = FALSE,
    paging = FALSE
  ))


  output$brush_info <- renderDataTable({
    t1 <- which(input$topic_graph_1 == tlabels())
    t2 <- which(input$topic_graph_2 == tlabels())

    df_scatter <- data.frame(model()$theta)[, c(t1, t2)]
    if (1 %in% input$include_doc_theta2) {
      df_scatter$STM_doc_ID <- 1:nrow(df_scatter)
    }
    df_scatterjoin <- stm_data()$out$meta %>% select(input$doccol2)
    df_scatter <- cbind(df_scatter, df_scatterjoin)
    nearbrushdata <- brushedPoints(df_scatter, input$plot_brush)
    nearbrushdata <-
      nearbrushdata[1:(min(nrow(
        brushedPoints(df_scatter, input$plot_brush)
      ), 50)), ]
    if (1 %in% input$include_doc_theta2) {
      colnames(nearbrushdata)[3] <- "STM document ID"
    }
    colnames(nearbrushdata)[1:2] <-
      c(paste0("Topic ", t1), paste0("Topic ", t2))
    return(nearbrushdata)

  },
  options = list(
    pageLength = 1,
    searching = FALSE,
    autoWidth = TRUE,
    scrollX = FALSE,
    lengthChange = FALSE,
    info = FALSE,
    paging = FALSE
  ))

  #### effectplot ####


  plot_effect_graph <- eventReactive(input$effectsubmit, {
    # inputs
    scalemax <- input$scalemax
    scalemin <- input$scalemin
    plotT <- which(input$effectTopic == tlabels())
    plotPers1 <- which(input$perspTopic1 == tlabels())
    plotPers2 <- which(input$perspTopic2 == tlabels())
    plotM <- input$plotType
    type <- input$plotType


    # differences (not implemented at the moment)
    # if (type == "difference") {
    #   covalue1 <- input$covar1
    #   covalue2 <- input$covar2
    # }
    # else {
    #   covalue1 <- NULL
    #   covalue2 <- NULL
    # }


    #### wordcloud ####
    if (type == "wordcloud") {
      # png(
      #   'plot.png',
      #   width = 9,
      #   height = 6,
      #   units = 'in',
      #   res = 120
      # )
      #
      # #  return(
      # cloud(
      #   model(),
      #   topic = plotT,
      #   random.order = FALSE,
      #   max.words = input$cloud_words,
      #   scale = c(scalemax, scalemin)
      # )
      # dev.off()


      cloud(
        model(),
        topic = plotT,
        random.order = FALSE,
        max.words = input$cloud_words,
        scale = c(scalemax, scalemin)
      )

    }

    #### pointestimate ####

    interaction <- reactive({
      input$moderator
    })

    if (interaction() == FALSE & type == "pointestimate") {
      return(
        plot_pointestimate(
          estobj = stm_effect_estimates(),
          variable = input$plotVar,
          topic = plotT,
          ci = input$effectci
        )
      )
    }

    if (interaction() == TRUE & type == "pointestimate") {
      modvar <- input$modvar
      modval1 <- input$modval1
      modval2 <- input$modval2


      return(
        plot_pointestimate_int(
          estobj = stm_effect_estimates(),
          variable = input$plotVar,
          topic = plotT,
          ci = input$effectci,
          modvar = modvar,
          modval1 =  modval1,
          modval2 =  modval2
        )
      )
    }

    #### perspectives ####
    if (type == "perspectives") {
      if (("content" %in% modelcall()$Attribute) &
          (plotPers1 == plotPers2)) {
        plabels <- model()$settings$covariates$yvarlevels

        # png(
        #   'plot.png',
        #   width = 8,
        #   height = 6,
        #   units = 'in',
        #   res = 150
        # )
        #
        #         plot(
        #           model(),
        #           topics = plotPers1,
        #           type = "perspectives",
        #           n = input$persp_words,
        #           main = plotPers1,
        #           covarlevels = c(input$perspCat1, input$perspCat2),
        #           plabels = c(input$perspCat1, input$perspCat2),
        #           text.cex = input$persp_cex
        #         )

        #dev.off()

        plot(
          model(),
          topics = plotPers1,
          type = "perspectives",
          n = input$persp_words,
          plabels = model()$settings$covariates$yvarlevels,
          text.cex = input$persp_cex
        )


      }
      else {
        # png(
        #   'plot.png',
        #   width = 8,
        #   height = 6,
        #   units = 'in',
        #   res = 150
        # )
        #
        # plot(
        #   model(),
        #   topics = c(plotPers1, plotPers2),
        #   type = "perspectives",
        #   plabels = c(input$perspTopic1, input$perspTopic2),
        #   n = input$persp_words,
        #   text.cex = input$persp_cex
        # )
        # dev.off()
        #


        p <-   plot(
          model(),
          topics = c(plotPers1, plotPers2),
          type = "perspectives",
          plabels = c(input$perspTopic1, input$perspTopic2),
          n = input$persp_words,
          text.cex = input$persp_cex
        )

      }
    }
    #

    #### continuous ####

    interaction <- reactive({
      input$moderator
    })

    if (interaction() == FALSE & type == "continuous") {
      return(
        plot_continuous(
          estobj = stm_effect_estimates(),
          variable = input$plotVar,
          topic = plotT,
          ci = input$effectci
        )
      )


    }


    ####  interaction  ####

    if (interaction() == TRUE & type == "continuous") {
      modvar <- input$modvar
      modval1 <- input$modval1
      modval2 <- input$modval2

      return(
        plot_continuous_int(
          estobj = stm_effect_estimates(),
          variable = input$plotVar,
          topic = plotT,
          ci = input$effectci,
          modvar = modvar,
          modval1 =  modval1,
          modval2 =  modval2
        )
      )
    }


  })

  output$effectplot <- renderPlot({
    withProgress(message = 'Creating graph..', value = 0,
                 {
                   incProgress(1)
                   plot_effect_graph()

                 })
  }, res = 90)



  #### correlation graph ####

  # Generating correlation graph

  calcGraph <-
    function(model, method,  cutoff = 0.05) {
      # calculate topic correlation graph
      if (method == "cutoff") {
        cormat <-
          topicCorr(model(), method = 'simple', cutoff = cutoff)$poscor
      }
      else {
        cormat <- topicCorr(model(), method = 'huge')$poscor
        if (!any(cormat != 0)) {
          print("There were no edges detected between the topics of this STM model.")
          return(NULL)

        }
      }


      g <-
        igraph::simplify(igraph::graph.adjacency(cormat, mode = 'undirected', weighted = TRUE))

      if (length(igraph::E(g)) == 0) {
        print(
          "There are no (sufficiently high) correlations between the topics of this STM model."
        )
        return(NULL)

      }
      igraph::V(g)$name <- tlabels()
      igraph::V(g)$props <- colMeans(model()$theta)

      return(g)
    }



  # graph <- reactive({
  #
  #   calcGraph(model(), input$graphmethod, input$cutoff)
  #
  # })



  plot_corr_graph <- eventReactive(input$graphsubmit, {
    graph <-  calcGraph(model(), input$graphmethod, input$cutoff)
    if (is.null(graph)) {
      return(NULL)
    }
    output <- plotGraph(graph,
                        labels = input$eLabels,
                        cutiso = input$cutiso)

    return(output)

  })



  output$graphplot <- renderPlot({
    withProgress(message = 'Creating graph..', value = 0,
                 {
                   incProgress(1)
                   plot_corr_graph()


                 })
  }, res = 90)


  output$stm_diag <- renderPlot({
    plot_stm_diag()
  })


  plotGraph <- function(g,  labels, cutiso) {
    # generate topic correlation graph

    #if (length(V(g)) == 0)
    #  return()

    toplot <- as_tbl_graph(g) %>%
      mutate(degree = centrality_degree(loops = FALSE)) %>%
      activate(edges) %>%
      filter(!edge_is_loop()) %>%
      mutate(weight = round(weight, 2),
             edge_label = '')


    if (labels == TRUE) {
      toplot <- toplot %>% activate(edges) %>%
        mutate(edge_label = as.character(weight))
    }

    if (cutiso == TRUE) {
      toplot <- toplot %>% activate(nodes) %>%
        filter(degree > 0)
    }



    plot <- toplot %>% ggraph(layout = 'fr') +
      geom_edge_link(
        aes(edge_width = weight, label =  edge_label),
        label_colour = '#fc8d62',
        edge_colour = '#377eb8',
        alpha = 0.5,
        label_size = 4,
        angle_calc = 'along',
        label_dodge = unit(3, "mm")
      ) +
      geom_node_point(size = 4, colour = 'black')  +
      geom_node_label(
        aes(label = name, size = props),
        colour = 'black',
        repel = TRUE,
        alpha = 0.85
      ) +
      theme_graph() +
      scale_size(range = c(2, 10), labels = scales::percent) +
      labs(size = 'Topic Proportion',
           edge_width = 'Topic Correlation') +
      scale_edge_width(range = c(0, 3))
    if (input$includelegend == FALSE) {
      plot <- plot +  guides(size = FALSE, edge_width = FALSE)
    }


    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)
  }



  ### model infos

  modelcall <- reactive({
    modelcall <- model()$settings$call[2:8]
    modelcall$estimateEffects <-
      gsub("  ", "", paste(format(stm_effect_estimates()$formula), collapse = ""))
    modelframe <-
      data.frame(names(modelcall), as.character(modelcall))
    names(modelframe) <- c("Attribute", "Value")

    modelframe <-
      modelframe %>%
      filter(!Attribute %in% c('documents', 'vocab', 'data', 'verbose')) %>%
      filter(Attribute != '')
    return(modelframe)
  })

  output$modelinfo <- renderDataTable(
    modelcall(),
    options = list(
      pageLength = 1,
      searching = FALSE,
      autoWidth = TRUE,
      scrollX = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    )
  )

  #### label dataframe ####

  labelframe <- reactive({
    labels <- tlabels()
    ids <- seq_along(labels)
    frequency <- round(colMeans(props()), 3)
    return(data.frame(
      Topic = ids,
      Label = labels,
      Proportion = frequency
    ))

  })


  output$labelframe <- renderDataTable(
    labelframe(),
    options = list(
      pageLength = 1,
      searching = FALSE,
      autoWidth = TRUE,
      scrollX = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    )
  )

  #### download handlers ####

  output$downloadLabels <- downloadHandler(
    filename = function() {
      'stmInsights_topiclabels.tsv'
    },
    content = function(file) {
      readr::write_tsv(labelframe(), file)

    }
  )

  output$download_plot <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 9,
          height = 6)
      print(plot_effect_graph())
      dev.off()
    },
    contentType = "image/png"
  )


  output$download_prop <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 9,
          height = 6)
      print(plotTopicProps(props()))
      dev.off()
    },
    contentType = "image/png"
  )


  output$download_scatter <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 9,
          height = 6)
      print({
        t1 <- which(input$topic_graph_1 == tlabels())
        t2 <- which(input$topic_graph_2 == tlabels())

        df_scatter <- data.frame(model()$theta)[, c(t1, t2)]
        plotScatterDoc(df_scatter, t1, t2)
      })
      dev.off()
    },
    contentType = "image/png"
  )


  output$download_prop_docs <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 9,
          height = 6)
      print(plotTopicPropsPerDoc(props()))
      dev.off()
    },
    contentType = "image/png"
  )




  output$download_graph <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 9,
          height = 6)
      print(plot_corr_graph())
      dev.off()
    },
    contentType = "image/png"
  )



  output$download_diag <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 9,
          height = 6)
      print(get_diags())
      dev.off()
    },
    contentType = "image/png"
  )



  output$picture <- renderImage({
    return(list(src = "img/manual01.png",contentType = "image/png",alt = "manual"))
  }, deleteFile = FALSE) 
  

  tags$head(tags$style(HTML('* {font-family: "Noto Serif KR"};')))

}



shinyApp(ui = ui, server = server)
