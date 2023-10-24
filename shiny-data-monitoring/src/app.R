library(shiny)
library(scales)
library(forcats)
library(tidyverse)
library(ggplot2)
library(DT)
library(shinythemes)
library(plotly)
library(ggthemes)
library(Hmisc)
library(kableExtra) # to create nice tables
library(DT) # to create interactive tables
library(here)
library(hablar)
library(lubridate)
library(janitor)
library(UpSetR)


m1m2ds_tbl <- read_csv(here("data","m1m2ds_tbl.csv")) %>% 
 as_tibble() %>% 
  mutate_all(as.factor)

cohort_score_data <-read_csv(here("data","cohort_score_data.csv")) %>% 
  mutate(form=as.factor(form)) %>% 
  mutate(cohort=as.factor(cohort)) %>%
  mutate(redcap_event_name=as.factor(redcap_event_name)) %>% 
  mutate(score = as.numeric(score))

freq_datam1m2_2 <- read_csv(here("data","freq_datam1m2_2.csv")) %>% 
  mutate(dataset=as.factor(dataset))


frfunc_table <- read_csv(here("data","frfunc_table.csv")) %>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group)) %>% 
  mutate(last_seen=as.factor(last_seen))

m2kfrfunc_table <- read_csv(here("data","m2kfrfunc_table.csv")) %>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group)) %>% 
  mutate(last_seen=as.factor(last_seen))

m1tcough1_notes <- read_csv(here("data","m1tcough1_notes.csv"))  %>% 
  mutate(redcap_data_access_group = as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

cough1_notes <- read_csv(here("data","cough1_notes.csv"))  %>% 
  mutate(redcap_data_access_group = as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

tr_final_image <- read_csv(here("data","tr_final_image.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group)) %>% 
  mutate(QST_test=as.factor(QST_test))%>% 
  mutate(last_seen=as.factor(last_seen))

m2tr_final_image <- read_csv(here("data","m2tr_final_image.csv")) %>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group)) %>% 
  mutate(QST_test=as.factor(QST_test))%>% 
  mutate(last_seen=as.factor(last_seen))

m2ktr_final_image <- read_csv(here("data","m2ktr_final_image.csv")) %>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group)) %>% 
  mutate(QST_test=as.factor(QST_test))%>% 
  mutate(last_seen=as.factor(last_seen))

m1ttr_final_image <- read_csv(here("data","m1ttr_final_image.csv")) %>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group)) %>% 
  mutate(QST_test=as.factor(QST_test))%>% 
  mutate(last_seen=as.factor(last_seen))

brflag_comments <- read_csv(here("data","brflag_comments.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

m2kbrflag_comments <- read_csv(here("data","m2kbrflag_comments.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

m1tbrflag_comments <- read_csv(here("data","m1tbrflag_comments.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

m2brflag_comments <- read_csv(here("data","m2brflag_comments.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

irerror1_2_3_4_5_6_7_8_notes <- read_csv(here("data","irerror1_2_3_4_5_6_7_8_notes.csv")) %>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

m2kirerror1_2_3_4_5_6_7_8_notes <- read_csv(here("data","m2kirerror1_2_3_4_5_6_7_8_notes.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

m1tirerror1_2_3_4_5_6_7_8_notes <- read_csv(here("data","m1tirerror1_2_3_4_5_6_7_8_notes.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group)) %>% 
  mutate(last_seen=as.factor(last_seen))


m2irerror1_2_3_4_5_6_7_8_notes <- read_csv(here("data","m2irerror1_2_3_4_5_6_7_8_notes.csv"))%>% 
  mutate(redcap_data_access_group=as.factor(redcap_data_access_group))%>% 
  mutate(last_seen=as.factor(last_seen))

m1_biomarker_35upsetplot <- read_csv(here("data","m1_biomarker_35upsetplot.csv")) %>% 
  as.data.frame()


m1_biomarker_7moupsetplot <- read_csv(here("data","m1_biomarker_7moupsetplot.csv")) %>% 
  as.data.frame()


m2_biomarker_35upsetplot <- read_csv(here("data","m2_biomarker_35upsetplot.csv")) %>% 
  as.data.frame()


m2_biomarker_7moupsetplot <- read_csv(here("data","m2_biomarker_7moupsetplot.csv")) %>% 
  as.data.frame()

m2k_biomarker_35upsetplot <- read_csv(here("data","m2k_biomarker_35upsetplot.csv")) %>% 
  as.data.frame()

m2k_biomarker_7moupsetplot <- read_csv(here("data","m2k_biomarker_7moupsetplot.csv")) %>% 
  as.data.frame()




# Define UI 
ui <- navbarPage(theme = shinytheme("superhero"),title = "A2CPSpy",
                 
                 
                 tabPanel(title = "Explore form completion status", 
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "inputVAR", 
                                          label = "select CRF/Survey:",
                                          choices = levels(m1m2ds_tbl$var),
                                          selected = "Imaging",multiple = FALSE),
                              
                              selectInput(inputId = "inputTime", 
                                          label = "select event:",
                                          choices = levels(m1m2ds_tbl$redcap_event_name),
                                          selected = "baseline_visit_arm_1", multiple = TRUE),
                              
                              selectInput(inputId = "inputLocation", 
                                          label = "select DAG:",
                                          choices = levels(m1m2ds_tbl$redcap_data_access_group), 
                                          selected = "rush_university_me", multiple = TRUE),
                              
                
                              
                              h5("Once the forms are filled, they are marked either as complete, incomplete or left blank. Completed forms were checked for errors which resulted in: 1. Complete forms without errors, 2. Complete forms with errors. 
                              The frequencies are calculated as follows: type of completion status/complete forms without errors + complete with errors + incomplete forms
             + forms with missing completion status. Not applicable such as 6 weeks visit for imaging was excluded from calculation. If certain data does not exist for example if there is no incomplete data, then the  quadrant for incomplete forms will not show in the display")
                              
                            ),
                            
                            
                            
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              tags$style(type="text/css", # Sanitize the output so that it is not rendered instead of showing warnings/errors
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"),
                              tabsetPanel(
                                tabPanel("Plots by levels of data completion", 
                                         splitLayout(style = "border: 1px solid silver;",cellWidths = c("50%", "50%"),
                                                     plotlyOutput("plot_complete"),
                                                     plotlyOutput("plot_errors")),
                                         splitLayout(style = "border: 2px solid silver;",cellWidths = c("50%", "50%"),
                                                     plotlyOutput("plot_incomplete"),
                                                     plotlyOutput("plot_notavailable"))) 
                                
                          
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel(
                   title = "UpSet plots to show completed forms without errors",
                   fluidRow(
                     column(width = 6,offset = 0,textOutput("text30m1"),
                            plotOutput("plot30m1")
                            
                     ),
                     column(width = 6,offset = 0,textOutput("text7m1"),
                            plotOutput("plot7m1")
                            
                     )
                   ),
                   fluidRow(
                     column(width = 6,offset = 0,textOutput("text30m2"),
                            plotOutput("plot30m2")
                            
                     ),
                     column(width = 6,offset = 0,textOutput("text7m2"),
                            plotOutput("plot7m2")
                            
                     )
                   ),
                   fluidRow(
                     column(width = 6,offset = 0,textOutput("text30m2k"),
                            plotOutput("plot30m2k")
                            
                     ),
                     column(width = 6,offset = 0,textOutput("text7m2k"),
                            plotOutput("plot7m2k")
                            
                     )
                   )
                 ),
                 
                 
                 tabPanel(title = "Biomarker score distribution by surgical cohort (Baseline Visit)", 
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              selectInput(inputId = "cohort_form", 
                                          label = "select form:",
                                          choices = levels(cohort_score_data$form),
                                          selected = "BPI",multiple = FALSE),
                              
              
                              
                            ),
                            mainPanel(
                              "Overlay Plot", 
                              plotlyOutput(outputId = "cohort_overlay"),
                              "Violin Plot",
                              plotlyOutput(outputId = "cohort_violin")
                              
                            )
                          )
                 ),
              
                 tabPanel(title = "Comparison of errors across MCC sites", 
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "mdag_error", 
                                          label = "select CRF/Survey:",
                                          choices = levels(freq_datam1m2_2$dataset)),
                              
                              
                
                              
                              h5("Counts of each error across DAGs")
                              
                            ),
                            
                            
                            
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              tags$style(type="text/css", # Sanitize the output so that it is not rendered instead of showing warnings/errors
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"),
                              tabsetPanel(
                                tabPanel("Plots", 
                                         splitLayout(cellArgs = list(style = "padding: 6px"),cellWidths = c("33%","33%","33%"),
                                                     plotlyOutput("plot_ns_comp"),
                                                     plotlyOutput("plot_chic_comp"),
                                                     plotlyOutput("plot_rush_comp")),
                                         splitLayout(style = "border: 1px grey;",cellWidths = c("50%","50%"),
                                                     plotlyOutput("plot_mich_comp"),
                                                     plotlyOutput("plot_sp_comp")),
                                         splitLayout(cellArgs = list(style = "padding: 6px"),cellWidths = c("50%","50%"),
                                                     plotlyOutput("plot_m2kmich_comp"),
                                                     plotlyOutput("plot_m2ksp_comp")))
                                
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel(title = "MCC1 TKA Reports (Baseline Visit)", 
                          sidebarLayout(
                            sidebarPanel(
                              conditionalPanel(condition = "input.tabselected==1",
                                               
                                               
                                               selectInput(inputId = "iLocation_img", 
                                                           label = "select DAG:",
                                                           choices = levels(irerror1_2_3_4_5_6_7_8_notes$redcap_data_access_group)),
                                               
                                               selectInput(inputId = "img_last_seen", 
                                                           label = "Last seen:",
                                                           choices = levels(irerror1_2_3_4_5_6_7_8_notes$last_seen))
                                               
                                               
                              ),
                              conditionalPanel(condition = "input.tabselected==2",
                                               
                                               selectInput(inputId = "iLocation_func", 
                                                           label = "select DAG:",
                                                           choices = levels(frfunc_table$redcap_data_access_group)),
                                               selectInput(inputId = "func_last_seen", 
                                                           label = "Last seen:",
                                                           choices = levels(frfunc_table$last_seen))
                                               
                              ),
                              conditionalPanel(condition = "input.tabselected==3",
                                               
                                               selectInput(inputId = "iLocation_qst", 
                                                           label = "select DAG:",
                                                           choices = levels(tr_final_image$redcap_data_access_group)),
                                               
                                               selectInput(inputId = "iqst_test", 
                                                           label = "select QST Test:",
                                                           choices = levels(tr_final_image$QST_test)),
                                               
                                               selectInput(inputId = "qst_last_seen", 
                                                           label = "Last seen:",
                                                           choices = levels(tr_final_image$last_seen))
                                               
                                               
                                               
                              ),
                              conditionalPanel(condition = "input.tabselected==4",
                                               
                                               selectInput(inputId = "iLocation_bld", 
                                                           label = "select DAG:",
                                                           choices = levels(brflag_comments$redcap_data_access_group)),
                                               selectInput(inputId = "bld_last_seen", 
                                                           label = "Last seen",
                                                           choices = levels(brflag_comments$last_seen))
                                               
                              )
                            ),
                            mainPanel(
                              tabsetPanel(id = "tabselected",
                                          tabPanel("MCC1 Functional",value = 2, DTOutput(outputId = "func")) ,
                                          tabPanel("MCC1 Imaging",value = 1, DTOutput(outputId = "img")), 
                                          tabPanel("MCC1 QST",value = 3, DTOutput(outputId = "qst")),
                                          tabPanel("MCC1 Blood Draw",value = 4, DTOutput(outputId = "bld"))
                              )
                            ))),
                 
                 
                 tabPanel(title = "MCC2 TKA Reports (Baseline Visit)", 
                          sidebarLayout(
                            sidebarPanel(
                              conditionalPanel(condition = "input.tabselectedm2k==1",
                                               
                                               
                                               selectInput(inputId = "iLocation_imgm2k", 
                                                           label = "select DAG:",
                                                           choices = levels(m2kirerror1_2_3_4_5_6_7_8_notes$redcap_data_access_group)),
                                               
                                               selectInput(inputId = "img_last_seenm2k", 
                                                           label = "Last seen:",
                                                           choices = levels(m2kirerror1_2_3_4_5_6_7_8_notes$last_seen))
                                               
                                               
                              ),
                              conditionalPanel(condition = "input.tabselectedm2k==2",
                                               
                                               selectInput(inputId = "iLocation_funcm2k", 
                                                           label = "select DAG:",
                                                           choices = levels(m2kfrfunc_table$redcap_data_access_group)),
                                               selectInput(inputId = "func_last_seenm2k", 
                                                           label = "Last seen:",
                                                           choices = levels(m2kfrfunc_table$last_seen))
                                               
                              ),
                              conditionalPanel(condition = "input.tabselectedm2k==3",
                                               
                                               selectInput(inputId = "iLocation_qstm2k", 
                                                           label = "select DAG:",
                                                           choices = levels(m2ktr_final_image$redcap_data_access_group)),
                                               
                                               selectInput(inputId = "iqst_testm2k", 
                                                           label = "select QST Test:",
                                                           choices = levels(m2ktr_final_image$QST_test)),
                                               
                                               selectInput(inputId = "qst_last_seenm2k", 
                                                           label = "Last seen:",
                                                           choices = levels(m2ktr_final_image$last_seen))
                                               
                                               
                                               
                              ),
                              conditionalPanel(condition = "input.tabselectedm2k==4",
                                               
                                               selectInput(inputId = "iLocation_bldm2k", 
                                                           label = "select DAG:",
                                                           choices = levels(m2kbrflag_comments$redcap_data_access_group)),
                                               selectInput(inputId = "bld_last_seenm2k", 
                                                           label = "Last seen",
                                                           choices = levels(m2kbrflag_comments$last_seen))
                                               
                              )
                            ),
                            mainPanel(
                              tabsetPanel(id = "tabselectedm2k",
                                          tabPanel("MCC2 TKA Functional",value = 2, DTOutput(outputId = "funcm2k")) ,
                                          tabPanel("MCC2 TKA Imaging",value = 1, DTOutput(outputId = "imgm2k")), 
                                          tabPanel("MCC2 TKA QST",value = 3, DTOutput(outputId = "qstm2k")),
                                          tabPanel("MCC2 TKA Blood Draw",value = 4, DTOutput(outputId = "bldm2k"))
                              )
                            ))),
                 
                 
                 
                 tabPanel(title = "MCC1 Thoracic Reports (Baseline Visit)", 
                          sidebarLayout(
                            sidebarPanel(
                              conditionalPanel(condition = "input.tabselectedm1t==1",
                                               
                                               
                                               selectInput(inputId = "iLocation_imgm1t", 
                                                           label = "select DAG:",
                                                           choices = levels(m1tirerror1_2_3_4_5_6_7_8_notes$redcap_data_access_group)),
                                               selectInput(inputId = "img_lastseenm1t", 
                                                           label = "Last seen",
                                                           choices = levels(m1tirerror1_2_3_4_5_6_7_8_notes$last_seen))
                                               
                                               
                                               
                              ),
                              conditionalPanel(condition = "input.tabselectedm1t==2",
                                               
                                               selectInput(inputId = "iLocation_funcm1t", 
                                                           label = "select DAG:",
                                                           choices = levels(m1tcough1_notes$redcap_data_access_group)),
                                               selectInput(inputId = "func_lastseenm1t", 
                                                           label = "Last seen",
                                                           choices = levels(m1tcough1_notes$last_seen))
                                               
                              ),
                              
                              
                              conditionalPanel(condition = "input.tabselectedm1t==3",
                                               
                                               selectInput(inputId = "iLocation_qstm1t", 
                                                           label = "select DAG:",
                                                           choices = levels(m1ttr_final_image$redcap_data_access_group)),
                                               
                                               selectInput(inputId = "iqst_testm1t", 
                                                           label = "select QST Test:",
                                                           choices = levels(m1ttr_final_image$QST_test)),
                                               
                                               selectInput(inputId = "qst_lastseenm1t", 
                                                           label = "Last seen",
                                                           choices = levels(m1ttr_final_image$last_seen))
                                               
                              ),
                              conditionalPanel(condition = "input.tabselectedm1t==4",
                                               
                                               selectInput(inputId = "iLocation_bldm1t", 
                                                           label = "select DAG:",
                                                           choices = levels(m1tbrflag_comments$redcap_data_access_group)),
                                               selectInput(inputId = "bld_lastseenm1t", 
                                                           label = "Last seen",
                                                           choices = levels(m1tbrflag_comments$last_seen))
                                               
                              )
                            ),
                            mainPanel(
                              tabsetPanel(id = "tabselectedm1t",
                                          tabPanel("MCC1 Thoracic Imaging",value = 1, DTOutput(outputId = "imgm1t")),
                                          tabPanel("MCC1 Thoracic Functional",value = 2, DTOutput(outputId = "funcm1t")) ,
                                          tabPanel("MCC1 Thoracic QST",value = 3, DTOutput(outputId = "qstm1t")),
                                          tabPanel("MCC1 Thoracic Blood Draw",value = 4, DTOutput(outputId = "bldm1t"))
                              )
                            ))),
                 
                 tabPanel(title = "MCC2 Thoracic Reports (Baseline Visit)", 
                          sidebarLayout(
                            sidebarPanel(
                              conditionalPanel(condition = "input.tabselectedm2==1",
                                               
                                               
                                               selectInput(inputId = "iLocation_imgm2", 
                                                           label = "select DAG:",
                                                           choices = levels(m2irerror1_2_3_4_5_6_7_8_notes$redcap_data_access_group)),
                                               selectInput(inputId = "imgm2_lastseen", 
                                                           label = "Last seen",
                                                           choices = levels(m2irerror1_2_3_4_5_6_7_8_notes$last_seen))
                                               
                                               
                                               
                              ),
                              conditionalPanel(condition = "input.tabselectedm2==2",
                                               
                                               selectInput(inputId = "iLocation_funcm2", 
                                                           label = "select DAG:",
                                                           choices = levels(cough1_notes$redcap_data_access_group)),
                                               selectInput(inputId = "funcm2_lastseen", 
                                                           label = "Last seen",
                                                           choices = levels(cough1_notes$last_seen))
                                               
                              ),
                              
                              
                              conditionalPanel(condition = "input.tabselectedm2==3",
                                               
                                               selectInput(inputId = "iLocation_qstm2", 
                                                           label = "select DAG:",
                                                           choices = levels(m2tr_final_image$redcap_data_access_group)),
                                               
                                               selectInput(inputId = "iqst_testm2", 
                                                           label = "select QST Test:",
                                                           choices = levels(m2tr_final_image$QST_test)),
                                               
                                               selectInput(inputId = "qstm2_lastseen", 
                                                           label = "Last seen",
                                                           choices = levels(m2tr_final_image$last_seen))
                                               
                              ),
                              conditionalPanel(condition = "input.tabselectedm2==4",
                                               
                                               selectInput(inputId = "iLocation_bldm2", 
                                                           label = "select DAG:",
                                                           choices = levels(m2brflag_comments$redcap_data_access_group)),
                                               selectInput(inputId = "bldm2_lastseen", 
                                                           label = "Last seen",
                                                           choices = levels(m2brflag_comments$last_seen))
                                               
                              )
                            ),
                            mainPanel(
                              tabsetPanel(id = "tabselectedm2",
                                          tabPanel("MCC2 Imaging",value = 1, DTOutput(outputId = "imgm2")),
                                          tabPanel("MCC2 Functional",value = 2, DTOutput(outputId = "funcm2")) ,
                                          tabPanel("MCC2 QST",value = 3, DTOutput(outputId = "qstm2")),
                                          tabPanel("MCC2 Blood Draw",value = 4, DTOutput(outputId = "bldm2"))
                              )
                            )))
                 
                 
)








# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$plot_complete <- renderPlotly({
    m1m2ds_tbl %>%
      filter(data_type != "dataNot_applicable") %>% 
      filter(var == input$inputVAR) %>% 
      filter(redcap_event_name %in%  input$inputTime) %>% 
      filter(redcap_data_access_group %in% input$inputLocation) %>% 
      group_by(redcap_data_access_group, redcap_event_name,data_type) %>%
      summarise(total=n()) %>% 
      group_by(redcap_data_access_group) %>% 
      pivot_wider(names_from = data_type,values_from = total) %>% 
      rowwise() %>%
      mutate(sum_all = sum(across(starts_with("data")), na.rm = T)) %>% 
      mutate(prop = datacomplete_with_no_errors/sum_all) %>% 
      ggplot(aes(x = redcap_data_access_group, y = prop, fill = redcap_event_name)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title="Plot showing % error free forms completed",
           x ="Forms completed without errors", y = "Percent") +
      scale_y_continuous(labels = percent)
  })
  
  output$plot_errors <-  renderPlotly({
    m1m2ds_tbl %>%
      filter(data_type != "dataNot_applicable") %>% 
      filter(var == input$inputVAR) %>% 
      filter(redcap_event_name %in%  input$inputTime) %>% 
      filter(redcap_data_access_group %in% input$inputLocation) %>% 
      group_by(redcap_data_access_group, redcap_event_name,data_type) %>%
      summarise(total=n()) %>% 
      group_by(redcap_data_access_group) %>% 
      pivot_wider(names_from = data_type,values_from = total) %>% 
      rowwise() %>%
      mutate(sum_all = sum(across(starts_with("data")), na.rm = T)) %>% 
      mutate(prop = datacomplete_with_errors/sum_all) %>% 
      ggplot(aes(x = redcap_data_access_group, y = prop, fill = redcap_event_name)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title="Plot showing % forms completed with errors",
           x ="Forms completed with errors", y = "Percent") +
      scale_y_continuous(labels = percent)
  })
  
  
  output$plot_incomplete <-  renderPlotly({
    m1m2ds_tbl %>%
      filter(data_type != "dataNot_applicable") %>% 
      filter(var == input$inputVAR) %>% 
      filter(redcap_event_name %in%  input$inputTime) %>% 
      filter(redcap_data_access_group %in% input$inputLocation) %>% 
      group_by(redcap_data_access_group, redcap_event_name,data_type) %>%
      summarise(total=n()) %>% 
      group_by(redcap_data_access_group) %>% 
      pivot_wider(names_from = data_type,values_from = total) %>% 
      rowwise() %>%
      mutate(sum_all = sum(across(starts_with("data")), na.rm = T)) %>% 
      mutate(prop = dataincomplete/sum_all) %>% 
      ggplot(aes(x = redcap_data_access_group, y = prop, fill = redcap_event_name)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title="Plot showing % incomplete forms ",
           x ="incomplete forms",  y = "Percent") +
      scale_y_continuous(labels = percent)
    
  })
  
  
  output$plot_notavailable <- renderPlotly({
    m1m2ds_tbl %>%
      filter(data_type != "dataNot_applicable") %>% 
      filter(var == input$inputVAR) %>% 
      filter(redcap_event_name %in%  input$inputTime) %>% 
      filter(redcap_data_access_group %in% input$inputLocation) %>% 
      group_by(redcap_data_access_group, redcap_event_name,data_type) %>%
      summarise(total=n()) %>% 
      group_by(redcap_data_access_group) %>% 
      pivot_wider(names_from = data_type,values_from = total) %>% 
      rowwise() %>%
      mutate(sum_all = sum(across(starts_with("data")), na.rm = T)) %>% 
      mutate(prop = datanot_available/sum_all) %>% 
      ggplot(aes(x = redcap_data_access_group, y = prop, fill = redcap_event_name)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title="Plot showing % forms with completion status 'Not Available'",
           x ="Status: Not Available", y = "Percent") +
      scale_y_continuous(labels = percent)
    
    
  })
  
  output$plot30m1 <- renderPlot({
    
    upset(m1_biomarker_35upsetplot, sets = c("KOOS","BPI", "Depression" ,  "Anxiety",  "Catastophizing" ,"FearMovement" ,"Sleep" , 
                                          "Sleep_hours" ,"Funtional","Blood_Draw" , "Imaging" , "QST" , "Cognition" ,"Resilience" ,"Social_Support" ,  "ACE","Baseline_Traj"),
          keep.order = TRUE, mb.ratio = c(0.55, 0.45), order.by = "freq")
  })
  
  output$text30m1 <- renderText({"MCC1 TKA < 35 days from Baseline visit"})

  output$plot7m1 <- renderPlot({
    
    upset(m1_biomarker_7moupsetplot, sets = c("KOOS","BPI", "Depression" ,  "Anxiety",  "Catastophizing" ,"FearMovement" ,"Sleep" , 
                                          "Sleep_hours" ,"Funtional","Blood_Draw" , "Imaging" , "QST" , "Cognition" ,"Resilience" ,"Social_Support" ,  "ACE" ,"Baseline_Traj", "6_months_Traj"),
          keep.order = TRUE, mb.ratio = c(0.55, 0.45), order.by = "freq")
  })
  
  output$text7m1 <- renderText({"MCC1 TKA > 7 months from Baseline visit"})
 
  output$plot30m2 <- renderPlot({
    
    upset(m2_biomarker_35upsetplot, sets = c("BPI", "Depression" ,  "Anxiety",  "Catastophizing" ,"FearMovement" ,"Sleep" , 
                                          "Sleep_hours" ,"Funtional","Blood_Draw" , "Imaging" , "QST" , "Cognition" ,"Resilience" ,"Social_Support" ,  "ACE","Baseline_Traj"),
          keep.order = TRUE, mb.ratio = c(0.55, 0.45), order.by = "freq")
  })
  
  output$text30m2 <- renderText({"MCC2 Thoracic < 35 days from Baseline visit"})
  
  output$plot7m2 <- renderPlot({
    
    upset(m2_biomarker_7moupsetplot, sets = c("BPI", "Depression" ,  "Anxiety",  "Catastophizing" ,"FearMovement" ,"Sleep" , 
                                             "Sleep_hours" ,"Funtional","Blood_Draw" , "Imaging" , "QST" , "Cognition" ,"Resilience" ,"Social_Support" ,  "ACE" ,"Baseline_Traj", "6_months_Traj"),
          keep.order = TRUE, mb.ratio = c(0.55, 0.45), order.by = "freq")
  })
  
  output$text7m2 <- renderText({"MCC2 Thoracic > 7 months from Baseline visit"})
  
  
  output$plot30m2k <- renderPlot({
    
    upset(m2k_biomarker_35upsetplot, sets = c("BPI", "Depression" ,  "Anxiety",  "Catastophizing" ,"FearMovement" ,"Sleep" , 
                                             "Sleep_hours" ,"Funtional","Blood_Draw" , "Imaging" , "QST" , "Cognition" ,"Resilience" ,"Social_Support" ,  "ACE","Baseline_Traj"),
          keep.order = TRUE, mb.ratio = c(0.55, 0.45), order.by = "freq")
  })
  
  output$text30m2k <- renderText({"MCC2 TKA < 35 days from Baseline visit"})
  
  output$plot7m2k <- renderPlot({
    
    upset(m2k_biomarker_7moupsetplot, sets = c("BPI", "Depression" ,  "Anxiety",  "Catastophizing" ,"FearMovement" ,"Sleep" , 
                                              "Sleep_hours" ,"Funtional","Blood_Draw" , "Imaging" , "QST" , "Cognition" ,"Resilience" ,"Social_Support" ,  "ACE" ,"Baseline_Traj", "6_months_Traj"),
          keep.order = TRUE, mb.ratio = c(0.55, 0.45), order.by = "freq")
  })
  
  output$text7m2k <- renderText({"MCC2 TKA > 7 months from Baseline visit"})
  

  
 
  new_tbl_react <- reactive({
    new_tbl %>% 
      filter(redcap_data_access_group == input$iLocation_newtbl & status %in% input$istatus_newtbl)
  })
  
  

  
  
### mcc1 knee reports 
  
  func_table_react <- reactive({
    frfunc_table %>% 
      filter(redcap_data_access_group == input$iLocation_func & last_seen == input$func_last_seen)
  })
  
  output$func <- renderDT({
    
    datatable(func_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    
    
  
  })
  
  image_table_react <- reactive({
    irerror1_2_3_4_5_6_7_8_notes %>% 
      filter(redcap_data_access_group == input$iLocation_img & last_seen == input$img_last_seen)
  })
  
  output$img <- renderDT({
    
    datatable(image_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    
    
  })
  
  qst_table_react <- reactive({
    tr_final_image  %>% 
      filter(redcap_data_access_group == input$iLocation_qst & QST_test == input$iqst_test  & last_seen == input$qst_last_seen)
  })
  
  output$qst <- renderDT({
    
    datatable(qst_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    
    
  })
  
  bld_table_react <- reactive({
    brflag_comments  %>% 
      filter(redcap_data_access_group == input$iLocation_bld  & last_seen == input$bld_last_seen)
  })
  
  output$bld <- renderDT({
    
    datatable(bld_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    })
 
 # mcc2 knee reports
    
  
 
  m2kfunc_table_react <- reactive({
    m2kfrfunc_table %>% 
      filter(redcap_data_access_group == input$iLocation_funcm2k & last_seen == input$func_last_seenm2k)
  })
  
  output$funcm2k <- renderDT({
    
    datatable(m2kfunc_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    
    
    
  })
  
  m2kimage_table_react <- reactive({
    m2kirerror1_2_3_4_5_6_7_8_notes %>% 
      filter(redcap_data_access_group == input$iLocation_imgm2k & last_seen == input$img_last_seenm2k)
  })
  
  output$imgm2k <- renderDT({
    
    datatable(m2kimage_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    
    
  })
  
  m2kqst_table_react <- reactive({
    m2ktr_final_image  %>% 
      filter(redcap_data_access_group == input$iLocation_qstm2k & QST_test == input$iqst_testm2k  & last_seen == input$qst_last_seenm2k)
  })
  
  output$qstm2k <- renderDT({
    
    datatable(m2kqst_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    
    
  })
  
  m2kbld_table_react <- reactive({
    m2kbrflag_comments  %>% 
      filter(redcap_data_access_group == input$iLocation_bldm2k  & last_seen == input$bld_last_seenm2k)
  })
  
  output$bldm2k <- renderDT({
    
    datatable(m2kbld_table_react(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
    
    
  })
  
  
  output$extra1 <- renderImage({list(src='www/screen.png',height=1100,width=1000)
  })
  
  
  
  output$plot_ns_comp <-  renderPlotly({
    freq_datam1m2_2 %>% 
      ungroup() %>% 
      filter(dataset == input$mdag_error) %>% 
      filter(redcap_data_access_group == "northshore") %>% 
      group_by(redcap_data_access_group,dataset,error) %>%
      ggplot(aes(x = error, y = total, fill = error)) +
      geom_bar(stat="identity") +
      labs(title="MCC1 TKA Northshore",
           x ="Errors", y = "Count") +
      theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5))
    
    
    
    
    
  })
  
  
  
  output$plot_chic_comp <-  renderPlotly({
    freq_datam1m2_2 %>% 
      ungroup() %>% 
      filter(dataset == input$mdag_error) %>% 
      filter(redcap_data_access_group == "uchicago") %>% 
      group_by(redcap_data_access_group,dataset,error) %>%
      ggplot(aes(x = error, y = total, fill = error)) +
      geom_bar(stat="identity") +
      labs(title=" MCC1 TKA Uchicago",
           x ="Errors", y = "Count") +
      theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5))
    
    
    
    
    
  })
  
  
  output$plot_rush_comp <-  renderPlotly({
    freq_datam1m2_2 %>% 
      ungroup() %>% 
      filter(dataset == input$mdag_error) %>% 
      filter(redcap_data_access_group == "rush_university_me") %>% 
      group_by(redcap_data_access_group,dataset,error) %>%
      ggplot(aes(x = error, y = total, fill = error)) +
      geom_bar(stat="identity") +
      labs(title="MCC1 TKA Rush University",
           x ="Errors", y = "Count") +
      theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5))
    
    
    
    
    
  })
  
  output$plot_sp_comp <-  renderPlotly({
    freq_datam1m2_2 %>% 
      ungroup() %>% 
      filter(dataset == input$mdag_error) %>% 
      filter(redcap_data_access_group == "spectrum_health") %>% 
      group_by(redcap_data_access_group,dataset,error) %>%
      ggplot(aes(x = error, y = total, fill = error)) +
      geom_bar(stat="identity") +
      labs(title="MCC2 Thoracotomy Spectrum Health",
           x ="Errors", y = "Count") +
      theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5))
    
    
    
    
    
  })
  
  output$plot_mich_comp <-  renderPlotly({
    freq_datam1m2_2 %>% 
      ungroup() %>% 
      filter(dataset == input$mdag_error) %>% 
      filter(redcap_data_access_group == "university_of_mich") %>% 
      group_by(redcap_data_access_group,dataset,error) %>%
      ggplot(aes(x = error, y = total, fill = error)) +
      geom_bar(stat="identity") +
      labs(title="MCC2 Thoracotomy UMich",
           x ="Errors", y = "Count") +
      theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5))
    
    
  })
  
  output$plot_m2kmich_comp <-  renderPlotly({
    freq_datam1m2_2 %>% 
      ungroup() %>% 
      filter(dataset == input$mdag_error) %>% 
      filter(redcap_data_access_group == "MCC2_TKA_university_of_mich") %>% 
      group_by(redcap_data_access_group,dataset,error) %>%
      ggplot(aes(x = error, y = total, fill = error)) +
      geom_bar(stat="identity") +
      labs(title="MCC2 TKA UMich",
           x ="Errors", y = "Count") +
      theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5))
    
    
    
  })
  
  


output$plot_m2ksp_comp <-  renderPlotly({
  freq_datam1m2_2 %>% 
    ungroup() %>% 
    filter(dataset == input$mdag_error) %>% 
    filter(redcap_data_access_group == "MCC2_TKA_spectrum_health") %>% 
    group_by(redcap_data_access_group,dataset,error) %>%
    ggplot(aes(x = error, y = total, fill = error)) +
    geom_bar(stat="identity") +
    labs(title="MCC2 TKA Spectrum Health",
         x ="Errors", y = "Count") +
    theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5))
  
 
})
  
 
  
  #mcc2 thoraci reports
  
  
  func_table_reactm2 <- reactive({
    cough1_notes %>% 
      filter(redcap_data_access_group == input$iLocation_funcm2 & last_seen == input$funcm2_lastseen)
  })
  
  output$funcm2 <- renderDT({
    
    datatable(func_table_reactm2(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  
  
  
  image_table_reactm2 <- reactive({
    m2irerror1_2_3_4_5_6_7_8_notes %>% 
      filter(redcap_data_access_group == input$iLocation_imgm2 & last_seen == input$imgm2_lastseen)
  })
  
  output$imgm2 <- renderDT({
    
    datatable(image_table_reactm2(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  
  
  
  bld_table_reactm2 <- reactive({
    m2brflag_comments %>% 
      filter(redcap_data_access_group == input$iLocation_bldm2 & last_seen == input$bldm2_lastseen)
  })
  
  output$bldm2 <- renderDT({
    
    datatable(bld_table_reactm2(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  
  
  dma_table_reactm2 <- reactive({
    dma_shrink_notes %>% 
      filter(redcap_data_access_group == input$iLocation_m2dma)
  })
  
  output$dmam2 <- renderDT({
    
    datatable(dma_table_reactm2(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  
  
  m2qst_table_reactm2 <- reactive({
    m2tr_final_image %>% 
      filter(redcap_data_access_group == input$iLocation_qstm2 & QST_test == input$iqst_testm2 & last_seen == input$qstm2_lastseen)
  })
  
  output$qstm2 <- renderDT({
    
    datatable(m2qst_table_reactm2(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })

  
  #mcc1 thoraci reports
  
  
  func_table_reactm1t <- reactive({
    m1tcough1_notes %>% 
      filter(redcap_data_access_group == input$iLocation_funcm1t & last_seen == input$func_lastseenm1t)
  })
  
  output$funcm1t <- renderDT({
    
    datatable(func_table_reactm1t(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  
  
  
  image_table_reactm1t <- reactive({
    m1tirerror1_2_3_4_5_6_7_8_notes %>% 
      filter(redcap_data_access_group == input$iLocation_imgm1t & last_seen == input$img_lastseenm1t)
  })
  
  output$imgm1t <- renderDT({
    
    datatable(image_table_reactm1t(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  
  
  
  bld_table_reactm1t <- reactive({
    m1tbrflag_comments %>% 
      filter(redcap_data_access_group == input$iLocation_bldm1t & last_seen == input$bld_lastseenm1t)
  })
  
  output$bldm1t <- renderDT({
    
    datatable(bld_table_reactm1t(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  

  
  qst_table_reactm1t <- reactive({
    m1ttr_final_image %>% 
      filter(redcap_data_access_group == input$iLocation_qstm1t & QST_test == input$iqst_testm1t & last_seen == input$qst_lastseenm1t)
  })
  
  output$qstm1t <- renderDT({
    
    datatable(qst_table_reactm1t(), style = "bootstrap",extensions = 'Buttons',filter = 'top',class = 'cell-border stripe', options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
      formatStyle(columns = colnames(.), fontSize = '10%')
    
  })
  
  
  
  

  
  m2new_tbl_react <- reactive({
    m2new_tbl %>% 
      filter(redcap_data_access_group == input$iLocation_m2newtbl & status %in% input$istatus_m2newtbl)
  })
  

  # overlay plots
  
  
  output$cohort_overlay <-  renderPlotly({
    cohort_score_data %>% 
      ungroup() %>% 
      filter(form == input$cohort_form) %>% 
      ggplot(aes(x=score, fill=cohort)) +
      geom_density(alpha=0.25)
    
    
    
    
  })
  
  
  output$cohort_violin <-  renderPlotly({
    cohort_score_data %>% 
      filter(form == input$cohort_form) %>% 
      ggplot(aes(x=cohort,y=score,fill=cohort)) +
      geom_violin(trim = FALSE) +
      stat_summary(fun.y ="median", geom="point", size=2, color="blue")
    
   
    
  })
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
