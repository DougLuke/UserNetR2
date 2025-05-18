#****************************************#
# Network Navigator                      #
# DEMO - Non-function version            #
# Bobbi Carothers, Douglas Luke          #
# 12/24/2024                             #
#****************************************#

# Preliminary Setup ####

# Libraries
library(DT) # Tooltips on table column headings
library(shiny)
library(igraph)
library(visNetwork)

# Question text
ConQ <- "On average, how often did you have direct professional contact (e.g.,meetings, phone calls, 
emails, faxes, or letters) with each of the following organizations in the past year? 
(Do not count listservs or mass emails.)"
ColQ <- "Please pick the response option that best represents the highest level to which [Organization] 
currently interacts with each of the following organizations."
ActQ <- "Please indicate which organizations you have collaborated with in the past year on the 
following activities. (Check all that apply.)"
RefQ <- "Did [Organization] send and/or receive referrals with the following organizations in the past 
year?"

# Repeated text
# Hover over node
HovNode <- "Hover over a node to see the commanding officer."
# Click on node
ClickNode <- "Click on a node to compare its ties to the network average."


# Begin ShinyUI ####

shinyUI(fluidPage(
  
  # Call CSS theme
  includeCSS("bootstrap.css"),
  # Additional css for the data tables
  tags$head(
    tags$style(type= "text/css",
               HTML("th { text-align: center !important; }"),
               HTML("td { text-align: center !important; }")
    )
  ),
  
  pageWithSidebar(
  
    # Overall title & logo
    headerPanel( list(
      tags$img(src='NN_Logo1.png', style='float:left;',
               width='380px', height='120px'),
      tags$br(h2('DEMO Network'))),
      windowTitle = 'NetNav: DEMO'),
    
    # Different sidebars depending on selected tab ####
    sidebarPanel(
      width=2,

      # Contact tab sidebar & options ####
      conditionalPanel(condition='input.condPanel == 2', 
                       tags$h3('Explore Contact'),
                       radioButtons('ContactLevel',
                                    label='Choose level of contact:',
                                    choices=list('At least yearly'=1,
                                                 'At least quarterly'=2,
                                                 'At least monthly'=3, 
                                                 'At least weekly'=4, 
                                                 'At least daily'=5
                                                 ),
                                   selected=1
                                   ),
                       radioButtons('ConNodeSize', 
                                    label='Size nodes by:',
                                    choices=list('Equally'=1, 
                                                 'Number of connections'=2,
                                                 'Ability to link unlinked organizations'=3
                                                 ),
                                    selected=1
                                    )
                       ),
      
      # Collaboration tab sidebar & options ####
      conditionalPanel(condition='input.condPanel == 3',
                       tags$h3('Explore Collaboration'),
                       radioButtons('CollabLevel', 
                                    label='Choose level of collaboration:',
                                   choices=list('At least exchange'=1, 
                                                'At least engage informally'=2,
                                                'At least engage formally'=3,
                                                'At least formally on multiple projects'=4
                                                ),
                                   selected=1
                                   ),
                       radioButtons('ColNodeSize', label='Size nodes by:',
                                    choices=list('Equally'=1, 
                                                 'Number of connections'=2, 
                                                 'Ability to link unlinked organizations'=3),
                                    selected=1
                                    )
                       ),
 
      # Activities tab sidebar & options ####
      conditionalPanel(condition='input.condPanel == 4',
                       tags$h3('Explore Activities'),
                       radioButtons('ActSelect', 
                                    label='Choose activities:',
                                    choices=list('Activity 1'='Act1',
                                                 'Activity 2'='Act2',
                                                 'Activity 3'='Act3',
                                                 'Activity 4'='Act4',
                                                 'Activity 5'='Act5',
                                                 'Activity 6'='Act6',
                                                 'Activity 7'='Act7'),
                                    selected='Act1'
                                    ),
                       radioButtons('ActNodeSize', 
                                    label='Size nodes by:',
                                    choices=list('Equally'=1, 
                                                 'Number of connections'=2,
                                                 'Ability to link unlinked organizations'=3),
                                    selected=1
                                    )
                       ),
      
      # Referrals tab sidebar & options ####
      conditionalPanel(condition='input.condPanel == 5',
                       tags$h3('Explore Referrals'),
                       radioButtons('RefNodeSize', 
                                    label='Size nodes by:',
                                    choices=list('Equally'=1, 
                                                 '# of organizations we send to'=2,
                                                 '# of organizations sending to us'=3),
                                    selected=1
                                    )
                       )
      
    ),
    
    # Output: main content for each tab ####
    mainPanel(
      width=10,
      conditionalPanel(condition='input.condPanel > 1',
                       absolutePanel(
                         top=175,left=175,width=600,
                         plotOutput('Legend1',height="60px")
                       )
      ),
      # Tabs
      tabsetPanel(id='condPanel',
                  tabPanel('Contact',
                           value=2,
                           wellPanel(tags$h4('Question Text:'),
                                     tags$p(ConQ)),
                           fluidRow(column(width=9,
                                          visNetworkOutput('ConNet',width='100%',height=600),
                                          conditionalPanel(
                                            condition="!input.ConEgo",
                                            DT::dataTableOutput('ConTable')
                                          )
                                      ),
                                     column(width=3,
                                             tags$h4(HovNode),
                                             conditionalPanel(
                                               condition="!input.ConNet_selected",
                                               tags$h4(ClickNode)
                                             ),
                                             conditionalPanel(
                                               condition="input.ConNet_selected",
                                               tags$h4(textOutput("ConEgoLabel"))
                                             ),
                                             plotOutput('ConDegBarPlot',width='auto',height=300),
                                             plotOutput('ConBetBarPlot',width='auto',height=300)
                                             )
                                    )
                           ),
                  tabPanel('Collaboration',
                           value=3,
                           wellPanel(tags$h4('Question Text:'),
                                     tags$p(ColQ)),
                           fluidRow(column(width=9,
                                           visNetworkOutput('ColNet', width='100%', height=600),
                                           conditionalPanel(
                                             condition="!input.ColEgo",
                                             DT::dataTableOutput('ColTable')
                                           )
                                      ),
                                    column(width=3,
                                            tags$h4(HovNode),
                                            conditionalPanel(
                                              condition="!input.ColNet_selected",
                                              tags$h4(ClickNode)
                                            ),
                                            conditionalPanel(
                                              condition="input.ColNet_selected",
                                              tags$h4(textOutput("ColEgoLabel"))
                                            ),
                                            plotOutput('ColDegBarPlot',width='auto',height=300),
                                            plotOutput('ColBetBarPlot',width='auto',height=300)
                                            )
                                    )
                           ),
                  tabPanel('Activities',
                           value=4,
                           wellPanel(tags$h4('Question Text:'),
                                     tags$p(ActQ)),
                           fluidRow(column(width=9,
                                           visNetworkOutput('ActNet',width='100%', height=600),
                                           conditionalPanel(
                                             condition="!input.ActEgo",
                                             DT::dataTableOutput('ActTable')
                                           )
                                      ),
                                    column(width=3,
                                            tags$h4(HovNode),
                                            conditionalPanel(
                                              condition="!input.ActNet_selected",
                                              tags$h4(ClickNode)
                                            ),
                                            conditionalPanel(
                                              condition="input.ActNet_selected",
                                              tags$h4(textOutput("ActEgoLabel"))
                                            ),
                                            plotOutput('ActDegBarPlot',width='auto',height=300),
                                            plotOutput('ActBetBarPlot',width='auto',height=300)
                                            )
                                    )
                           ),
                  tabPanel('Referrals',
                           value=5,
                           wellPanel(tags$h4('Question Text:'),
                                     tags$p(RefQ)),
                           fluidRow(column(width=9,
                                           visNetworkOutput('RefNet',width='100%', height=600),
                                           conditionalPanel(
                                             condition="!input.RefEgo",
                                             DT::dataTableOutput('RefTable')
                                           )
                                      ),
                                    column(width=3,
                                            tags$h4(HovNode),
                                            conditionalPanel(
                                              condition="!input.RefNet_selected",
                                              tags$h4(ClickNode)
                                            ),
                                            conditionalPanel(
                                              condition="input.RefNet_selected",
                                              tags$h4(textOutput("RefEgoLabel"))
                                            ),
                                            plotOutput('RefOutdegBarPlot',width='auto',height=300),
                                            plotOutput('RefIndegBarPlot',width='auto',height=300)
                                            )
                                    )
                           )
                  ),
      # Footer
      wellPanel(textOutput('UpD'))
    )
  ))
)
