body <- mainPanel(
  tags$head(tags$style(HTML('.skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: black;
                            }'))),
  
  fluidRow(
    box(
      title = "new customer", width = 6, solidHeader = TRUE,background = "black",
      plotOutput('new_customer',height='300px')
    ),
    box(
      title = "revenue", width = 6, solidHeader = TRUE,background = "black",
      plotOutput('revenue',height='300px')
    )
  ),
  fluidRow(
    box(
      title = "repeat customer", width = 6, solidHeader = TRUE,background = "black",
      plotOutput('old_customer',height='300px')
    ),
    box(
      title = "Revenue brought by customers", width = 6, solidHeader = TRUE,background = "black",
      plotOutput('new_old_customer',height='300px')
    )
  )
  )

ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  sidebarPanel(width=3,
               sidebarMenu(
                 passwordInput("pass", "Username", value = ""),
                 passwordInput("pas", "Password", value = ""),
                 conditionalPanel(
                   condition =  paste("input.pass =='",toString(user),"' && input.pas == '",toString(password),"'",sep=''),
                   radioButtons("website", "Website:",
                                choices = c("a" = 'A',
                                            "b" = 'B',
                                            "c" = 'C'
                                ),selected = 'a'
                   ),
                   selectInput("month", "Month:",
                               c("Sep" = "sep",
                                 "Oct" = "oct",
                                 "Nov" = "nov",
                                 "Dec" = "dec"))
                   ,
                   textOutput('text')
               )
               )
               ),
  conditionalPanel(
    condition =  paste("input.pass =='",toString(user),"' && input.pas == '",toString(password),"'",sep=''),
    body
  )
)
