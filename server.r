function(input, output, session) {
  
  rv <- reactiveValues(logout_init = 0)
  
  user_info <- callModule(login, "login", 
                          user_base = user_base, 
                          log_out = reactive(rv$logout_init))
  
  output$logout_button <- renderUI({
    if(user_info()$user_auth) {
      div(style = "padding: 8px;",
          tags$li(actionButton("logout", "Log out", class = "btn-danger", style = "color: black;"), class = "dropdown")
      )
    } else {
      return(NULL)
    }
  })
  
  observeEvent(input$logout, {
    rv$logout_init <- rv$logout_init + 1
  })
  
  observe({
    if(user_info()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else if (!isTruthy(user_info()$user_auth)) {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
#### Render Menu ####
  output$sidebar <- renderMenu({
    req(user_info()$user_auth)
    
    if(user_info()$permission == "admin") {
      sidebarMenu(
        id = "tabs",

        menuItem(
          "data",
          tabName="data",
          icon=icon("th")
        ),
        menuItem(
          "segments",
          tabName="segments",
          icon=icon("th")
        )
      
      )
      
    } 
    # placeholder if other views needed
    
  })
  
#### Render Tabs ####
  
  output$data_tab <- renderUI({
    
    req(user_info()$permission == "admin")
    
    tagList(
      # tabName='upload',
      h2("Data Upload"),
      fileInput(
        "file1",
        "Choose csv file",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      checkboxInput("header", "Header", TRUE),
      fluidRow(
        column(10,
               allign='center',
               offset=1,
               DT::dataTableOutput("header_tab")
        )
      ),
      uiOutput("ui_id_checkboxes"),
      fluidRow(
        box(
          uiOutput('ui_grouping_dropdown')
        ),
        box(
          uiOutput('ui_n_groups_slider')
        )
      ),
      fluidRow(
        column(
          8,
          offset=2,
          allign='center',
          uiOutput('ui_used_cols_checkboxes'),
        )
      ),
      fluidRow(
        column(8,
               allign='center',
               offset=2,
               DT::dataTableOutput("avg_tab"),
        )
      )
    )
    
  })

  output$segments_tab <- renderUI({
    
    req(user_info()$permission == "admin")
    
    tagList(
      # tabName="segments",
      h2("Result of Segmentation"),
      fluidRow(
        box(
          sliderInput(
            "slide_supp",
            "Choose support parameter",
            min=MIN_SUPPORT,
            max=MAX_SUPPORT,
            value=round(mean(c(MIN_SUPPORT, MAX_SUPPORT))/3,digits=1)
          )
        ),
        box(
          uiOutput('slider_min_len')
        )
      ),
      fluidRow(
        box(
          sliderInput(
            "slide_precluster",
            "How many clusters in the first step",
            min=MIN_PRECLUSTERS,
            max=MAX_PRECLUSTERS,
            value=trunc(mean(c(MIN_PRECLUSTERS, MAX_PRECLUSTERS)))
          )
        ),
        box(
          uiOutput('ui_cluster_selected')
        )
      ),
      fluidRow(
        column(8,
               allign='center',
               offset=2,
               plotOutput('plot_freq'), 
        )  
      ),
      DT::dataTableOutput("clusters_tab")
    )
    
  })
  
#### Render Outputs ####
  
  output$header_tab <- DT::renderDataTable({
    # render table of data header
    inFile <- input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    return(get_data() %>% head(10))
    
  }, options = list(
    scrollX = TRUE, 
    dom='t',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#EB8C00'});",
      "}")
    )
  )
  
  output$ui_id_checkboxes <- renderUI({
    
    inFile <- input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    checkboxGroupInput(
      'id_cols', 
      'Select ID columns', 
      colnames(get_data()),
      inline = TRUE
    )
  })
  
  output$ui_used_cols_checkboxes <- renderUI({
    
    inFile <- input$file1
    id_cols <- input$id_cols
    
    if(is.null(inFile) | is.null(id_cols)){
      return(NULL)
    }
    
    checkboxGroupInput(
      'cols_used', 
      sprintf('Select maximum of %s columns to be Analysed', MAX_SELECTED_COLS), 
      colnames(get_data())[!(colnames(get_data()) %in% id_cols)],
      inline = TRUE
    )
  })
  
  # set the maximum of selected columns
  observe({
    if(length(input$cols_used) > MAX_SELECTED_COLS){
      updateCheckboxGroupInput(session, "cols_used", selected=tail(input$cols_used, MAX_SELECTED_COLS))
    }
  })
  
  output$ui_grouping_dropdown <- renderUI({
    
    inFile <- input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    selectInput(
      'group_method', 
      'Select method of variable grouping', 
      c(
        'k-means',
        'quantiles'
      ),
      selected='quantiles'
    )
  })
  
  output$ui_n_groups_slider <- renderUI({
    
    inFile <- input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    sliderInput(
      'slide_n_group',
      'Number of groups within variable',
      min=MIN_GROUPS,
      max=MAX_GROUPS,
      value=trunc(mean(c(MIN_GROUPS, MAX_GROUPS))),
      step=1
    )

  })
  
  output$avg_tab <- DT::renderDataTable({
    # render table of averages per group
    inFile <- input$file1
    ids <- input$id_cols
    cols_used <- input$cols_used
    if(is.null(inFile) | is.null(ids) | is.null(cols_used)){
      return(NULL)
    }
    avg_tab <- avg_calc()
    dt_avg_tab <- avg_tab %>% 
      DT::datatable(
        rownames=FALSE,
        options=list(
          dom='t',
          initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#EB8C00'});",
          "}")
          )
        ) %>% 
      formatRound(2:ncol(avg_tab)) 
    return(dt_avg_tab)
  })
  
  output$slider_min_len <- renderUI({
    # render slider for min number of variables used in clusters
    inFile <- input$file1
    ids <- input$id_cols
    cols_used <- input$cols_used
    if(is.null(inFile) | is.null(ids) | is.null(cols_used)){
      return(NULL)
    }
    sliderInput(
      'slide_min_len',
      'Mininum of variables used',
      min=2,
      max=length(cols_used) - length(ids),
      value=length(cols_used) - length(ids),
      step=1
    )
  })
  
  output$ui_cluster_selected <- renderUI({
    
    preclusters_n <- input$slide_precluster
    
    if(is.null(preclusters_n)){
      return(NULL)
    }
    
    selectInput(
      'cluster_selected', 
      'Select which cluster should be examined further', 
      1:preclusters_n,
      selected=1
    )
  })
  
  output$clusters_tab <- DT::renderDataTable({
    # render table of clusters
    inFile <- input$file1
    ids <- input$id_cols
    cols_used <- input$cols_used
    if(is.null(inFile) | is.null(ids) | is.null(cols_used)){
      return(NULL)
    }
    
    seg_tab <- seg_calc()
    dt_seg_tab <- seg_tab %>% 
      DT::datatable(
        rownames=FALSE,
        options = list(
          sDom  = '<"top">lrt<"bottom">ip',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#EB8C00'});",
            "}")
          )
      )%>%
      formatStyle(names(seg_tab)[1:(length(names(seg_tab))-1)], 
                  backgroundColor = styleInterval(style_brks, style_clrs))
    return(dt_seg_tab)
  })
  
  output$plot_freq <- renderPlot({
    inFile <- input$file1
    ids <- input$id_cols
    cols_used <- input$cols_used
    if(is.null(inFile) | is.null(ids) | is.null(cols_used)){
      return(NULL)
    }
    data_quant <- get_quant()
    
    preclustered_data <- data_quant %>%
      select(-all_of(input$id_cols)) %>%
      # calculate k-means
      k_means_rep(input$slide_precluster) %>%
      .$cluster %>%
      # bind with initial dataset
      cbind(data_quant, cluster = .)
    
    data_cluster_further <- preclustered_data %>%
      filter(cluster == input$cluster_selected) %>%
      select(-all_of(c('cluster', input$id_cols))) %>%
      as('transactions')

    return(data_cluster_further %>% itemFrequencyPlot())    

  })
  
#### Reactive Functions ####
  
  get_data <- reactive({
    inFile <- input$file1
    data <- read.csv(inFile$datapath, header = input$header)
    return(data)
  })

  get_quant <- reactive({

    data <- get_data() %>% 
      select(all_of(c(input$id_cols, input$cols_used))) %>%
      check_and_handle_NA(method='drop')
    
    if(input$group_method == "k-means"){
      data_quant <- data %>%
        k_means_wrapper(
          k=input$slide_n_group,
          id_cols=input$id_cols
        )
    } else if (input$group_method == "quantiles"){
      data_quant <- data %>%
        quantiles_wrapper(
          n_quantiles_max=input$slide_n_group,
          id_cols=input$id_cols
        )
    }
    return(data_quant)
  })
  
  seg_calc <- reactive({
    # get clusters
    data_quant <- get_quant()
    
    preclustered_data <- data_quant %>%
      select(-all_of(input$id_cols)) %>%
      # calculate k-means
      k_means_rep(input$slide_precluster) %>%
      .$cluster %>%
      # bind with initial dataset
      cbind(data_quant, cluster = .)
    
    data_cluster_further <- preclustered_data %>%
      filter(cluster == input$cluster_selected) %>%
      select(-all_of('cluster'))
    
    clusters_tab <- clusters_tab(
      data=data_cluster_further,
      id_cols=input$id_cols,
      support=input$slide_supp,
      minlen=input$slide_min_len,
      print_freq=FALSE,
      inspect_items=FALSE
    )
    return(clusters_tab)
  })
  avg_calc <- reactive({
    # calculate averages
    data <- get_data() %>% 
      select(all_of(c(input$id_cols, input$cols_used))) %>%
      check_and_handle_NA(method='drop')
    data_quant <- get_quant()
    avg_tab <- avg_tab_wrapper(
      data_orig=data,
      data_grouped=data_quant,
      id_cols=input$id_cols
    )
    return(avg_tab)
  })
    

  
  
}
