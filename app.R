library(shiny)
library(shinyFiles)
library(shinyjs)
library(immunarch)
library(ggplot2)
library(bslib)
library(prodlim)
library(survival)
options(ggrepel.max.overlaps = Inf)

test_dir <- "TCR_shiny_clones/"

# Define UI for TCRseq app ----
ui = tagList(  
  useShinyjs(),
    navbarPage(
    title = "TCRseq", 
    # Theme for the page, from bslib library
    theme = bs_theme(version = 4, bootswatch = "minty"),        
    tabPanel(
      title = "Load data",                        
      fluidRow(                
        column(
          3,                                        
          actionButton(inputId = "load", label = "Load test data", class = "btn-secondary"),          
          # Metadata headers and other options
          tags$hr(),
          uiOutput("metadata_headers"),
          conditionalPanel(
            condition = "input.grouping && input.grouping != 'None'",
            checkboxInput(inputId = "show_test", label = "Show pairwise test"),
            checkboxInput(inputId = "show_error", label = "Show error bars")
          ),
          textOutput("msg"),   
        ),
        column(
          9,
          title = "Clone count",
          plotOutput(outputId = "clone_count_plot", height = "600px"),          
        )
      ),            
    ),
    
    navbarMenu(
      title = "Advanced plots",

      tabPanel(
        title = "CDR3 length",
        fluidRow(
          column(
            3,
            tags$h2("CDR3 length"),
            tags$p("CDR3 length plot made with R package immunarch."),
          ),
          column(
            9,
            plotOutput(outputId = "cdr3_plot", height = "600px")
          )     
        ) 
      ),            
      tabPanel(
        title = "Clustering",
        fluidRow(
          column(
            3,
            tags$h2("t-SNE and K-means clustering"),
            tags$p("t-SNE plot with K-means clustering made with R package immunarch. 
                   Based on Jensen-Shannon Divergence of gene usage."),
            
            tags$h3("Gene usage settings"),
            conditionalPanel(
              condition = "input.tcr_bcr == 'TCR'",
              selectInput(
                inputId="tcr_receptor",
                label="TCR receptor selection",
                choices=c("TRAV", "TRBV", "TRAJ", "TRBJ"),
                selected = "TRBV"
              )
            ),            
            conditionalPanel(
              condition = "input.tcr_bcr == 'BCR'",
              selectInput(
                inputId="bcr_receptor",
                label="BCR receptor selection",
                choices=c("IGHV", "IGKV", "IGLV", "IGHD"),
                selected = "IGHV"
              )
            ),
            tags$hr(),
            tags$h3("Clustering settings"),
            numericInput(inputId = "cluster_k", label = "K-means k", value = 2),
            numericInput(inputId = "cluster_perp", label = "t-SNE perplexity", value = 1),
            #numericInput(inputId = "cluster_theta", label = "t-SNE theta", value = 1),
          ),
          column(9,
            plotOutput(outputId = "tsne_plot", height = "600px")    
          )
        )
      ),
      
      tabPanel(
        title = "Kaplan-Meier survival",
        fluidRow(
          column(
            3,
            tags$h2("Kaplan-Meier survival"),
            tags$p("Survival plot made with R package prodlim. 
                   You need to select the columns from your metadata file to use.
                   "),
            uiOutput("km_status"),
            uiOutput("km_time"),
            uiOutput("km_category"),
            actionButton(inputId = "km_load", label = "Create plot", class = "btn-secondary"),
          ),
          column(
            9,
            plotOutput(outputId = "kaplan_meier_plot", height = "600px")
          ),          
        )
      )
      
    )
  )  
)

# Define server logic to plot various variables against mpg ----
server = function(input, output, session) {
  #### Folder location handlers
  volumes = c(
    wd=".",
    home="~",
    root="/",
    test="~/CTI/panNET/clones/TCR_clones_cleanimmunarch_test"
  )
  shinyDirChoose(
    input, 
    'directory_select', 
    roots=volumes, 
    session=session, 
    defaultRoot = "home"
  )


  #### Metadata category input section
  output$metadata_headers = renderUI({
    f = read.table(paste0(test_dir, "/metadata.txt"), sep = "\t", header = T)
    
    output_choices = c("None")
    for(i in 1:length(names(f))){
      if (length(levels(as.factor(f[,i])))/length(f[,i]) < 0.5){
        output_choices = c(output_choices, names(f)[i])
      }
    }
    
    selectInput(
      inputId="grouping",
      label="Pick a category from metadata headers to group samples by",
      choices=output_choices
    )
  })

  
  
  #### Data loading
  user_path =  test_dir#eventReactive(input$load,{
    #parseDirPath(volumes, input$directory_select)
  #})
  #output$directory_selected = renderText({
  #  parseDirPath(volumes, input$directory_select)
  #})
  

  
  tcr_immunarch = reactive({
    # Let the user know something is happening by changing the button
    removeCssClass(id = "load", class = "btn-secondary")
    addCssClass(id = "load", class = "btn-danger")
    shinyjs::html(id = "load", html = "Loading...")
                   
    # Do the actual processing and save
    result = repLoad(test_dir, .mode="single")
        
    # Change the button again
    removeCssClass(id = "load", class = "btn-danger")
    addCssClass(id = "load", class = "btn-success")
    shinyjs::html(id = "load", html = "Loaded!")
        
    # Return the result to assign it to tcr_immunarch
    result
  })
  
  # Also change the label of the button when user selects new path
  observeEvent(input$directory_select, {
    removeCssClass(id = "load", class = "btn-success")
    addCssClass(id = "load", class = "btn-secondary")
    shinyjs::html(id = "load", html = "Load data")
  })

  observeEvent(input$load, {
    
    withCallingHandlers({
        shinyjs::html("text", "")        
        tmp <- tcr_immunarch()$data
      },
        message = function(m) {
          shinyjs::html(id = "msg", html = paste('<span style="color:grey;font-size:10px">',m$message,'</span>'), add = TRUE)
      })
          
        
    metadata = read.table(paste0(test_dir, "/metadata.txt"), sep = "\t", header = T)
    output$km_status = renderUI({
      selectInput(
        inputId="km_status_input",
        label="Status column",
        choices=names(metadata)
      )
    })
    output$km_time = renderUI({
      selectInput(
        inputId="km_time_input",
        label="Time column",
        choices=names(metadata)
      )
    })    
    output$km_category = renderUI({
      selectInput(
        inputId="km_category_input",
        label="Investigative category column",
        choices=names(metadata)
      )
    })
  })
  
  
  tcr_custom_count = eventReactive(input$load,{
    metadata = read.table(paste0(test_dir, "/metadata.txt"), sep = "\t", header = T)
    filenames <- list.files(test_dir, pattern="*.tsv", full.names=TRUE)
    all_counts = data.frame()
    withProgress(message = 'Loading files', value = 0, {
    for (s in filenames){
      incProgress(1/n, detail = paste(length(filenames),"/", i))      
      s_clones = read.table(s, sep = "\t", header = T)
      file_name_split = unlist(strsplit(s, "/"))
      sample_name = file_name_split[length(file_name_split)]
      sample_name = gsub(".tsv", "", sample_name)
      all_counts = rbind(
        all_counts, 
        cbind(
          sample_name, 
          nrow(s_clones), 
          sum(as.numeric(s_clones$cloneCount))
        )
      )
    }
    })
    names(all_counts) = c("sample", "unique_clone_count", "total_clone_count")
    all_counts$unique_clone_count = as.numeric(all_counts$unique_clone_count)
    all_counts$total_clone_count = as.numeric(all_counts$total_clone_count)
    all_counts = merge(all_counts, metadata, by.x = "sample", by.y = "Sample", all.x=T)
    

    
    all_counts
  })
  
  
  
  imm_gu = reactive({
    if (input$tcr_bcr == "TCR"){
      geneUsage(tcr_immunarch()$data, .gene = paste0("hs.",input$tcr_receptor), .norm = T, .ambig = "exc")
    }
    else if(input$tcr_bcr == "BCR"){
      geneUsage(tcr_immunarch()$data, .gene = paste0("hs.",input$bcr_receptor), .norm = T, .ambig = "exc")
    }
  })
  
  imm_cl_tsne = reactive({
    # print(input$cluster_theta)
    geneUsageAnalysis(
      imm_gu(), 
      "js+tsne+kmeans", 
      .perp = input$cluster_perp, 
      .k = input$cluster_k, 
      # .theta = input$cluster_theta,
    )
  })

  #### Plotting outputs
  
  output$clone_count_plot = renderPlot({
    if(input$load > 0 && !is.null(input$grouping)){
      print(input$grouping)
      flag_test = input$show_test
      flag_errorbars = !input$show_error
      if (input$grouping != "None"){
        exp_vol = repExplore(tcr_immunarch()$data, .method = "volume")
        p0 = vis(exp_vol, .by = c(input$grouping), .meta = tcr_immunarch()$meta, .test=flag_test, .errorbars.off = flag_errorbars)
        p0
      }else{
        exp_vol = repExplore(tcr_immunarch()$data, .method = "volume")
        p0 = vis(exp_vol, .meta = tcr_immunarch()$meta)
        p0
      }
    }
  })
  
  
  output$cdr3_plot = renderPlot({
    if(input$load > 0){
      if (input$grouping != "None"){
        exp_len = repExplore(tcr_immunarch()$data, .method = "len", .col = "aa")
        p0 = vis(exp_len, .by = c(input$grouping), .meta = tcr_immunarch()$meta)
        p0
      }else{
        exp_len = repExplore(tcr_immunarch()$data, .method = "len", .col = "aa")
        p0 = vis(exp_len)
        p0
      }
    }
  })
  
  output$tsne_plot = renderPlot({
    if(input$load > 0){
      p0 = vis(imm_cl_tsne(), .plot = "clust")
      p0
    }
  })
  
  observeEvent(input$km_load,{
    kmform = as.formula(paste0("Surv(",input$km_time_input,",",input$km_status_input,") ~ ", input$km_category_input))
    output$kaplan_meier_plot = renderPlot({
      if(input$load > 0){
        km_data = tcr_custom_count()
        kmfitX <- prodlim(kmform, data = km_data)
        p0 = plot(kmfitX,legend.x="bottomright",atRisk.cex=1.3,atrisk.title="No. subjects")
        p0
      }
    })
  })
  

  
}

shinyApp(ui, server)