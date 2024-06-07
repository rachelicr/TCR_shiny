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
#result = repLoad(test_dir, .mode="single")
#flist <- list.files(test_dir, pattern="*.tsv", full.names=TRUE)    
# for (s in flist){
#       i <- i + 1
#       incProgress(1/n, detail = paste(s,length(flist),"/", i))      
#       s_clones = read.table(s, sep = "\t", header = T)
#       file_name_split = unlist(strsplit(s, "/"))
#       sample_name = file_name_split[length(file_name_split)]
#       sample_name = gsub(".tsv", "", sample_name)
#       all_counts = rbind(
#         all_counts, 
#         cbind(
#           sample_name, 
#           nrow(s_clones), 
#           sum(as.numeric(s_clones$cloneCount))
#         )
#       )
#     }

foo <- function() {
  result = repLoad(test_dir, .mode="single")
  result
}


  ui = fluidPage(
    shinyjs::useShinyjs(),
    actionButton("btn","Click me"),
    textOutput("text")
  )
  server = function(input,output, session) {
    observeEvent(input$btn, {
      withCallingHandlers({
        shinyjs::html("text", "")
        foo()        
      },
        message = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
      })
    })
  }


shinyApp(ui = ui, server = server)