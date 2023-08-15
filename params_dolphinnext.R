
#!/usr/bin/env Rscript

###################################################
#input: a csv fille with the 4 colums: module, process, paramter and value.
#output: a txt file with the correct syntax to set pipeline default parameters.
#
# The value in the file need to be with quotation marks, comma or parenthetically as defined in the process.
# The process name in the module need to be identical to the process name.
###################################################

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  print("Please provide a file path as an argument.")
   stop("Script stopped because there is no file") 
}

file_path <- args[1]

table<-read.csv(file_path)

edit_lines <- c()
params_lines <- c()
pattern_line_edit <- 'params.edit_MODULE_PROCESS = "no" //* @dropdown @options:"yes","no"  @show_settings:"PROCESS"'
pattern_edit_check_line <- 'if(params.edit_MODULE_PROCESS == "no"){'
for(module in unique(table$module)){
  
  module_table <- table[table$module==module, ]
  
  for(process in unique(module_table$process)){
    
    process_table <- module_table[module_table$process==process,]
    
    ## add edit query
    line_edit <- gsub("MODULE", module, pattern_line_edit)
    line_edit <- gsub("PROCESS", process, line_edit)
    edit_lines <- c(edit_lines, line_edit)
    
    ## add edit check line
    edit_check_line <- gsub("MODULE", module, pattern_edit_check_line)
    edit_check_line <- gsub("PROCESS", process, edit_check_line)
    params_lines <- c(params_lines, edit_check_line)

    
    ## add process comment
    line_param_comment <- paste0("\t// Process Parameters for ", module , "_",process, ":")
    params_lines <- c(params_lines, line_param_comment)
    
    ## add process params
    line_process_params <- sapply(1:nrow(process_table), function(i){
      paste0("\tparams.", module , "_",process, ".",process_table$paramter[i], " = ", process_table$value[i])
    })
    
    params_lines <- c(params_lines, line_process_params)
    
    ## close query
    params_lines <- c(params_lines, "}")
  }
}

lines <- c(edit_lines, params_lines)

file_path <- "file.txt"
writeLines(lines, con=file_path)
