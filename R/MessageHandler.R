library(R6)
#TODO: document the R6 object

R6::R6Class("MessageHandler", 
            public = list(
              initialize = function(){
                private$n_warn <- 0 
                private$n_error <- 0 
                private$n_message <- 0 
                private$lst_warn_msg <- list() 
                private$lst_error_msg <- list() 
                private$lst_message_msg <- list()
              },
              add_message = function(message){
                private$n_message <- private$n_message + 1
                private$lst_message_msg <- append(private$lst_message_msg, 
                                                  message)
              }, 
              add_warning = function(message){
                private$n_warn <- private$n_warn + 1
                private$lst_warn_msg <- append(private$lst_warn_msg, 
                                               message)
              }, 
              add_error = function(message){
                private$n_error <- private$n_error + 1
                private$lst_error_msg <- append(private$lst_error_msg, 
                                                message)
              }, 
              show_messages = function(){
                cat(private$print_counter_messages(private$n_message, 
                                               private$lst_message_msg, 
                                               type = "messages"))
              }, 
              show_warnings = function(){
                cat(private$print_counter_messages(private$n_warn, 
                                               private$lst_warn_msg, 
                                               type = "warnings"))
              }, 
              show_errors = function(){
                cat(private$print_counter_messages(private$n_error, 
                                               private$lst_error_msg, 
                                               type = "errors"))
              },
              finish = function(stop_with_error = TRUE){
                
                fn_call <- sys.call(-1)
                fn_call <- utils::capture.output(fn_call)
                
                if(private$n_message + private$n_message + private$n_error > 0){
                  message(sprintf("Function call: %s", 
                                  fn_call))
                }
                
                
                if (private$n_message > 0){
                  message(private$print_counter_messages(private$n_message, 
                                                         private$lst_message_msg, 
                                                         type = "messages"))
                }
                if (private$n_message > 0){
                  warning(private$print_counter_messages(private$n_warn, 
                                                         private$lst_warn_msg, 
                                                         type = "warnings"))
                }
                
                if (private$n_error > 0){
                  errors <- private$print_counter_messages(private$n_error, 
                                                           private$lst_error_msg, 
                                                           type = "errors")
                  if(stop_with_error){
                    stop(errors, call. = FALSE)
                  } else{
                    warning(errors, call. = FALSE)
                  }
                  
                }
              }
            ), 
            private = list(n_warn = NULL, 
                           n_error = NULL, 
                           n_message = NULL, 
                           lst_warn_msg = NULL, 
                           lst_error_msg = NULL, 
                           lst_message_msg = NULL, 
                           print_counter_messages = function(count, messages, type){
                             first_msg <- sprintf("There are %d %s: \n", 
                                                  count, 
                                                  type)
                             line_length <- nchar(first_msg) - 2

                             second_part <- c(paste(rep(x = "-", times = line_length), collapse = ""), "\n")
                             third_part <- sprintf("%d : %s \n", 
                                         1:count, 
                                         unlist(messages))
                             return(c(first_msg, second_part, third_part))
                             
                           })
            ) -> MessageHandler


