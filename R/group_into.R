#' Adding a new categorical column into the table 
#' 
#' This function will create a new table that includes the original table and a 
#' new column which is a categorical variable with values depending on the values 
#' requested by the user.
#'
#' @param table original table need to be modified (it makes sense to name the 
#` variable simple like a single word "table")
#' @param source_name name of the column to be categorized (the name is intuitive)
#' @param new_name name of the new column which has categorized values (the name 
#` is intuitive)
#' @param categories a vector of categories, it needs to be in ascending order
#`(because the element in the vector is a category so I take the plural form)
#'
#' @return the table with a new categorical column
#' @export
#'
#' @examples 
#' sample <- data.frame("value"=1:10)
#' (grouped_sample <- group_into(sample, "value", "group", c("Small", "Medium", "Large")))
group_into <- function(table, source_name, new_name, categories) {
  # check if source_name in the table
  stopifnot(source_name %in% colnames(table))
  
  # check length of categories is greater than 0
  stopifnot(length(categories) > 0)
  
  # group the samples depending on the value of source_name 
  new_table <- table
  new_table[, new_name] <- cut(table[[source_name]], breaks = length(categories), labels = categories)
  
  return(new_table)
}