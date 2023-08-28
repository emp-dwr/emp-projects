
# Functions for WQ QA/QC --------------------------------------------------

#' Add Sheet
#' adds Excel sheet to existing workbook
#' @param wb the workbook to write to
#' @param df the relevant dataframe
#' @param sheet_name the name of the sheet
#'
add_sheet <- function(wb, df, sheet_name) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df, startRow = 1, startCol = 1)
}


#' Flag < RL Data
#' finds data < RL in Excel sheet and highlights it in red
#' @param wb the relevant Excel workbook
#' @param sheet the relevant Excel workbook sheet
#' @param df the data frame containing the sheet's data
#'
add_formatting <- function(wb, sheet, df){
  # to avoid applying the function to the metadata columns, we need to define what the last metadata column is  
    # create vector of metadata columns
  vec_meta <- colnames(df)[colnames(df) %in% colnames(df_field)]
    # determine the last column to contain metadata, add 1 so the function starts at the next column
  col_num <- length(vec_meta)+1
  
  # define styles for cells
  # neg_style is for '<' data, norm_style is for 'N/A'
  neg_style <- openxlsx::createStyle(fontColour = '#9C0006', bgFill = '#FFC7CE')  
  norm_style <- openxlsx::createStyle(fontColour = '#000000',bgFill = '#FFFFFF', border = 'TopBottomLeftRight', borderColour = '#d4d4d4',
                                      borderStyle = 'thin')
  
  # apply conditional formatting to format all cells with the '<' (&lt;) character
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = col_num:length(df), 
                                  rows = 1:nrow(df)+1, type = 'notcontains', rule = '&lt;', style = neg_style)
  
  # apply conditional formatting so all cells containing 'N/A' look normal
  # needed because 'N/A' cells are flagged in above code
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = col_num:length(df), 
                                  rows = 1:nrow(df)+1, type = 'contains', rule = 'N/A', style = norm_style)
  
  # return the notebook variable
  return(wb)
}