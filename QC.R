## QC 

## Baculum RFID QC

baculum_c17 <- c("1DCD17A6",
"1DCD1764", 
"1DCD1776",
"1DCD176C",
"1DCD1769",
"1DCD1762",
"1DCD1792",
"1DCD1750",
"1DCD1784",
"1DCD1781",
"1DCD178D",
"1DCD1790",
"1DCD176B",
"1DCD176A",
"1DCD176E",
"1DCD1771",
"1DCD174A",
"1DCD178C",
"1DCD1779",
"1DCD1756",
"1DCD1794",
"1DCD177B",
"1DCD17A1",
"1DCD174D",
"1DCD175F",
"1DCD1761")

Jerry_excel_orig_1_test %>% 
  subset(rfid %in% baculum_c17 & sex == "M") %>% 
  nrow() == length(baculum_c17)
