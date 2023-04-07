# TODO: Add comment
# 
# Author: JenC
###############################################################################
install.packages('pals')
library(pals)

pal.safe(parula, main="parula")
pal.safe(coolwarm, main="coolwarm")

ftab <- flextable( head( iris ) )
ftab <- set_caption(ftab, "my caption")
ftab

ft <- flextable(mtcars[1:50,])
print(ft)
reference_docx: reference-word-doc.docx
officer::styles_info("reference-word-doc.docx")

word_document:
		reference_docx: reference-word-doc.docx
        page_margins:
		  bottom: 1
          top: 1
          right: 1.25
          left: 1.25
          header: 0.5
          footer: 0.5
          gutter: 0.5
		
update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()

 #rmarkdown::render("C:\\Users\\jennifer.cahalan\\Work\\JAC Stuff\\ADP2024\\ADP2024-code (JAC code)\\allocation-background.Rmd")
 rmarkdown::render("C:\\Users\\jennifer.cahalan\\Work\\JAC Stuff\\ADP2024\\2024_ADP\\analyses\\allocation_background\\allocation-background-word.Rmd")
 
 rmarkdown::render("C:\\Users\\jennifer.cahalan\\Work\\JAC Stuff\\ADP2024\\ADP2024-code (JAC code)\\Var_Allocation\\Allocation_by_psc_var.rmd")
  
 rmarkdown::render("C:\\Users\\jennifer.cahalan\\Work\\JAC Stuff\\NPGOP_Sampling\\ProjectCode\\Good_Code\\2022\\annual_review_summary_27feb2023.Rmd")
 rmarkdown::render("C:\\Users\\jennifer.cahalan\\Work\\JAC Stuff\\NPGOP_Sampling\\ProjectCode\\Good_Code\\create-reference-word-doc.Rmd")
 
 install.packages('tinytex')

 