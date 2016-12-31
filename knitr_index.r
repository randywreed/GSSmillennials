## This creates a map of all knitr objects in Gss_plots
Fname="Gss_Plots.r"
comand=paste0("grep -n -A 1 '## @knitr' ",Fname," > knitr_index_",Fname,".txt")
#system("grep -n -A 1 '## @knitr' Gss_Plots.r > knitr_index_GSSMillennials.txt")
system(comand)
