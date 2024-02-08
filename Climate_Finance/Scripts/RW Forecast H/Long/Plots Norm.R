#### Boosting ####
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Long Norm/",plotnames[i],".jpeg")
  jpeg(filename = names, width = 1920, height = 1080)
  
  print(ggplot(results[results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = "Forecast (h=1)"), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=1)" = "red", "Y" = "black"))+
          scale_x_date(date_breaks = "1 year", labels = date_format("%Y"),expand = c(0.05,0.005))+
          ggtitle(plotnames[i])+
          labs(x= NULL, y = NULL)+
          theme(legend.position = c(0.07,0.95),
                legend.background = element_rect(fill = NA),
                legend.key = element_rect(size = 30),
                legend.text = element_text(size = 30),
                plot.title = element_text(hjust = 0.5, size = 40),
                axis.text = element_text(size = 20)))
  dev.off()
}

#### Linear ####
for (i in 1:6) { 
  names <- paste0("Output/Plots/RW Forecast/Long Norm/",plotnames[i],".jpeg")
  jpeg(filename = names,
       width = 1920,
       height = 1080)
  
  print(ggplot(results[results$model == models[i],])+
          geom_line(aes(x = date, y = fc, color = "Forecast (h=1)"), size = 1)+
          geom_line(aes(x = date, y = Y, color = "Y"), size = 1)+
          scale_color_manual("", values = c("Forecast (h=1)" = "red", "Y" = "black"))+
          scale_x_date(date_breaks = "1 year", labels = date_format("%Y"),expand = c(0.05,0.005))+
          ggtitle(plotnames[i])+
          labs(x= NULL, y = NULL)+
          theme(legend.position = c(0.07,0.95),
                legend.background = element_rect(fill = NA),
                legend.key = element_rect(size = 30),
                legend.text = element_text(size = 30),
                plot.title = element_text(hjust = 0.5, size = 40),
                axis.text = element_text(size = 20)))
  
  dev.off()
}