library(ggplot2)
library(shiny)

library(shinydashboard)
library(plotly)
library(dplyr)
library(corrplot)
library(shiny)
library(gridExtra)
heart.dat = read.csv("C:/R/heartfailure.csv",stringsAsFactors = FALSE)
ui<-dashboardPage( skin="purple",
    dashboardHeader(title="Heart Failure visualizations"),
    dashboardSidebar(
                     sidebarMenu("Heart failure visualizations",icon=icon("dashboard"))
                   ),
    dashboardBody(
                     fluidPage(
                       fluidRow(
                       box( 
                         title="Serum Creatinine Vs Diabetes",width=11,background = "maroon",status="primary",
                         plotlyOutput("plot1",height="300px"))),
                        fluidRow(box(
                         title="Serum Sodium Vs Age",width=11,background = "orange",status="primary","",
                         plotlyOutput("plot2",height = "300px"))),
                       fluidRow( box(
                         title="Box plot-death event with count variable segregated by gender",width=11,background = "lime",status="primary","",
                         plotlyOutput("plot3",height = "300px"))),
                       
                         fluidRow( box(
                         title="Age Vs Death Event",width=11,background = "aqua",status="primary","",
                         plotlyOutput("plot5",height = "300px"))),
                         fluidRow(box(
                         title="Density Distribution of Ejection Fraction",width=11,background = "yellow",status="primary","",
                         plotlyOutput("plot6",height = "300px"))),
                         fluidRow(box(
                         title="Smoking Vs Ejection Fraction",width=11,background = "olive",status="primary","",
                         plotlyOutput("plot7",height = "300px"))),
                         fluidRow(box(
                         title="Categorical values VS heart failure",width=11,background = "red",status="primary","",
                         plotOutput("plot8",height="300px"))),
                         fluidRow(box(
                         title="Density Distribution of Creatinine Phosphokinase ",width=8,background = "teal",status="primary","",
                         plotlyOutput("plot9",height="300px"))),
                       fluidRow(box(
                         title="Creatinine Phosphokinase for Anaemia State Segregated by Death Event",width=11,background = "lime",status="primary","",
                         plotlyOutput("plot10",height = "300px")))
                       
                       
                     )
                     )
)


server<-function(input,output){
  output$plot1<-renderPlotly({ggplot(heart.dat,aes(x=as.factor(diabetes),y=serum_creatinine,colour=diabetes))+geom_point(position = "jitter")+ggtitle("Serum Creatinine Vs Diabetes")
    
  })
  output$plot2<-renderPlotly({ggplot(heart.dat,aes(x=age,y=serum_sodium),DEATH_EVENT=1)+geom_point()+ggtitle("Serum Sodium Vs Age")})
  output$plot3<-renderPlotly({ggplot(heart.dat, aes(DEATH_EVENT,time))+ geom_point()+labs(title= "Box plot-death event with count variable segregated by gender",x= " death", y= "Count")+ geom_boxplot(fill="lightblue", color= "black", notch = TRUE)+
      facet_wrap('sex')})
  
  output$plot5<-renderPlotly({ggplot(heart.dat, aes(x=age,fill=as.factor(DEATH_EVENT))) + 
      geom_histogram(binwidth = 5, position = "identity",alpha = 0.5,color = "white") + 
      xlab("Age") + ylab("Number of subjects") + theme_classic() +ggtitle("Age Vs Death Event")})
  output$plot6<-renderPlotly({ggplot(heart.dat,aes(x = ejection_fraction,fill = as.factor(DEATH_EVENT)))+
      geom_density(alpha = 0.2) + theme_classic() + 
      labs(title = "Density Distribution of Ejection Fraction") + 
      geom_vline(aes(xintercept = mean(ejection_fraction[DEATH_EVENT == 0])), color = "darkblue")+
      geom_vline(aes(xintercept = mean(ejection_fraction[DEATH_EVENT==1])), color = "darkorange") + 
      geom_curve(xend = 40.4, yend = 0.03, x = 48, y = 0.03, arrow = arrow(length = unit(0.2,"cm")),size = 1,color = "darkblue")+
      geom_curve(xend = 33.6, yend = 0.04, x = 45, y = 0.042, arrow = arrow(length = unit(0.2,"cm")),size = 1, color = "darkorange")+
      geom_text(label = paste0("Mean value for Non-Death Events"), x = 60, y = 0.03,size = 2.5)+
      geom_text(label = "Mean value for Death Event", x = 55, y = 0.042,size = 2.5) + 
      annotate("text",x = 70, y = 0.05, label = "Mean EF \n Non-Death Events: 40.3 \n Death Events: 33.5")})
  output$plot9<-renderPlotly({ggplot(heart.dat,aes(x = creatinine_phosphokinase,fill = as.factor(DEATH_EVENT)))+
      geom_density(alpha = 0.2) + theme_classic() + 
      labs(title = "Density Distribution of Creatinine Phosphokinase") + 
      geom_vline(aes(xintercept = mean(creatinine_phosphokinase[DEATH_EVENT == 0])), color = "darkred")+
      geom_vline(aes(xintercept = mean(creatinine_phosphokinase[DEATH_EVENT==1])), color = "darkblue") +
      geom_curve(xend = 540, yend = 0.0009, x = 2000, y = 0.0012, arrow = arrow(length = unit(0.2,"cm")),size = 1,color = "darkred")+
      geom_curve(xend = 670, yend = 0.0013, x = 2400, y = 0.0015, arrow = arrow(length = unit(0.2,"cm")),size = 1, color = "darkblue")+
      geom_text(label = paste0("Mean value for Non-Death Events"), x = 2500, y = 0.00124,size = 2.5)+
      geom_text(label = "Mean value for Death Event", x = 2700, y = 0.00156,size = 2.5)+ 
      annotate("text",x = 5500, y = 0.0005, label = "Mean Creatinine Phosphokinase (mcg/L)\n For Non-Death Events: 540 \n For Death Events: 670")})
  output$plot8<-renderPlot({anaemia = ggplot(heart.dat, aes(x = as.factor(DEATH_EVENT), fill = as.factor(anaemia))) + geom_bar(position = "identity", alpha=0.8) +
    theme_classic()+ scale_x_discrete(labels  = c("No Death","Death")) + labs(subtitle = "Anaemia") +
    scale_fill_manual(values = c("grey","skyblue"), name = "Anaemia",labels = c("negative","positive"))
  
  diabetes = ggplot(heart.dat, aes(x = as.factor(DEATH_EVENT), fill = as.factor(diabetes))) + geom_bar(position = "identity", alpha=0.8) +
    theme_classic()+ scale_x_discrete(labels  = c("No Death","Death")) + labs(subtitle = "Diabetes") +
    scale_fill_manual(values = c("grey","skyblue"), name = "Diabetes",labels = c("negative","positive"))
  
  highbp =  ggplot(heart.dat, aes(x = as.factor(DEATH_EVENT), fill = as.factor(high_blood_pressure))) + geom_bar(position = "identity", alpha=0.8) +
    theme_classic()+ scale_x_discrete(labels  = c("No Death","Death")) + labs(subtitle = "High BP") +
    scale_fill_manual(values = c("grey","skyblue"), name = "High BP",labels = c("negative","positive"))
  
  sex = ggplot(heart.dat, aes(x = as.factor(DEATH_EVENT), fill = as.factor(sex))) + geom_bar(position = "identity", alpha=0.8) +
    theme_classic()+ scale_x_discrete(labels  = c("No Death","Death")) + labs(subtitle = "Sex") +
    scale_fill_manual(values = c("grey","skyblue"), name = "Sex",labels = c("Female","Male"))
  
  smoking = ggplot(heart.dat, aes(x = as.factor(DEATH_EVENT), fill = as.factor(smoking))) + geom_bar(position = "identity", alpha=0.8) +
    theme_classic()+ scale_x_discrete(labels  = c("No Death","Death")) + labs(subtitle = "Smoking") +
    scale_fill_manual(values = c("grey","skyblue"), name = "Smoking",labels = c("negative","positive"))
  grid.arrange(anaemia,diabetes,highbp,smoking,sex,ncol=2)})
  output$plot7<-renderPlotly({ggplot(heart.dat, aes(x=as.factor(smoking),y=ejection_fraction,colour=smoking))+geom_point(position = "jitter")+ggtitle("Smoking Vs Ejection Fraction")})
  output$plot10<-renderPlotly({ggplot(heart.dat, aes(x=as.factor(anaemia), y= creatinine_phosphokinase,
                                                     fill=DEATH_EVENT))+geom_violin()+ stat_summary(aes(x=as.factor(anaemia),
                                                                                                        y= creatinine_phosphokinase,fill=DEATH_EVENT),fun=median, geom='point', col="red",
                                                                                                    size=3)+facet_wrap('DEATH_EVENT')+
      geom_jitter(position=position_jitter(0.1),alpha=0.2)+labs(title = "Creatinine Phosphokinase for Anaemia State Segregated by Death Event",
                                                                x = "Anaemia", y ="Creatinine Phosphokinase")})
  
  
  
}
shinyApp(ui,server)
