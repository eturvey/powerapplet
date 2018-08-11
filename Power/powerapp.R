library(shiny)
## Power
#Version 2.0 For GitHub Purposes
## Explore the factors that effect power
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  headerPanel("Power Applet"),
  sidebarPanel(
    selectInput("select", label = "Sample Type", 
                choices = list("1 Sample Proportion" = 1, "2 Sample Proportion" = 2, "1 Sample Mean" = 3, "2 Sample Mean" = 4), 
                selected = 1),
    conditionalPanel(
      condition = "input.select == 1",
      numericInput("Hyp","Hypothesised Probability of Sucess",min =0, max = 1, value = .5),
      numericInput("n","Sample Size",min =0, value = 5),
      numericInput("reps","Number of Reps",min =0,  value = 100),
      checkboxInput("checkbox", label = "Done?", value = FALSE),
      conditionalPanel(
        condition="input.checkbox == true",
        numericInput("Alt","Alternative Probability of Sucess",min =0, max = 1, value = .6),
        checkboxInput("checkbox2", label = "Done?", value = FALSE),
        conditionalPanel(
          condition="input.checkbox2 == true",
          numericInput("los","Level of Significance",min =0,max=1,  value = .05),
          checkboxInput("checkbox3", label = "Done?", value = FALSE)
        )
    )
    ),

        
      
      
      
      
  
    conditionalPanel(
      condition = "input.select == 2",
      numericInput("Hyp1","Group 1 Hypothesised Probability of Sucess",min =0, max = 1, value = .5),
      numericInput("Hyp2","Group 2 Hypothesised Probability of Sucess",min =0, max = 1, value = .5),
      numericInput("Alt","Alternative Difference in Probabilities (G1-G2)",min =0, max = 1, value = .1),
      numericInput("n","Sample Size of Each Group",min =1, value = 5),
      numericInput("reps","Number of Reps",min =0,  value = 100),
      numericInput("los","Level of Significance",min =0,max=1,  value = .05)
      
    ),
  
  conditionalPanel(
    condition = "input.select == 3",
    numericInput("stde","Standard Deviation of Population", value = 10),
    numericInput("Hypt","Hypothesized Mean", value = 10),
    
    
    numericInput("Altt","Alternative Mean", value = 15),
    numericInput("nt","Sample Size",min =1, value = 5),
    numericInput("repst","Number of Reps",min =0,  value = 100),
    numericInput("lost","Level of Significance",min =0,max=1,  value = .05)
    
  )
),
  mainPanel(
    conditionalPanel(
      
      condition = "input.select == 1",
        plotOutput(outputId="plot1samp",width="100%",height=600)
  
      

    )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.select == 3",
        plotOutput("plot3samp")
        
        
      )),
    mainPanel(
      conditionalPanel(
        condition = "input.select == 2",
        plotOutput("plot2samp")
        
        
      )
  )
)


server <- function(input,output) {
  output$plot1samp <- renderPlot({

    nully = rep(NA, times=input$reps)
    alty = rep(NA, times=input$reps)
    for (i in 1:input$reps) {
      setoffivenull = sample(c(T,F),size=input$n, replace=T, prob=c(input$Hyp,1-input$Hyp))
      setoffivealt = sample(c(T,F),size=input$n, replace=T, prob=c(input$Alt,1-input$Alt))
      totalnull = sum(setoffivenull)
      totalalt = sum(setoffivealt)
      nully[i]=totalnull
      alty[i]=totalalt

    }
    dat <- data.frame(DistributionType = factor(rep(c("null","alt"), each=input$reps)), 
                      num = c(nully,alty))
    temp = rep(NA, times = input$n)
    for (i in 1:input$n) {
      temp[i] <- sum(nully==i)/length(nully)
    }
    min = 0
    backwards = input$n
    while (min < input$los){
      hold = min
      min = min + temp[backwards]
      backwards = backwards-1
    }
    tempalt = rep(NA, times = input$n)
    for (i in 1:input$n) {
      tempalt[i] <- sum(alty==i)/length(alty)
    }
    current = input$n
    altmin = 0
    while (backwards+2 <= current){
      altmin = altmin + tempalt[current]
      current = current-1
    }
    rejreg = backwards+2
    #need to deal with the +2 case
    colorsforoutline <- dat$num == rejreg:length(input$n)
    
    p1 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="null"), fill="green",aes(x=num))+ggtitle("Null Distribution")+xlab("Number of Successes")+ylab("Count")
    
    p3 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="alt"), fill="blue",aes(x=num))+ggtitle("Alternative Distribution")+xlab("Number of Successes")+ylab("Count")
 
    filler <- ggplot()
    p2 <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
      theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
      ggtitle("Combined Distribution")+xlab("Number of Successes")+ylab("Count")+
      geom_vline(xintercept=rejreg, col="red")+geom_rect(aes(xmin=rejreg, xmax=Inf, ymin=0, ymax=Inf, alpha=.3),fill="red")+
      guides(alpha=FALSE)+annotate("text", -Inf, Inf, label = paste("Rejection Region: X >=", rejreg), hjust = 0, vjust = 1,col="red",fontface="bold")+
      annotate("text",  -Inf, Inf, label = paste("Hypothesized Proportion of Reps:", hold*input$reps, "/", input$reps, "=", round(hold,2)), hjust = 0, vjust = 3,col="red",fill="blue",fontface="bold")+
      annotate("text", -Inf, Inf, label = paste("Alternaitve Proportion of Reps:", altmin*input$reps, "/", input$reps, "=", round(altmin,2) ), hjust = 0, vjust = 5,col="red",fontface="bold")

    lay <- rbind(c(1,1,1,1,1,1,2,2,2,2,2,2),
                 c(1,1,1,1,1,1,2,2,2,2,2,2),
                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                 c(3,3,3,3,3,3,3,3,3,3,3,3))
    if(input$checkbox == T){
      grid.arrange(p1,filler,filler,layout_matrix=lay)
    }
    if(input$checkbox2 == T){
      grid.arrange(p1,p3,filler,layout_matrix=lay)
    }
    if(input$checkbox3 == T){
      grid.arrange(p1,p3,p2,layout_matrix=lay)
    }
    if(input$checkbox == F){
      grid.arrange(filler,filler,filler,layout_matrix=lay)
    }
 
  
  
  
  })
  output$plot2samp <- renderPlot({
    nully = rep(NA, times=input$reps)
    alty = rep(NA, times=input$reps)
    for (i in 1:input$reps) {
      setofnull1 = sample(c(T,F),size=input$n, replace=T, prob=c(input$Hyp,1-input$Hyp1))
      setofnull2 = sample(c(T,F),size=input$n, replace=T, prob=c(input$Hyp,1-input$Hyp2))
      setoffivealt = sample(c(T,F),size=input$n, replace=T, prob=c(input$Alt,1-input$Alt))
      totalnull = sum(setofnull1)+sum(setofnull2)
      totalalt = sum(setoffivealt)
      nully[i]=totalnull
      alty[i]=totalalt
    }
    
  })
  output$plot3samp <- renderPlot({
    nully = rep(NA, times=input$repst)
    alty = rep(NA, times=input$repst)
    for (i in 1:input$repst) {
      setofnull = rnorm(input$nt,input$Hypt,input$stde)
      setofalt = rnorm(input$nt,input$Altt,input$stde)
      totalnull = mean(setofnull)
      totalalt = mean(setofalt)
      nully[i]=totalnull
      alty[i]=totalalt
    }
    dat <- data.frame(DistributionType = factor(rep(c("null","alt"), each=input$repst)), 
                      num = c(nully,alty))
    ordernull <- sort(nully)
    orderalt <- sort(alty)
    index = input$repst+1
    alpha = 0
    while (alpha <= input$lost){
      alpha=alpha+(1/input$repst)
      index = index-1
    }
    holder = (input$repst-index)/input$repst
    rejreggreateq = ordernull[index+1]
    altind = input$repst
    while(ordernull[index+1]<=orderalt[altind] & altind>1){
      altind = altind-1
    }
    altans = orderalt[altind]
    holderalt = (input$repst-altind+1)/input$repst
    ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("grey36","white"))+
      theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid")) + ggtitle("Distribution")+xlab("Number of Successes")+ylab("Count")+
      geom_vline(xintercept=rejreggreateq, col="red")+geom_rect(aes(xmin=rejreggreateq, xmax=Inf, ymin=0, ymax=Inf, alpha=.3),fill="red") +
      guides(alpha=FALSE)+annotate("text", -Inf, Inf, label = paste("Rejection Region: X >=", round(rejreggreateq,2)), hjust = 0, vjust = 1,col="red")+
      annotate("text", -Inf, Inf, label = paste("Hypothesized Proportion of Reps:", input$repst-index, "/", input$repst, "=", round(holder,2)), hjust = 0, vjust = 3,col="red")+
      annotate("text", -Inf, Inf, label = paste("Alternaitve Proportion of Reps:", input$repst-altind+1, "/", input$repst, "=", round(holderalt,2)), hjust = 0, vjust = 5,col="red")
    
     
    
    
  })
}



shinyApp(ui = ui, server = server)
