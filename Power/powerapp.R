library(shiny)
## Power
#Version 2.0 For GitHub Purposes
## Explore the factors that effect power
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  headerPanel("Power Applet"),
  sidebarPanel(
    
    #Start by choosing type of test
    
    selectInput("select", label = "Sample Type", 
                choices = list("1 Sample Proportion" = 1, "2 Sample Proportion" = 2, "1 Sample Mean" = 3, "2 Sample Mean" = 4), 
                selected = 1),
    conditionalPanel(
      condition = "input.select == 1",
      
      #Initial Options if you choose 1 Samp Prop
      
      numericInput("Hyp","Hypothesised Probability of Sucess",min =0, max = 1, value = .5),
      numericInput("n","Sample Size",min =0, value = 5),
      numericInput("reps","Number of Reps",min =0,  value = 100),
      checkboxInput("checkbox", label = "Done?", value = FALSE),
      conditionalPanel(
        
        #After determaining null properties, ask about alt
        
        condition="input.checkbox == true",
        numericInput("Alt","Alternative Probability of Sucess",min =0, max = 1, value = .6),
        checkboxInput("checkbox2", label = "Done?", value = FALSE),
        conditionalPanel(
          
          #once alt is determained, ask about test (direction and LOS)
          
          condition="input.checkbox2 == true",
          selectInput("side", label = "Direction", 
                      choices = list("Greater" = 1, "Less" = 2, "Two Tailed" = 3), 
                      selected = 1),
          numericInput("los","Level of Significance",min =0,max=1,  value = .05),
          checkboxInput("checkbox3", label = "Done?", value = FALSE),
          radioButtons("radio",label= "Number/Prop of Success",choices = list("Number of Successes" = 1,"Proportion of Successes" = 2), selected = 1)
          
        )
    )
    ),

        
      
      
      
      
  
    conditionalPanel(
      condition = "input.select == 2",
      
      #Options for 2 sample Prop
      numericInput("Hyp1","Hypothesised Probability of Sucess Group 1",min =0, max = 1, value = .5),
      numericInput("Hyp2","Hypothesised Probability of Sucess Group 2",min =0, max = 1, value = .5),
      numericInput("n2","Sample Size for Each Group",min =0, value = 5),
      numericInput("reps2","Number of Reps",min =0,  value = 100),
      checkboxInput("checkboxp1", label = "Done?", value = FALSE),
      conditionalPanel(
        
        #After determaining null properties, ask about alt
        
        condition="input.checkboxp1 == true",
        numericInput("alt1","Alternative Probability of Sucess for Group 1",min =0, max = 1, value = .6),
        numericInput("alt2","Alternative Probability of Sucess for Group 2",min =0, max = 1, value = .6),
        
        checkboxInput("checkboxp2", label = "Done?", value = FALSE),
        conditionalPanel(
          
          #once alt is determained, ask about test (direction and LOS)
          
          condition="input.checkboxp2 == true",
          selectInput("side2", label = "Direction", 
                      choices = list("Greater" = 1, "Less" = 2, "Two Tailed" = 3), 
                      selected = 1),
          numericInput("los2","Level of Significance",min =0,max=1,  value = .05),
          checkboxInput("checkboxp3", label = "Done?", value = FALSE),
          radioButtons("radio2",label= "Number/Prop of Success",choices = list("Number of Successes" = 1,"Proportion of Successes" = 2), selected = 1)
          
      
      
    ))),
  
  conditionalPanel(
    
    #Options for 1 Samp Mean
    
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
      
      #Print this graph if it is a 1 Samp Prop, make it fill screen
      
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
  output$selected_var <- renderText({
    "Print this shit"
  })

  output$plot1samp <- renderPlot({
    
    #Plot for 1 samp Prop
    
    #Create vectors the length of repetitions
    nully = rep(NA, times=input$reps)
    alty = rep(NA, times=input$reps)
    
    #Fill these vectors with the sum of T's in samples of T/F (with specified prob) of size n (specified)
    for (i in 1:input$reps) {
      setoffivenull = sample(c(T,F),size=input$n, replace=T, prob=c(input$Hyp,1-input$Hyp))
      setoffivealt = sample(c(T,F),size=input$n, replace=T, prob=c(input$Alt,1-input$Alt))
      totalnull = sum(setoffivenull)
      totalalt = sum(setoffivealt)
      nully[i]=totalnull
      alty[i]=totalalt

    }
    
    #Creates a data frame with all of the values; labeled with null or alt
    dat <- data.frame(DistributionType = factor(rep(c("null","alt"), each=input$reps)), 
                      num = c(nully,alty))
    
    #creates a vector of the probability that 0:n number of successes will occur under null and then alt
    temp = rep(NA, times = input$n+1)
    for (i in 1:input$n+1) {
      temp[i] <- sum(nully==i-1)/length(nully)
    }
    temp[1]=1-sum(temp[1:input$n+1])
    
    tempalt = rep(NA, times = input$n+1)
    for (i in 1:input$n+1) {
      tempalt[i] <- sum(alty==i-1)/length(alty)
    }
    tempalt[1]=1-sum(tempalt[1:input$n+1])
  
  #do this if it is a greater than test
    
    if(input$side == 1){
      #adds up these probabilities until we reach a number greater than LOS (starting from the end) - if first number is above LOS use n+1
      min = 0
      backwards = input$n + 1
      if(temp[backwards] > input$los){
        print('ehlllp')
        min = 0
        backwards = input$n + 1
      }else{
        print('hellp')
        while (min < input$los){
          holdlos = min
          holdpos = backwards
          min = min + temp[backwards]
          backwards = backwards-1
        }
        min = holdlos
        backwards = holdpos 
      }
      
      #add up probabilities under alt until rejection region is reached
      
      current = input$n+1
      altmin = 0
      while (backwards+1 <= current){

        altmin = altmin + tempalt[current]
        current = current-1
      }
      rejreg = backwards
  
  
   #create three stages of graphs: 1. just the null, 2. null and alt, 3. Combined dist with info about the rejection region   
      if(input$radio==1){
        p1 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="null"), fill="green",aes(x=num))+ggtitle("Null Distribution")+xlab("Number of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="null")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="null")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
    
        p3 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="alt"), fill="blue",aes(x=num))+ggtitle("Alternative Distribution")+xlab("Number of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="alt")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="alt")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
        filler <- ggplot()
        words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X >=", rejreg),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", min*input$reps, "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", altmin*input$reps, "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())
        p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
          theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
          ggtitle("Combined Distribution")+xlab("Number of Successes")+ylab("Count")+
          geom_vline(xintercept=rejreg, col="red")+geom_rect(aes(xmin=rejreg, xmax=Inf, ymin=0, ymax=Inf, alpha=.3),fill="red")+
          guides(alpha=FALSE)
      }else{
        dat$num = dat$num/input$n
        p1 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="null"), fill="green",aes(x=num))+ggtitle("Null Distribution")+xlab("Proportion of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="null")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="null")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
        
        p3 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="alt"), fill="blue",aes(x=num))+ggtitle("Alternative Distribution")+xlab("Proportion of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="alt")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="alt")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
        words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X >=", rejreg/input$n),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", min*input$reps, "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", altmin*input$reps, "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())
        filler <- ggplot()
        p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
          theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
          ggtitle("Combined Distribution")+xlab("Proportion of Successes")+ylab("Count")+
          geom_vline(xintercept=rejreg/input$n, col="red")+geom_rect(aes(xmin=rejreg/input$n, xmax=Inf, ymin=0, ymax=Inf, alpha=.3),fill="red")+
          guides(alpha=FALSE)

        
         }
##   
#do this if less than test
##
##
      
    }else if (input$side == 2){

      #adds up these probabilities until we reach a number greater than LOS (starting from the beginning) - if first number is above LOS use 0
      min = 0
      backwards = 1
      if(temp[backwards] > input$los){
        min = 0
        backwards = -1
      }else{
 
        while (min < input$los){
          holdlos = min
          holdpos = backwards
          min = min + temp[backwards]
          backwards = backwards+1
        }
        min = holdlos
        backwards = holdpos - 2
      }
      
      #add up probabilities under alt until rejection region is reached
      
      current = 1
      altmin = 0
      while (backwards+1 >= current){
        
        altmin = altmin + tempalt[current]
        current = current+1
      }
      rejreg = backwards
      
      
      #create three stages of graphs: 1. just the null, 2. null and alt, 3. Combined dist with info about the rejection region   
      
      
      if(input$radio == 1){
        p1 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="null"), fill="green",aes(x=num))+ggtitle("Null Distribution")+xlab("Number of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="null")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="null")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
        
        p3 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="alt"), fill="blue",aes(x=num))+ggtitle("Alternative Distribution")+xlab("Number of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="alt")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="alt")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
        
        filler <- ggplot()
        
        if(rejreg == -1){
        words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X <", 0),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", round(min*input$reps,2), "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", round(altmin*input$reps,2), "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())
        p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
          theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
          ggtitle("Combined Distribution")+xlab("Number of Successes")+ylab("Count")+
          geom_vline(xintercept=0, col="red")+geom_rect(aes(xmin=-Inf, xmax=0, ymin=0, ymax=Inf, alpha=.3),fill="red")+
          guides(alpha=FALSE)
        }else{
          words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X <=", rejreg),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", round(min*input$reps,2), "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", round(altmin*input$reps,2), "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank())
          p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
            theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
            ggtitle("Combined Distribution")+xlab("Number of Successes")+ylab("Count")+
            geom_vline(xintercept=rejreg, col="red")+geom_rect(aes(xmin=-Inf, xmax=rejreg, ymin=0, ymax=Inf, alpha=.3),fill="red")+
            guides(alpha=FALSE)
        }
      }else{
        dat$num = dat$num/input$n
        p1 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="null"), fill="green",aes(x=num))+ggtitle("Null Distribution")+xlab("Proportion of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="null")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="null")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
        filler<-ggplot()
        p3 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="alt"), fill="blue",aes(x=num))+ggtitle("Alternative Distribution")+xlab("Number of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="alt")$num)), hjust = 0, vjust = 1,fontface="bold")+
          annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="alt")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
        if(rejreg == -1){
          words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X <", 0),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", round(min*input$reps,2), "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", round(altmin*input$reps,2), "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank())
          p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
            theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
            ggtitle("Combined Distribution")+xlab("Number of Successes")+ylab("Count")+
            geom_vline(xintercept=0, col="red")+geom_rect(aes(xmin=-Inf, xmax=0, ymin=0, ymax=Inf, alpha=.3),fill="red")+
            guides(alpha=FALSE)
        }else{
          words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X <=", rejreg),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", round(min*input$reps,2), "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", round(altmin*input$reps,2), "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank())
          p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
            theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
            ggtitle("Combined Distribution")+xlab("Number of Successes")+ylab("Count")+
            geom_vline(xintercept=rejreg, col="red")+geom_rect(aes(xmin=-Inf, xmax=rejreg, ymin=0, ymax=Inf, alpha=.3),fill="red")+
            guides(alpha=FALSE)
        }
      }
  #do this if two tailed test
    }else if(input$side==3){
      min = 0
      front = 1
      end = input$n+1
      if(temp[front]+temp[end] > input$los){
        front = -1
        end = input$n+1
      }else{
        while (min < input$los){
          holdlos = min
          holdfro = front
          holden = end
          min = min + temp[end]+temp[front]
          front = front+1
          end=end-1
        }
        min = holdlos
        end = holden
        front = holdfro -2
      }
      
      #add up probabilities under alt until rejection region is reached
      
      currentfro = 1
      currentend = input$n+1
      altmin = 0
      while (front >= currentfro-1 & end <= currentend-1){
        
        altmin = altmin + tempalt[currentfro] + tempalt[currentend]
        currentfro = currentfro+1
        currentend = currentend-1
      }

      rejregfro = front
      rejregend = end
      
      
      #create three stages of graphs: 1. just the null, 2. null and alt, 3. Combined dist with info about the rejection region   
     if(input$radio==1){ 
      p1 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="null"), fill="green",aes(x=num))+ggtitle("Null Distribution")+xlab("Number of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="null")$num)), hjust = 0, vjust = 1,fontface="bold")+
        annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="null")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
      
      p3 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="alt"), fill="blue",aes(x=num))+ggtitle("Alternative Distribution")+xlab("Number of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="alt")$num)), hjust = 0, vjust = 1,fontface="bold")+
        annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="alt")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
      
      words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X <=", rejregfro, "and X >=", rejregend),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", min*input$reps, "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", altmin*input$reps, "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
      
      filler <- ggplot()
      p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
        theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
        ggtitle("Combined Distribution")+xlab("Number of Successes")+ylab("Count")+
        geom_vline(xintercept=rejregfro, col="red")+geom_vline(xintercept=rejregend, col="red")+geom_rect(aes(xmin=-Inf, xmax=rejregfro, ymin=0, ymax=Inf, alpha=.3),fill="red")+geom_rect(aes(xmin=rejregend, xmax=Inf, ymin=0, ymax=Inf, alpha=.3),fill="red")+
        guides(alpha=FALSE)
     }else{
       dat$num = dat$num/input$n
       p1 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="null"), fill="green",aes(x=num))+ggtitle("Null Distribution")+xlab("Proportion of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="null")$num)), hjust = 0, vjust = 1,fontface="bold")+
         annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="null")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
       
       p3 <- ggplot()+geom_histogram(data=subset(dat,DistributionType=="alt"), fill="blue",aes(x=num))+ggtitle("Alternative Distribution")+xlab("Proportion of Successes")+ylab("Count")+annotate("text", -Inf, Inf, label = paste("Mean:", mean(subset(dat,DistributionType=="alt")$num)), hjust = 0, vjust = 1,fontface="bold")+
         annotate("text",  -Inf, Inf, label = paste("SD:", round(sd(subset(dat,DistributionType=="alt")$num),2)), hjust = 0, vjust = 3,fill="blue",fontface="bold")
       
       words <- qplot(1:10,1:10,geom="blank")+annotate("text", x=5, y=9, label= paste("Rejection Region: X <=", rejregfro/input$n, "and X >=", rejregend/input$n),col="red",fontface="bold")+annotate("text",x=5,y=7,label=paste("Hypothesized Proportion of Reps:", min*input$reps, "/", input$reps, "=", round(min,2)),col="red",fontface="bold")+annotate("text",x=5,y=5,label=paste("Alternaitve Proportion of Reps:", altmin*input$reps, "/", input$reps, "=", round(altmin,2)),col="red",fontface="bold")+
         theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
       
       filler <- ggplot()
       p2g <- ggplot()+geom_histogram(data=dat, aes(x=num,fill=DistributionType))+scale_fill_manual(values=c("blue","green"))+
         theme(legend.background = element_rect(fill="grey90",size=0.5, linetype="solid"))+ 
         ggtitle("Combined Distribution")+xlab("Proportion of Successes")+ylab("Count")+
         geom_vline(xintercept=rejregfro/input$n, col="red")+geom_vline(xintercept=rejregend/input$n, col="red")+geom_rect(aes(xmin=-Inf, xmax=rejregfro/input$n, ymin=0, ymax=Inf, alpha=.3),fill="red")+geom_rect(aes(xmin=rejregend/input$n, xmax=Inf, ymin=0, ymax=Inf, alpha=.3),fill="red")+
         guides(alpha=FALSE)
     }
    }
    
  #sets up the layout of the graphs
    lay <- rbind(c(1,1,1,1,1,1,2,2,2,2,2,2),
                 c(1,1,1,1,1,1,2,2,2,2,2,2),
                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                 c(4,4,4,4,4,4,4,4,4,4,4,4))
    
    #puts certain graphs up if certain checkbox's are marked

    if(input$checkbox == T){
      grid.arrange(p1,filler,filler,filler,layout_matrix=lay)
    }
    if(input$checkbox2 == T){
      grid.arrange(p1,p3,filler,filler,layout_matrix=lay)
    }
    if(input$checkbox3 == T){
      if(input$side == 1){
        grid.arrange(p1,p3,p2g,words,layout_matrix=lay)
      } else if(input$side == 2){
        grid.arrange(p1,p3,p2g,words,layout_matrix=lay)
      } else if(input$side == 3){
        grid.arrange(p1,p3,p2g,words,layout_matrix=lay)
      }
    }
    if(input$checkbox == F){
      grid.arrange(filler,filler,filler,filler,layout_matrix=lay)
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
