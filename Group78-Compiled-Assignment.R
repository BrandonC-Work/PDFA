#-------------Install and load packages--------------------
#define packages to install
packageNames <- c('ggplot2', 'dplyr', 'lattice', 'treemapify')
#------------------------------------------------
is_installed <- function(pkg){
  return(require(pkg, character.only = TRUE))
}

install_missing <- function(pkg){
  install.packages(pkg, dependencies = TRUE)
}

for(pkg in packageNames){
  if(!is_installed(pkg)){
    install_missing(pkg)
  }
  library(pkg, character.only = TRUE)
}
#---------------------------------------------------------
#-----Generate a report of the loaded pacakages-----------
generate_report <- function(){
  cat("The following packages have been loaded:", "\n")
  cat("\n")
  installed_packages <- search()
  for(pkg in installed_packages){
    if(grepl("package:", pkg)){
      cat(sub("package:", "", pkg), "\n")
    }
  }
}
# To check packages that have been loaded
generate_report()
#----------------------------------------------------------


# import csv file
# assignment csv file #To be changed according to user filepath
assg_data = read.csv("C:\\Users\\brand\\OneDrive\\Desktop\\PFDA\\PFDA-Assignment\\student_prediction.csv", header = TRUE)

# csv file for testing
#assg_data = read.csv("C:\\Users\\brand\\OneDrive\\Desktop\\PFDA\\PFDA-Assignment\\test.csv", header = TRUE)
#----------------------------------------------------------------------------------------------------------------------------
options(max.print = 999999)

# cleaning 
# check the current names
names(assg_data)

# change column names
names(assg_data) = c("StuID","StuAge","StuGen","GradHS_Type","Scholarship_Type","Add_Work",
                     "Art_Sport","StuPartner","StuSal","StuTransport","StuAccomodation",
                     "StuMom_Edu","StuDad_Edu","NoSibilings","Parents_Status",
                     "StuMom_Job","StuDad_Job","W_StudyHrs","ReadFreq_XSci","ReadFreq_Sci",
                     "Att_Dept","Impact_Suc","Att_Class","Prep_Ex1","Prep_Ex2","TakingNotes",
                     "ListenClass","Discuss_Imp","Flip_Class","CGPA_LSem","Exp_CGPA","CourseID","Grade")
#assg_data

# delete rows (remove null rows - na.omit())
assg_data <- na.omit(assg_data)

# exclude duplicates
assg_data <- unique(assg_data)  #distinct()

head(assg_data,10)

##Attributes
#---------------------------------------------------------------------------------------------------
#Student Age
assg_data$StuAge<- factor(assg_data$StuAge,levels=c(1,2,3), labels=c("18-21","22-25","Above 26"))

#Student Gender
assg_data$StuGen<- factor(assg_data$StuGen,levels=c(1,2), labels=c("Female","Male"))

#Student Graduated Highschool Type
assg_data$GradHS_Type<- factor(assg_data$GradHS_Type,levels=c(1,2,3), labels=c("Private","State","Other"))

#Scholarship Type
assg_data$Scholarship_Type<- factor(assg_data$Scholarship_Type,levels=c(1,2,3,4,5), labels=c("None","25%","50%","75%","Full"))

#Additional work
assg_data$Add_Work<- factor(assg_data$Add_Work,levels=c(1,2), labels=c("Yes","No"))

#Regular artistic or sports activity
assg_data$Art_Sport<- factor(assg_data$Art_Sport,levels=c(1,2), labels=c("Yes","No"))

#Do you have a partner
assg_data$StuPartner<- factor(assg_data$StuPartner,levels=c(1,2), labels=c("Yes","No"))

#Total salary if available 
assg_data$StuSal<- factor(assg_data$StuSal,levels=c(1,2,3,4,5), labels=c("USD 135-200", "USD 201-270", "USD 271-340","USD 341-410","Above 410"))

#Transportation to the university
assg_data$StuTransport<- factor(assg_data$StuTransport,levels=c(1,2,3,4), labels=c("Bus", "Private car/taxi", "Bicycle", "Other"))

#Accommodation type in Cyprus
assg_data$StuAccomodation<- factor(assg_data$StuAccomodation,levels=c(1,2,3,4), labels=c("Rental", "Dormitory", "With family", "Other"))

#Mother's education
assg_data$StuMom_Edu<- factor(assg_data$StuMom_Edu,levels=c(1,2,3,4,5,6), labels=c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D."))

#Father's education
assg_data$StuDad_Edu<- factor(assg_data$StuDad_Edu,levels=c(1,2,3,4,5,6), labels=c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D."))

# Number of sisters/brothers (if available)
assg_data$NoSibilings<- factor(assg_data$NoSibilings,levels=c(1,2,3,4,5), labels=c("1", "2", "3", "4", "5 or above"))

#Parental status
assg_data$Parents_Status<- factor(assg_data$Parents_Status,levels=c(1,2,3), labels=c("Married", "Divorced", "Died - One or Both"))

#Mother's occupation
assg_data$StuMom_Job<- factor(assg_data$StuMom_Job,levels=c(1,2,3,4,5,6), labels=c("Retired","Housewife","Government Officer","Private Sector Employee", "Self-Employment","Other"))

#Father's occupation
assg_data$StuDad_Job<- factor(assg_data$StuDad_Job,levels=c(1,2,3,4,5), labels=c("Retired","Government Officer","Private Sector Employee", "Self-Employment","Other"))

#Weekly study hours
assg_data$W_StudyHrs<- factor(assg_data$W_StudyHrs,levels=c(1,2,3,4,5), labels=c("None","<5 Hours","6-10 Hours","11-20 Hours","More than 20 Hours"))

#Reading frequency (non-scientific books/journals)
assg_data$ReadFreq_XSci<- factor(assg_data$ReadFreq_XSci,levels=c(1,2,3), labels=c("None","Sometimes","Often"))

#Reading frequency (scientific books/journals)
assg_data$ReadFreq_Sci<- factor(assg_data$ReadFreq_Sci,levels=c(1,2,3), labels=c("None","Sometimes","Often"))

#Attendance to the seminars/conferences related to the department
assg_data$Att_Dept<- factor(assg_data$Att_Dept,levels=c(1,2), labels=c("Yes","No"))

#Impact of your projects/activities on your success
assg_data$Impact_Suc<- factor(assg_data$Impact_Suc,levels=c(1,2,3), labels=c("Positive","Negative","Neutral"))

# Attendance to classes
assg_data$Att_Class<- factor(assg_data$Att_Class,levels=c(1,2,3), labels=c("Always","Sometimes","Never"))

#Preparation to midterm exams
assg_data$Prep_Ex1<- factor(assg_data$Prep_Ex1,levels=c(1,2,3), labels=c("Alone","With Friends","Not Applicable"))

#Preparation to midterm exams
assg_data$Prep_Ex2<- factor(assg_data$Prep_Ex2,levels=c(1,2,3), labels=c("Closest date to the exam","Regularly during the semester","Never"))

#Taking notes in classes
assg_data$TakingNotes<- factor(assg_data$TakingNotes,levels=c(1,2,3), labels=c("Never","Sometimes","Always"))

#Listening in classes
assg_data$ListenClass<- factor(assg_data$ListenClass,levels=c(1,2,3), labels=c("Never","Sometimes","Always"))

#Discussion improves my interest and success in the course
assg_data$Discuss_Imp<- factor(assg_data$Discuss_Imp,levels=c(1,2,3), labels=c("Never","Sometimes","Always"))

#Flip-classroom
assg_data$Flip_Class<- factor(assg_data$Flip_Class,levels=c(1,2,3), labels=c("Not Useful","Useful","Not Applicable"))

#Cumulative grade point average in the last semester (/4.00)
assg_data$CGPA_LSem<- factor(assg_data$CGPA_LSem,levels=c(1,2,3,4,5), labels=c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49"))

#Expected Cumulative grade point average in the graduation (/4.00)
assg_data$Exp_CGPA<- factor(assg_data$Exp_CGPA,levels=c(1,2,3,4,5), labels=c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49"))

#OUTPUT Grade 
assg_data$Grade<- factor(assg_data$Grade,levels=c(0,1,2,3,4,5,6,7), labels=c("Fail","DD","DC","CC","CB","BB","BA","AA"))

#---------------------------------------------------------------------------------------------------
### Only used to check if there are any errors in the data set. ###
##Attributes with ifelse condition.
#---------------------------------------------------------------------------------------------------
#Student Age
assg_data$StuAge <- factor(ifelse(assg_data$StuAge %in% 1:3, assg_data$StuAge, "error"), 
                           levels = c(1, 2, 3, "error"), labels = c("18-21", "22-25", "Above 26", "error"))
#Student Gender
assg_data$StuGen <- factor(ifelse(assg_data$StuGen %in% 1:2, assg_data$StuGen, "error"), 
                           levels = c(1, 2, "error"), labels = c("Female", "Male", "error"))
#Student Graduated Highschool Type
assg_data$GradHS_Type <- factor(ifelse(assg_data$GradHS_Type %in% 1:3, assg_data$GradHS_Type, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Private", "State","Other","error"))
#Scholarship Type
assg_data$Scholarship_Type <- factor(ifelse(assg_data$Scholarship_Type %in% 1:5, assg_data$Scholarship_Type, "error"), 
                                     levels = c(1, 2, 3, 4, 5, "error"), labels = c("None","25%","50%","75%","Full","error"))
#Additional work
assg_data$Add_Work <- factor(ifelse(assg_data$Add_Work %in% 1:2, assg_data$Add_Work, "error"), 
                             levels = c(1, 2, "error"), labels = c("Yes", "No","error"))
#Regular artistic or sports activity
assg_data$Art_Sport <- factor(ifelse(assg_data$Art_Sport %in% 1:2, assg_data$Art_Sport, "error"), 
                              levels = c(1, 2, "error"), labels = c("Yes", "No","error"))
#Do you have a partner
assg_data$StuPartner <- factor(ifelse(assg_data$StuPartner %in% 1:2, assg_data$StuPartner, "error"), 
                               levels = c(1, 2, "error"), labels = c("Yes", "No","error"))
#Total salary if available 
assg_data$StuSal <- factor(ifelse(assg_data$StuSal %in% 1:5, assg_data$StuSal, "error"), 
                           levels = c(1, 2, 3,4, 5, "error"), labels = c("USD 135-200", "USD 201-270", "USD 271-340","USD 341-410","Above 410","error"))
#Transportation to the university
assg_data$StuTransport <- factor(ifelse(assg_data$StuTransport %in% 1:4, assg_data$StuTransport, "error"), 
                                 levels = c(1, 2, 3, 4, "error"), labels = c("Bus", "Private car/taxi", "Bicycle", "Other","error"))
#Accommodation type in Cyprus
assg_data$StuAccomodation <- factor(ifelse(assg_data$StuAccomodation %in% 1:4, assg_data$StuAccomodation, "error"), 
                                    levels = c(1, 2, 3, 4, "error"), labels = c("Rental", "Dormitory", "With family", "Other","error"))
#Mother's education
assg_data$StuMom_Edu <- factor(ifelse(assg_data$StuMom_Edu %in% 1:6, assg_data$StuMom_Edu, "error"), 
                               levels = c(1, 2, 3, 4, 5, 6, "error"), labels = c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D.","error"))
#Father's education
assg_data$StuDad_Edu <- factor(ifelse(assg_data$StuDad_Edu %in% 1:6, assg_data$StuDad_Edu, "error"), 
                               levels = c(1, 2, 3, 4, 5, 6, "error"), labels = c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D.","error"))

# Number of sisters/brothers (if available)
assg_data$NoSibilings <- factor(ifelse(assg_data$NoSibilings %in% 1:5, assg_data$NoSibilings, "error"), 
                                levels = c(1, 2, 3, 4, 5, "error"), labels = c("1", "2", "3", "4", "5 or above","error"))

#Parental status
assg_data$Parents_Status <- factor(ifelse(assg_data$Parents_Status %in% 1:3, assg_data$Parents_Status, "error"), 
                                   levels = c(1, 2, 3, "error"), labels = c("Married", "Divorced", "Died - One or Both","error"))

#Mother's occupation
assg_data$StuMom_Job <- factor(ifelse(assg_data$StuMom_Job %in% 1:6, assg_data$StuMom_Job, "error"), 
                               levels = c(1, 2, 3, 4, 5, 6, "error"), labels = c("Retired","Housewife","Government Officer","Private Sector Employee", "Self-Employment","Other","error"))

#Father's occupation
assg_data$StuDad_Job <- factor(ifelse(assg_data$StuDad_Job %in% 1:5, assg_data$StuDad_Job, "error"), 
                               levels = c(1, 2, 3, 4, 5, "error"), labels = c("Retired","Government Officer","Private Sector Employee", "Self-Employment","Other","error"))

#Weekly study hours
assg_data$W_StudyHrs <- factor(ifelse(assg_data$W_StudyHrs %in% 1:5, assg_data$W_StudyHrs, "error"), 
                               levels = c(1, 2, 3, 4, 5, "error"), labels = c("None","<5 Hours","6-10 Hours","11-20 Hours","More than 20 Hours","error"))

#Reading frequency (non-scientific books/journals)
assg_data$ReadFreq_XSci <- factor(ifelse(assg_data$ReadFreq_XSci %in% 1:3, assg_data$ReadFreq_XSci, "error"), 
                                  levels = c(1, 2, 3, "error"), labels = c("None","Sometimes","Often","error"))

#Reading frequency (scientific books/journals)
assg_data$ReadFreq_Sci <- factor(ifelse(assg_data$ReadFreq_Sci %in% 1:3, assg_data$ReadFreq_Sci, "error"), 
                                 levels = c(1, 2, 3, "error"), labels = c("None","Sometimes","Often","error"))
#Attendance to the seminars/conferences related to the department
assg_data$Att_Dept <- factor(ifelse(assg_data$Att_Dept %in% 1:2, assg_data$Att_Dept, "error"), 
                             levels = c(1, 2, "error"), labels = c("Yes","No","error"))

#Impact of your projects/activities on your success
assg_data$Impact_Suc <- factor(ifelse(assg_data$Impact_Suc %in% 1:3, assg_data$Impact_Suc, "error"), 
                               levels = c(1, 2, 3, "error"), labels = c("Positive","Negative","Neutral","error"))

# Attendance to classes
assg_data$Att_Class <- factor(ifelse(assg_data$Att_Class %in% 1:3, assg_data$Att_Class, "error"), 
                              levels = c(1, 2, 3, "error"), labels = c("Always","Sometimes","Never","error"))

#Preparation to midterm exams
assg_data$Prep_Ex1 <- factor(ifelse(assg_data$Prep_Ex1 %in% 1:3, assg_data$Prep_Ex1, "error"), 
                             levels = c(1, 2, 3, "error"), labels = c("Alone","With Friends","Not Applicable","error"))

#Preparation to midterm exams
assg_data$Prep_Ex2 <- factor(ifelse(assg_data$Prep_Ex2 %in% 1:3, assg_data$Prep_Ex2, "error"), 
                             levels = c(1, 2, 3, "error"), labels = c("Closest date to the exam","Regularly during the semester","Never","error"))

#Taking notes in classes
assg_data$TakingNotes <- factor(ifelse(assg_data$TakingNotes %in% 1:3, assg_data$TakingNotes, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Never","Sometimes","Always","error"))

#Listening in classes
assg_data$ListenClass <- factor(ifelse(assg_data$ListenClass %in% 1:3, assg_data$ListenClass, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Never","Sometimes","Always","error"))

#Discussion improves my interest and success in the course
assg_data$Discuss_Imp <- factor(ifelse(assg_data$Discuss_Imp %in% 1:3, assg_data$Discuss_Imp, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Never","Sometimes","Always","error"))

#Flip-classroom
assg_data$Flip_Class <- factor(ifelse(assg_data$Flip_Class %in% 1:3, assg_data$Flip_Class, "error"), 
                               levels = c(1, 2, 3, "error"), labels = c("Not Useful","Useful","Not Applicable","error"))

#Cumulative grade point average in the last semester (/4.00)
assg_data$CGPA_LSem <- factor(ifelse(assg_data$CGPA_LSem %in% 1:5, assg_data$Flip_Class, "error"), 
                              levels = c(1, 2, 3, 4, 5, "error"), labels = c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49","error"))

#Expected Cumulative grade point average in the graduation (/4.00)
assg_data$Exp_CGPA <- factor(ifelse(assg_data$Exp_CGPA %in% 1:5, assg_data$Exp_CGPA, "error"), 
                             levels = c(1, 2, 3, 4, 5, "error"), labels = c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49","error"))

#OUTPUT Grade 
assg_data$Grade <- factor(ifelse(assg_data$Grade %in% 0:7, assg_data$Grade, "error"), 
                          levels = c(0, 1, 2, 3, 4, 5, 6, 7, "error"), labels = c("Fail","DD","DC","CC","CB","BB","BA","AA","error"))

#---------------------------------------------------------------------------------------------------
####################### To analyse if having art or sports activity will affect the student's attendance to classes  ###########################
# Objective 1 Analysis - Ahmed Mirhaan Ibrahim Rushdee TP071328
#Analysis 1 - Art or sport activities against class attendance
#######################################################################################################

#Make subset containing the data for students having regular artistic or sports activities
Art_Sport_Yes_data <- subset(assg_data,Art_Sport == "Yes") %>% arrange(Att_Class)

#displays which is the most common response for regular artistic or sports activities
names(which.max(summary(Art_Sport_Yes_data$Att_Class)))

#viewing the number of students who does have Regular artistic or sports activities
nrow(Art_Sport_Yes_data)

#viewing the summary and % of class attendance for the students that has those activities
summary(Art_Sport_Yes_data$Att_Class)

Art_Sport_Yes_data %>% group_by(Att_Class) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)

#display lollipop graph for the summary of class attendance  
result <- data.frame(
  Attendance = c("Always", "Sometimes", "Never"),
  Responses = c(
    nrow(Art_Sport_Yes_data[Art_Sport_Yes_data$Att_Class=="Always",]),
    nrow(Art_Sport_Yes_data[Art_Sport_Yes_data$Att_Class=="Sometimes",]),
    nrow(Art_Sport_Yes_data[Art_Sport_Yes_data$Att_Class=="Never",])
    )
)

result$Attendance <- factor(result$Attendance, levels = c("Always", "Sometimes", "Never" ))

ggplot(
  result, aes(
    x= Attendance, y=Responses, fill= Attendance)
  ) + 
  geom_segment(
    aes(x = Attendance, xend = Attendance, y=0, yend = Responses), linewidth=5, color= "black" 
  )+
  geom_point(
    size=7, color= "black"
  )+
  geom_segment(
    aes(x = Attendance, xend = Attendance, y=0, yend = Responses), linewidth=3, color=c("green", "yellow", "red") 
  )+
  geom_point(
    size=5, color=c("green", "yellow", "red")
  )+
  geom_text(
    aes(label = Responses, y = Responses), vjust = -1.5,  fontface="bold"
  ) + 
  theme(
    legend.position="none"
  )+
  labs(
    x = "Class Attendance",
    y = "Number of Student Responses", 
    title = "Attendance for Students who have Regular artistic or sports activity "
  )+ 
  coord_flip()

#Analysis 2 - art or sport activitiy, class attendance, and partner
#######################################################################################################

#make subsets from the previous subset to see who responded with low and high attendance
#low class Attendance
ASAC_Low_data <- subset(Art_Sport_Yes_data,Att_Class != "Always")

#high class Attendance
ASAC_High_data <- subset(Art_Sport_Yes_data,Att_Class == "Always")

#viewing the number of students who does have Regular activities
#low class Attendance
nrow(ASAC_Low_data)

#high class Attendance
nrow(ASAC_High_data)

#displays which is the most common response about students Partner status 
#low class Attendance
names(which.max(summary(ASAC_Low_data$StuPartner)))

#high class Attendance
names(which.max(summary(ASAC_High_data$StuPartner)))

#viewing the summary and % of student Partner status for the subsets
#Low class Attendance
summary(ASAC_Low_data$StuPartner)

#n() counts the number of observations in the current group.
ASAC_Low_data %>% group_by(StuPartner) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)

#High class Attendance
summary(ASAC_High_data$StuPartner)

ASAC_High_data %>% group_by(StuPartner) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)

#display a tree map for the affect of partner status on attendance
result <- data.frame(
  Attendance = rep(c("Always", "Sometimes", "Never"), each = 2),
  Partner = rep(
    c("Yes", "No"),
    times = 3
    ),
  Responses = c(
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$StuPartner == "Yes"),
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$StuPartner == "No"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$StuPartner == "Yes"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$StuPatner == "No"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$StuPartner == "Yes"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$StuPartner == "No")
  ) 
) 

result$Partner <- factor(result$Partner, levels = c("Yes", "No"))

ggplot(result, aes(area = Responses , fill = Partner , subgroup = Attendance, label = Responses)) +
  geom_treemap(
  )+
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(
    place = "centre", 
    grow = TRUE, 
    colour = "black", 
    min.size = 0,
    alpha = 0.7,
    fontface = "bold" 
  )+
  geom_treemap_text(
    place = "centre",
    size = 12,
    fontface = "bold" 
  )+ 
  labs(
    title = "Attendance and Partner Status for Students who have Regular artistic or sports activity ",
    fill = "Has a Partner"
  ) + 
  scale_fill_manual(
    values = c("steelblue","maroon")
  )

#Analysis 3 - art or sport activitiy, class attendance, partner, and additional work
#######################################################################################################

#displays  which is the most common response about students additional work 
#low class Attendance
names(which.max(summary(ASAC_Low_data$Add_Work)))

#high class Attendance
names(which.max(summary(ASAC_High_data$Add_Work)))

##viewing the summary and % of student additional work for the subsets
#Low class Attendance
summary(ASAC_Low_data$Add_Work)

ASAC_Low_data %>% group_by(Add_Work) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)

#High class Attendance
summary(ASAC_High_data$Add_Work)

ASAC_High_data %>% group_by(Add_Work) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)

#display a tree map for the affect of additional work on attendance
result <- data.frame(
  Attendance = rep(c("Always", "Sometimes", "Never"), each = 2),
  Additional_Work = rep(c("Yes", "No"), times = 3),
  Responses = c(
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$Add_Work == "Yes"),
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$Add_Work == "No"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$Add_Work == "Yes"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$Add_Work == "No"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$Add_Work == "Yes"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$Add_Work == "No")
  ) 
) 

result$Attendance <- factor(result$Attendance, levels = c("Always", "Sometimes", "Never" ))
result$Additional_Work <- factor(result$Additional_Work, levels = c("Yes", "No"))

ggplot(result, aes(area = Responses , fill = Additional_Work , subgroup = Attendance, label = Responses)) +
  geom_treemap(
  )+
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(
    place = "centre", 
    grow = TRUE, 
    colour = "black", 
    min.size = 0,
    alpha = 0.7,
    fontface = "bold" 
  )+
  geom_treemap_text(
    place = "centre",
    size = 12,
    fontface = "bold" 
  )+ 
  labs(
    title = "Attendance and Additional Work for Students who have Regular artistic or sports activity ",
    fill = "Has Additional Work"
  ) + 
  scale_fill_manual(
    values=c("deepskyblue","lightcyan")
  )

#Analysis 4 - art or sport activitiy, class attendance, partner, additional work, and public transport
#######################################################################################################

#displays which is the highest response from students about their transport
#low class Attendance
names(which.max(summary(ASAC_Low_data$StuTransport)))

#high class Attendance
names(which.max(summary(ASAC_High_data$StuTransport)))

##viewing the summary and % of student transport for the subsets
#Low class Attendance
summary(ASAC_Low_data$StuTransport)

ASAC_Low_data %>% group_by(StuTransport) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)

#High class Attendance
summary(ASAC_High_data$Add_Work)

ASAC_High_data %>% group_by(StuTransport) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)


#display a bar chart for the affect of transport work on attendance 
result <- data.frame(
  Attendance = rep(c("Always", "Sometimes", "Never"), each = 4),
  Transport = rep(c("Bus", "Private car/taxi", "Bicycle", "Other"), times = 3),
  Responses = c(
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$StuTransport == "Bus"),
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$StuTransport == "Private car/taxi"),
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$StuTransport == "Bicycle"),
    sum(Art_Sport_Yes_data$Att_Class == "Always" & Art_Sport_Yes_data$StuTransport == "Other"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$StuTransport == "Bus"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$StuTransport == "Private car/taxi"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$StuTransport == "Bicycle"),
    sum(Art_Sport_Yes_data$Att_Class == "Sometimes" & Art_Sport_Yes_data$StuTransport == "Other"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$StuTransport == "Bus"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$StuTransport == "Private car/taxi"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$StuTransport == "Bicycle"),
    sum(Art_Sport_Yes_data$Att_Class == "Never" & Art_Sport_Yes_data$StuTransport == "Other")
  ) 
) 

result$Attendance <- factor(result$Attendance, levels = c("Always", "Sometimes", "Never" ))
result$Transport <- factor(result$Transport, levels = c("Bus", "Private car/taxi", "Bicycle", "Other"))

ggplot(result, aes(fill=Transport, y=Responses, x=Attendance)) +
  geom_bar(
    position="dodge", stat="identity", color = "black"
  )+
  geom_text(
    aes(label = Responses, y = Responses), position = position_dodge(width = 0.9), vjust = -0.4, fontface = "bold"
  )+
  labs(
    x = "Class Attendance",
    y = "Number of Student Responses", 
    title = "Attendance and Transport for Students who have Regular artistic or sports activity ",
    fill = "Transport used"
  ) + 
  scale_fill_manual(
    values=c("yellow","orange","forestgreen","dimgray")
  )


#external factors
###############################################################################################

#Make subset for low attendance
ext <- subset(assg_data,Att_Class != "Always") %>% arrange(Att_Class)

#check which is the highest response
names(which.max(summary(ext$StuMom_Job)))

#viewing total number of rows
nrow(ext)

#viewing total for each response
summary(ext$ListenClass)

#viewing percentage for each response
ext %>% group_by(StuMom_Job) %>% summarise("Percentage (%)" =(n()/nrow(.))*100)

#conducting chi squared test
ext$StuMom_Job %>% table() %>% chisq.test()

####################### To analyse if having additional work will affect the student's attendance to classes  ###########################
# Objective 2 Analysis - Brandon Chan Hoe Xing TP063581
# Data Preparation
#---------------------------------------------------------------------------
# Subset of hypo attributes from main dataset

addWork_Att <- subset(assg_data, Add_Work=="Yes") %>% arrange(Att_Class)

nrow(addWork_Att) #533 out of 1534 has additional Work

summary(addWork_Att$Att_Class) #Always-411 #Sometimes-122 #Never-0

#---------------------------------------------------------------------------
lowAtt_addWork <- subset(addWork_Att,Att_Class %in% c("Sometimes", "Never"))

summary(lowAtt_addWork$StuTransport) #Bus-99 #Private car/taxi-23 #Bicycle-0 #Other-0

PT_lowAtt_addWork <- nrow(lowAtt_addWork[(lowAtt_addWork$StuTransport=="Bus"), ])


### Percentage calculation
#---------------------------------------------------------------------------
group_counts <- nrow(addWork_Att)
total_count <- nrow(assg_data)
percentage_by_group <- (group_counts / total_count) * 100
percentage_by_group

group_counts <- nrow(addWork_Att[(addWork_Att$Att_Class =="Sometimes") ,])
total_count <- nrow(addWork_Att)
percentage_by_group <- (group_counts / total_count) * 100
percentage_by_group
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
### Graphs
## Frame Preparation
#---------------------------------------------------------------------------
# Class attendance for students that have additional work
addWork_Att <- subset(assg_data, Add_Work=="Yes") %>% arrange(Att_Class)

AW_ATT_df <- data.frame(
  Attend_Class=c("Always","Sometimes","Never"),
  Response=c(
    nrow(addWork_Att[(addWork_Att$Att_Class =="Always") ,]),
    nrow(addWork_Att[(addWork_Att$Att_Class =="Sometimes") ,]),
    nrow(addWork_Att[(addWork_Att$Att_Class =="Never") ,])
  )
)
# Students who have low attendance against additional work
low_Att <- subset(assg_data, Att_Class !="Always") %>% arrange(Att_Class)
nrow(low_Att) #381 out of 1534
summary(low_Att)

AW_lowATT_df <- data.frame(
  Additional_Work=c("Yes", "No"),
  Response=c(
    nrow(low_Att[(low_Att$Add_Work =="Yes") ,]), #122
    nrow(low_Att[(low_Att$Add_Work =="No") ,]) #259
  )
)
## Realign the columns
AW_ATT_df$Attend_Class <- factor(AW_ATT_df$Attend_Class, levels = c("Always", "Sometimes","Never"))

AW_lowATT_df$Additional_Work <- factor(AW_lowATT_df$Additional_Work, levels = c("Yes", "No"))

#---------------------------------------------------------------------------
# class attendance for student with additional work and takes public transport
PT_AW_lowATT_df <- data.frame(
  Additional_Work=c("Yes", "No","Yes","No","No","Yes","No","Yes"),
  Public_Transport=c("Bus","Private car/taxi","Bicycle","Other"),
  Response=c(
    nrow(low_Att[(low_Att$Add_Work =="Yes") &(low_Att$StuTransport =="Bus"),]), #99
    nrow(low_Att[(low_Att$Add_Work =="No") &(low_Att$StuTransport =="Private car/taxi"),]),#42
    nrow(low_Att[(low_Att$Add_Work =="Yes") &(low_Att$StuTransport =="Bicycle"),]), #0
    nrow(low_Att[(low_Att$Add_Work =="No") &(low_Att$StuTransport =="Other"),]),#45
    
    nrow(low_Att[(low_Att$Add_Work =="No") &(low_Att$StuTransport =="Bus"),]),#172
    nrow(low_Att[(low_Att$Add_Work =="Yes") &(low_Att$StuTransport =="Private car/taxi"),]), #23
    nrow(low_Att[(low_Att$Add_Work =="No") &(low_Att$StuTransport =="Bicycle"),]),#0
    nrow(low_Att[(low_Att$Add_Work =="Yes") &(low_Att$StuTransport =="Other"),]) #0
    
  )
)
## Realign the columns
PT_AW_lowATT_df$Additional_Work <- factor(PT_AW_lowATT_df$Additional_Work, 
                                          levels = c("Yes", "No"))
PT_AW_lowATT_df$Public_Transport <- factor(PT_AW_lowATT_df$Public_Transport, 
                                           levels = c("Bus","Private car/taxi","Bicycle","Other"))

#---------------------------------------------------------------------------
## class attendance for student with additional work and takes public transport
# Subset of previous subset where student has additional work, and uses bus, bicycle or others.
## WIP ##
PT_AW_lowAtt <- subset(low_Att, (StuTransport=="Bus")) %>% arrange(Add_Work)

PR_PT_AW_lowATT_df <- data.frame(
  Additional_Work=c("Yes","No","No","Yes"),
  Student_Partner=c("Yes","No"),
  Response=c(
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="Yes") &(PT_AW_lowAtt$StuPartner =="Yes"),]), #30
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="No") &(PT_AW_lowAtt$StuPartner =="No"),]), # 106
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="No") &(PT_AW_lowAtt$StuPartner =="Yes"),]), #66
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="Yes") &(PT_AW_lowAtt$StuPartner =="No"),]) #69
    
  )
)

## Realign the columns
PR_PT_AW_lowATT_df$Additional_Work <- factor(PR_PT_AW_lowATT_df$Additional_Work, levels = c("Yes","No"))
PR_PT_AW_lowATT_df$Student_Partner <- factor(PR_PT_AW_lowATT_df$Student_Partner, levels = c("Yes","No"))


#---------------------------------------------------------------------------
## class attendance for student with additional work and takes public transport and does not have a partner

AS_PR_PT_AW_lowATT_df <- data.frame(
  Additional_Work=c("Yes","No","No","Yes"),
  Art_Sport=c("Yes","No"),
  Response=c(
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="Yes") &(PT_AW_lowAtt$StuPartner =="No") &(PT_AW_lowAtt$Art_Sport =="Yes") ,]), #39
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="No") &(PT_AW_lowAtt$StuPartner =="No") &(PT_AW_lowAtt$Art_Sport =="No"),]), #72
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="No") &(PT_AW_lowAtt$StuPartner =="No") &(PT_AW_lowAtt$Art_Sport =="Yes") ,]), #34
    nrow(PT_AW_lowAtt[(PT_AW_lowAtt$Add_Work =="Yes") &(PT_AW_lowAtt$StuPartner =="No") &(PT_AW_lowAtt$Art_Sport =="No"),]) #30
    
  )
)
## Realign the columns
AS_PR_PT_AW_lowATT_df$Additional_Work <- factor(AS_PR_PT_AW_lowATT_df$Additional_Work, levels = c("Yes","No"))
AS_PR_PT_AW_lowATT_df$Art_Sport <- factor(AS_PR_PT_AW_lowATT_df$Art_Sport, levels = c("Yes","No"))
#---------------------------------------------------------------------------
### Visual ###
#---------------------------------------------------------------------------
### Lolipop Graph
# Attendance and Additional Work
ggplot(AW_ATT_df, aes(x=Attend_Class, y=Response)) +
  geom_point(aes(color=Attend_Class), size=7, fill="beige") +
  geom_segment(aes(x=Attend_Class, xend=Attend_Class, y=0, yend=Response), size=1)+
  geom_text(aes(label=Response), vjust=-1)+
  labs(title = "Class Attendance and Response for Students with Additional Work",
       x = "Attends Classes",
       y = "Number of Student Responses") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c("pink","green","cyan"))
#-------------------------------------------------------------------------------------
### Lolipop Graph
# Low attendance and additional work
ggplot(AW_lowATT_df, aes(x=Additional_Work, y=Response)) +
  geom_point(aes(color=Additional_Work), size=7, fill="beige") + 
  geom_segment(aes(x=Additional_Work, xend=Additional_Work, y=0, yend=Response), size=1)+
  geom_text(aes(label=Response), vjust=-1)+
  labs(title = "Class Attendance and Response for Students with Additional Work",
       x = "Additional Work",
       y = "Number of Student Responses") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c("pink","green"))

#--------------------------------------------------------------------------------------------------------
### Clustered Bar Graph
## low attendance, additional work with public transport
PT_AW_lowATT_df <- PT_AW_lowATT_df %>%
  group_by(Additional_Work, Public_Transport) %>%
  summarise(Response = sum(Response))


ggplot(PT_AW_lowATT_df, aes(x = Additional_Work, y = Response, fill = Public_Transport)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label=Response, y=Response/2), vjust=-1, position = position_dodge(width = 0.9))+
  labs(title = "Class Attendance and Response for Students with Additional Work", 
       x = "Additional Work",
       y = "Number of Student Responses", 
       fill = "Transport Method") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("Bicycle" = "red", "Bus" = "cyan", "Other" = "yellow", "Private car/taxi" = "green"))

#---------------------------------------------------------------------------
### Stacked bar graph from the right
## low attendance, additional work, public transport, and partner
PR_PT_AW_lowATT_df <- PR_PT_AW_lowATT_df %>%
  group_by(Additional_Work, Student_Partner) %>%
  summarise(Response = sum(Response))

ggplot(PR_PT_AW_lowATT_df, aes(x = Additional_Work, y = Response, fill = Student_Partner)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label=Response, y=Response+1), vjust=0.5, position = position_stack(v=0.5))+
  labs(title = "Class Attendance and Response for Students with Additional Work", 
       x = "Additional Work",
       y = "Number of Student Responses", 
       fill = "Student has a partner") +
  theme_minimal() +
  scale_y_reverse()+
  coord_flip()
#---------------------------------------------------------------------------
### Stacked bar graph from the top
## low attendance, additional work, public transport, partner, and art or sports activities
AS_PR_PT_AW_lowATT_df <- AS_PR_PT_AW_lowATT_df %>%
  group_by(Additional_Work, Art_Sport) %>%
  summarise(Response = sum(Response))

ggplot(AS_PR_PT_AW_lowATT_df, aes(x = Additional_Work, y = Response, fill = Art_Sport)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label=Response, y=Response+1), vjust=0.5, position = position_stack(v=0.5))+
  labs(title = "Class Attendance and Response for Students with Additional Work", 
       x = "Additional Work",
       y = "Number of Student Responses", 
       fill = "Student has art or sports activity") +
  theme_minimal() +
  scale_y_reverse()+
  scale_fill_manual(values = c("Yes" = "cyan", "No" = "green"))
#---------------------------------------------------------------------------
## Fisher's Exact Testing ##
# Additional Work and Attendance to class
AW_ATT_test <- table(assg_data$Add_Work, assg_data$Att_Class)
fisher_test_result = fisher.test(AW_ATT_test)
print(fisher_test_result)
#---------------------------------------------------------------
#External factor #Listening to class, ListenClass
LC_ATT_test <- table(assg_data$ListenClass, assg_data$Att_Class)
fisher_test_result = fisher.test(LC_ATT_test)
print(fisher_test_result)
#---------------------------------------------------------------------------
####################### To analyse if having a partner will affect the student's attendance to classes  ###########################
# Objective 3 Analysis - Amelia Sow Jing Yi TP070881




####################### To analyse if using public transport will affect the student's attendance to classes  ###########################
# Objective 4 Analysis - Ong JingQing TP063906








