# import csv file
assg_data = read.csv("C:\\Users\\brand\\OneDrive\\Desktop\\PFDA\\PFDA-Assignment\\student_prediction.csv", header = TRUE)
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

# delete rows (remove null rows - na.omit())
assg_data <- na.omit(assg_data)

# exclude duplicates
assg_data <- unique(assg_data)  #distinct()

##Attributes
#---------------------------------------------------------------------------------------------------
#Student Age
#assg_data$StuAge<- factor(assg_data$StuAge,levels=c(1,2,3), labels=c("18-21","22-25","Above 26"))
assg_data$StuAge <- factor(ifelse(assg_data$StuAge %in% 1:3, assg_data$StuAge, "error"), 
                           levels = c(1, 2, 3, "error"), labels = c("18-21", "22-25", "Above 26", "error"))
#Student Gender
#assg_data$StuGen<- factor(assg_data$StuGen,levels=c(1,2), labels=c("Female","Male"))
assg_data$StuGen <- factor(ifelse(assg_data$StuGen %in% 1:2, assg_data$StuGen, "error"), 
                           levels = c(1, 2, "error"), labels = c("Female", "Male", "error"))
#Student Graduated Highschool Type
#assg_data$GradHS_Type<- factor(assg_data$GradHS_Type,levels=c(1,2,3), labels=c("Private","State","Other"))
assg_data$GradHS_Type <- factor(ifelse(assg_data$GradHS_Type %in% 1:3, assg_data$GradHS_Type, "error"), 
                           levels = c(1, 2, 3, "error"), labels = c("Private", "State","Other","error"))
#Scholarship Type
#assg_data$Scholarship_Type<- factor(assg_data$Scholarship_Type,levels=c(1,2,3,4,5), labels=c("None","25%","50%","75%","Full"))
assg_data$Scholarship_Type <- factor(ifelse(assg_data$Scholarship_Type %in% 1:5, assg_data$Scholarship_Type, "error"), 
                                levels = c(1, 2, 3, 4, 5, "error"), labels = c("None","25%","50%","75%","Full","error"))
#Additional work
#assg_data$Add_Work<- factor(assg_data$Add_Work,levels=c(1,2), labels=c("Yes","No"))
assg_data$Add_Work <- factor(ifelse(assg_data$Add_Work %in% 1:2, assg_data$Add_Work, "error"), 
                                levels = c(1, 2, "error"), labels = c("Yes", "No","error"))
#Regular artistic or sports activity
#assg_data$Art_Sport<- factor(assg_data$Art_Sport,levels=c(1,2), labels=c("Yes","No"))
assg_data$Art_Sport <- factor(ifelse(assg_data$Art_Sport %in% 1:2, assg_data$Art_Sport, "error"), 
                             levels = c(1, 2, "error"), labels = c("Yes", "No","error"))
#Do you have a partner
#assg_data$StuPartner<- factor(assg_data$StuPartner,levels=c(1,2), labels=c("Yes","No"))
assg_data$StuPartner <- factor(ifelse(assg_data$StuPartner %in% 1:2, assg_data$StuPartner, "error"), 
                              levels = c(1, 2, "error"), labels = c("Yes", "No","error"))
#Total salary if available 
#assg_data$StuSal<- factor(assg_data$StuSal,levels=c(1,2,3,4,5), labels=c("USD 135-200", "USD 201-270", "USD 271-340","USD 341-410","Above 410"))
assg_data$StuSal <- factor(ifelse(assg_data$StuSal %in% 1:5, assg_data$StuSal, "error"), 
                               levels = c(1, 2, 3,4, 5, "error"), labels = c("USD 135-200", "USD 201-270", "USD 271-340","USD 341-410","Above 410","error"))
#Transportation to the university
#assg_data$StuTransport<- factor(assg_data$StuTransport,levels=c(1,2,3,4), labels=c("Bus", "Private car/taxi", "Bicycle", "Other"))
assg_data$StuTransport <- factor(ifelse(assg_data$StuTransport %in% 1:4, assg_data$StuTransport, "error"), 
                               levels = c(1, 2, 3, 4, "error"), labels = c("Bus", "Private car/taxi", "Bicycle", "Other","error"))
#Accommodation type in Cyprus
#assg_data$StuAccomodation<- factor(assg_data$StuAccomodation,levels=c(1,2,3,4), labels=c("Rental", "Dormitory", "With family", "Other"))
assg_data$StuAccomodation <- factor(ifelse(assg_data$StuAccomodation %in% 1:4, assg_data$StuAccomodation, "error"), 
                                 levels = c(1, 2, 3, 4, "error"), labels = c("Rental", "Dormitory", "With family", "Other","error"))
#Mother's education
#assg_data$StuMom_Edu<- factor(assg_data$StuMom_Edu,levels=c(1,2,3,4,5,6), labels=c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D."))
assg_data$StuMom_Edu <- factor(ifelse(assg_data$StuMom_Edu %in% 1:6, assg_data$StuMom_Edu, "error"), 
                                    levels = c(1, 2, 3, 4, 5, 6, "error"), labels = c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D.","error"))
#Father's education
#assg_data$StuDad_Edu<- factor(assg_data$StuDad_Edu,levels=c(1,2,3,4,5,6), labels=c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D."))
assg_data$StuDad_Edu <- factor(ifelse(assg_data$StuDad_Edu %in% 1:6, assg_data$StuDad_Edu, "error"), 
                               levels = c(1, 2, 3, 4, 5, 6, "error"), labels = c("Primary School", "Secondary School", "High School", "University","MSc.","Ph.D.","error"))

# Number of sisters/brothers (if available)
#assg_data$NoSibilings<- factor(assg_data$NoSibilings,levels=c(1,2,3,4,5), labels=c("1", "2", "3", "4", "5 or above"))
assg_data$NoSibilings <- factor(ifelse(assg_data$NoSibilings %in% 1:5, assg_data$NoSibilings, "error"), 
                               levels = c(1, 2, 3, 4, 5, "error"), labels = c("1", "2", "3", "4", "5 or above","error"))

#Parental status
#assg_data$Parents_Status<- factor(assg_data$Parents_Status,levels=c(1,2,3), labels=c("Married", "Divorced", "Died - One or Both"))
assg_data$Parents_Status <- factor(ifelse(assg_data$Parents_Status %in% 1:3, assg_data$Parents_Status, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Married", "Divorced", "Died - One or Both","error"))

#Mother's occupation
#assg_data$StuMom_Job<- factor(assg_data$StuMom_Job,levels=c(1,2,3,4,5,6), labels=c("Retired","Housewife","Government Officer","Private Sector Employee", "Self-Employment","Other"))
assg_data$StuMom_Job <- factor(ifelse(assg_data$StuMom_Job %in% 1:6, assg_data$StuMom_Job, "error"), 
                                   levels = c(1, 2, 3, 4, 5, 6, "error"), labels = c("Retired","Housewife","Government Officer","Private Sector Employee", "Self-Employment","Other","error"))

#Father's occupation
#assg_data$StuDad_Job<- factor(assg_data$StuDad_Job,levels=c(1,2,3,4,5), labels=c("Retired","Government Officer","Private Sector Employee", "Self-Employment","Other"))
assg_data$StuDad_Job <- factor(ifelse(assg_data$StuDad_Job %in% 1:5, assg_data$StuDad_Job, "error"), 
                               levels = c(1, 2, 3, 4, 5, "error"), labels = c("Retired","Government Officer","Private Sector Employee", "Self-Employment","Other","error"))

#Weekly study hours
#assg_data$W_StudyHrs<- factor(assg_data$W_StudyHrs,levels=c(1,2,3,4,5), labels=c("None","<5 Hours","6-10 Hours","11-20 Hours","More than 20 Hours"))
assg_data$W_StudyHrs <- factor(ifelse(assg_data$W_StudyHrs %in% 1:5, assg_data$W_StudyHrs, "error"), 
                               levels = c(1, 2, 3, 4, 5, "error"), labels = c("None","<5 Hours","6-10 Hours","11-20 Hours","More than 20 Hours","error"))

#Reading frequency (non-scientific books/journals)
#assg_data$ReadFreq_XSci<- factor(assg_data$ReadFreq_XSci,levels=c(1,2,3), labels=c("None","Sometimes","Often"))
assg_data$ReadFreq_XSci <- factor(ifelse(assg_data$ReadFreq_XSci %in% 1:3, assg_data$ReadFreq_XSci, "error"), 
                               levels = c(1, 2, 3, "error"), labels = c("None","Sometimes","Often","error"))

#Reading frequency (scientific books/journals)
#assg_data$ReadFreq_Sci<- factor(assg_data$ReadFreq_Sci,levels=c(1,2,3), labels=c("None","Sometimes","Often"))
assg_data$ReadFreq_Sci <- factor(ifelse(assg_data$ReadFreq_Sci %in% 1:3, assg_data$ReadFreq_Sci, "error"), 
                                  levels = c(1, 2, 3, "error"), labels = c("None","Sometimes","Often","error"))
#Attendance to the seminars/conferences related to the department
#assg_data$Att_Dept<- factor(assg_data$Att_Dept,levels=c(1,2), labels=c("Yes","No"))
assg_data$Att_Dept <- factor(ifelse(assg_data$Att_Dept %in% 1:2, assg_data$Att_Dept, "error"), 
                                  levels = c(1, 2, "error"), labels = c("Yes","No","error"))

#Impact of your projects/activities on your success
#assg_data$Impact_Suc<- factor(assg_data$Impact_Suc,levels=c(1,2,3), labels=c("Positive","Negative","Neutral"))
assg_data$Impact_Suc <- factor(ifelse(assg_data$Impact_Suc %in% 1:3, assg_data$Impact_Suc, "error"), 
                             levels = c(1, 2, 3, "error"), labels = c("Positive","Negative","Neutral","error"))

# Attendance to classes
#assg_data$Att_Class<- factor(assg_data$Att_Class,levels=c(1,2,3), labels=c("Always","Sometimes","Never"))
assg_data$Att_Class <- factor(ifelse(assg_data$Att_Class %in% 1:3, assg_data$Att_Class, "error"), 
                               levels = c(1, 2, 3, "error"), labels = c("Always","Sometimes","Never","error"))

#Preparation to midterm exams
#assg_data$Prep_Ex1<- factor(assg_data$Prep_Ex1,levels=c(1,2,3), labels=c("Alone","With Friends","Not Applicable"))
assg_data$Prep_Ex1 <- factor(ifelse(assg_data$Prep_Ex1 %in% 1:3, assg_data$Prep_Ex1, "error"), 
                              levels = c(1, 2, 3, "error"), labels = c("Alone","With Friends","Not Applicable","error"))

#Preparation to midterm exams
#assg_data$Prep_Ex2<- factor(assg_data$Prep_Ex2,levels=c(1,2,3), labels=c("Closest date to the exam","Regularly during the semester","Never"))
assg_data$Prep_Ex2 <- factor(ifelse(assg_data$Prep_Ex2 %in% 1:3, assg_data$Prep_Ex2, "error"), 
                             levels = c(1, 2, 3, "error"), labels = c("Closest date to the exam","Regularly during the semester","Never","error"))

#Taking notes in classes
#assg_data$TakingNotes<- factor(assg_data$TakingNotes,levels=c(1,2,3), labels=c("Never","Sometimes","Always"))
assg_data$TakingNotes <- factor(ifelse(assg_data$TakingNotes %in% 1:3, assg_data$TakingNotes, "error"), 
                             levels = c(1, 2, 3, "error"), labels = c("Never","Sometimes","Always","error"))

#Listening in classes
#assg_data$ListenClass<- factor(assg_data$ListenClass,levels=c(1,2,3), labels=c("Never","Sometimes","Always"))
assg_data$ListenClass <- factor(ifelse(assg_data$ListenClass %in% 1:3, assg_data$ListenClass, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Never","Sometimes","Always","error"))

#Discussion improves my interest and success in the course
#assg_data$Discuss_Imp<- factor(assg_data$Discuss_Imp,levels=c(1,2,3), labels=c("Never","Sometimes","Always"))
assg_data$Discuss_Imp <- factor(ifelse(assg_data$Discuss_Imp %in% 1:3, assg_data$Discuss_Imp, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Never","Sometimes","Always","error"))

#Flip-classroom
#assg_data$Flip_Class<- factor(assg_data$Flip_Class,levels=c(1,2,3), labels=c("Not Useful","Useful","Not Applicable"))
assg_data$Flip_Class <- factor(ifelse(assg_data$Flip_Class %in% 1:3, assg_data$Flip_Class, "error"), 
                                levels = c(1, 2, 3, "error"), labels = c("Not Useful","Useful","Not Applicable","error"))

#Cumulative grade point average in the last semester (/4.00)
#assg_data$CGPA_LSem<- factor(assg_data$CGPA_LSem,levels=c(1,2,3,4,5), labels=c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49"))
assg_data$CGPA_LSem <- factor(ifelse(assg_data$CGPA_LSem %in% 1:5, assg_data$Flip_Class, "error"), 
                               levels = c(1, 2, 3, 4, 5, "error"), labels = c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49","error"))

#Expected Cumulative grade point average in the graduation (/4.00)
#assg_data$Exp_CGPA<- factor(assg_data$Exp_CGPA,levels=c(1,2,3,4,5), labels=c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49"))
assg_data$Exp_CGPA <- factor(ifelse(assg_data$Exp_CGPA %in% 1:5, assg_data$Exp_CGPA, "error"), 
                              levels = c(1, 2, 3, 4, 5, "error"), labels = c("<2.00","2.00-2.49","2.50-2.99","3.00-3.49","Above 3.49","error"))

#OUTPUT Grade 
#assg_data$Grade<- factor(assg_data$Grade,levels=c(0,1,2,3,4,5,6,7), labels=c("Fail","DD","DC","CC","CB","BB","BA","AA"))
assg_data$Grade <- factor(ifelse(assg_data$Grade %in% 0:7, assg_data$Grade, "error"), 
                              levels = c(0, 1, 2, 3, 4, 5, 6, 7, "error"), labels = c("Fail","DD","DC","CC","CB","BB","BA","AA","error"))
#---------------------------------------------------------------------------------------------------
#Viewing data based on specific columns 
View(assg_data[,c("StuID","Att_Class","StuPartner","StuTransport","Add_Work","Art_Sport")])
View(assg_data[,c("StuID","Att_Class","StuPartner")])
View(assg_data[,c("StuID","Att_Class","StuTransport")])
View(assg_data[,c("StuID","Att_Class","Add_Work")])
View(assg_data[,c("StuID","Att_Class","Art_Sport")])

View(head(assg_data[,c("StuID","Att_Class","StuPartner","StuTransport","Add_Work","Art_Sport")],10))
View(tail(assg_data[,c("StuID","Att_Class","StuPartner","StuTransport","Add_Work","Art_Sport")],10))

View(head(assg_data[,c("StuID","Att_Class","StuPartner")],10))
View(tail(assg_data[,c("StuID","Att_Class","StuPartner")],10))

View(head(assg_data[,c("StuID","Att_Class","StuTransport")],10))
View(tail(assg_data[,c("StuID","Att_Class","StuTransport")],10))

View(head(assg_data[,c("StuID","Att_Class","Add_Work")],10))
View(tail(assg_data[,c("StuID","Att_Class","Add_Work")],10))

View(head(assg_data[,c("StuID","Att_Class","Art_Sport")],10))
View(tail(assg_data[,c("StuID","Att_Class","Art_Sport")],10))

summary(assg_data[,c("StuID","Att_Class","StuPartner","StuTransport","Add_Work","Art_Sport")])
summary(assg_data[,c("StuID","StuAge")])
assg_data[,c("StuID","StuAge")]

summary(assg_data)





####################### To analyse if not having a personal vehicle will affect the student's attendance at classes  ###########################
## Val ##
#import library
library(dplyr)

# number of students and attendance
nrow(assg_data[assg_data$Att_Class =="Always",]) #1153
nrow(assg_data[assg_data$Att_Class =="Sometimes",]) #381
nrow(assg_data[assg_data$Att_Class =="Never",]) #0

# number of students and transportation 
nrow(assg_data[assg_data$StuTransport =="Bus",]) #1030
nrow(assg_data[assg_data$StuTransport =="Private car/taxi",]) #270
nrow(assg_data[assg_data$StuTransport =="Other",]) #224
nrow(assg_data[assg_data$StuTransport =="Bicycle",]) #10


# Public transport (BUS,ALWAYS)
# Hypothesis: Students that does not have personal vehicle -> low attendance
# Analysis: Students that does not have personal vehicle -> high attendance
# contradicting our hypothesis -> taking public transport will not have low attendance
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") ,]) #759
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Never") ,]) #0
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Sometimes") ,]) #271

# Additional activity (BUS,ALWAYS,NO)
# Hypothesis: Students that participate in activity regularly -> low attendance
# Analysis: Students that participate in activity regularly -> low attendance
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="Yes"),]) #277
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No"),])  #482

# Active partner (BUS,ALWAYS,NO,NO)
#Hypothesis: Students that have active partner -> low attendance
#Analysis: Students that have active partner -> low attendance
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="Yes"),]) #239
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="No"),]) #243

# Additional work (BUS,ALWAYS,NO,NO,NO)
#Hypothesis: Students that have additional work -> low attendance
#Analysis: Students that have additional work -> low attendance
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="Yes") &(assg_data$Add_Work =="Yes"),]) #79
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="No") &(assg_data$Add_Work =="No"),]) #174

# Analyse will transport affect students' attendance 
#Always: 1. Bus-759   2. Private car-205   3. Other-179   4. Bicycle-10  -> does not have private car will not have low attendance

# BUS
# Analysis: Students who take bus to classes
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") ,]) #759

# BUS + Always + No_Activity + Yes_Partner + Yes_Work
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="Yes") &(assg_data$Add_Work =="Yes"),]) #79
# BUS + Always + No_Activity + No_Partner + No_Work
nrow(assg_data[(assg_data$StuTransport =="Bus") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="No") &(assg_data$Add_Work =="No"),]) #174

# PRIVATE CAR/TAXI
# Analysis: Students who take private car/taxi to classes
nrow(assg_data[(assg_data$StuTransport =="Private car/taxi") &(assg_data$Att_Class =="Always") ,]) #205

# PRIVATE CAR/TAXI + Always + No_Activity + Yes_Partner + Yes_Work
nrow(assg_data[(assg_data$StuTransport =="Private car/taxi") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="Yes") &(assg_data$Add_Work =="Yes"),]) #8
# PRIVATE CAR/TAXI + Always + No_Activity + No_Partner + No_Work
nrow(assg_data[(assg_data$StuTransport =="Private car/taxi") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="No") &(assg_data$Add_Work =="No"),]) #29

# OTHER
# Analysis: Students who take other transport to classes
nrow(assg_data[(assg_data$StuTransport =="Other") &(assg_data$Att_Class =="Always") ,]) #179

# OTHER + Always + No_Activity + Yes_Partner + Yes_Work
nrow(assg_data[(assg_data$StuTransport =="Other") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="Yes") &(assg_data$Add_Work =="Yes"),]) #13
# OTHER + Always + No_Activity + No_Partner + No_Work
nrow(assg_data[(assg_data$StuTransport =="Other") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="No") &(assg_data$Add_Work =="No"),]) #63

# BICYCLE
# Analysis: Students who take bicycles to classes
nrow(assg_data[(assg_data$StuTransport =="Bicycle") &(assg_data$Att_Class =="Always") ,]) #10

# BICYCLE + Always + No_Activity + Yes_Partner + Yes_Work
nrow(assg_data[(assg_data$StuTransport =="Bicycle") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="Yes") &(assg_data$Add_Work =="Yes"),]) #3
# BICYCLE + Always + No_Activity + No_Partner + No_Work
nrow(assg_data[(assg_data$StuTransport =="Bicycle") &(assg_data$Att_Class =="Always") &(assg_data$Art_Sport =="No") 
               &(assg_data$StuPartner =="No") &(assg_data$Add_Work =="No"),]) #0

####################### To analyse if having additional work will affect the student's attendance to classes  ###########################
## Brandon ##
#import library
library(dplyr)

# number of students and attendance
nrow(assg_data[assg_data$Att_Class =="Always",]) #1153
nrow(assg_data[assg_data$Att_Class =="Sometimes",]) #381
nrow(assg_data[assg_data$Att_Class =="Never",]) #0

# number of students and additional work 
nrow(assg_data[assg_data$Add_Work =="Yes",]) #533
nrow(assg_data[assg_data$Add_Work =="No",]) #1001

#---------------------------------------------------------------------------------------------------

# Additional Work (No,ALWAYS)
# Hypothesis: Students that have additional work -> low attendance

# Analysis: Students that have additional work -> low attendance
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") ,]) #411
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Never") ,]) #0
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Sometimes") ,]) #122
# Analysis: Students that does not have additional work -> high attendance
nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") ,]) #742
nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Never") ,]) #0
nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Sometimes") ,]) #259


#---------------------------------------------------------------------------------------------------

# Transport (No,ALWAYS,Bus)
# Hypothesis: Students that uses public transport -> low attendance
# Analysis: Students that uses public transport -> low attendance
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus"),]) #253
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Private car/taxi"),]) #110
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bicycle"),]) #10
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Other"),]) #38

nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus"),]) #506
nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Private car/taxi"),]) #95
nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bicycle"),]) #0
nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Other"),]) #141

#---------------------------------------------------------------------------------------------------

# Active partner (No,Always,Bus,No)
#Hypothesis: Students that have active partner -> low attendance
#Analysis: Students that have active partner -> low attendance
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus")
               &(assg_data$StuPartner =="Yes"),]) #130

nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus")
               &(assg_data$StuPartner =="No"),]) #270

#---------------------------------------------------------------------------------------------------

# Regular Artistic or Sport activity (No,ALWAYS,Bus,NO,NO)
#Hypothesis: Students that participate in art or sport -> low attendance
#Analysis: Students that have additional work -> low attendance
nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus")
               &(assg_data$StuPartner =="No")&(assg_data$Art_Sport =="Yes"),]) #54

nrow(assg_data[(assg_data$Add_Work =="Yes") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus")
               &(assg_data$StuPartner =="No")&(assg_data$Art_Sport =="No"),]) #69
#Analysis: Students that have additional work -> low attendance
nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus")
               &(assg_data$StuPartner =="No")&(assg_data$Art_Sport =="Yes"),]) #96

nrow(assg_data[(assg_data$Add_Work =="No") &(assg_data$Att_Class =="Always") &(assg_data$StuTransport =="Bus")
               &(assg_data$StuPartner =="No")&(assg_data$Art_Sport =="No"),]) #174

#---------------------------------------------------------------------------------------------------






