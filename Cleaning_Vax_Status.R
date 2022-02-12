#Checking vaccination coding

  ###Cleaning vaccination status###
#First dataset: yes vaccinated
Vacc_yes<-merged.data3%>%
  filter(Vax_rec=="YES")%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Second dataset: not vaccinated
Vacc_no<-merged.data3%>%
  filter(Vax_rec=="NO")%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Third dataset: No response to vaccination question
Vacc_noresponse<-merged.data3%>%
  filter(Vax_rec=="NO RESPONSE")%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Fourth datasaet: NA vaccinated
Vacc_NA<-merged.data3%>%
  filter(is.na(Vax_rec))%>%
  select(VisitID, PatientID, Test_date, Vax_rec)%>%
  distinct()

#Did someone that said yes also have no to the vaccination question?
  yesnovacc<-inner_join(Vacc_yes, Vacc_no, by="PatientID")
  yesnovacc<-yesnovacc%>%
    mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))
  
update_yes_1<-yesnovacc%>%
   filter(timebtwn<0)%>%
  select(VisitID.y)%>%
  add_column(updated_vax_rec="YES")%>%
  rename(VisitID=VisitID.y)%>%
  distinct()

#Did someone that said yes also have a no response?
#No
yesnoresponsevacc<-inner_join(Vacc_yes, Vacc_noresponse, by="PatientID")

yesnoresponsevacc<-yesnoresponsevacc%>%
  mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))

update_yes_2<-yesnoresponsevacc%>%
  filter(timebtwn<0)%>%
  select(VisitID.y)%>%
  add_column(updated_vax_rec="YES")%>%
  rename(VisitID=VisitID.y)%>%
  distinct()

#Which na IDs respond yes earlier?
anyna<-inner_join(Vacc_yes, Vacc_NA, by="PatientID")

anyna<-anyna%>%
  mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))

update_yes_3<-anyna%>%
  filter(timebtwn<0)%>%
  select(VisitID.y)%>%
  add_column(updated_vax_rec="YES")%>%
  rename(VisitID=VisitID.y)%>%
  distinct()

update_yes<-rbind(update_yes_1, update_yes_2, update_yes_3)

update_yes<-update_yes%>%
  distinct()

####Checking if anyone entered as fully vaccinated is partially or not vaccinated later###

#First dataset: yes vaccinated
Vacc_yes<-main%>%
  filter(vax_status=="Fully vaccinated")%>%
  select(VisitID, PatientID, Test_date, vax_status)%>%
  distinct()

#Second dataset: partially vaccinated
Vacc_part<-main%>%
  filter(vax_status=="Partially vaccinated")%>%
  select(VisitID, PatientID, Test_date, vax_status)%>%
  distinct()

#Third dataset: Unvaccinated
Vacc_no<-main%>%
  filter(Vax_rec=="Unvaccinated")%>%
  select(VisitID, PatientID, Test_date, vax_status)%>%
  distinct()

#Did someone who said fully vaccinated marked as partially vaccinated later to the vaccination question?
fullpartvacc<-inner_join(Vacc_yes, Vacc_part, by="PatientID")
fullpartvacc<-fullpartvacc%>%
  mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))
#No

#Did someone that said fully vaccinated marked as unvaccinated later?
#No
yesnoresponsevacc<-inner_join(Vacc_yes, Vacc_no, by="PatientID")

yesnoresponsevacc<-yesnoresponsevacc%>%
  mutate(timebtwn=difftime(Test_date.x, Test_date.y, units="days"))
#no


