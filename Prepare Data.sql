-- create schema tb_study;
-- ################## PHASE1 DETECTION

-- ##################################################################################################################################
-- Meta tables
drop table if exists gp;
create table gp (
    GPID int not null auto_increment,
    GPName varchar(45) not null,
    Qualification varchar(45) null,
    Speciality varchar(45) null,
    primary key (GPID)
);
insert into gp 
select 0,GPID,Qualification1,Speciality1 from tbreach_rpt.gp
union
select 0,GPID,Qualification,Specialization from tbreach2_rpt.gp;

drop table if exists users;
create table users (
    UserID int not null auto_increment,
    UserName varchar(45) null,
    Role varchar(45) null,
    primary key (UserID)
);
insert into users 
select 0, UserName, Role from tbreach_rpt.users
union
select 0, UserName, Role from tbreach2_rpt.users;
-- ##################################################################################################################################

-- ##################################################################################################################################
-- Prepare patient table
drop table if exists _patient;
create table _patient select PatientID,
    CHWID,
    GPID,
    Weight,
    Height,
    BloodGroup,
    DateRegistered,
    TreatmentCenter,
    (TreatmentSupporter is not null) as HasTreatmentSupporter,
    DiseaseConfirmed,
    DiseaseCategory,
    DiseaseSite,
    Severity,
    Regimen,
    DoseCombination,
    OtherDoseDescription,
    TreatmentPhase,
    PatientStatus,
    PatientType,
    DiseaseHistory,
    TreatedPreviously,
    CompletedPreviousTreatment from
    tbreach_rpt.patient 
union select 
    PatientID,
    CHWID,
    GPID,
    Weight,
    Height,
    BloodGroup,
    DateRegistered,
    TreatmentCenter,
    (TreatmentSupporter is not null) as HasTreatmentSupporter,
    DiseaseConfirmed,
    DiseaseCategory,
    DiseaseSite,
    Severity,
    Regimen,
    DoseCombination,
    Streptomycin,
    TreatmentPhase,
    PatientStatus,
    PatientType,
    DiseaseHistory,
    TreatedPreviously,
    CompletedPreviousTreatment
from
    tbreach2_rpt.patient;
drop table if exists _person;
create table _person select PID,
    Salutation,
    LastName,
    Surname,
    Gender,
    DOB,
    MaritalStatus,
    Domicile,
    Religion,
    Caste,
    RoleInSystem,
    Alive from
    tbreach_rpt.person 
union select 
    PID,
    Salutation,
    LastName,
    Surname,
    Gender,
    DOB,
    MaritalStatus,
    Domicile,
    Religion,
    Caste,
    RoleInSystem,
    Alive
from
    tbreach2_rpt.person;
-- Merge Patient and Person tables, removing names
drop table if exists patient;
create table patient select PatientID,
    CHWID,
    GPID,
    DOB,
    Gender,
    MaritalStatus,
    Domicile,
    Religion,
    Caste,
    Weight,
    Height,
    DateRegistered,
    TreatmentCenter,
    HasTreatmentSupporter,
    DiseaseConfirmed,
    DiseaseCategory,
    DiseaseSite,
    Severity,
    Regimen,
    DoseCombination,
    OtherDoseDescription,
    TreatmentPhase,
    PatientStatus,
    PatientType,
    DiseaseHistory,
    TreatedPreviously,
    CompletedPreviousTreatment from
    _patient as P
        inner join
    _person as R ON R.PID = P.PatientID
where
    DiseaseConfirmed = 1
        and PatientType is not null;
alter table patient add primary key (PatientID);

-- Normalize attributes
update patient 
set 
    DiseaseCategory = 'CAT I'
where
    DiseaseCategory = 'CATEGORY 1';
update patient 
set 
    DiseaseCategory = 'CAT II'
where
    DiseaseCategory = 'CATEGORY 2';
update patient 
set 
    Regimen = 'RHZE'
where
    Regimen = 'RHEZ';
-- Height
update patient 
set 
    Height = null
where
    Height >= 444;
update patient 
set 
    Height = Height * 30.48
where
    Height < 40;-- Into cm
update patient 
set 
    Height = null
where
    Height = 0;-- Nullify 0 data
-- Weight
update patient 
set 
    Weight = null
where
    Weight >= 444;
update patient 
set 
    Weight = Weight * 0.4535
where
    Weight > 140;-- Into Kg
update patient 
set 
    Weight = null
where
    Weight = 0; -- Nullify 0 data
-- Cleanup
drop table if exists _patient;
drop table if exists _person;
-- ##################################################################################################################################

-- ##################################################################################################################################
-- Create screening table
drop table if exists enc_screening;
create table enc_screening select EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    AGE,
    FEVER,
    COUGH,
    COUGH_DURATION,
    PRODUCTIVE_COUGH,
    HAEMOPTYSIS,
    NIGHT_SWEATS,
    WEIGHT_LOSS,
    TB_HISTORY,
    TB_FAMILY_HISTORY from
    tbreach_rpt.enc_suspect_id
where
    PID1 in (select 
            PatientID
        from
            patient) 
union select 
    EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    AGE,
    FEVER,
    COUGH,
    COUGH_DURATION,
    PRODUCTIVE_COUGH,
    HAEMOPTYSIS,
    NIGHT_SWEATS,
    WEIGHT_LOSS,
    TB_HISTORY,
    TB_FAMILY_HISTORY
from
    tbreach2_rpt.enc_suspect_id
where
    PID1 in (select 
            PatientID
        from
            patient);
update enc_screening 
set 
    COUGH_DURATION = 'LESS THAN 2 WEEKS'
where
    COUGH_DURATION = '< 2 WEEKS';
update enc_screening 
set 
    COUGH_DURATION = '2 TO 3 WEEKS'
where
    COUGH_DURATION = '2-3 WEEKS';
update enc_screening 
set 
    COUGH_DURATION = 'MORE THAN 3 WEEKS'
where
    COUGH_DURATION = '> 3 WEEKS';

drop table if exists screening;
create table screening select EncounterID,
    PID1 as PatientID,
    PID2 as ScreenerID,
    EnteredDate as ScreeningDate,
    cast(AGE as unsigned) as ScreeningAge,
    FEVER as Fever,
    COUGH as Cough,
    COUGH_DURATION as CoughDuration,
    PRODUCTIVE_COUGH as ProductiveCough,
    HAEMOPTYSIS as BloodInCough,
    NIGHT_SWEATS as NightSweats,
    WEIGHT_LOSS as WeightLoss,
    TB_HISTORY as TBHistory,
    TB_FAMILY_HISTORY as TBInFamily from
    enc_screening;
delete from screening 
where
    PatientID = '03212052401'
    and EncounterID = 1; -- Unexpected duplicate
alter table screening add primary key (PatientID);
-- Cleanup
drop table if exists enc_screening;
-- ##################################################################################################################################

-- ##################################################################################################################################
-- Prepare baseline test results table
drop table if exists _sputumresults;
create table _sputumresults select PatientID,
    SputumTestID,
    Month,
    DateSubmitted,
    DateTested,
    SmearResult from
    tbreach_rpt.sputumresults
where
    Month = 0 and SmearResult is not null
        and PatientID in (select 
            PatientID
        from
            patient) 
union select 
    PatientID,
    SputumLabID,
    Month,
    SmearOrderDate,
    DateSmearTested,
    SmearResult
from
    tbreach2_rpt.sputumresults
where
    Month = 0 and SmearResult is not null
        and PatientID in (select 
            PatientID
        from
            patient);
update _sputumresults 
set 
    SmearResult = '1-9AFB'
where
    SmearResult = '1-9 AFB';

-- In case of multiple sputum results, keep the positive one, if both are positive or negative, keep any one
drop table if exists tmp;
create table tmp select PatientID from
    _sputumresults
group by PatientID
having count(*) > 1;
delete from _sputumresults 
where
    SmearResult = 'NEGATIVE'
    and PatientID in (select 
        PatientID
    from
        tmp);
drop table tmp;

drop table if exists _genexpertresults;
create table _genexpertresults select PatientID,
    LaboratoryID,
    DateTested,
    Remarks,
    GeneXpertResult,
    DrugResistance from
    tbreach_rpt.genexpertresults
where
    DateTested is not null
        and PatientID in (select 
            PatientID
        from
            patient) 
union select 
    PatientID,
    SputumLabID,
    DateGeneXpertTested,
    Details,
    GeneXpertResult,
    GeneXpertResistance
from
    tbreach2_rpt.sputumresults
where
    Month = 0
        and DateGeneXpertTested is not null
        and PatientID in (select 
            PatientID
        from
            patient);

delete from _genexpertresults 
where
    GeneXpertResult = ''
    or GeneXpertResult = 'ERROR'
    or GeneXpertResult = 'INVALID';-- Not valid GXP tests

update _genexpertresults 
set 
    GeneXpertResult = 'POSITIVE'
where
    GeneXpertResult like '%POSITIVE%';
update _genexpertresults 
set 
    GeneXpertResult = 'NEGATIVE'
where
    GeneXpertResult like '%NEGATIVE%';
update _genexpertresults 
set 
    DrugResistance = 'INDETERMINATE'
where
    DrugResistance like '%INDETERMINATE%';
update _genexpertresults 
set 
    DrugResistance = 'RESISTANT'
where
    DrugResistance like '%RESISTANT%';
update _genexpertresults 
set 
    DrugResistance = 'SENSITIVE'
where
    DrugResistance like '%SENSITIVE%';

-- In case of multiple sputum results, keep the positive one, if both are positive or negative, keep any one
drop table if exists tmp;
create table tmp select PatientID from
    _genexpertresults
group by PatientID
having count(*) > 1;
delete from _genexpertresults 
where
    GeneXpertResult = 'NEGATIVE'
    and PatientID in (select 
        PatientID
    from
        tmp);
drop table tmp;

drop table if exists _xrayresults;
create table _xrayresults select PatientID, XRayDate, DateReported, XRayResults, Remarks from
    tbreach_rpt.xrayresults
where
    XRayResults is not null
        and PatientID in (select 
            PatientID
        from
            patient) 
union select 
    PatientID, XRayDate, DateReported, XRayResult, Remarks
from
    tbreach2_rpt.xrayresults
where
    XRayResult is not null
        and PatientID in (select 
            PatientID
        from
            patient);

delete from _xrayresults 
where
    XRayResults = 'IMAGE UNCLEAR'; -- Not a valid XRay

-- One way to distinguish baseline Xrays from others is to match Baseline treatment date
drop table if exists enc_baseline;
create table enc_baseline as select EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    FDC_TABLETS,
    PATIENT_CATEGORY,
    PATIENT_TYPE,
    STREPOMYCIN as STREPTOMYCIN,
    REGIMEN,
    WEIGHT from
    tbreach_rpt.enc_baseline
where
    PID1 in (select 
            PatientID
        from
            patient) 
union select 
    EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    FDC_TABLETS,
    PATIENT_CATEGORY,
    PATIENT_TYPE,
    STREPTOMYCIN,
    REGIMEN,
    WEIGHT
from
    tbreach2_rpt.enc_baseline
where
    PID1 in (select 
            PatientID
        from
            patient);
drop table if exists tmp;
create table tmp select X . * from
    _xrayresults as X
        left outer join
    enc_baseline as B ON B.PID1 = X.PatientID
        and B.PID1 in (select 
            PatientID
        from
            patient)
where
    X.XRayDate < B.EnteredDate;

drop table if exists _xrayresults;
create table _xrayresults as select * from
    tmp;
drop table tmp;

drop table if exists baseline_results;
create table baseline_results select P.PatientID,
    S.Month,
    S.SputumTestID,
    S.DateSubmitted as DateSputumSubmitted,
    S.DateTested as DateSputumTested,
    datediff(S.DateTested, S.DateSubmitted) as SputumResultDelay,
    S.SmearResult,
    X.XRayDate,
    X.DateReported as DateXRayReported,
    datediff(X.DateReported, X.XRayDate) as XRayResultDelay,
    X.XRayResults,
    X.Remarks as XRayRemarks,
    G.DateTested as DateGXPTested,
    G.GeneXpertResult,
    G.DrugResistance,
    G.Remarks as GXRemarks from
    patient as P
        left outer join
    (select 
        *
    from
        _sputumresults
    group by PatientID) as S USING (PatientID)
        left outer join
    (select 
        *
    from
        _xrayresults
    group by PatientID) as X USING (PatientID)
        left outer join
    (select 
        *
    from
        _genexpertresults
    group by PatientID) as G USING (PatientID);
alter table baseline_results add primary key (PatientID);
-- Clean up
drop table if exists _sputumresults;
drop table if exists _genexpertresults;
drop table if exists _xrayresults;
-- ##################################################################################################################################

-- ##################################################################################################################################
-- Create clinical diagnosis table
drop table if exists enc_cdf;
create table enc_cdf as select * from
    tbreach_rpt.enc_cdf
where
    PID1 in (select 
            PatientID
        from
            patient) 
union select 
    *
from
    tbreach2_rpt.enc_cdf
where
    PID1 in (select 
            PatientID
        from
            patient);

-- Keep only the latest encounter in case of duplicates
drop table if exists _diagnosis;
create table _diagnosis select a . * from
    enc_cdf as a
where
    EnteredDate = (select 
            EnteredDate
        from
            enc_cdf
        where
            PID1 = a.PID1
        order by EnteredDate desc
        limit 1);

drop table if exists diagnosis;
create table diagnosis as select PID1 as PatientID,
    PID2 as GPID,
    EnteredDate as DiagnosisDate,
    ANTIBIOTIC_TRIAL as DiagnosisAntibiotic,
    CLINICAL_SYMP_TB as TBSymptomsDiagnosed,
    CONTACT_HISTORY as TBContactDiagnosed,
    DIAGNOSIS as Diagnosis,
    LARGE_LYMPH_NODES as LargeLymphDiagnosed,
    LYMPH_NODE_BIOPSY as LymphBiopsyDiagnosed,
    MANTOUX as MantouxDiagnosed,
    PAST_HISTORY as PastTBDiagnosed,
    XRAY_SUGGESTIVE as XRaySuggestiveDiagnosed,
    OTHER_THAN_TB_DETAIL as OtherTBDiagnosis,
    concat(OTHER_DIAGNOSTIC_DETAIL, ' ', NOTES) as DiganosisNotes from
    _diagnosis;
alter table diagnosis add primary key (PatientID);
-- Cleanup
drop table if exists enc_cdf;
drop table if exists _diagnosis;
-- ##################################################################################################################################

-- ##################################################################################################################################
-- Create Baseline Treatment table
drop table if exists enc_baseline;
create table enc_baseline as select EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    FDC_TABLETS,
    PATIENT_CATEGORY,
    PATIENT_TYPE,
    STREPOMYCIN as STREPTOMYCIN,
    REGIMEN,
    WEIGHT from
    tbreach_rpt.enc_baseline
where
    PID1 in (select 
            PatientID
        from
            patient) 
union select 
    EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    FDC_TABLETS,
    PATIENT_CATEGORY,
    PATIENT_TYPE,
    STREPTOMYCIN,
    REGIMEN,
    WEIGHT
from
    tbreach2_rpt.enc_baseline
where
    PID1 in (select 
            PatientID
        from
            patient);
update enc_baseline 
set 
    WEIGHT = null
where
    WEIGHT > 200;
update enc_baseline 
set 
    PATIENT_CATEGORY = 'CAT I'
where
    PATIENT_CATEGORY = 'CATEGORY 1';
update enc_baseline 
set 
    PATIENT_CATEGORY = 'CAT II'
where
    PATIENT_CATEGORY = 'CATEGORY 2';
update enc_baseline 
set 
    REGIMEN = 'RHZE'
where
    REGIMEN = 'RHEZ';
update enc_baseline 
set 
    REGIMEN = 'HE'
where
    REGIMEN = 'EH';

drop table if exists baseline_treatment;
create table baseline_treatment select PID1 as PatientID,
    PID2 as BaselineCHWID,
    EnteredDate as BaselineDate,
    WEIGHT as BaselineWeight,
    PATIENT_CATEGORY as BaselinePatientCategory,
    PATIENT_TYPE as BaselinePatientType,
    REGIMEN as BaselineRegimen,
    FDC_TABLETS as FDCTablets,
    STREPTOMYCIN as Streptomycin from
    enc_baseline;
alter table baseline_treatment add primary key (PatientID);
-- Cleanup
drop table if exists enc_baseline;
-- ##################################################################################################################################

-- ################## PHASE2 TREATMENT 

-- ##################################################################################################################################
-- Create drugs history record
drop table if exists _drug_history;
create table _drug_history select PatientID,
    DispensationNo,
    DateDispensed,
    PillsDelivered,
    PillsQuotaDelivered,
    StreptomycinDelivered,
    StreptomycinQuotaDelivered,
    Remarks from
    tbreach_rpt.drughistory 
union select 
    PatientID,
    DispensalNo,
    DateDispensed,
    PillsDelivered,
    PillsQuotaDelivered,
    StreptomycinDelivered,
    StreptomycinQuotaDelivered,
    Remarks
from
    tbreach2_rpt.drughistory;

-- Create combined table for drug history
drop table if exists drug_history;
create table drug_history select PatientID,
    count(DispensationNo) as TotalDispersals,
    sum(PillsDelivered) as TotalPils,
    sum(PillsQuotaDelivered) as TotalPillsQuote,
    sum(StreptomycinDelivered) as TotalStreptomycin,
    sum(StreptomycinQuotaDelivered) as TotalStreptomycinQuote from
    _drug_history
group by PatientID;

-- Create Follow-up table
drop table if exists enc_follow_up;
create table enc_follow_up as select EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    MONTH,
    ifnull(FDC_TABLETS, FDC_TABLET) as FDC_TABLETS,
    STREPOMYCIN as STREPTOMYCIN,
    REGIMEN,
    WEIGHT,
    TREATMENT_PHASE from
    tbreach_rpt.enc_follow_up
where
    PID1 in (select 
            PatientID
        from
            patient) 
union select 
    EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    MONTH,
    FDC_TABLETS,
    STREPOMYCIN as STREPTOMYCIN,
    REGIMEN,
    WEIGHT,
    TREATMENT_PHASE
from
    tbreach2_rpt.enc_follow_up
where
    PID1 in (select 
            PatientID
        from
            patient);

-- Create Treatment refusal table
drop table if exists enc_refusal;
create table enc_refusal as select EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    REASON,
    WHAT_REFUSED from
    tbreach2_rpt.enc_refusal
where
    PID1 in (select 
            PatientID
        from
            patient) 
union select 
    EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    REASON,
    WHAT_REFUSED
from
    tbreach_rpt.enc_refusal
where
    PID1 in (select 
            PatientID
        from
            patient);

drop table if exists refusal;
create table refusal select a . * from
    enc_refusal as a
where
    EncounterID = (select 
            max(EncounterID)
        from
            enc_refusal
        where
            PID1 = a.PID1);
-- ###################################################################################################################################

-- ################## PHASE3 OUTCOMES 

-- ##################################################################################################################################
-- Create End of Follow-up table
drop table if exists enc_end_fol;
create table enc_end_fol as select EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    REASON,
    OTHER_REASON from
    tbreach_rpt.enc_end_fol
where
    PID1 in (select 
            PatientID
        from
            patient) 
union select 
    EncounterID,
    PID1,
    PID2,
    DateEncounterStart,
    DateEncounterEnd,
    EnteredDate,
    ENTERED_DATE,
    REASON,
    OTHER_REASON
from
    tbreach2_rpt.enc_end_fol
where
    PID1 in (select 
            PatientID
        from
            patient);

drop table if exists end_follow_up;
create table end_follow_up select a . * from
    enc_end_fol as a
where
    concat(EncounterID, EnteredDate) = (select 
            concat(EncounterID, EnteredDate)
        from
            enc_end_fol
        where
            PID1 = a.PID1
        order by EnteredDate desc , EncounterID desc
        limit 1);
update end_follow_up 
set 
    REASON = 'TREATMENT FAILURE'
where
    REASON = 'TX FAILURE'
        or REASON = 'FAILURE';
update end_follow_up 
set 
    REASON = 'TREATMENT COMPLETED'
where
    REASON = 'TX COMPLETED'
        or REASON = 'TREAMTENT COMPLETED';
update end_follow_up 
set 
    REASON = 'TRANSFERRED'
where
    REASON like '%TRANSFER%';
update end_follow_up 
set 
    REASON = 'MOVED'
where
    REASON = 'DEAFAULT'
        or REASON = 'DEFAULTED'
        or REASON = 'DEFAULT/MOVED OUT OF AREA';
update end_follow_up 
set 
    REASON = 'MDR PATIENT'
where
    REASON like '%MDR%'
        or REASON like '%RESISTANT%';

drop table if exists treatment_outcome;
create table treatment_outcome select PID1 as PatientID,
    EnteredDate as TreatmentEndDate,
    REASON as TreatmentOutcome,
    OTHER_REASON as TreatmentOutcomeDetails from
    end_follow_up;
alter table treatment_outcome add primary key (PatientID);
-- Cleanup
drop table if exists enc_end_fol;
drop table if exists end_follow_up;
-- ##################################################################################################################################

-- ################## PHASE4 MERGING DATA

-- ##################################################################################################################################
-- Create a table to hide actual IDs
drop table if exists ids;
create table ids (
    ID int not null auto_increment,
    OriginalID varchar(45) not null,
    primary key (ID)
);
insert into ids 
select 0, PatientID from patient where PatientID is not null;
-- Create unified table till baseline for each patient
drop table if exists tb_data;
create table tb_data (
    PatientID varchar(50) NOT NULL DEFAULT '',
    GP varchar(12) DEFAULT '',
    GPQualification varchar(45) DEFAULT '',
    GPSpeciality varchar(45) DEFAULT '',
    Screener varchar(12) NOT NULL DEFAULT '',
    DOB varchar(50) DEFAULT '',
    BirthYear varchar(50) DEFAULT '',
    ScreeningAge varchar(50) DEFAULT '',
    Gender char(1) NOT NULL DEFAULT '',
    MaritalStatus varchar(20) DEFAULT '',
    Religion varchar(20) DEFAULT '',
    Caste varchar(20) DEFAULT '',
    Weight varchar(50) DEFAULT '',
    Height varchar(50) DEFAULT '',
    ScreeningDate varchar(50) DEFAULT '',
    ScreeningYear varchar(50) DEFAULT '',
    DateRegistered varchar(50) DEFAULT '',
    RegistrationYear varchar(50) DEFAULT '',
    HasTreatmentSupporter bit(1) NOT NULL DEFAULT '',
    DiseaseConfirmed bit(1) DEFAULT '',
    DiseaseCategory varchar(12) DEFAULT '',
    DiseaseSite varchar(50) DEFAULT '',
    LastRegimen varchar(12) DEFAULT '',
    DoseCombination varchar(50) DEFAULT '',
    OtherDoseDescription varchar(255) DEFAULT '',
    TreatmentPhase varchar(12) DEFAULT '',
    PatientStatus varchar(12) DEFAULT '',
    PatientType varchar(50) DEFAULT '',
    TreatedPreviously bit(1) DEFAULT '',
    CompletedPreviousTreatment bit(1) DEFAULT '',
    Fever longtext,
    Cough longtext,
    CoughDuration longtext,
    ProductiveCough longtext,
    BloodInCough longtext,
    NightSweats longtext,
    WeightLoss longtext,
    TBHistory longtext,
    TBInFamily longtext,
    SmearTested varchar(3)CHARACTER SET utf8 NOT NULL DEFAULT '',
    DateSputumSubmitted varchar(50) DEFAULT '',
    DateSputumTested varchar(50) DEFAULT '',
    SputumResultDelay varchar(50) DEFAULT '',
    SmearResult varchar(12) DEFAULT '',
    SmearPositive varchar(3)CHARACTER SET utf8 NOT NULL DEFAULT '',
    XRayDone varchar(3)CHARACTER SET utf8 NOT NULL DEFAULT '',
    XRayDate varchar(50) DEFAULT '',
    DateXRayReported varchar(50) DEFAULT '',
    XRayResultDelay varchar(50) DEFAULT '',
    XRayResults varchar(255) DEFAULT '',
    XRayRemarks varchar(255) DEFAULT '',
    XRayIndicative varchar(3)CHARACTER SET utf8 NOT NULL DEFAULT '',
    GeneXpertTested varchar(3)CHARACTER SET utf8 NOT NULL DEFAULT '',
    DateGXPTested varchar(50) DEFAULT '',
    GXPDelay varchar(50) DEFAULT '',
    GeneXpertResult varchar(50) DEFAULT '',
    DrugResistance varchar(50) DEFAULT '',
    GXRemarks varchar(255) DEFAULT '',
    GXPPositive varchar(3)CHARACTER SET utf8 NOT NULL DEFAULT '',
    DiagnosedBy varchar(12) DEFAULT '',
    DiagnosisDate varchar(50) DEFAULT '',
    DiagnosisAntibiotic longtext,
    TBSymptomsDiagnosed longtext,
    TBContactDiagnosed longtext,
    Diagnosis longtext,
    LargeLymphDiagnosed longtext,
    LymphBiopsyDiagnosed longtext,
    MantouxDiagnosed longtext,
    PastTBDiagnosed longtext,
    XRaySuggestiveDiagnosed longtext,
    OtherTBDiagnosis longtext,
    DiagnosisNotes longtext,
    BaselineCHWID varchar(12) DEFAULT '',
    BaselineDate varchar(50) DEFAULT '',
    BaselineWeight longtext,
    BaselinePatientCategory longtext,
    BaselinePatientType longtext,
    BaselineRegimen longtext,
    FDCTablets longtext,
    Streptomycin longtext,
    TreatmentEndDate varchar(50) DEFAULT '',
    TreatmentDuration varchar(50) DEFAULT '',
    TreatmentOutcome longtext,
    TreatmentOutcomeDetails longtext,
    PRIMARY KEY (PatientID)
);
insert into tb_data
select I.ID as PatientID, P.GPID as GP, G.Qualification as GPQualification, G.Speciality as GPSpeciality, S.ScreenerID as Screener, 
P.DOB, year(P.DOB) as BirthYear, S.ScreeningAge, P.Gender, P.MaritalStatus, P.Religion, P.Caste, P.Weight, P.Height, 
S.ScreeningDate, year(S.ScreeningDate) as ScreeningYear, P.DateRegistered, year(P.DateRegistered) as RegistrationYear, P.HasTreatmentSupporter, 
P.DiseaseConfirmed, P.DiseaseCategory, P.DiseaseSite, P.Regimen as LastRegimen, P.DoseCombination, P.OtherDoseDescription, 
P.TreatmentPhase, P.PatientStatus, P.PatientType, P.TreatedPreviously, P.CompletedPreviousTreatment, 
S.Fever, S.Cough, S.CoughDuration, S.ProductiveCough, S.BloodInCough, S.NightSweats, S.WeightLoss, S.TBHistory, S.TBInFamily, 
(case R.SmearResult is null when 0 then 'YES' else 'NO' end) as SmearTested, R.DateSputumSubmitted, R.DateSputumTested, R.SputumResultDelay, R.SmearResult, (case ifnull(R.SmearResult, 'NEGATIVE') when 'NEGATIVE' then 'NO' else 'YES' end) as SmearPositive, 
(case R.XRayResults is null when 0 then 'YES' else 'NO' end) as XRayDone, R.XRayDate, R.DateXRayReported, R.XRayResultDelay, R.XRayResults, R.XRayRemarks, (case ifnull(R.XRayResults, 'NORMAL') when 'NORMAL' then 'NO' else 'YES' end) as XRayIndicative, 
(case R.GeneXpertResult is null when 0 then 'YES' else 'NO' end) as GeneXpertTested, R.DateGXPTested, datediff(R.DateSputumTested, R.DateGXPTested) as GXPDelay, R.GeneXpertResult, R.DrugResistance, R.GXRemarks, (case ifnull(R.GeneXpertResult, 'NEGATIVE') when 'NEGATIVE' then 'NO' else 'YES' end) as GXPPositive, 
D.GPID as DiagnosedBy, D.DiagnosisDate, D.DiagnosisAntibiotic, D.TBSymptomsDiagnosed, D.TBContactDiagnosed, D.Diagnosis, D.LargeLymphDiagnosed, D.LymphBiopsyDiagnosed, D.MantouxDiagnosed, D.PastTBDiagnosed, D.XRaySuggestiveDiagnosed, D.OtherTBDiagnosis, D.DiganosisNotes as DiagnosisNotes, 
T.BaselineCHWID, T.BaselineDate, T.BaselineWeight, T.BaselinePatientCategory, T.BaselinePatientType, T.BaselineRegimen, T.FDCTablets, T.Streptomycin,O.TreatmentEndDate, datediff(O.TreatmentEndDate, T.BaselineDate) as TreatmentDuration, 
O.TreatmentOutcome, O.TreatmentOutcomeDetails from patient as P 
inner join screening as S using (PatientID)
left outer join baseline_results as R using (PatientID)
left outer join diagnosis as D using (PatientID)
left outer join baseline_treatment as T using (PatientID)
left outer join treatment_outcome as O using (PatientID)
left outer join gp as G on G.GPName = P.GPID
inner join ids as I on I.OriginalID = P.PatientID
order by DateRegistered;

-- Replace NULLs with empty strings
update tb_data 
set 
    PatientID = ifnull(PatientID, ''),
    GP = ifnull(GP, ''),
    GPQualification = ifnull(GPQualification, ''),
    GPSpeciality = ifnull(GPSpeciality, ''),
    Screener = ifnull(Screener, ''),
    DOB = ifnull(DOB, ''),
    BirthYear = ifnull(BirthYear, ''),
    ScreeningAge = ifnull(ScreeningAge, ''),
    Gender = ifnull(Gender, ''),
    MaritalStatus = ifnull(MaritalStatus, ''),
    Religion = ifnull(Religion, ''),
    Caste = ifnull(Caste, ''),
    Weight = ifnull(Weight, ''),
    Height = ifnull(Height, ''),
    ScreeningDate = ifnull(ScreeningDate, ''),
    ScreeningYear = ifnull(ScreeningYear, ''),
    DateRegistered = ifnull(DateRegistered, ''),
    RegistrationYear = ifnull(RegistrationYear, ''),
    HasTreatmentSupporter = ifnull(HasTreatmentSupporter, ''),
    DiseaseConfirmed = ifnull(DiseaseConfirmed, ''),
    DiseaseCategory = ifnull(DiseaseCategory, ''),
    DiseaseSite = ifnull(DiseaseSite, ''),
    LastRegimen = ifnull(LastRegimen, ''),
    DoseCombination = ifnull(DoseCombination, ''),
    OtherDoseDescription = ifnull(OtherDoseDescription, ''),
    TreatmentPhase = ifnull(TreatmentPhase, ''),
    PatientStatus = ifnull(PatientStatus, ''),
    PatientType = ifnull(PatientType, ''),
    TreatedPreviously = ifnull(TreatedPreviously, ''),
    CompletedPreviousTreatment = ifnull(CompletedPreviousTreatment, ''),
    Fever = ifnull(Fever, ''),
    Cough = ifnull(Cough, ''),
    CoughDuration = ifnull(CoughDuration, ''),
    ProductiveCough = ifnull(ProductiveCough, ''),
    BloodInCough = ifnull(BloodInCough, ''),
    NightSweats = ifnull(NightSweats, ''),
    WeightLoss = ifnull(WeightLoss, ''),
    TBHistory = ifnull(TBHistory, ''),
    TBInFamily = ifnull(TBInFamily, ''),
    SmearTested = ifnull(SmearTested, ''),
    DateSputumSubmitted = ifnull(DateSputumSubmitted, ''),
    DateSputumTested = ifnull(DateSputumTested, ''),
    SputumResultDelay = ifnull(SputumResultDelay, ''),
    SmearResult = ifnull(SmearResult, ''),
    SmearPositive = ifnull(SmearPositive, ''),
    XRayDone = ifnull(XRayDone, ''),
    XRayDate = ifnull(XRayDate, ''),
    DateXRayReported = ifnull(DateXRayReported, ''),
    XRayResultDelay = ifnull(XRayResultDelay, ''),
    XRayResults = ifnull(XRayResults, ''),
    XRayRemarks = ifnull(XRayRemarks, ''),
    XRayIndicative = ifnull(XRayIndicative, ''),
    GeneXpertTested = ifnull(GeneXpertTested, ''),
    DateGXPTested = ifnull(DateGXPTested, ''),
    GXPDelay = ifnull(GXPDelay, ''),
    GeneXpertResult = ifnull(GeneXpertResult, ''),
    DrugResistance = ifnull(DrugResistance, ''),
    GXRemarks = ifnull(GXRemarks, ''),
    GXPPositive = ifnull(GXPPositive, ''),
    DiagnosedBy = ifnull(DiagnosedBy, ''),
    DiagnosisDate = ifnull(DiagnosisDate, ''),
    DiagnosisAntibiotic = ifnull(DiagnosisAntibiotic, ''),
    TBSymptomsDiagnosed = ifnull(TBSymptomsDiagnosed, ''),
    TBContactDiagnosed = ifnull(TBContactDiagnosed, ''),
    Diagnosis = ifnull(Diagnosis, ''),
    LargeLymphDiagnosed = ifnull(LargeLymphDiagnosed, ''),
    LymphBiopsyDiagnosed = ifnull(LymphBiopsyDiagnosed, ''),
    MantouxDiagnosed = ifnull(MantouxDiagnosed, ''),
    PastTBDiagnosed = ifnull(PastTBDiagnosed, ''),
    XRaySuggestiveDiagnosed = ifnull(XRaySuggestiveDiagnosed, ''),
    OtherTBDiagnosis = ifnull(OtherTBDiagnosis, ''),
    DiagnosisNotes = ifnull(DiagnosisNotes, ''),
    BaselineCHWID = ifnull(BaselineCHWID, ''),
    BaselineDate = ifnull(BaselineDate, ''),
    BaselineWeight = ifnull(BaselineWeight, ''),
    BaselinePatientCategory = ifnull(BaselinePatientCategory, ''),
    BaselinePatientType = ifnull(BaselinePatientType, ''),
    BaselineRegimen = ifnull(BaselineRegimen, ''),
    FDCTablets = ifnull(FDCTablets, ''),
    Streptomycin = ifnull(Streptomycin, ''),
    TreatmentEndDate = ifnull(TreatmentEndDate, ''),
    TreatmentDuration = ifnull(TreatmentDuration, ''),
    TreatmentOutcome = ifnull(TreatmentOutcome, ''),
    TreatmentOutcomeDetails = ifnull(TreatmentOutcomeDetails, '');

-- Save as CSV file
select "PatientID","GP","GPQualification","GPSpeciality","Screener","DOB","BirthYear","ScreeningAge","Gender","MaritalStatus","Religion","Caste","Weight","Height","ScreeningDate","ScreeningYear","DateRegistered","RegistrationYear","HasTreatmentSupporter","DiseaseConfirmed","DiseaseCategory","DiseaseSite","LastRegimen","DoseCombination","OtherDoseDescription","TreatmentPhase","PatientStatus","PatientType","TreatedPreviously","CompletedPreviousTreatment","Fever","Cough","CoughDuration","ProductiveCough","BloodInCough","NightSweats","WeightLoss","TBHistory","TBInFamily","SmearTested","DateSputumSubmitted","DateSputumTested","SputumResultDelay","SmearResult","SmearPositive","XRayDone","XRayDate","DateXRayReported","XRayResultDelay","XRayResults","XRayRemarks","XRayIndicative","GeneXpertTested","DateGXPTested","GXPDelay","GeneXpertResult","DrugResistance","GXRemarks","GXPPositive","DiagnosedBy","DiagnosisDate","DiagnosisAntibiotic","TBSymptomsDiagnosed","TBContactDiagnosed","Diagnosis","LargeLymphDiagnosed","LymphBiopsyDiagnosed","MantouxDiagnosed","PastTBDiagnosed","XRaySuggestiveDiagnosed","OtherTBDiagnosis","DiagnosisNotes","BaselineCHWID","BaselineDate","BaselineWeight","BaselinePatientCategory","BaselinePatientType","BaselineRegimen","FDCTablets","Streptomycin","TreatmentEndDate","TreatmentDuration","TreatmentOutcome","TreatmentOutcomeDetails"
union
select * FROM tb_data 
into outfile 'd:\\Datasets\\Tuberculosis\\dataset.csv'
fields terminated by ',' enclosed by '"' lines terminated by '\n'
-- ##################################################################################################################################
