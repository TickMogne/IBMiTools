**Free

//
// Tool0018
//
// Created by: github.com/TickMogne, 2023.04.26
//
// End all active jobs with a specific name.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib
//  (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-Pi *N;
  JobName Char(10);
End-Pi;

Dcl-Ds Error LikeDs(ERRC0100); 
Dcl-S Receiver Char(640);
Dcl-Ds OLJB0200 Qualified;
  JobName Char(10) Pos(1);
  UserName Char(10) Pos(11);
  JobNumber Char(6) Pos(21);
  ActiveJobStatus Char(4) Pos(61);
End-Ds;
Dcl-S VariableDefinition Char(100);
Dcl-Ds ListInformation LikeDs(ListInformation_Ds);
Dcl-Ds SortInformation;
  *N Int(10) Inz(0);
  *N Int(10) Inz(0);
  *N Int(10) Inz(0);
  *N Int(5) Inz(0);
  *N Char(1) Inz('1');
  *N Char(1) Inz(x'00');
End-Ds;
Dcl-Ds SelectionInformation Len(118) Qualified;
  JobName Char(10) Pos(1);
  UserName Char(10) Pos(11);
  JobNumber Char(6) Pos(21);
  JobType Char(1) Pos(27);
  *N Char(1) Pos(28);
  OffsetPrimaryJobStatus Int(10) Pos(29) Inz(0);
  NumberOfPrimaryJobStatus Int(10) Pos(33) Inz(0);
  *N Int(10) Pos(37) Inz(0);
  *N Int(10) Pos(41) Inz(0);
  *N Int(10) Pos(45) Inz(0);
  *N Int(10) Pos(49) Inz(0);
  *N Int(10) Pos(53) Inz(0);
  *N Int(10) Pos(57) Inz(0);
  *N Int(10) Pos(61) Inz(0);
  *N Int(10) Pos(65) Inz(0);
  *N Int(10) Pos(69) Inz(0);
  *N Int(10) Pos(73) Inz(0);
  *N Int(10) Pos(77) Inz(0);
  *N Int(10) Pos(81) Inz(0);
  *N Int(10) Pos(85) Inz(0);
  *N Int(10) Pos(89) Inz(0);
  *N Int(10) Pos(93) Inz(0);
  *N Int(10) Pos(97) Inz(0);
  *N Int(10) Pos(101) Inz(0);
  *N Int(10) Pos(105) Inz(0);
  PrimaryStatus Char(10) Pos(109);
End-Ds;
Dcl-S Keys Int(10) Dim(32);
Dcl-S I Int(10);
Dcl-S J Int(10);

// Job selection
Clear SelectionInformation;
SelectionInformation.JobName = JobName;
SelectionInformation.UserName = '*ALL';
SelectionInformation.JobNumber = '*ALL';
SelectionInformation.JobType = '*';
SelectionInformation.OffsetPrimaryJobStatus = 108;
SelectionInformation.NumberOfPrimaryJobStatus = 1;
SelectionInformation.PrimaryStatus = '*ACTIVE';
Keys(1) = 0101;

// Open the list and receive the first entry
qgyoljob(Receiver : %Size(Receiver ): 'OLJB0200': VariableDefinition: 100: ListInformation: 2:
  SortInformation: SelectionInformation: %Size(SelectionInformation): 1: Keys: Error);
// API Error
If (Error.BytesAvailable > 0);
  Return;
EndIf;
I = 0;
DoW (ListInformation.RecordsReturned > 0);
  For J = 1 To ListInformation.RecordsReturned;
    OLJB0200 = %Subst(Receiver: 1 + (J-1) * %Size(OLJB0200): %Size(OLJB0200));
    ExecuteCommand('ENDJOB JOB(' + %Trim(OLJB0200.JobNumber) + '/' + %Trim(OLJB0200.UserName) + '/' +
      %Trim(OLJB0200.JobName) + ') OPTION(*IMMED)': Error);
    I += 1;
  EndFor;
  If ((ListInformation.ListStatus = '2') Or (ListInformation.ListStatus = '3'));
    Leave;
  EndIf;
  qgygtle(OLJB0200: %Size(OLJB0200): ListInformation.Handle: ListInformation: 2: I+1: Error);
  If (Error.BytesAvailable > 0);
    Leave;
  EndIf;
EndDo;

// Close the list
If (ListInformation.Handle <> *Blanks);
  qgyclst(ListInformation.Handle: Error);
EndIf;

Return;
