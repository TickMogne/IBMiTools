**Free

//
// Tool0014
//
// Created by: github.com/TickMogne, 2022.11.18
//
// Delete old journal receivers.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0014) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0014;
  Dcl-Pi Tool0014;
    Journal Char(20);
    NumberOfDaysToKeep Int(10);
  End-Pi;
  Dcl-Ds Error LikeDs(ERRC0100);
  Dcl-DS InformationToRetrieve;
    *N Int(10) Inz(1);
    *N Int(10) Inz(12);
    *N Int(10) Inz(1);
    *N Int(10) Inz(0);
  End-Ds;
  Dcl-Ds RJRN0100 Len(50000) Qualified;
    BytesReturned Int(10) Pos(1);
    BytesAvailable Int(10) Pos(5);
    OffsetToInformation Int(10) Pos(9);
    NumberOfKeys Int(10) Pos(449);
  End-Ds;
  Dcl-S RJRN0100KeyPointer Pointer;
  Dcl-Ds RJRN0100Key Len(20) Based(RJRN0100KeyPointer) Qualified;
    *N Int(10) Pos(1);
    Offset Int(10) Pos(5);
    LengthHeader Int(10) Pos(9);
    NumberOfEntries Int(10) Pos(13);
    LengthOfEntry Int(10) Pos(17);
  End-Ds;
  Dcl-S RJRN0100Key1InfoPointer Pointer;
  Dcl-Ds RJRN0100Key1Info Based(RJRN0100Key1InfoPointer) Qualified;
    Name Char(10);
    Library Char(10);
    *N Char(5);
    AttachedDateTime Char(13);
    Status Char(1);
    *N Char(13);
    *N Char(8);
    *N Char(8);
    *N Int(10);
    *N Char(56);
  End-Ds;
  Dcl-S i Int(10);
  Dcl-S dt1 Timestamp Inz(*Sys);
  Dcl-S dt2 Timestamp;
  Dcl-S diff Int(10);
  Dcl-S Command Char(256);

  // Retrieve Journal Information
  QjoRetrieveJournalInformation(RJRN0100: %Size(RJRN0100): Journal: 'RJRN0100': InformationToRetrieve: Error);

  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

  // Check if buffer length was sufficient
  If (RJRN0100.BytesReturned = RJRN0100.BytesAvailable);
    // Check if the requested information is available
    If (RJRN0100.NumberOfKeys = 1);
      // Set the pointer for RJRN0100Key
      RJRN0100KeyPointer = %Addr(RJRN0100) + RJRN0100.OffsetToInformation + 4;
      // Set The pointer for RJRN0100Key1Info entry
      RJRN0100Key1InfoPointer = RJRN0100KeyPointer + RJRN0100Key.Offset + RJRN0100Key.LengthHeader;
      i = 0;
      // Loop through the entries
      Dow i < RJRN0100Key.NumberOfEntries - 2;
        // Check if not attached
        If (RJRN0100Key1Info.Status <> '1');
          dt2 = %Timestamp('20' +
                           %Subst(RJRN0100Key1Info.AttachedDateTime: 2: 2) + '-' +
                           %Subst(RJRN0100Key1Info.AttachedDateTime: 4: 2) + '-' +          
                           %Subst(RJRN0100Key1Info.AttachedDateTime: 6: 2) + '-' +          
                           %Subst(RJRN0100Key1Info.AttachedDateTime: 8: 2) + '.' +          
                           %Subst(RJRN0100Key1Info.AttachedDateTime: 10: 2) + '.' +          
                           %Subst(RJRN0100Key1Info.AttachedDateTime: 12: 2) + '.000000');
          // Check the difference between dt1 and dt2
          diff = %Diff(dt1: dt2: *DAYS);
          // Check if it should be deleted
          If (diff > NumberOfDaysToKeep);
            Command = 'DLTJRNRCV JRNRCV(' + %Trim(RJRN0100Key1Info.Library) + '/' + %Trim(RJRN0100Key1Info.Name) + ') DLTOPT(*IGNINQMSG)';
            Monitor;
              qcmdexc(Command: %Len(%Trim(Command)));
            On-Error;
            EndMon;            
          EndIf;
        EndIf;
        // Shift the entry pointer
        RJRN0100Key1InfoPointer = RJRN0100Key1InfoPointer + RJRN0100Key.LengthOfEntry;
        i = i + 1;
      EndDo;
    Else;
      EscapeMessage(*Omit: 'Key Information is not available.');
    EndIf;
  Else;
    EscapeMessage(*Omit: 'Buffer RJRN0100 is too small.');
  EndIf;

End-Proc;
