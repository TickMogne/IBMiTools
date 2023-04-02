**Free

//
// Tool0015
//
// Created by: github.com/TickMogne, 2023.04.02
//
// Show a list result on the display.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib
//  (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

Dcl-F Tool0015 WorkStn UsrOpn IndDs(Indicators);
Dcl-F QSysPrt Printer(80) OflInd(QSysPrtOverflow) UsrOpn;

Dcl-Pi *N;
  DataAreaName Char(20);
End-Pi;

Dcl-Ds Tool0015DataArea Qualified;
  UserIndexName Char(20);
  NumberOfViews Int(10);
  NumberOfRecords Int(10);
  Title Char(78);
  Header1 Char(78);
  // more Headers
End-Ds;

Dcl-Ds Indicators Len(99);
  F03 Ind Pos(3);
  F09 Ind Pos(9);
  F10 Ind Pos(10);
  PAGEUP Ind Pos(31);
  PAGEDOWN Ind Pos(32);
End-Ds;
Dcl-Ds Error LikeDs(ERRC0100);
Dcl-S Header Char(78);
Dcl-S HeaderPointer Pointer;
Dcl-S ActiveView Int(10);
Dcl-S ActivePage Int(10);
Dcl-S Load Ind;
Dcl-Ds *N;
  DSLA Char(78) Dim(18) Pos(1);
  DSLA01 Char(78) Pos(1);
  DSLA02 Char(78) Pos(79);
  DSLA03 Char(78) Pos(157);
  DSLA04 Char(78) Pos(235);
  DSLA05 Char(78) Pos(313);
  DSLA06 Char(78) Pos(391);
  DSLA07 Char(78) Pos(469);
  DSLA08 Char(78) Pos(547);
  DSLA09 Char(78) Pos(625);
  DSLA10 Char(78) Pos(703);
  DSLA11 Char(78) Pos(781);
  DSLA12 Char(78) Pos(859);
  DSLA13 Char(78) Pos(937);
  DSLA14 Char(78) Pos(1015);
  DSLA15 Char(78) Pos(1093);
  DSLA16 Char(78) Pos(1171);
  DSLA17 Char(78) Pos(1249);
  DSLA18 Char(78) Pos(1327);
End-Ds;
Dcl-S I Int(10);
Dcl-S Key Int(10);
Dcl-S CPFMessageFile Char(20) Inz('QCPFMSG   *LIBL');
Dcl-S Elo Char(8);
Dcl-S Ret Int(10);
Dcl-S KeyChar Char(4);
Dcl-S Entry Char(20000);
Dcl-S Message Char(80);
Dcl-Ds QSysPrtLine Len(80) Qualified;
  Line Char(80) Pos(1);
End-Ds;

ActiveView = 1;
ActivePage = 1;
Load = *On;

// Put the cursor to the status line
DHLINE = 24;
DHPOSI = 1;

// Read the list definition
Tool0015DataArea = RetrieveDataArea(DataAreaName: 1: %Size(Tool0015DataArea): Error);
If (Error.BytesAvailable > 0); // Check error
  EscapeMessage(Error);
  Return;
EndIf;

// Load the text of the headers
HeaderPointer = %Alloc(Tool0015DataArea.NumberOfViews * 78);
memcpy(HeaderPointer: %Addr(Tool0015DataArea.Header1): 78);
If (Tool0015DataArea.NumberOfViews > 1);
  I = 2;
  DoW (I <= Tool0015DataArea.NumberOfViews);
    Header = RetrieveDataArea(DataAreaName: %Size(Tool0015DataArea)+((I-2)*78)+1: 78: Error);
    memcpy(HeaderPointer + (I-1)*%Size(Header): %Addr(Header): 78);
    I = I + 1;
  EndDo;
EndIf;

// Init the display text
TITL78 = CenterText(Tool0015DataArea.Title: 78);
HEAD78 = Tool0015DataArea.Header1;
KEYS78 = 'F3=Exit  F9=Print';
If (Tool0015DataArea.NumberOfViews > 1);
  KEYS78 = %Trim(KEYS78) + '  F10=Change view';
EndIf;

Open Tool0015;
Write TOOL0015X;
// Write the number of records in the status line
Message = 'Number of records: ' + %Char(Tool0015DataArea.NumberOfRecords);
qmhsndpm('CPF9897': CPFMessageFile: Message: %Len(%Trim(Message)): '*STATUS': '*EXT': 0: '': Error);

Clear Indicators;
// Loop until F3 was pressed
DoW (F03 = *Off);
  // Load the content of the page
  If (Load = *On);
    Clear DSLA;
    I = 1;
    Key = (ActivePage-1) * 18 + 1;
    Dow ((I <= 18) And (Key <= Tool0015DataArea.NumberOfRecords));
      memcpy(%Addr(KeyChar): %Addr(Key): 4);
      qusrtvui(Entry: %Size(Entry): Elo: 8: Ret: '': Tool0015DataArea.UserIndexName: 'IDXE0100': 1: 1:
        KeyChar: 4: 0: Error);
      DSLA(I) = %Subst(Entry: 13 + (ActiveView-1)*78: 78);
      I += 1;
      Key += 1;
    EndDo;
  EndIf;
  // Show the page
  Exfmt TOOL0015A;
  Load = *Off; 
  If (PAGEUP = *On); // PageUp was pressed
    If (ActivePage > 1);
      ActivePage -= 1;
      Load = *On;
    Else;
      qmhsndpm('CPD6A66': CPFMessageFile: '': 0: '*STATUS': '*EXT': 0: '': Error);
    EndIf;
  ElseIf (PAGEDOWN = *On); // PageDown was pressed
    If ((ActivePage*18+1) <= Tool0015DataArea.NumberOfRecords);
      ActivePage += 1;
      Load = *On;
    Else;
      qmhsndpm('CPD6A69': CPFMessageFile: '': 0: '*STATUS': '*EXT': 0: '': Error);
    EndIf;
  ElseIf (F09 = *On); // F9 - print - was pressed
    Open QSysPrt;
    QSysPrtOverflow = *On;
    I = 1;
    Key = (ActivePage-1) * 18 + 1;
    Dow ((I <= 18) And (Key <= Tool0015DataArea.NumberOfRecords));
      If (QSysPrtOverflow = *On);
        QSysPrtLine.Line = ' ' + TITL78;
        Write QSysPrt QSysPrtLine;
        QSysPrtOverflow = *Off;
      EndIf;    
      memcpy(%Addr(KeyChar): %Addr(Key): 4);
      qusrtvui(Entry: %Size(Entry): Elo: 8: Ret: '': Tool0015DataArea.UserIndexName: 'IDXE0100': 1:
        1: KeyChar: 4: 0: Error);
      QSysPrtLine.Line = ' ' + %Subst(Entry: 13 + (ActiveView-1)*78: 78);
      Write QSysPrt QSysPrtLine;
      I += 1;
      Key += 1;
    EndDo;
    Close QSysPrt;
  ElseIf ((F10 = *On) And (Tool0015DataArea.NumberOfViews > 1)); // F10 - change view - pass pressed
    ActiveView += 1;
    If (ActiveView > Tool0015DataArea.NumberOfViews);
      ActiveView = 1;
    EndIf;
    memcpy(%Addr(HEAD78): HeaderPointer + (ActiveView-1)*78: 78);
    Load = *On;
  EndIf;
EndDo;
Close Tool0015;

Dealloc HeaderPointer;

Return;
