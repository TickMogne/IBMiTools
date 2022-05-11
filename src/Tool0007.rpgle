**Free

//
// Tool0007
//
// Created by: github.com/TickMogne, 2022.05.11
//
// Enable disabled NetServer user.
// See the command Tool0007.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0007) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0007;
  Dcl-Pi Tool0007;
    UserName Char(10);
  End-Pi;
  Dcl-Ds ZLSS0200 Len(14) Qualified;
    *N Int(10) Inz(1);
    UserName Char(10);
  End-Ds;
  Dcl-Ds Error LikeDs(ERRC0100);

  ZLSS0200.UserName = UserName;

  // Enable user 
  qzlschsi(ZLSS0200: %Size(ZLSS0200): 'ZLSS0200': Error);
  If (Error.BytesAvailable > 0); // Check error
    EscapeMessage(Error);
    Return;
  EndIf;

End-Proc;