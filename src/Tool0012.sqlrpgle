**Free

//
// Tool0012
//
// Created by: github.com/TickMogne, 2022.07.08
//
// Retrieve the user's SMTP-Name (see the command WRKNAMSMTP).
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

Ctl-Opt DftActGrp(*No) Main(Tool0012) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0012;
  Dcl-Pi Tool0012;
    UserName Char(8);
    SmtpName Char(256);
  End-Pi;
  Dcl-S Buffer Char(32);
  Dcl-S SystemName Char(8);
  Dcl-Ds Data Qualified;
    smtpuid Char(24);
    domroute Char(256);
  End-Ds;

  // Get the system name
  qwcrneta(Buffer: %Size(Buffer): 1: 'SYSNAME   ': Error);
  SystemName = %Subst(Buffer: 25: 8);

  UserName = Upper(UserName);

  Exec Sql SELECT smtpuid, domroute
    INTO :Data
    FROM qusrsys/qatmsmtpa
    WHERE userid = :UserName AND address = :SystemName AND smtpflag = 'D';

  // If found
  If (SQLSTATE = '00000');
    SmtpName = Lower(%Trim(Data.smtpuid) + '@' + %Trim(Data.domroute));
  Else;
    SmtpName = *Blanks;
  EndIf;

End-Proc;
