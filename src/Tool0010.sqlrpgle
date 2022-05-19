**Free

//
// Tool0010
//
// Created by: github.com/TickMogne, 2022.05.18
//
// Exit program for ODBC connect (to protocol or deny a request).
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

// CREATE TABLE QGPL.ODBCLOG ( 
//   DATETIME TIMESTAMP, 
//   USERNAME CHAR(10), 
//   SERVERID CHAR(10),
//   FORMATNAME CHAR(10),
//   REQUESTFUNCTION FOR COLUMN REQFUNC INT,
//   INTERFACETYPE FOR COLUMN INTFCTYPE CHAR(63),
//   INTERFACENAME FOR COLUMN INTFCNAME CHAR(127),
//   INTERFACELEVEL FOR COLUMN INTFCLEVEL CHAR(63))
//   RCDFMT ODBCLOGR

// ADDEXITPGM EXITPNT(QIBM_QZDA_INIT)
//            FORMAT(ZDAI0100)            
//            PGMNBR(*LOW)                
//            PGM(QGPL/TOOL0010)        

Ctl-Opt DftActGrp(*No) Main(Tool0010) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0010;
  Dcl-Pi Tool0010;
    Flag Char(1);
    Request LikeDs(ZDAI0100);
  End-Pi;
  Dcl-Ds ZDAI0100 Qualified Template;
    UserName Char(10);
    ServerId Char(10);
    FormatName Char(8);
    RequestFunction Int(10);
    InterfaceType Char(63);
    InterfaceName Char(127);
    InterfaceLevel Char(63);
  End-Ds;
  Dcl-Ds Log ExtName('QGPL/ODBCLOG') Qualified End-Ds;

  // Allow all requests
  Flag = '1';

  // You can put here possible checks to deny this request
  // If (Request.UserName = 'DEVIL');
  //   Flag = '0';
  // EndIf;

  Exec Sql SET OPTION COMMIT = *NONE;

  // Build the protocol record
  Log.DATETIME = %Timestamp();
  Log.USERNAME = Request.UserName;
  Log.SERVERID = Request.ServerId;
  Log.FORMATNAME = Request.FormatName;
  Log.REQFUNC = Request.RequestFunction;
  Log.INTFCTYPE = Request.InterfaceType;
  Log.INTFCNAME = Request.InterfaceName;
  Log.INTFCLEVEL = Request.InterfaceLevel;

  // Write the protocol file
  Exec Sql INSERT INTO qgpl.odbclog
    VALUES(:Log); 

End-Proc;

