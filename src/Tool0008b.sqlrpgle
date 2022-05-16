**Free

//
// Tool0008b
//
// Created by: github.com/TickMogne, 2022.05.15
//
// Exit program for FTP server logon.
//
// Compile parameters:
//  INCDIR('xxx/TMLib/src/') where xxx the parent library of the repository TMLib (github.com/TickMogne/TMLib)
//

// CREATE TABLE QGPL.FTPLOG ( 
// 	DATETIME TIMESTAMP, 
// 	APP_ID INTEGER, 
// 	OP_ID INTEGER DEFAULT NULL, 
// 	"USER" VARCHAR(32) CCSID 1141, 
// 	REMOTE_IP VARCHAR(32) CCSID 1141, 
// 	INFO VARCHAR(1024) DEFAULT NULL CCSID 1141)   
// 	RCDFMT FTPLOGR;

// ADDEXITPGM EXITPNT(QIBM_QTMF_SVR_LOGON)
//            FORMAT(TCPL0100)            
//            PGMNBR(*LOW)                
//            PGM(QGPL/TOOL0008B)        

Ctl-Opt DftActGrp(*No) Main(Tool0008b) BndDir('TMLIB_M');

/include TMApi_Inc.rpgle
/include TMLib_Inc.rpgle

// Main procedure
Dcl-Proc Tool0008b;
  Dcl-Pi Tool0008b;
    ApplicationIdentifier Int(10);
    UserId Char(32) Options(*Varsize);
    UserIdLength Int(10);
    AuthString Char(64) Options(*Varsize);
    AuthStringLength Int(10);
    RemoteIPAddress Char(128) Options(*Varsize);
    RemoteIPAddressLength Int(10);
    ReturnCode Int(10);
    UserProfile Char(10);
    Password Char(10);
    Library Char(10);
  End-Pi;
  Dcl-Ds Log ExtName('QGPL/FTPLOG') Qualified End-Ds;
  Dcl-S LogNull Int(3) Dim(6) Inz(0);

  Exec Sql SET OPTION COMMIT = *NONE;

  Log.datetime = %Timestamp();
  Log.app_id = ApplicationIdentifier;
  Log.op_id = 0;
  LogNull(3) = -1;
  Log.user = %Subst(UserId: 1: UserIdLength);
  Log.remote_ip = %Subst(RemoteIPAddress: 1: RemoteIPAddressLength);
  Log.info = '';
  LogNull(6) = -1;

  Exec Sql INSERT INTO qgpl.ftplog
    VALUES(:Log:LogNull);  

  ReturnCode = 1;

End-Proc;

