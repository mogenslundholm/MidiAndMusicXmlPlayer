//---------------------------------------------------------------------------
//
//  Program:     UnitSemaphores is part of MidiAndMusicXmlPlayerTest.exe
//
//  Project:     MidiAndMusicXmlPlayerTest is a test program for
//               MidiAndMusicXmlPlayer.exe
//
//  Purpose:     This object provides message semaphore funtions to send and
//               receive messages.
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayerTest.dpr)
//
//  Description: A cyclic buffer is used to handle messages. The WIN32
//               function is used to do the actual syncronize.
//
//  Notes:       The limit is 10 semaphores. Change SemAntal to increase
//---------------------------------------------------------------------------

unit UnitSemaphores;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses windows, sysutils;

const SemaAntal=10;        // Max number of semaphores
      SemaBufSize=1000;    // Max number of messages on each message semaphore

type

  TSemaphore=object
    public
    CykBuf:    array[1..SemaAntal,0..SemaBufSize-1] of string;
    CykRep:    array[1..SemaAntal,0..SemaBufSize-1] of longint;
    First:     array[1..SemaAntal] of longint;
    Last:      array[1..SemaAntal] of longint;
    Full:      array[1..SemaAntal] of bool;
    SemaInUse: array[1..SemaAntal] of bool;

    procedure Initialize;
    procedure UseSemaphore(Sema: longint);
    procedure CloseSemaphore(Sema: longint);
    procedure Send(       Sema: longint;       // Semaphore ID
                          reply: longint;      // The Semaphore for reply
                          mess: string);       // The message

    procedure Receive(    Sema: longint;       // Semaphore ID
                          var reply: longint;  // The Reply Semaphore
                          var mess: string);   // The message

    function ReceiveNoWait(Sema: longint;      // Semaphore ID
                          var reply: longint;  // The reply Semaphore
                          var mess: string): boolean; // The message
    function NoOfItems(Sema: longint): longint;

    private
    Mutex: array[1..SemaAntal] of THandle;     // Win32 semaphore for access
    Free:  array[1..SemaAntal] of THandle;     // Win32 semaphore for free positions
    Used:  array[1..SemaAntal] of THandle;     // Win32 semaphore for used positions
    end;

const PlayStart=1;
      PlayReturn=2;
      FileCentralStart=3;
      FileCentral=4;
      PlayTicket=5;
      PlayLog=6;
      NoReturn=9;

var   Semaphore: TSemaphore;
      TimeRunning: boolean=false;
      FileNumber: integer;
      CurrentFile: string;
      FileNumberFinished: integer;


implementation

//---------------------------------------------------------------------------
//
//     Procedure:  Initialize
//
//     Purpose:    Initialise the Semaphores by setting all no be not in use
//
//     Parameters: <none>
//
//     Returns:    void
//
//     Notes:      For each semaphore UseSemaphore mus be called
//
//---------------------------------------------------------------------------

procedure TSemaphore.Initialize;
var
  i: longint;

  begin
  for i:=1 to SemaAntal do
    begin
    SemaInUse[SemaAntal]:=false;
    end;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  UseSemaphore
//
//     Purpose:    Initialise use of a specific the Semaphore
//
//     Parameters: Sema = number of the semaphore
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TSemaphore.UseSemaphore(Sema: longint);

  begin
  Mutex[Sema]:=CreateSemaphore(nil,1,MaxLong,nil);
  Free[Sema]:=CreateSemaphore(nil,SemaBufSize,MaxLong,nil);
  Used[Sema]:=CreateSemaphore(nil,0,MaxLong,nil);
  First[Sema]:=0;
  Last[Sema]:=0;
  SemaInUse[Sema]:=true;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  CloseSemaphore
//
//     Purpose:    Stop using semaphore
//
//     Parameters: Sema = the number of the semaphore
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TSemaphore.CloseSemaphore(Sema: longint);

  begin
  CloseHandle(Mutex[Sema]);
  CloseHandle(Free[Sema]);
  CloseHandle(Used[Sema]);
  Used[Sema]:=0;
  First[Sema]:=0;
  Last[Sema]:=0;
  SemaInUse[Sema]:=false;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  Send
//
//     Purpose:    Send a message to the specified semaphore
//
//     Parameters: Sema = the number of the semaphore
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TSemaphore.Send(Sema: longint;    // Semaphore ID
                          reply: longint;   // The Semaphore for reply
                          mess: string);    // The message
var
  OldValue: longint;

  begin
  WaitForSingleObject(Free[Sema],INFINITE);
  WaitForSingleObject(Mutex[Sema],INFINITE);
  CykBuf[Sema,Last[Sema]]:=mess;
  CykRep[Sema,Last[Sema]]:=reply;
  Last[Sema]:=(Last[Sema]+1) mod SemaBufSize;
  ReleaseSemaphore(Mutex[Sema],1,@Oldvalue);
  ReleaseSemaphore(Used[Sema],1,@Oldvalue);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  Receive
//
//     Purpose:    Receive a message from the specified semaphore
//
//     Parameters: Sema = the number of the semaphore
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TSemaphore.Receive(Sema: longint;
                          var reply: longint;
                          var mess: string);
var
  OldValue: longint;

  begin
  WaitForSingleObject(Used[Sema],INFINITE);
  WaitForSingleObject(Mutex[Sema],INFINITE);
  mess:=CykBuf[Sema,First[Sema]];
  reply:=CykRep[Sema,First[Sema]];
  First[Sema]:=(First[Sema]+1) mod SemaBufSize;
  ReleaseSemaphore(Free[Sema],1,@Oldvalue);
  ReleaseSemaphore(Mutex[Sema],1,@Oldvalue);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  ReceiveNoWait
//
//     Purpose:    Receive a message from the specified semaphore, but
//                 do note wait
//
//     Parameters: Sema = the number of the semaphore
//
//     Returns:    true = a message was there
//
//     Notes:      none
//
//---------------------------------------------------------------------------

function TSemaphore.ReceiveNoWait(Sema: longint;
                          var reply: longint;
                          var mess: string): boolean;
var
  OldValue: longint;

  begin
  WaitForSingleObject(Mutex[Sema],INFINITE);
  if First[Sema]=Last[Sema] then
    begin
    mess:='';
    reply:=0;
    ReceiveNoWait:=false;
    end
  else
    begin
    WaitForSingleObject(Used[Sema],INFINITE);
    mess:=CykBuf[Sema,First[Sema]];
    reply:=CykRep[Sema,First[Sema]];
    First[Sema]:=(First[Sema]+1) mod SemaBufSize;
    ReleaseSemaphore(Free[Sema],1,@Oldvalue);
    ReceiveNoWait:=true;
    end;
  ReleaseSemaphore(Mutex[Sema],1,@Oldvalue);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  NoOfItems
//
//     Purpose:    Return the number of messages on the semaphore
//
//     Parameters: Sema = the number of the semaphore
//
//     Returns:    The current number of messages on the semaphore
//
//     Notes:      none
//
//---------------------------------------------------------------------------

function TSemaphore.NoOfItems(Sema: longint): longint;

var
  OldValue: longint;
  No: integer;

  begin
  WaitForSingleObject(Mutex[Sema],INFINITE);
  No:=(Last[Sema]-First[Sema]+SemaBufSize) mod SemaBufSize;
  ReleaseSemaphore(Mutex[Sema],1,@Oldvalue);
  NoOfItems:=No;
  end;

end.


