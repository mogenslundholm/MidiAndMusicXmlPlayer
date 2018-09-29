//---------------------------------------------------------------------------
//
//  Program:     UnitMessage.pas in MidiAndMusicXmlPlayer
//
//  Project:     MidiAndMusicXmlPlayer.exe and MidiAndMusicXmlPlayer.app
//
//  Purpose:     To provide a message window for MidiAndMusicXmlPlayer.exe and
//               and MidiAndMusicXmlPlayer.app.
//               Used at start up and in case of errors.
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayer.dpr) or
//               Lazarus (MidiAndMusicXmlPlayer.lpi)
//               or Delphi XE (MidiAndMusicXmlPlayer.dproj)
//
//  Description: A number of track bars are defined, one for each channel
//               and one for main volume. The number of each track/channel
//               is put into edit boxes
//
//---------------------------------------------------------------------------

unit UnitMessage;

{$ifdef FPC} 
{$mode delphi}
{$endif}

interface

uses
{$ifdef FPC}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
{$else}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
{$endif}
  UnitMidiDefinitions;


type

  { TFormMessage }

  TFormMessage = class(TForm)
    LabelMessage: TLabel;
    Timer: TTimer;
{$ifdef FPC}
    procedure OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OnDropFiles(Sender: TObject; const FileNames: array of String);
{$endif}
    procedure TimerTimer(Sender: TObject);
    procedure OnCreate(Sender: TObject);
  private
    { Private declarations }
    TimeCount: int64;
  public
    procedure ErrorMessage(c: string; e: string);
    procedure Create;
    { Public declarations }
  end;

var
  FormMessage: TFormMessage;

implementation


{$ifdef FPC} 
{$R *.lfm}
{$else}
{$R *.DFM}
{$endif}

{ TFormMessage }

//---------------------------------------------------------------------------
//
//     Function:   TimerTimer
//
//     Purpose:    Process the timer event. Count at start up to shift focus
//                 to the midi window. This is done to avoid the boxes jumping
//                 around in the midi window.
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMessage.TimerTimer(Sender: TObject);

{$ifdef FPC}
var s: string;
{$endif}

  begin
  if TimeCount<10 then inc(TimeCount);
  if (SystemState in [MidiUp,MidiPlaying]) and
     (Errors=0) then
    begin
    if (TimeCount>=10) and MidiVisible then
      Visible:=false;
    end;


  if SystemState=MidiNoFile then
    begin
{$ifdef FPC}
    s:=Paramstr(1);
    LabelMessage.Caption:='No File '+s;
{$endif}
    Visible:=false;
    end;
  //// Funkar inte
  if SystemState=MidiStarting then
    begin
    Visible:=true;
    end;
{$ifdef Windows}
  if Visible then Setfocus;
{$endif}
  end;

//---------------------------------------------------------------------------
//
//     Function:   FormDropFiles
//
//     Purpose:    To take dropped file as parameter for the Midi program
//                 In Lazarus this windows gets the drop-message
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      Only first file is opened
//                 Delphi gets the dropped file to the UnitMidi-window
//---------------------------------------------------------------------------

{$ifdef FPC}


procedure TFormMessage.OnDropFiles(Sender: TObject;
  const FileNames: array of String);

var s: string;   // First file name of dropped files

  begin
  s:=FileNames[0];
  FileName:=s;
  Create;
  SystemState:=MidiStarting;
  if (High(FileNames)>0) and (not FileExists(FileName) and
     (not FileExists(FileName))) then
    begin
    SystemState:=MidiNoFile;
    Visible:=false;
    end;
  end;

//---------------------------------------------------------------------------
//
//     Function:   OnClose
//
//     Purpose:    Handle the close message
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      Only for Lazarus
//
//---------------------------------------------------------------------------

procedure TFormMessage.OnClose(Sender: TObject; var CloseAction: TCloseAction);

  begin
  if Visible then
    begin
  Visible:=false;
  CloseAction:=caHide;
    end
  else
    MidiShallClose:=true;
  Errors:=0;
  end;

{$endif}

//---------------------------------------------------------------------------
//
//     Function:   OnCreate
//
//     Purpose:    Make the message window at start up - waiting for the
//                 midi window to be ready.
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMessage.OnCreate(Sender: TObject);

  begin
  FileName:=ParamStr(1);
{$ifdef FPC}
{$ifdef Darwin}
  if (ParamCount>0) and FileExists(ParamStr(1)) then FileName:=ParamStr(1);
  Create;
{$else}
  if (ParamCount>0) and FileExists(AnsiToUtf8(FileName)) then
    begin
    FileName:=AnsiToUtf8(FileName);
    Create;
    end
  else if (ParamCount>0) and FileExists(FileName) then
    begin
    Create;
    end
  else if (ParamCount>0) then
    begin
    SystemState:=MidiError;
    ErrorMessage('File Open','File does not exist: '+FileName)
    end
  else
    begin
    SystemState:=MidiNoFile;
    Visible:=false;
    end;

{$endif}
{$else}
  if (ParamCount>0) and FileExists(ParamStr(1)) then
    begin
    FileName:=ParamStr(1);
    Create;
    end
  else if (ParamCount>0) then
    begin
    SystemState:=MidiError;
    messagebox(0,PChar('File does not exist: '+ParamStr(1)),'File Open',0);
    halt(1);
    end
  else
    begin
    SystemState:=MidiNoFile;
    Visible:=false; 
    end;
{$endif}

{$ifndef FPC}
  if (ParamCount>0) and (not FileExists(FileName)) then
{$else}
  if (ParamCount>0) and (not FileExists(FileName) and
     (not FileExists(FileName))) then
{$endif}
    begin
    if SystemState<>MidiError then SystemState:=MidiNoFile;
    Visible:=false;
    end;
  end;

//---------------------------------------------------------------------------
//
//     Function:   Create
//
//     Purpose:    Make the message window at start up - waiting for the
//                 midi window to be ready. This function may also be called
//                 later for new initialisation
//
//     Parameters: none
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMessage.Create;

var w: integer;    // Width of the message text
    h: integer;    // Height of the message text
    wmin: integer;  // Minimum width of the message text

  begin
{$ifdef Darwin}
  Visible:=true;
{$endif}
  SystemState:=MidiStarting;
  MidiVisible:=false;
  Color:=$20FFFF;
  TimeCount:=0;
  Caption:='Midi and MusicXml Player '+VersionText;
  LabelMessage.Caption:=LinguaTextStarting;
  if (ParamCount>=1) or (FileName<>'') then
{$ifdef FPC}
{$ifdef DARWIN}
  LabelMessage.Caption:=LinguaTextLoading+': '+ExtractFileName(FileName);
{$else}
////  LabelMessage.Caption:=LinguaTextLoading+': '+ExtractFileName(AnsiToUTF8(FileName));
  LabelMessage.Caption:=LinguaTextLoading+': '+ExtractFileName(FileName);
{$endif}
{$else}
  LabelMessage.Caption:=LinguaTextLoading+': '+ExtractFileName(FileName);
{$endif}

  LabelMessage.Font.Size:=12;
  LabelMessage.Font.Name:='Arial';
  Font.Size:=12;
  Font.Name:='Arial';

  w:=LabelMessage.Canvas.TextExtent(LabelMessage.Caption).cx;
  h:=LabelMessage.Canvas.TextExtent(LabelMessage.Caption).cy;
  Width:=(7*w) div 5;
  // Minimum width
  wmin:=LabelMessage.Canvas.TextExtent('Midi and MusicXml Player '+'Version 0.99999 Build xxxx').cx;
  if ((7*wmin) div 5)>Width then Width:=((7*wmin) div 5);
  LabelMessage.Left:=(Width div 2)-(w div 2);
{$ifdef FPC}
  Height:=4*h;
{$else}
  Height:=6*h;
{$endif}
{$ifdef Windows}

{$ifdef FPC}
  LabelMessage.Top:=(ClientHeight div 2)-(Canvas.TextExtent('A').cy div 2);
{$else}
  LabelMessage.Top:=(Height div 2)-Canvas.TextExtent('A').cy-h;
{$endif}
  // Be sure to see it
  Left:=(Screen.Width div 50)-(Width div 50);
  Top:=(Screen.Height div 50)-(Height div 50);
{$endif}
  Refresh;
  end;

//---------------------------------------------------------------------------
//
//     Function:   ErrorMessage
//
//     Purpose:    Make window visible and write error text
//
//     Parameters: e: The error text to be written
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMessage.ErrorMessage(c: string; e: string);

var w: integer;     // Width of the message text
    h: integer;     // Height of the message text
    wmin: integer;  // Minimum width of the message text
    nf: integer;     // New font size

  begin
  inc(Errors);
  nf:=12;

    repeat
    LabelMessage.Font.Size:=nf;
    Font.Size:=nf;
  LabelMessage.Caption:=e;
  Caption:=c;
  w:=LabelMessage.Canvas.TextExtent(LabelMessage.Caption).cx;
  h:=LabelMessage.Canvas.TextExtent(LabelMessage.Caption).cy;
    dec(nf);
    until (w<4*Screen.Width div 5) or (nf<6);

  Width:=((9*w) div 8)+ClientWidth div 10;
  // Minimum width
  wmin:=LabelMessage.Canvas.TextExtent(VersionText).cx;
  if ((9*wmin) div 8)>Width then
    begin
    Width:=((9*wmin) div 8)+ClientWidth div 10;
    w:=wmin;
    end;

  LabelMessage.Left:=(Width div 2)-(w div 2);
{$ifdef FPC}
  Height:=4*h;
{$else}
  Height:=6*h;
{$endif}
  Visible:=true;
  SetFocus;
{$ifdef FPC}
  Refresh;
  Sleep(100);
{$endif}
  end;

end.
