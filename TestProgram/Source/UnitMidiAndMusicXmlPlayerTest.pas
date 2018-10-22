//---------------------------------------------------------------------------
//
//  Program:     MidiAndMusicXmlPlayerTest.dpr
//
//  Project:     This is a test program for MidiAndMusicXmlPlayer.exe
//
//  Purpose:     It takes all MusicXml files and runs MidiAndMusicXmlPlayer
//               in test mode (-T). At the end it calls the program WinMerge
//               to compare the files.
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayerTest.dpr)
//
//  Description: Two kind of threads are defined: FileCentral and PlayProgram
//               in units UnitFileCentral and Unit PlayProgram
//
//---------------------------------------------------------------------------

unit UnitMidiAndMusicXmlPlayerTest;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  UnitPlayProcess,
  UnitFileCentral,
  UnitSemaphores, ExtCtrls, ComCtrls, Menus, Buttons;

const NoOfProcesses = 7;   // This makes max 7 instances - for 4 kernels

type
  TFormMidiAndMusicXmlPlayerTest = class(TForm)
    ButtonRunTest: TButton;
    Timer: TTimer;
    LabelTimeElapsed: TLabel;
    StatusBar: TStatusBar;
    MemoLog: TMemo;
    EditTimeElapsed: TEdit;
    EditFileNumber: TEdit;
    LabelFileNumber: TLabel;
    LabelRunTestLog: TLabel;
    MainMenu1: TMainMenu;
    MainMenu: TMenuItem;
    MenuFile: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemWinMerge: TMenuItem;
    ImageButtonRunTest: TImage;
    Help1: TMenuItem;
    About: TMenuItem;
    procedure ButtonRunTestClick(Sender: TObject);
    procedure OnCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure OnResize(Sender: TObject);
    procedure MenuItemWinMergeClick(Sender: TObject);
    procedure OnClick(Sender: TObject);
    procedure OnClose(Sender: TObject; var Action: TCloseAction);
    procedure AboutClick(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    PlayProcess: array[1..NoOfProcesses] of TPlayProcess;
  public
    { Public declarations }
  end;

var
  FormMidiAndMusicXmlPlayerTest: TFormMidiAndMusicXmlPlayerTest;

implementation

var
  TimeElapsed: int64 = 0;

{$R *.DFM}

//---------------------------------------------------------------------------
//
//     Procedure:  ExtZero
//
//     Purpose:    To add a zero showing the time if only one digit
//                 (f.x. "5" becomes "05" )
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

function ExtZero(s: string): string;

  begin
  while Length(s)<2 do s:='0'+s;
  ExtZero:=s;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  ButtonRunTestClick
//
//     Purpose:    To start execution of test, when the button is pressed
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.ButtonRunTestClick(Sender: TObject);

  begin
  TimeElapsed:=0;
  MemoLog.Clear;
  ButtonRunTest.Enabled:=false;
  FileNumberFinished:=0;
  Semaphore.Send(FileCentralStart,NoReturn,'Start');
  end;

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

procedure TFormMidiAndMusicXmlPlayerTest.OnCreate(Sender: TObject);

var i: integer;

  begin
  // Define two fields in the status line
  StatusBar.Panels.Add;
  StatusBar.Panels.Add;
  MemoLog.Clear;

  // Initialize all semaphores
  Semaphore.Initialize;
  Semaphore.UseSemaphore(PlayStart);
  Semaphore.UseSemaphore(PlayTicket);
  Semaphore.UseSemaphore(PlayReturn);
  Semaphore.UseSemaphore(PlayLog);
  Semaphore.UseSemaphore(FileCentralStart);

  // Create the processes
  TFileCentral.Create(false);
  for i:=1 to NoOfProcesses do
  PlayProcess[i]:=TPlayProcess.Create(false);
  if (ParamCount>0) and (ParamStr(1)='-C') then
    begin
    ButtonRunTestClick(Sender);
    end;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  TimerTimer
//
//     Purpose:    Process the timer event. Show elapsed time
//                 (EditTimeElapsed-field) and test if more files are
//                 finished. If so show them in the log-window (MemoLog-field).
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.TimerTimer(Sender: TObject);

var SemReturn: longint;
    SemMessage: string;

  begin
  if TimeRunning then inc(TimeElapsed);
  EditTimeElapsed.Text:=ExtZero(IntToStr(TimeElapsed div 600))+':'+
                        ExtZero(IntToStr((TimeElapsed mod 600) div 10));
  EditFileNumber.Text:=IntToStr(FileNumber);

  while Semaphore.ReceiveNoWait(PlayLog,SemReturn,SemMessage) do
    MemoLog.Lines.Add(SemMessage);
  while Semaphore.ReceiveNoWait(PlayReturn,SemReturn,SemMessage) do
    inc(FileNumberFinished);

  StatusBar.Panels[1].Text:=CurrentFile;
  // Since WinMerge is executed also FileNumberFinished will be increased at end
  if FileNumberFinished>FileNumber then
    begin
    FileNumberFinished:=FileNumber;
    ButtonRunTest.Enabled:=true;
    ImageButtonRunTest.Enabled:=true;
    end;
  StatusBar.Panels[0].Text:='No: '+IntToStr(FileNumber-FileNumberFinished);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  OnResize
//
//     Purpose:    To position the Window-fields in the window
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.OnResize(Sender: TObject);

var i,j: integer;
    TextSize: TSize;  // For the button text size

  begin
  Color:=$CF;
  LabelTimeElapsed.Top:=LabelTimeElapsed.Height;
  LabelTimeElapsed.Left:=2;
  LabelTimeElapsed.Width:=ClientWidth div 8;

  LabelFileNumber.Top:=LabelTimeElapsed.Height;
  LabelFileNumber.Left:=LabelTimeElapsed.Left+LabelTimeElapsed.Width+5;
  LabelFileNumber.Left:=LabelTimeElapsed.Width+5;
  LabelFileNumber.Width:=ClientWidth div 8;

  EditTimeElapsed.Top:=LabelTimeElapsed.Top+LabelTimeElapsed.Height;
  EditTimeElapsed.Left:=LabelTimeElapsed.Left;
  EditTimeElapsed.Width:=LabelTimeElapsed.Width;

  EditFileNumber.Top:=EditTimeElapsed.Top;
  EditFileNumber.Left:=LabelFileNumber.Left;
  EditFileNumber.Font:=EditTimeElapsed.Font;
  EditFileNumber.Width:=LabelFileNumber.Width;

  ButtonRunTest.Top:=EditTimeElapsed.Top-30;
  ButtonRunTest.Left:=LabelFileNumber.Left+LabelFileNumber.Width+5;
  ButtonRunTest.Width:=ClientWidth-ButtonRunTest.Left-2;
  ButtonRunTest.Height:=EditTimeElapsed.Height;
  ButtonRunTest.Visible:=false;

  // A button looks so bad in WIN 7
  ImageButtonRunTest.Create(nil);
  ImageButtonRunTest.Top:=EditTimeElapsed.Top;
  ImageButtonRunTest.Left:=LabelFileNumber.Left+LabelFileNumber.Width+5;
  ImageButtonRunTest.Width:=ClientWidth-ButtonRunTest.Left-5;
  ImageButtonRunTest.Height:=EditTimeElapsed.Height;
  ImageButtonRunTest.Canvas.Brush.Color:=$A08080;
  ImageButtonRunTest.Canvas.Pen.Color:=$0;
  ImageButtonRunTest.Canvas.Font.Size:=14;
  ImageButtonRunTest.Canvas.Font.Name:='Arial';
  TextSize:=ImageButtonRunTest.Canvas.TextExtent('Run Test');
  for i:=2 to ImageButtonRunTest.Width-3 do
  for j:=2 to ImageButtonRunTest.Height-3 do
    ImageButtonRunTest.Canvas.Pixels[i,j]:=$A08080;
  for i:=1 to ImageButtonRunTest.Width-1 do
    ImageButtonRunTest.Canvas.Pixels[i,ImageButtonRunTest.Height-1]:=$0;
  for i:=1 to ImageButtonRunTest.Height-1 do
    ImageButtonRunTest.Canvas.Pixels[ImageButtonRunTest.Width-1,i]:=$0;
  for i:=2 to ImageButtonRunTest.Width-1 do
    ImageButtonRunTest.Canvas.Pixels[i,ImageButtonRunTest.Height-2]:=$0;
  for i:=2 to ImageButtonRunTest.Height-1 do
    ImageButtonRunTest.Canvas.Pixels[ImageButtonRunTest.Width-2,i]:=$0;
  ImageButtonRunTest.Canvas.TextOut((ImageButtonRunTest.Width-TextSize.cx) div 2,2,'Run Test');

  LabelRunTestLog.Top:=ButtonRunTest.Top+ButtonRunTest.Height+5;
  LabelRunTestLog.Left:=LabelTimeElapsed.Left;
  LabelRunTestLog.Width:=LabelTimeElapsed.Width;

  Memolog.Top:=LabelRunTestLog.Top+LabelRunTestLog.Height+2+30;
  Memolog.Left:=2;
  Memolog.Width:=ClientWidth-5;
  Memolog.Height:=ClientHeight-MemoLog.Top-StatusBar.Height-4;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  MenuItemWinMergeClick
//
//     Purpose:    To handle selection of the menu-item WinMerge
//                 if Winmerge has been closed and you want to reopen
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.MenuItemWinMergeClick(Sender: TObject);

var Command: string;

  begin
  Command:='"C:\Program Files (x86)\WinMerge\WinMergeU.exe" . ..\org';
  Semaphore.Send(PlayStart,NoReturn,Command);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  OnClick
//
//     Purpose:    To start execution of test, when the button is pressed
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      Click on the image, that looks like a button (looks better)
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.OnClick(Sender: TObject);

  begin
  ButtonRunTestClick(Sender);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  OnClose
//
//     Purpose:    To close the semaphores
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      none
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.OnClose(Sender: TObject;
  var Action: TCloseAction);

  begin
  Semaphore.CloseSemaphore(PlayStart);

  Semaphore.CloseSemaphore(PlayTicket);
  Semaphore.CloseSemaphore(FileCentralStart);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  AboutClick
//
//     Purpose:    To show the version number
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      None
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.AboutClick(Sender: TObject);

  begin
  MessageBox(0,'Version 0.905D Copyright Mogens Lundholm','Program Testing',0);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:  OnKeyPress
//
//     Purpose:    To start execution of test, when the button is pressed
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      Press any key to start processing
//
//---------------------------------------------------------------------------

procedure TFormMidiAndMusicXmlPlayerTest.OnKeyPress(Sender: TObject;
  var Key: Char);

  begin
  ButtonRunTestClick(Sender);
  end;

end.


