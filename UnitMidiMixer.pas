//---------------------------------------------------------------------------
//
//  Program:     UnitMidiMixer.pas
//
//  Project:     MidiAndMusicXmlPlayer.exe, MidiAndMusicXmlPlayer.app
//
//  Purpose:     To adjust the volume of each track/channel
//
//  Compilation: Compile with Delphi 4 (Midi.dpr) or Lazarus (Midi.lpr)
//
//  Description: A number of track bars are defined, one for each channel
//               and one for main volume. The number of each track/channel
//               is put into edit boxes.
//
//---------------------------------------------------------------------------

unit UnitMidiMixer;

{$ifdef FPC}
{$MODE Delphi}
{$endif}
interface

uses
{$ifdef FPC}
  LCLIntf, LCLType, LMessages,
{$else}
  Windows,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, UnitMidiDefinitions;

type

  { TFormMidiMixer }

  TFormMidiMixer = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    EditMain: TEdit;
    TrackBarMain: TTrackBar;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    TrackBar9: TTrackBar;
    TrackBar10: TTrackBar;
    TrackBar11: TTrackBar;
    TrackBar12: TTrackBar;
    TrackBar13: TTrackBar;
    TrackBar14: TTrackBar;
    TrackBar15: TTrackBar;
    TrackBar16: TTrackBar;
    Timer: TTimer;
    BevelOuter: TBevel;
    BevelMain: TBevel;
    BevelMixer: TBevel;
    LabelMixer100: TLabel;
    LabelMixer200: TLabel;
    LabelMixer0: TLabel;
    LabelMain100: TLabel;
    LabelMain0: TLabel;
    procedure OnCreate(Sender: TObject);
    procedure OnResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    // Use arrays for the trackbars and the edit boxes
    Trackbars: array[1..16] of TTrackbar;
    EditBoxes: array[1..16] of TEdit;

  public
    { Public declarations }
  end;

function TrackbarValue(v: integer): integer;

var
  FormMidiMixer: TFormMidiMixer;

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.DFM}
{$endif}

const GLUE = 5;

//---------------------------------------------------------------------------
//
//     Function:    TrackbarValue
//
//     Purpose:     Trackbars are opposite direction on MAC
//
//     Parameters:  Value to be converted
//
//     Returns:     On PC:   127-Value
//                  On MAC:  Value
//
//     Notes:       none
//
//---------------------------------------------------------------------------

function TrackbarValue(v: integer): integer;

  begin
{$ifdef Darwin}
  TrackbarValue:=v;
{$else}
  TrackbarValue:=127-v;
{$endif}
  end;

//---------------------------------------------------------------------------
//
//     Function:    OnCreate
//
//     Purpose:     Make all the boxes and trackbars be addressed through
//                  the arrays
//
//     Parameters:  Windows standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure TFormMidiMixer.OnCreate(Sender: TObject);

  begin
  Color:=$CF;
  Trackbars[1]:=Trackbar1;
  Trackbars[2]:=Trackbar2;
  Trackbars[3]:=Trackbar3;
  Trackbars[4]:=Trackbar4;
  Trackbars[5]:=Trackbar5;
  Trackbars[6]:=Trackbar6;
  Trackbars[7]:=Trackbar7;
  Trackbars[8]:=Trackbar8;
  Trackbars[9]:=Trackbar9;
  Trackbars[10]:=Trackbar10;
  Trackbars[11]:=Trackbar11;
  Trackbars[12]:=Trackbar12;
  Trackbars[13]:=Trackbar13;
  Trackbars[14]:=Trackbar14;
  Trackbars[15]:=Trackbar15;
  Trackbars[16]:=Trackbar16;
  EditBoxes[1]:=Edit1;
  EditBoxes[2]:=Edit2;
  EditBoxes[3]:=Edit3;
  EditBoxes[4]:=Edit4;
  EditBoxes[5]:=Edit5;
  EditBoxes[6]:=Edit6;
  EditBoxes[7]:=Edit7;
  EditBoxes[8]:=Edit8;
  EditBoxes[9]:=Edit9;
  EditBoxes[10]:=Edit10;
  EditBoxes[11]:=Edit11;
  EditBoxes[12]:=Edit12;
  EditBoxes[13]:=Edit13;
  EditBoxes[14]:=Edit14;
  EditBoxes[15]:=Edit15;
  EditBoxes[16]:=Edit16;
  end;

//---------------------------------------------------------------------------
//
//     Function:    OnResize
//
//     Purpose:     Position and size all elements relative in window
//
//     Parameters:  Windows standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure TFormMidiMixer.OnResize(Sender: TObject);

var i: integer;

  begin
  for i:=1 to 16 do
    begin
    Trackbars[i].Top:=2*EditBoxes[i].Height+GLUE;
    Trackbars[i].Left:=(ClientWidth div 21)*(i+2);
    Trackbars[i].Height:=(ClientHeight div 20)*17;
    EditBoxes[i].Top:=EditBoxes[i].Height;
    EditBoxes[i].Left:=Trackbars[i].Left;
    EditBoxes[i].Width:=Trackbars[1].Width;
    EditBoxes[i].Text:=IntToStr(i);
    if i>MidiNumberOfChannels then
      begin
      EditBoxes[i].Enabled:=false;
      Trackbars[i].Enabled:=false;
      EditBoxes[i].Color:=$202020;
{$ifndef FPC}
      Trackbars[i].SliderVisible:=false;
{$endif}
      end
    else
      begin
      EditBoxes[i].Color:=$00CF00;
      EditBoxes[i].Enabled:=true;
      Trackbars[i].Enabled:=true;
{$ifndef FPC}
      Trackbars[i].SliderVisible:=true;
{$endif}
      end;
    end;

  TrackbarMain.Top:=2*EditMain.Height+GLUE;
  TrackbarMain.Left:=(ClientWidth div 40);
  TrackbarMain.Height:=(ClientHeight div 20)*17;
  TrackbarMain.Width:=ClientWidth div 25;
  EditMain.Top:=EditMain.Height;
  EditMain.Left:=TrackbarMain.Left;
  EditMain.Width:=ClientWidth div 15;
  EditMain.Text:='Main';
  EditMain.Color:=EditBoxes[1].Color;

  TrackbarMain.Visible:=true;
  EditMain.Visible:=true;
  for i:=1 to 16 do Trackbars[i].Visible:=true;
  for i:=1 to 16 do EditBoxes[i].Visible:=true;
  for i:=1 to 16 do EditBoxes[i].ReadOnly:=true;
  BevelOuter.Top:=GLUE;
  BevelOuter.Left:=GLUE div 2;
  BevelOuter.Height:=ClientHeight-GLUE;
  BevelOuter.Width:=ClientWidth-GLUE;

  BevelMain.Top:=2*GLUE;
  BevelMain.Left:=2*GLUE;
  BevelMain.Height:=ClientHeight-3*GLUE;
  BevelMain.Width:=((ClientWidth*12) div 100)-2*GLUE;

  BevelMixer.Top:=2*GLUE;
  BevelMixer.Left:=((ClientWidth*12) div 100)+GLUE;;
  BevelMixer.Height:=ClientHeight-3*GLUE;
  BevelMixer.Width:=ClientWidth-((ClientWidth*12) div 100)-3*GLUE;

  LabelMixer200.Top:=TrackBars[1].Top;
  LabelMixer200.Left:=TrackBars[16].Left+TrackBars[16].Width+GLUE;
  LabelMixer100.Top:=TrackBars[1].Top+(TrackBars[1].Height div 2)-(LabelMixer0.Height div 2);
  LabelMixer100.Left:=TrackBars[16].Left+TrackBars[16].Width+GLUE;
  LabelMixer0.Top:=TrackBars[1].Top+TrackBars[1].Height-LabelMixer0.Height;
  LabelMixer0.Left:=TrackBars[16].Left+TrackBars[16].Width+GLUE;

  LabelMain100.Top:=TrackBars[1].Top;
  LabelMain100.Left:=TrackBarMain.Left+TrackBarMain.Width+GLUE;
  LabelMain0.Top:=TrackBars[1].Top+TrackBars[1].Height-LabelMixer0.Height;
  LabelMain0.Left:=TrackBarMain.Left+TrackBarMain.Width+GLUE;
  end;

//---------------------------------------------------------------------------
//
//     Function:    TimerTimer
//
//     Purpose:     In order to avoid to many resizes, a flag is set
//                  and updating done with this timer.
//
//     Parameters:  Windows standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure TFormMidiMixer.TimerTimer(Sender: TObject);

var
  i: integer;

  begin
  if MidiMixerMustResize then
    begin
    // Set trackbar positions
    MidiMixerMustResize:=false;
    OnResize(Sender);
    TrackbarMain.Position:=TrackbarValue(MidiDataMixer[0]);
    for  i:=1 to 16 do Trackbars[i].Position:=TrackbarValue(MidiDataMixer[i]);
    end
  else
    begin
    // Read trackbar positions
    MidiDataMixer[0]:=TrackbarValue(TrackbarMain.Position);
     // Outside or zero - set neutral
    if (MidiDataMixer[0]>127) or (MidiDataMixer[0]<=0) then MidiDataMixer[0]:=64;
    for  i:=1 to 16 do MidiDataMixer[i]:=TrackbarValue(Trackbars[i].Position);
    end;
  end;

end.
