//---------------------------------------------------------------------------
//
//  Program:     UnitScore.pas in MidiAndMusicXmlPlayer
//
//  Project:     MidiAndMusicXmlPlayer.dpr / MidiAndMusicXmlPlayer.lpi
//
//  Purpose:     Draw the notes of the song. .
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayer.dpr) or
//               Lazarus (MidiAndMusicXmlPlayer.lpi)
//               or Delphi XE (MidiAndMusicXmlPlayer.dproj)
//
//  Description: The time playing a note determines the position. (Short
//               notes will be very close)
//---------------------------------------------------------------------------

unit UnitScore;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

interface

uses
{$ifdef Windows}
  Windows,
{$endif}

////  Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, UnitMidiDefinitions, StdCtrls, ComCtrls
{$ifdef FPC}
  , 
  FileUtil
{$endif}
  ;

// Note types: -1 = undefined, 0 = whole, 1 = half, 2 = quarter etc.
type TNoteType = -1..6;
// Points (Dots), max four points can be set
type TNotePoints = 0..4;

type TSymbolType = (SymbolClef,SymbolKey,SymbolTime,SymbolNote,Symbolpause);
// Type for note type and number of points added to make the length
type TNoteLength =
                  record
                  NoteType: TNoteType;
                  NotePoints: TNotePoints;
                  NoteTime: int64;
                  end;

type

  { TFormScore }

  TFormScore = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuOptions: TMenuItem;
    MenuItemInitialPause: TMenuItem;
    MenuItemExit: TMenuItem;
    StatusBar: TStatusBar;
    ScrollBox: TScrollBox;
    ImageScore: TImage;
    ImageNotes: TImage;
    MenuItemFourMeasures: TMenuItem;
    procedure OnCreate(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MenuItemInitialPauseClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure OnClose(Sender: TObject; var Action: TCloseAction);
    procedure OnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItemFourMeasuresClick(Sender: TObject);
  private
    { Private declarations }
    BitmapNotes: TBitmap;
    // Geometry
    PixLine: integer;            // a. pixels for note line
    NumLines: integer;           // b. number of lines
    NumInstruments: integer;     // c. number of channels/instruments
    PixSpaceInSys: integer;      // d. space,  pixels between instruments
    PixSpaceBetweenSys: integer; // e1. the space in pixels between systems
    PixBeforeNotes: integer;     // e2. space for clef, key and time signature
    PixHeightTotal: integer;     // f. height in pixels
    PixWidthTotal: integer;      // o. width in pixels
    PixHeaderHeight: integer;    // g. top space in pixels (heading)
    PixLeftMargin: integer;      // h.
    PixRightMargin: integer;     // i.
    PixTopMargin: integer;       // j.
    PixBottomMargin: integer;    // k.
    NumMess: integer;            // m. Number of measures per line
    PageNumber: integer;         // m2. Page number of Score
    // Computed geometry
    PixSysWidth: integer;        // n. Number of pixels in width of system
    PixSysWidthReduced: integer; // n2. Width of system without clef, time, key
    NumSystems: integer;         // Number of systems computed from above numbers
////    TotalHeight: integer;        // Computed height of number of systems
    TempoOriginal: integer;      // Original tempo (quater beats per minute)
    Quantisation: integer;       // Number of notes per quartar note ////
    MidiInitialPause: integer;   // Number of quants before playing

    // No clef exist in midi files (that we have right now)
    // Guess the clef be calculation midian note value
    NoteMidianSum: array[0..Tracks] of integer;
    NoteMidianCount: array[0..Tracks] of integer;
    NoteMidianValue: array[0..Tracks] of integer;
    NoteOffset: array[0..Tracks] of integer;

    // Methods
    procedure DrawLines(Sender: TObject);
    procedure DrawNote(       SymbolType: TSymbolType;
                              Note: integer;
                              Instrument: integer;// Channel/TRack
                              OffsetX: integer;  // Only for key signature
                              System: integer;   // Only for clef, key and time
                              Position: integer;  // Position in quants
                              NoteType: integer;  // Whole, half, quarter etc..
                              NotePoints: TNotePoints);
    procedure DrawConnection(Note: integer;
                              Instrument: integer;
                              PositionStart: integer;
                              PositionEnd: integer);
    function GetNoteLength(time: int64; timetotal: int64): TNoteLength;
    function GetNotePosition(time: int64): int64;
    function GetNoteTime(time: int64): int64;
  public
    { Public declarations }
  end;

var
  FormScore: TFormScore;

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.DFM}
{$endif}

// Symbol size in Notes.bmp
const NoteWidth = 16;
const NoteHeight = 32;
const NoteCenter = 27;
// Symbol numbers in Notes.bmp
const FirstClef = 30;              // Equal to number ot note symbols before clef
const FirstNumber = FirstClef+11;  // There are 7 clefs and 3 cross //// ????
const FlatSymbol = 39;            // The sharp symbol
const SharpSymbol = 40;            // The sharp symbol
const NaturalSymbol = 41;          // The natural symbol
const TheNoteG = 70;               // According to Midi Standard
const TheNoteA = 71;               // According to Midi Standard
const TheNoteD = 62;               // According to Midi Standard
const TheNoteF = 65;               // According to Midi Standard
const TheNoteC = 60;               // According to Midi Standard

// An average of notevalues determine if C-clef or F-clef
const SplitNote = 58;              // Above is C-clef, below is F-clef

const TheNote7D = 36;              // after conversion to 7 note system
const TheNote7F = 46;              // after conversion to 7 note system
// Some lite extra space somewhere
const GLUE = 10;

type TNoteVal = record
                NoteValue: integer;      // Note according to midi specification
                NoteLength: TNoteLength; // Length, 0 = full, 1 = half + points
                NoteTrack: integer;      // Track for the note
                MidiEventIndex: integer; // Index to the Midi Event
                PauseValue: TNoteLength;     // Pause after the note
                PlayTime: int64;         // Time for playing note (quant count)
                NotePosition: int64;     // Time for playing note in quants
                PausePosition: int64;    // Pause position after the note
                end;

var NoteValues: array[0..MaxEvents] of TNoteVal;
    NoteValIndex: integer;

//---------------------------------------------------------------------------
//
//     Function:    OnCreate
//
//     Purpose:     If a special file for notes exist then use this.
//                  Initialize geometry parameters
//
//     Parameters:  Windows standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure TFormScore.OnCreate(Sender: TObject);

var FileName: string;
    i: integer;

  begin
  // Read symbols
  MenuItemInitialPause.Checked:=true;
  MidiInitialPause:=256;

  NoteValIndex:=0;
  FileName:='Notes.bmp';
  BitmapNotes:=TBitmap.Create;
  {$ifdef FPC}
  if FileExists(FileName) then BitmapNotes.LoadFromFile(FileName)
  {$else}
  if FileExists(FileName) then BitmapNotes.LoadFromFile(FileName)
  {$endif}
  else BitmapNotes:=ImageNotes.Picture.Bitmap;
  // Geometry
  PixLine:=5;
  NumLines:=5;
  NumInstruments:=4;
  NumMess:=2;
  PixSpaceInSys:=30;
  PixSpaceBetweenSys:=10;
  PixBeforeNotes:=70;
  ImageScore.Height:=Screen.Height;
  ImageScore.Height:=Screen.Height;
  // Go for A4 geometry
  ImageScore.Width:=((ImageScore.Height*210) div 297);
  PixWidthTotal:=ImageScore.Width-2;
  PixHeightTotal:=ImageScore.Height-2;

  PixHeaderHeight:=50;
  PixLeftMargin:=100;
  PixRightMargin:=20;
  PixTopMargin:=10;
  PixBottomMargin:=100;

  TempoOriginal:=100;
  Quantisation:=6;
  PageNumber:=1;

  for i:=0 to Tracks do
    begin
    NoteMidianSum[i]:=0;
    NoteMidianCount[i]:=0;
    end;
  end;

function MakeNoteLength(NoteType: TNoteType;
                        NotePoints: TNotePoints;
                        NoteTime: int64): TNoteLength;

var
  NoteLength: TNoteLength;

  begin
  NoteLength.NoteType:=NoteType;
  NoteLength.NotePoints:=NotePoints;
  NoteLength.NoteTime:=NoteTime;
  MakeNoteLength:=NoteLength;
  end;

//// Val eller Type eller ????
function TFormScore.GetNoteLength(time: int64; timetotal: int64): TNoteLength;

var NoteLength: integer;
    NoteVal: integer;
    GetNoteLengthOld: TNoteLength;
    TestLength: integer;
    NoteType: TNoteType;
    NotePoints: TNotePoints;
    NoteTime: int64;
////    NoteEndPosition: int64;
////    NotePosition: int64;

  begin
  NoteTime:=GetNoteTime(time);
////  NotePosition:=GetNotePosition(timetotal-time);
////  NoteEndPosition:=GetNotePosition(timetotal);
  // Compute note length in quarter notes x 1000 and convert to note value 0..6
  NoteLength:=1000*(time+(MidiDivision div (2 shl Quantisation))) div MidiDivision;
  if NoteLength>4000 then NoteVal:=0        // Whole note
  else if NoteLength>2000 then NoteVal:=1   // Half note
  else if NoteLength>1000 then NoteVal:=2   // Quarter note
  else if NoteLength>500 then NoteVal:=3    // Eights note
  else if NoteLength>250 then NoteVal:=4    // Sixteens note
  else if NoteLength>125 then NoteVal:=5    // Thirtytwos note
  else if NoteLength>62 then NoteVal:=6     // Sixtyfours note
  else NoteVal:=-1;                         // No note (too short)
  GetNoteLengthOld:=MakeNoteLength(NoteVal,0,0);

  // Compute note length in quarter notes x 1024 and convert to note value 0..6
  NoteLength:=1024*(time+(MidiDivision div (2 shl Quantisation))) div MidiDivision;
  // A whole note is 4 times a quarter note
  TestLength:=4*1024;
  NoteType:=0;
  NotePoints:=0;
  if NoteLength<64 then
    begin
    NoteType:=-1;
    NotePoints:=0;
    end
  else while (NoteLength<TestLength) and (NoteType<=6) do
    begin
    inc(NoteType);
    NoteLength:=NoteLength shl 1;
    end;
  // Points? Test remaining length (12 bits to 4 bits)
  if NoteType>=0 then
    begin
    NoteLength:=(NoteLength-TestLength) shr 8;
    if NoteLength=15 then NotePoints:=4
    else if NoteLength=14 then NotePoints:=3
    else if NoteLength>=12 then NotePoints:=2
    else if NoteLength>=8 then NotePoints:=1
    end;

  GetNoteLength:=MakeNoteLength(NoteVal,NotePoints,NoteTime);
  end;

// In quants
function TFormScore.GetNotePosition(time: int64): int64;

var NotePosition: integer;

  begin
  // Compute note length in 64 th notes
  NotePosition:=PowerOfTwo(Quantisation)*(time+(MidiDivision div (2 shl Quantisation))) div MidiDivision;
  GetNotePosition:=NotePosition;
  end;

// In quants
function TFormScore.GetNoteTime(time: int64): int64;

var NoteTime: integer;

  begin
  // Compute note time in 64 th notes
  NoteTime:=PowerOfTwo(Quantisation)*(time+(MidiDivision div (2 shl Quantisation))) div MidiDivision;
  GetNoteTime:=NoteTime;
  end;



// 60 = Mid piano = C
//// Note val: 0 = full, 1 = half, 2 = quarter etc.

// Used for a set of all flat and a set of sharp notes
type TNoteValue = MidiNoteFirst..MidiNoteLast;
     TNotes = set of TNoteValue;

type TNoteTable = array[0..11] of TNoteValue;

// Indexes 0 = C, 1 = #C, 2 = D, 3 = #D, 4 = E, 5 = F, 6 = #F
//         7 = G, 8 = #G, 9 = A, 10 = #A, 11 = B
// Values 0 = C, 1 = D, 2 = E, 3 = F, 4 = G, 5 = A, B = 6
const NoteValTab: TNoteTable = (0,0,1,1,2,3,3,4,4,5,5,6);
const NoteUppTab: TNoteTable = (0,1,0,1,0,0,1,0,1,0,1,0);

// Table of key (number of flats and sharps) to notes
//
const FlatAndSharpToNoteTab: TNoteTable = (10,3,8,1,6,11,4,10,3,8,1,6);

var // Til objektet
    FlatNotes: TNotes;
    SharpNotes: TNotes;
    LocalFlatNotes: TNotes;
    LocalSharpNotes: TNotes;
    OldMeasureNumber: integer;

////
// Position:
// positive values = the quant value of the note
// negative values =
//                     -1 : The time signature position
//                     -2 : The key position
//                     -3 : The clef position
//
procedure TFormScore.DrawNote(SymbolType: TSymbolType;
                              Note: integer;
                              Instrument: integer;
                              OffsetX: integer;  // Only for key signature
                              System: integer;   // Only for clef, key and time
                              Position: integer;
                              NoteType: integer;
                              NotePoints: TNotePoints);

var
    MeasureNumber: integer;      // The measure, count from 0
    MeasurePosition: integer;    // Quant number in the measure, count from 0
    PositionX: integer;
    PositionY: integer;
    Sharp: boolean;
    Flat: boolean;
    Note7: integer;

    i,j,k: integer;              // Loop count through image with note
    OffsetY: integer;            // Offset computed from note value
    PointSize: integer;
    PointAdjustment: integer;    // Point must be beteen lines
    c: integer;                  // Color for points/dots

var x1,y1: integer; // Coordinates from Notes image (font)
    x2,y2: integer; // Coordinates to drawin canvas
    ok: boolean;    // Note / Symbol shall be drawn

  begin
  Sharp:=false;
  Flat:=false;

  // Convert the 12 notes to 7 note system
  Note7:=((Note div 12)*7+NoteValTab[Note mod 12]);

  // Adjustment to avoid point comming on line
  if (Note7 mod 2)=1 then
    PointAdjustment:=PixLine div 2
  else
    PointAdjustment:=0;

  // Adjustment for sharp and flat notes
  if Position>=0 then
    begin
    // If flat note (i.e. 249..255)
    if NoteUppTab[Note mod 12]>0 then
      begin
      if MidiKeySignature>128 then
        begin
        inc(Note7);
        Flat:=true;
        end
      else   // Sharp
        begin
        Sharp:=true;
        end;
      end;
    end;

  // For F clef - let Note7 be position in note system
  if Position>=0 then
  Note7:=Note7+NoteOffset[Instrument];

  // Key signature - set flats or sharps inside system  (modulo 7)
  if Position=-2 then    //// Definition
  Note7:=Note7-((7*7-NoteOffset[Instrument]) mod 7);

  if SymbolType=SymbolPause then Note7:=41;

  OffsetY:=Note7-35+1;
  OffsetY:=-(((OffsetY*PixLine)+1) div 2);

  // Adjust position for bas (F) clef
{  if Position>=0 then
    Note7:=Note7+NoteOffset[Instrument];}

  if Position>=0 then
    begin
    // Measure number
{    MeasureNumber:=(Position*MidiTimeSignatureUpper) div
                   (PowerOfTwo(Quantisation+2)*MidiTimeSignatureLower);
}
    MeasureNumber:=(Position*MidiTimeSignatureLower) div
                   (PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper);

    if OldMeasureNumber<>MeasureNumber then
      begin
      LocalFlatNotes:=[];
      LocalSharpNotes:=[];
      OldMeasureNumber:=MeasureNumber;
      end;

    // Since Quantisation is for a quarter note, add two to get whole measure
    MeasurePosition:=(Position*MidiTimeSignatureLower) mod
                    (PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper);

    MeasurePosition:=((PixSysWidth div NumMess) div 10)+
              ((PixSysWidth div NumMess)*(MeasurePosition*7) div 10)
                div (PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper);

    ok:=(MeasureNumber>=NumMess*NumSystems*(PageNumber-1)) and
     (MeasureNumber<NumMess*NumSystems*PageNumber);
    if ok then
      begin
      PositionX:=PixBeforeNotes+MeasurePosition+
                (MeasureNumber mod NumMess)*((PixSysWidth-PixBeforeNotes) div NumMess);
      PositionY:=PixTopmargin+PixHeaderHeight+OffsetY+1+
                 ((MeasureNumber div NumMess) mod NumSystems)*(NumInstruments*
                   (PixLine*(NumLines)+PixSpaceInSys)+PixSpaceBetweenSys)+
                 (Instrument)*(PixLine*(NumLines)+PixSpaceInSys);
      end;
    end
  else
    begin
    ok:=true;
      case Position of
      -1: PositionX:=(NoteWidth div 4)+1+OffsetX;
      -2: PositionX:=NoteWidth+(NoteWidth div 2)+1+OffsetX;
      -3: PositionX:=PixBeforeNotes-(NoteWidth div 4)+OffsetX;
      -4: PositionX:=PixBeforeNotes-(NoteWidth div 4)+OffsetX;
      else PositionX:=PixBeforeNotes div 2+OffsetX;
      end;
   PositionY:=PixTopmargin+PixHeaderHeight+OffsetY+1+
                 (System)*(NumInstruments*
                   (PixLine*(NumLines)+PixSpaceInSys)+PixSpaceBetweenSys)+
              (Instrument)*(PixLine*(NumLines)+PixSpaceInSys);

    end;
  // Is note inside current page?
  if ok then
    begin
    // Set new position
    if Position>=0 then
      begin
      end;

    // Draw the note
    for i:=0 to NoteWidth-1 do
    for j:=0 to NoteHeight*2-1 do
      begin
      x1:=i+NoteType*NoteWidth;
      y1:=j;
      x2:=i+PixLeftMargin+PositionX;
      y2:=j+PositionY;
      if (BitmapNotes.Canvas.Pixels[x1,y1]<>$FFFFFF) and
         (ImageScore.Canvas.Pixels[x2,y2]=$FFFFFF)
      then
        ImageScore.Canvas.Pixels[x2,y2]:=BitmapNotes.Canvas.Pixels[x1,y1];
      end;

    // Draw points to make note longer
    PointSize:=1;
    for k:=0 to NotePoints-1 do
    for i:=-PointSize-1 to PointSize+1 do
    for j:=-PointSize-1 to PointSize+1 do
      begin
      x2:=i+PixLeftMargin+PositionX+NoteWidth+k*4*PointSize;
      y2:=j+PositionY+NoteHeight-PixLine-PointAdjustment;
      if (i*i+j*j<=PointSize*PointSize) then ImageScore.Canvas.Pixels[x2,y2]:=0
      else
      if (i*i+j*j-PointSize*PointSize<=PointSize) then
        begin
        //// Ought to be 255 but 208 looks better (for point size 1)
        c:=(((i*i+j*j-PointSize*PointSize))*208) div PointSize;
        ImageScore.Canvas.Pixels[x2,y2]:=c+(c shl 8)+(c shl 16);
        end;
      end;


    // Sharp and flat notes shall have sharp/flat symbol in front
    // if not flat note in key
    if (Sharp) or (Flat and (not((Note in FlatNotes) or (Note in LocalFlatNotes))))
      then
      begin
      LocalFlatNotes:=LocalFlatNotes+[Note];
      for i:=0 to NoteWidth-1 do
      for j:=0 to NoteHeight*2-1 do
        begin
        if Sharp then
          x1:=i+SharpSymbol*NoteWidth
        else
          x1:=i+FlatSymbol*NoteWidth;
        y1:=j;
        x2:=i+PixLeftMargin+PositionX-NoteWidth div 2;
        y2:=j+PositionY;
        if (BitmapNotes.Canvas.Pixels[x1,y1]<>$FFFFFF) and
           (ImageScore.Canvas.Pixels[x2,y2]=$FFFFFF)
        then
          ImageScore.Canvas.Pixels[x2,y2]:=BitmapNotes.Canvas.Pixels[x1,y1];
        end;
      end;

    if (Position>=0) and not(Flat) and
      ((Note-1) in FlatNotes) or ((Note-1) in LocalFlatNotes) then
    for i:=0 to NoteWidth-1 do
    for j:=0 to NoteHeight*2-1 do
      begin
      y1:=j;
      x2:=i+PixLeftMargin+PositionX-NoteWidth div 2;
      x1:=i+NaturalSymbol*NoteWidth;
      y2:=j+PositionY;
      if (BitmapNotes.Canvas.Pixels[x1,y1]<>$FFFFFF) and
         (ImageScore.Canvas.Pixels[x2,y2]=$FFFFFF)
      then
        ImageScore.Canvas.Pixels[x2,y2]:=BitmapNotes.Canvas.Pixels[x1,y1];
      end;

    // Short extra lines if outside the 5 note lines //// 20?

    // Lines beneath
    if (Note7<TheNote7D) and (Note7>TheNote7D-20) then
    for i:=-NoteWidth div 8 to NoteWidth-1-(NoteWidth div 4) do
    for j:=1 to ((TheNote7D-Note7+1) div 2) do     //// 40
      begin
      // Only half as many lines as notes below
      OffsetY:=((Note7+2) div 2)*2-35+1;
      //// Adjustment? the notes must be wrong positioned
      OffsetY:=-(((OffsetY*PixLine)+1) div 2)+3;
      PositionY:=PixTopmargin+PixHeaderHeight+OffsetY+
                     (MeasureNumber div NumMess)*(NumInstruments*
                       (PixLine*(NumLines)+PixSpaceInSys)+PixSpaceBetweenSys)+
                     (Instrument)*(PixLine*(NumLines)+PixSpaceInSys);
      y2:=PositionY+NoteHeight-j*PixLine;
      x2:=i+PixLeftMargin+PositionX;
      ImageScore.Canvas.Pixels[x2,y2]:=0;
      end;
    // Lines above
    if (Note7>TheNote7F) and (Note7<TheNote7F+20) then
    for i:=-NoteWidth div 8 to NoteWidth-1-(NoteWidth div 4) do
    for j:=1 to ((Note7-TheNote7F+1) div 2) do     //// 40
      begin
      // Only half as many lines as notes below
      OffsetY:=((Note7-1) div 2)*2-35+1;
      //// Adjustment? the notes must be wrong positioned
      OffsetY:=-(((OffsetY*PixLine)-1) div 2)+3;
      PositionY:=PixTopmargin+PixHeaderHeight+OffsetY+
                     (MeasureNumber div NumMess)*(NumInstruments*
                       (PixLine*(NumLines)+PixSpaceInSys)+PixSpaceBetweenSys)+
                     (Instrument)*(PixLine*(NumLines)+PixSpaceInSys);
      y2:=PositionY+NoteHeight+(j-3)*PixLine-1;
      x2:=i+PixLeftMargin+PositionX;
      ImageScore.Canvas.Pixels[x2,y2]:=0;
      end;
    end;
  end;

// A note may pass meassure limit - connect and tie the note
procedure TFormScore.DrawConnection(Note: integer;
                              Instrument: integer;
                              PositionStart: integer;
                              PositionEnd: integer);

const WidthRatio=5;   // Width of tie/ection compared to curve

var
    MeasureNumberStart: integer;      // The first measure, count from 0
    MeasureNumberEnd: integer;        // The last measure, count from 0
    MeasurePositionStart: integer;    // First quant number in the measure, count from 0
    PageCurrent: integer;             // Current page
    MeasurePositionEnd: integer;      // Last quant number in the measure, count from 0
    PositionXStart: integer;          // Computed X position, first
    PositionXEnd: integer;            // Computed X position last
    PositionY: integer;               // Computed Y position
    Sharp: boolean;
    Flat: boolean;
    Note7: integer;
    ModDivisor: integer;
    ModOffset: integer;

    i,j: integer;              // Loop count through image with note
    ii: integer;
    OffsetYStart: integer;       // Offset computed from note value
    OffsetYEnd: integer;         // Offset computed from note value
    x_normal: int64;           // An x value normalised to -1000 to 1000
    y_normal: int64;           // An y value computed from x_normal
    grey1: integer;            // Make preyttier with extra grey pixel
    grey2: integer;            // both up and down

var x2,y2: integer; // Coordinates to drawin canvas
    ok: boolean;    // Note / Symbol shall be drawn

  begin
  Sharp:=false;
  Flat:=false;             //// IKKE BRUGT - IKKE PROGRAMMERET!

  // Convert the 12 notes to 7 note system
  Note7:=((Note div 12)*7+NoteValTab[Note mod 12]);

  // Adjustment for sharp and flat notes
  if PositionStart>=0 then
    begin
    // If flat note (i.e. 249..255)
    if NoteUppTab[Note mod 12]>0 then
      begin
      if MidiKeySignature>128 then
        begin
        inc(Note7);
        Flat:=true;
        end
      else   // Sharp
        begin
        Sharp:=true;
        end;
      end;
    end;

  // For F clef - let Note7 be position in note system
  if PositionStart>=0 then Note7:=Note7+NoteOffset[Instrument];

  OffsetYStart:=Note7-35+1;
  OffsetYStart:=-(((OffsetYStart*PixLine)+1) div 2);
  OffsetYEnd:=Note7-35+1;
  OffsetYEnd:=-(((OffsetYEnd*PixLine)+1) div 2);

  // Adjust position for bas (F) clef
{  if Position>=0 then
    Note7:=Note7+NoteOffset[Instrument];}

  if PositionStart>=0 then
    begin
    // Measure number
    MeasureNumberStart:=(PositionStart*MidiTimeSignatureLower) div
                   (PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper);
    MeasureNumberEnd:=(PositionEnd*MidiTimeSignatureLower) div
                   (PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper);

    // Since Quantisation is for a quarter note, add two to get whole measure
    // Compute position in measure
    MeasurePositionStart:=(PositionStart) mod
     ((PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper) div MidiTimeSignatureLower);
    MeasurePositionStart:=((PixSysWidth div NumMess) div 10)+
              ((PixSysWidth div NumMess)*(MeasurePositionStart*7) div 10)
                div (PowerOfTwo(Quantisation+2));

    MeasurePositionEnd:=(PositionEnd) mod
     ((PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper) div MidiTimeSignatureLower);
    MeasurePositionEnd:=((PixSysWidth div NumMess) div 10)+
              ((PixSysWidth div NumMess)*(MeasurePositionEnd*7) div 10)
                div (PowerOfTwo(Quantisation+2));

    ok:=((MeasureNumberStart>=NumMess*NumSystems*(PageNumber-1)) and
     (MeasureNumberStart<NumMess*NumSystems*PageNumber)) or
     ((MeasureNumberEnd>=NumMess*NumSystems*(PageNumber-1)) and
     (MeasureNumberEnd<NumMess*NumSystems*PageNumber));
    if ok then
      begin
      PositionXStart:=PixBeforeNotes+MeasurePositionStart+(NoteWidth div 3)+
                (MeasureNumberStart{ mod NumMess})*((PixSysWidth-PixBeforeNotes) div NumMess);
      PositionXEnd:=PixBeforeNotes+MeasurePositionEnd-(NoteWidth div 3)+
                (MeasureNumberEnd{ mod NumMess})*((PixSysWidth-PixBeforeNotes) div NumMess);

      PositionY:=PixTopmargin+PixHeaderHeight+OffsetYStart+1+
                 ((MeasureNumberStart div NumMess) mod NumSystems)*(NumInstruments*
                   (PixLine*(NumLines)+PixSpaceInSys)+PixSpaceBetweenSys)+
                 (Instrument)*(PixLine*(NumLines)+PixSpaceInSys);
      ModDivisor:={PixBeforeNotes+MeasurePositionStart+(NoteWidth div 3)+}
                (1{ mod NumMess})*((PixSysWidth-PixBeforeNotes) {div NumMess});
      ModOffset:=PixBeforeNotes+MeasurePositionStart+(NoteWidth div 3)+
                (0{ mod NumMess})*((PixSysWidth-PixBeforeNotes) div NumMess);
      end;
    end;
  // Is note inside current page?
  if ok then
    begin
    // Draw the connection
    for i:=PositionXStart to PositionXEnd  do
      begin
      ii:=((i-PixBeforeNotes+(NoteWidth div 2)) mod ModDivisor)+PixBeforeNotes-(NoteWidth div 2);
      PageCurrent:=(((i-PixBeforeNotes+(NoteWidth div 2)) div ModDivisor) div NumSystems)+1;

      PositionY:=
                  PixTopmargin+PixHeaderHeight+OffsetYStart+1+
                 ((((((i-PixBeforeNotes+(NoteWidth div 2)) div ModDivisor)))) mod NumSystems)*(NumInstruments*
                   (PixLine*(NumLines)+PixSpaceInSys)+PixSpaceBetweenSys)+
                 (Instrument)*(PixLine*(NumLines)+PixSpaceInSys);
      // Compute width of connection(Fat in the middle).
      // Scale to 1000 instead of using reals.
      x_normal:=(1000*(2*i-PositionXStart-PositionXEnd)) div
                  (PositionXEnd-PositionXStart);
      y_normal:=4000000-x_normal*x_normal;

////      y_real:=sqrt(y_normal*1.0);
      y_normal:=(round(sqrt(y_normal*1.0)));
      y_normal:=y_normal-round(sqrt(3000000.0));
      y_normal:=y_normal*PixLine;
      grey1:=255-((y_normal) mod 256);
      grey1:=grey1+(grey1 shl 8)+(grey1 shl 16);


      grey2:=(((y_normal-(y_normal div WidthRatio))) mod 256);
      grey2:=grey2+(grey2 shl 8)+(grey2 shl 16);

      // Add an adjustment for y computed from x (higher in the middle)
      // (So that the line is fatter in the middle)
      if PageNumber=PageCurrent then
        begin
        for j:=0 to (y_normal div (WidthRatio*256)) do
          begin
          x2:=ii+PixLeftMargin+NoteWidth div 2;
          y2:=PositionY-((y_normal-j*256) div 256);
          if ImageScore.Canvas.Pixels[x2,y2]=$FFFFFF
          then
            ImageScore.Canvas.Pixels[x2,y2]:=0;
          end;
        ImageScore.Canvas.Pixels[ii+PixLeftMargin+NoteWidth div 2,PositionY-((y_normal) div 256)-1]:=grey1;
        ImageScore.Canvas.Pixels[ii+PixLeftMargin+NoteWidth div 2,PositionY-((y_normal-(y_normal div WidthRatio)) div 256)]:=grey2;
        end;
      end;
    end;
  end;


// Refer to the drawing score.ppt
//// NAME?
procedure TFormScore.DrawLines(Sender: TObject);

{$ifdef Windows}
var i,j,k: integer;       // Loop counters
    h: integer;           // Calculation height of pages
    time: int64;          // Milliseconds since previous note
    timetotal: int64;     //
    timepause: int64;     //
    ChannelNo: integer;   //// Track?
    PageNumberSize: TSize;
    MeasureSize: integer;
    AvailableSize: integer;
    NoteLength1: TNoteLength;
    NoteLength2: TNoteLength;
{$endif}
  begin
{$ifdef Windows}
  Screen.Cursor:=crHourGlass;
  ImageScore.Canvas.Brush.Color:=$FFFFFF;
  ImageScore.Canvas.Rectangle(0,0,ImageScore.Width-1,ImageScore.Height-1);

  ImageScore.Canvas.Font.Size:=16;
  PageNumberSize:=ImageScore.Canvas.TextExtent(IntToStr(PageNumber));
  ImageScore.Canvas.TextOut(PixWidthTotal-PixRightMargin-PageNumberSize.cx,
                     GLUE,IntToStr(PageNumber));

  NumInstruments:=MidiNumberOfChannels;
  // Build a set of flat notes in this key
  FlatNotes:=[];
  if MidiKeySignature>=250 then
  for i:=MidiNoteFirst to MidiNoteLast do
    begin
      begin
      for j:=0 to 255-MidiKeySignature do
        begin
        if (i mod 12)=(FlatAndSharpToNoteTab[j]) then
          FlatNotes:=FlatNotes+[i];
        end;
      end;
    end;
  // Copy all notes to the note array (NoteValues[])
  NoteValIndex:=0;
  timetotal:=0;
  timepause:=0;
  for i:=0 to MidiEventIndexIn-1 do
    begin
    timetotal:=timetotal+MidiEventDelta[i];
    if not (((MidiEvents[i].Bytes[0] and $F0)=$90) and (MidiEvents[i].Bytes[2]<>0)) then
      timepause:=timepause+MidiEventDelta[i]
    else
      begin
      // The is note on
      ChannelNo:=MidiEvents[i].Bytes[0] and 15;
      NoteMidianSum[ChannelNo]:=NoteMidianSum[ChannelNo]+MidiEvents[i].Bytes[1];
      inc(NoteMidianCount[ChannelNo]);

      // Compute length of note in milliseconds by searching for note offs
      time:=0;
      j:=i+1;
      // While not same note and channel
      while (j<MidiEventIndexIn) and (not (
            (((MidiEvents[j].Bytes[0] and $F0)=$80) or
            ((MidiEvents[j].Bytes[0] and $F0)=$90) and (MidiEvents[j].Bytes[2]=0) ) and
            (MidiEvents[j].Bytes[1]=MidiEvents[i].Bytes[1]) and
            ((MidiEvents[j].Bytes[0] and 15)=(MidiEvents[i].Bytes[0] and 15))
            )) do
        begin
        time:=time+MidiEventDelta[j];
        inc(j);
        end;
      time:=time+MidiEventDelta[j];
      timepause:=0;

      // Read pause after this note in milliseconds
      j:=j+1;
      while (j<MidiEventIndexIn) and (not(
            ((MidiEvents[j].Bytes[0] and $90)=$90) and
            ((MidiEvents[j].Bytes[0] and 15)=(MidiEvents[i].Bytes[0] and 15))
            )) do
        begin
        timepause:=timepause+MidiEventDelta[j];
        inc(j);
        end;
      timepause:=timepause+MidiEventDelta[j];
      // Extract information about note length, value, track
      NoteValues[NoteValIndex].NoteLength:=GetNoteLength(time,timetotal);
      NoteValues[NoteValIndex].NoteTrack:=MidiEvents[i].Bytes[0] and 15;
      NoteValues[NoteValIndex].NoteValue:=MidiEvents[i].Bytes[1];
      NoteValues[NoteValIndex].MidiEventIndex:=i;
      // And maybe a pause after
      if timepause>0 then
        NoteValues[NoteValIndex].PauseValue:=GetNoteLength(timepause,time+timetotal)
      else
        begin
        NoteValues[NoteValIndex].PauseValue.NoteType:=-1;
        NoteValues[NoteValIndex].PauseValue.NotePoints:=0;
        end;
      NoteValues[NoteValIndex].PlayTime:=timetotal;
      //// If note comes before start, then cancel MidiInitialPause
      if GetNotePosition(timetotal)<MidiInitialPause then MidiInitialPause:=0; 
      NoteValues[NoteValIndex].NotePosition:=GetNotePosition(timetotal)-MidiInitialPause;
      // A pause shall not be put before measure, therefore adjust to quantisation
{
      NoteValues[NoteValIndex].PausePosition:=
        GetNotePosition(timetotal+timepause)-MidiInitialPause+
        (MidiDivision div (2 shl Quantisation-2));
}
      NoteValues[NoteValIndex].PausePosition:=
        GetNotePosition(timetotal+time)-MidiInitialPause+
        (MidiDivision div (2 shl Quantisation-2));
      inc(NoteValIndex);
      end;
    end;

  ImageScore.Top:=20;
  ImageScore.Left:=0;
  //// Draw the base lines
  NumSystems:=0;
  PixSpaceBetweenSys:=10; //// definition: PixSpaceBetweenSystemsOrg eller
  // Number of system per page
  NumSystems:=(PixHeightTotal-
    (PixTopMargin+PixBottomMargin+PixHeaderHeight+2*GLUE)) div
    (PixSpaceBetweenSys+NumInstruments*((NumLines-1)*PixLine+PixSpaceInSys)-1);

  // The height with this value (remove remainder)
  h:=PixHeightTotal-
   (PixHeightTotal-
    (PixTopMargin+PixBottomMargin+PixHeaderHeight+2*GLUE)) mod
    (PixSpaceBetweenSys+NumInstruments*((NumLines-1)*PixLine+PixSpaceInSys)-1);

  // The total height gives the width - paper is A4
  PixSysWidth:=PixWidthTotal-PixLeftMargin-PixRightMargin;
  // Draw the note systems. Increase PixSpaceBetweenSys to fit page
  if NumSystems>1 then
    PixSpaceBetweenSys:=PixSpaceBetweenSys+
        (PixHeightTotal-h) div (NumSystems-1);
        //// Her krasjer den med NumSystems=1, Brudemarsch från Jämtland

  ImageScore.Canvas.MoveTo(1,1);
  ImageScore.Canvas.LineTo(1,PixHeightTotal-2);
  ImageScore.Canvas.MoveTo(PixWidthTotal-2,1);
  ImageScore.Canvas.LineTo(PixWidthTotal-2,PixHeightTotal-2);

  ImageScore.Canvas.MoveTo(1,1);
  ImageScore.Canvas.LineTo(PixWidthTotal-2,1);
  ImageScore.Canvas.MoveTo(1,PixHeightTotal-2);
  ImageScore.Canvas.LineTo(PixWidthTotal-2,PixHeightTotal-2);

  for i:=0 to NumSystems-1 do
    begin
    for j:=0 to NumInstruments-1 do
      begin
      for k:=0 to Numlines-1 do
        begin
        // Horizontal lines
        ImageScore.Canvas.MoveTo(PixLeftMargin,
          PixTopMargin+PixHeaderHeight+
          (i*((NumInstruments*(PixLine*NumLines+PixSpaceInSys)+PixSpaceBetweenSys)))+
          (j*(PixLine*NumLines+PixSpaceInSys))+
          k*PixLine);
        ImageScore.Canvas.LineTo(PixLeftMargin+PixSysWidth,
          PixTopMargin+PixHeaderHeight+
          (i*((NumInstruments*(PixLine*NumLines+PixSpaceInSys)+PixSpaceBetweenSys)))+
          (j*(PixLine*NumLines+PixSpaceInSys))+
          k*PixLine);
        end;
      // The clef symbol    //// another parm needed for each instruments
      if NoteMidianCount[j]>0 then
        NoteMidianValue[j]:=NoteMidianSum[j] div NoteMidianCount[j]
      else
        NoteMidianValue[j]:=60;

      // If not defined in the midifile - set clef from average note
      if (MidiClef[j]=Clef_Undefined) and (NoteMidianCount[j]>0) then
        begin
        if NoteMidianValue[j]>SplitNote then MidiClef[j]:=Clef_C else
        MidiClef[j]:=Clef_F;
        end;

      if (MidiClef[j]=Clef_C) or (MidiClef[j]=Clef_Undefined) then
        begin // G clef
        NoteOffset[j]:=0;
        DrawNote(SymbolClef,TheNoteG,j,0,i,-1,FirstClef,0);
        // Time signature
        DrawNote(SymbolClef,TheNoteG+3,j,0,i,-3,FirstNumber+1+MidiTimeSignatureUpper,0);
        DrawNote(SymbolClef,TheNoteG-4,j,0,i,-4,FirstNumber+1+MidiTimeSignatureLower,0);
        end
      else
        begin // F clef is second clef
        NoteOffset[j]:=12;
        DrawNote(SymbolClef,TheNoteG-3,j,0,i,-1,FirstClef+1,0);
        // Time signature
        DrawNote(SymbolClef,TheNoteG+3,j,0,i,-3,FirstNumber+1+MidiTimeSignatureUpper,0);
        DrawNote(SymbolClef,TheNoteG-4,j,0,i,-4,FirstNumber+1+MidiTimeSignatureLower,0);
        end;
      // The key signature. Flat? Write the keysymbol at next(up) note (+1)
      if MidiKeySignature>128 then
      if true then
        begin
        if MidiClef[j]=Clef_F then
          begin
          for k:=0 to 255-MidiKeySignature do
          if FlatAndSharpToNoteTab[k]>5 then
          DrawNote(SymbolKey,TheNoteC+FlatAndSharpToNoteTab[k]+1,j,(k*NoteWidth+1) div 3,i,-2,FlatSymbol,0)
          else // put 7 notes above
          DrawNote(SymbolKey,TheNoteC+FlatAndSharpToNoteTab[k]+13,j,(k*NoteWidth+1) div 3,i,-2,FlatSymbol,0);
          end
        else
          begin
          for k:=0 to 255-MidiKeySignature do
          if FlatAndSharpToNoteTab[k]>5 then
          DrawNote(SymbolKey,TheNoteC+FlatAndSharpToNoteTab[k]+1,j,(k*NoteWidth+1) div 3,i,-2,FlatSymbol,0)
          else // put 7 notes above
          DrawNote(SymbolKey,TheNoteC+FlatAndSharpToNoteTab[k]+13,j,(k*NoteWidth+1) div 3,i,-2,FlatSymbol,0);
          end
        end;
      end;

    // Vertical lines. Reduce system to have room for clef, key and time signature
    PixSysWidthReduced:=PixWidthTotal-PixLeftMargin-PixRightMargin-PixBeforeNotes;
    // Left most line (left of clef)
    ImageScore.Canvas.MoveTo(PixLeftMargin,
      PixTopMargin+PixHeaderHeight+
      (i*((NumInstruments*(PixLine*NumLines+PixSpaceInSys)+PixSpaceBetweenSys)))
      );
    ImageScore.Canvas.LineTo(PixLeftMargin,
      PixTopMargin+PixHeaderHeight+
      (i*((NumInstruments*(PixLine*NumLines+PixSpaceInSys)+PixSpaceBetweenSys)))+
      ((NumInstruments-1)*(PixLine*NumLines+PixSpaceInSys))+
      (NumLines-1)*PixLine);
    // The other lines in note system (The measures) //// Stavning???? mess?
    // No line for j=0 (first measure)
    for j:=1 to NumMess do
      begin
      ImageScore.Canvas.MoveTo(PixLeftMargin+PixBeforeNotes+(j*PixSysWidthReduced) div NumMess,
        PixTopMargin+PixHeaderHeight+
        (i*((NumInstruments*(PixLine*NumLines+PixSpaceInSys)+PixSpaceBetweenSys)))
        );
      ImageScore.Canvas.LineTo(PixLeftMargin+PixBeforeNotes+(j*PixSysWidthReduced) div NumMess,
        PixTopMargin+PixHeaderHeight+
        (i*((NumInstruments*(PixLine*NumLines+PixSpaceInSys)+PixSpaceBetweenSys)))+
        ((NumInstruments-1)*(PixLine*NumLines+PixSpaceInSys))+
        (NumLines-1)*PixLine);
      end;
    end;

  // Do not know the width of scroll bar (No VertScrollBar.Width)
  Width:=ImageScore.Width+30;     //// ???? 30?
////  Width:=ImageScore.ClientWidth+ScrollBox.VertScrollBar.Width;
  Width:=ImageScore.ClientWidth+100;     //// ????

  // Draw all the notes and eventually pause after
  for i:=0 to NoteValIndex-1 do
  if i>=0 then
    begin
    //// Note to long?       Assert MidiTimeSignatureLower<>0 ???
    MeasureSize:=(PowerOfTwo(Quantisation+2)*MidiTimeSignatureUpper) div MidiTimeSignatureLower;
    AvailableSize:=MeasureSize-
                         (NoteValues[i].NotePosition mod MeasureSize);

    if NoteValues[i].NoteLength.NoteTime>MeasureSize-
                         (NoteValues[i].NotePosition mod MeasureSize) then
      begin
      NoteLength1:=NoteValues[i].NoteLength;
      NoteLength2:=NoteValues[i].NoteLength;

      NoteLength1:=GetNoteLength((MidiDivision*AvailableSize) div (PowerOfTwo(Quantisation)),0);
      NoteLength2:=GetNoteLength((MidiDivision*(NoteValues[i].NoteLength.NoteTime-AvailableSize)) div (PowerOfTwo(Quantisation)),0);

      DrawNote(SymbolNote,NoteValues[i].NoteValue,
             NoteValues[i].NoteTrack,
             0,
             0,
             NoteValues[i].NotePosition,
             NoteLength1.NoteType*5,
             NoteValues[i].NoteLength.NotePoints);
      DrawNote(SymbolNote,NoteValues[i].NoteValue,
             NoteValues[i].NoteTrack,
             0,
             0,
             NoteValues[i].NotePosition+AvailableSize,
             NoteLength2.NoteType*5,
             NoteValues[i].NoteLength.NotePoints);
      DrawConnection(NoteValues[i].NoteValue,
             NoteValues[i].NoteTrack,
             NoteValues[i].NotePosition,
             NoteValues[i].NotePosition+AvailableSize
             );
      end
    else
      DrawNote(SymbolNote,NoteValues[i].NoteValue,
             NoteValues[i].NoteTrack,
             0,
             0,
             NoteValues[i].NotePosition,
             NoteValues[i].NoteLength.NoteType*5,
             NoteValues[i].NoteLength.NotePoints);
    if NoteValues[i].PauseValue.NoteType>=0 then
      begin
      DrawNote(SymbolPause,NoteValues[i].NoteValue,
               NoteValues[i].NoteTrack,
               0,
               0,
               NoteValues[i].PausePosition,
               NoteValues[i].PauseValue.NoteType*5+4,
               NoteValues[i].PauseValue.NotePoints);
      end;
    end;
  Screen.Cursor:=crDefault;
{$endif}
  end;

//---------------------------------------------------------------------------
//
//     Function:   OnKeyPress
//
//     Purpose:    Hot keys:
//                 "44" = Show next page
//                 "45" = Show previous page
//                 Esc = Stop and terminate midi program
//
//     Parameters: Sender = Windows standard
//                 Key    = The pressed key
//
//     Returns:    void
//
//     Notes:      None
//
//---------------------------------------------------------------------------

procedure TFormScore.OnKeyPress(Sender: TObject; var Key: Char);

  begin
  if Key=chr(27) then
    begin
    MidiShallClose:=true;
    ////halt(0); //// Close and save?
    end
  else
    begin
    if Key=chr(44) then
      begin // Page down - show next page
      inc(PageNumber);
      end;
    if Key=chr(45) then
      begin // Page up - show previous page
      if PageNumber>0 then dec(PageNumber);
      end;
    DrawLines(Sender); // Draw new score window    //// Name of FrawLines?
    end;
  end;

//---------------------------------------------------------------------------
//
//     Function:   OnMouseWheel
//
//     Purpose:    To scroll the score page when mouse wheel is turned
//
//     Parameters: Sender = Windows standard (not used)
//                 Shift = Windows standard (not used)
//                 WheelDelta = Windows standard (the actual movement)
//                 MousePos = Windows standard (not used)
//                 Handled = Windows standard (not used)
//
//     Returns:    void
//
//     Notes:      None
//
//---------------------------------------------------------------------------

procedure TFormScore.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  begin
  // Scale the mouse wheel movement by 1/10 - gives ok move
  ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position-WheelDelta div 10;
  end;

//---------------------------------------------------------------------------
//
//     Function:   MenuItemInitialPauseClick
//
//     Purpose:    Du to a problem in Musicator an initial pause
//                 may be present but not wanted. The menu item can
//                 add this pause on score if needed
//
//     Parameters: Sender = Windows standard (not used)
//
//     Returns:    void
//
//     Notes:      None
//
//---------------------------------------------------------------------------

procedure TFormScore.MenuItemInitialPauseClick(Sender: TObject);

  begin
  MenuItemInitialPause.Checked:=not MenuItemInitialPause.Checked;
  if MenuItemInitialPause.Checked then
    MidiInitialPause:=64*MidiTimeSignatureUpper   //// definition?
  else
    MidiInitialPause:=0;
  end;

//---------------------------------------------------------------------------
//
//     Function:   MenuItemExitClick
//
//     Purpose:    Exit the program (all windows)
//
//     Parameters: Sender = Windows standard (not used)
//
//     Returns:    void
//
//     Notes:      None
//
//---------------------------------------------------------------------------

procedure TFormScore.MenuItemExitClick(Sender: TObject);

  begin
  MidiShallClose:=true;
  end;

//---------------------------------------------------------------------------
//
//     Function:   OnClose
//
//     Purpose:    Close(Exit) the program (all windows)
//
//     Parameters: Sender = Windows standard (not used)
//                 Action = Windows standard (not used)
//
//     Returns:    void
//
//     Notes:      None
//
//---------------------------------------------------------------------------

procedure TFormScore.OnClose(Sender: TObject; var Action: TCloseAction);

  begin
  MidiShallClose:=true;
  end;

//---------------------------------------------------------------------------
//
//     Function:   OnKeyDOwn
//
//     Purpose:    Handle key press. Keys: Page Up/Down, Arrow Up/Down
//
//     Parameters: Sender     = Windows standard (not used)
//                 Key        = The key code
//                 ShiftState = (Ignored - CTRL, ALT etc.)
//
//     Returns:    void
//
//     Notes:      None
//
//---------------------------------------------------------------------------

procedure TFormScore.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  begin
    case Key of
    // Page Down
    34: begin inc(PageNumber); DrawLines(Sender); end;
    // Page Up
    33: begin if PageNumber>1 then dec(PageNumber); DrawLines(Sender); end;
    // Arrow Up
    38: ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position-10;
    // Arrow Down
    40: ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position+10;
    end;

  end;

procedure TFormScore.MenuItemFourMeasuresClick(Sender: TObject);

  begin
  nummess:=4;
  end;

end.


