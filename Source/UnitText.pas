//---------------------------------------------------------------------------
//
//  Program:     UnitText.pas in MidiAndMusicXmlPlayer
//
//  Project:     MidiAndMusicXmlPlayer.exe and MidiAndMusicXmlPlayer.app
//
//  Purpose:     To show the text corresponding to the notes
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayer.dpr) or
//               Lazarus (MidiAndMusicXmlPlayer.lpi)
//               or Delphi XE (MidiAndMusicXmlPlayer.dproj)
//
//  Description: A crossreference table connects the text to the notes.
//               Draw an arrow at the current position in the tab
//               For the selected voice (=channel)
//---------------------------------------------------------------------------

unit UnitText;

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
  UnitMidiDefinitions, StdCtrls, Menus, ExtCtrls, 
{$ifdef Darwin}
  Types, 
{$endif}
  ComCtrls;

type

  { TFormText }

  TFormText = class(TForm)
    Timer: TTimer;
    StatusBar: TStatusBar;
    TabControl: TTabControl;
    ScrollBox: TScrollBox;
    ImageText: TImage;
    MemoText: TMemo;
    procedure OnPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure OnResize(Sender: TObject);
    procedure OnChange(Sender: TObject);
    procedure OnCreate(Sender: TObject);
    procedure OnActivate(Sender: TObject);
    procedure OnChangeScrollBar(Sender: TObject);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CopyTextToFile;
  private
    { Private declarations }
    TrackSelection: integer;
    ArrowX: integer;
    ArrowY: integer;
    LastArrowX: integer;
    LastArrowY: integer;
    LastTextNumber: integer;
    MaxLines: integer;
    Crossreference: array[0..Channels-1] of integer;
    CrossreferenceIndex: integer;
    CrossreferencePresent: array[0..Channels-1] of boolean;
    TabIndex: integer;       // The tab selected is the track number
    LastHandledIndex: int64;
    ShownLines: integer;     // Number of lines shown
    NumberOfLines: integer;  // Number of lines total
    LineHeight: integer;     // Heigth of one line
    PageSize: integer;       // Size Of Scroll button
    TabsAreInitialised: boolean; // To avoid doing this more than once
    TabToChannel: array[0..Channels] of integer;

    // All of the texts
    TextLine: array[0..15,0..MaxTextLines] of string;
    // Draw the arrow at this position
    procedure ArrowOut(X: integer;Y: integer);
    // Remove the arrow at this position (draw white)
    procedure NoArrowOut(X: integer;Y: integer);

    procedure ResizeText(Sender: TObject);
    procedure PaintText(Sender: TObject);

  public
    { Public declarations }
  end;

var
  FormText: TFormText;

  MustResize: boolean;
  MustPaint: boolean;
  ImageToggle: boolean = false;

const Indentation=12;

implementation
{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.DFM}
{$endif}

//---------------------------------------------------------------------------
//
//     Procedure:     OnPaint
//
//     Purpose:       To draw the text window. //// MERE !!!!!
//                    During read of the midi file (in memory) the text
//                    array MidiTexts[] is build with all texts. The array
//                    MidiTextsTrack[] contains the track number for the text
//                    Only texts for the current track (tab number) is written
//
//     Parameters:    Sender = windows standard
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.OnPaint(Sender: TObject);

    begin
  if MustPaint then
      begin
    PaintText(Sender);
    MustPaint:=false;
      end;
    end;

//---------------------------------------------------------------------------
//
//     Procedure:     PaintText
//
//     Purpose:       To draw the text window. //// MERE !!!!!
//                    During read of the midi file (in memory) the text
//                    array MidiTexts[] is build with all texts. The array
//                    MidiTextsTrack[] contains the track number for the text
//                    Only texts for the current track (tab number) is written
//
//     Parameters:    Sender = windows standard
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.PaintText(Sender: TObject);

var i: integer;

    begin
  if TextsAreInitialised then
      begin
    for i:=0 to MaxLines do
      ImageText.Canvas.TextOut(Indentation,LineHeight*i,TextLine[CrossReference[TabIndex]-1,i]);

    NoArrowOut(LastArrowX,LastArrowY);

    ArrowX:=Indentation-8;
    ArrowY:=LineHeight;
    ArrowOut(ArrowX,ArrowY);
    LastArrowX:=ArrowX;
    LastArrowY:=ArrowY;

{$ifdef Textbebug}
  StatusBar.Panels.Items[0].Text:=IntToStr(textnumber);
  StatusBar.Panels.Items[1].Text:=IntToStr(MidiEventIndexOut);
  StatusBar.Panels.Items[2].Text:=IntToStr(MidiTextIndexLine[textnumber]);
  StatusBar.Panels.Items[3].Text:=IntToStr(MidiTextIndexPos[textnumber]);
{$endif}
  CrossreferenceIndex:=0;
    end;
  end;    // OnPaint

//---------------------------------------------------------------------------
//
//     Procedure:     ArrowOut
//
//     Purpose:       To draw an arrow at the current point of text
//
//     Parameters:    X = x-position, horisontal
//                    Y = y-position, vertical
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.ArrowOut(X: integer;Y: integer);

var i,j: integer;

  begin
  for i:=0 to 7 do
  for j:=-i to i do
  ImageText.Canvas.Pixels[X+j+10,Y+i+15]:=0;
{
  R.Top:=Y+15;
  R.Left:=X;
  R.Right:=X+20;
  R.Bottom:=Y+25;
  InvalidateRect(0,@R,true);
}
  end;

//---------------------------------------------------------------------------
//
//     Function:      NoArrowOut
//
//     Purpose:       To remove an arrow at some position
//
//     Parameters:    X = x-position, horisontal
//                    Y = y-position, vertical
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.NoArrowOut(X: integer;Y: integer);

var i,j: integer;

  begin
  for i:=0 to 8 do
  for j:=-i to i do
  ImageText.Canvas.Pixels[X+j+10,Y+i+15]:=$FFFFFF;
  end;

//---------------------------------------------------------------------------
//
//     Function:      Strip
//
//     Purpose:       Remove spaces in a string in the start and in the end
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

function Strip(s: string): string;

  begin
  while (length(s)>0) and (s[Length(s)]=' ') do s:=copy(s,1,Length(s)-1);
  while (length(s)>0) and (s[1]=' ') do s:=copy(s,2,Length(s)-1);
  Strip:=s;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:     TimerTimer
//
//     Purpose:       Handle both moving the text cursor and also
//                    generate all the text-lines when playing shall start
//                    (after reading the input Midi- or MusicXml-File)
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.TimerTimer(Sender: TObject);

var
    i: integer;
    j: integer;
    linenumber: array[0..15] of integer;
    linestring: array[0..15] of string;
    textsnumber: array[0..15] of integer;
    textnumber: integer;
    track: integer;
    s: string;
    p: integer; // Position of "&quote"
    Rect: TRect;

  begin
  textnumber:=LastTextNumber;

  TrackSelection:=CrossReference[TabIndex];

  if (TrackSelection>0) and (SystemState=MidiPlaying) then
  // Position the cursor (shown as an arrow). I.e. if more notes
  // are played (events are handled), then check and move cursor
  for i:=LastHandledIndex+1 to MidiEventIndexOut do
    begin
    LastHandledIndex:=MidiEventIndexOut;

    // Get track/channel number from NoteOn($90) Midi-event. This track?
    if  { (CrossReferenceIndex<MidiChannelMap[TrackSelection]) and }
        // Is this tab selected?
        (CrossreferencePresent[TabToChannel[TabIndex]+1]) and
       ((MidiEvents[i].Bytes[0] and $F0)=$90 {+MidiChannelMap[TrackSelection]} ) then
      begin // Yes, this track is selected in the tab
      textnumber:=MidiTextsIndexNew[i];
      if (((MidiTextIndexLine[textnumber]>0) and (MidiTextIndexLine[textnumber]<=MaxLines)) or
         (MidiTextIndexPos[textnumber]>0))
             and (MidiTextsTrack[textnumber]=TrackSelection) then
        begin
        ArrowX:=MidiTextIndexPos[textnumber]; ////+
////          FormText.ImageText.Canvas.TextExtent(MidiTexts[textnumber]).cx;
        ArrowY:=(MidiTextIndexLine[textnumber])*LineHeight-1;
        if (LastTextNumber<>textnumber) and (MidiTexts[textnumber]<>'') then
        begin
          if ArrowX=0 then    //// TEST
            ArrowX:=0;
          if MidiTexts[textnumber]='' then
            MidiTexts[textnumber]:='';
          if ArrowX<LastArrowX then
            ArrowX:=ArrowX;
        FormText.NoArrowOut(LastArrowX,LastArrowY);
        FormText.ArrowOut(ArrowX,ArrowY);
        LastArrowX:=ArrowX;
        LastArrowY:=ArrowY;
          LastTextNumber:=textnumber;
////          Refresh;       //// ????
        end;
      end;
      end;
    end;
  if (SystemState=MidiStarting) or (SystemState=MidiUp) then
    begin
    LastHandledIndex:=0;
    end;
  TabControl.TabIndex:=TabIndex;

  if MustResize then
    begin
    ResizeText(Sender);
    MustResize:=false;
    end;
  if MustPaint then
    begin
    ImageText.Canvas.Draw(0,0,ImageText.Picture.Graphic);
    invalidate;
    end;

  if ((SystemState=MidiFinished) or (SystemState=MidiUp)) then textnumber:=0;

  if (textnumber>=0) and ((SystemState=MidiPlaying) or (SystemState=MidiPaused)) then
    begin
    if LineHeight*MidiTextIndexLine[textnumber]>(2*ClientHeight div 3)+
                                        ScrollBox.VertScrollBar.Position then
    ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position+4
    else if LineHeight*MidiTextIndexLine[textnumber]>(ClientHeight div 2)+
                                        ScrollBox.VertScrollBar.Position then
    ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position+2
    else if LineHeight*MidiTextIndexLine[textnumber]>(ClientHeight div 3)+
                                        ScrollBox.VertScrollBar.Position then
    ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position+1;
    end;


  // Produce the texts needed. Compose every line
  if ((SystemState=MidiUp) or (SystemState=MidiPlaying) or
      (SystemState=MidiPaused)) and (not TextsAreInitialised) then
    begin
    Rect.Left:=0;
    Rect.Top:=0;
    Rect.Right:=ImageText.Width-1;
    Rect.Bottom:=ImageText.Height-1;
    ImageText.Canvas.FillRect(Rect);
    MaxLines:=-1;
    //    TextLine: array[0..15,0..MaxTextLines] of string;
    for i:=0 to 15 do
    for j:=0 to MaxTextLines do TextLine[i,j]:='';
    for i:=0 to 15 do CrossreferencePresent[i]:=false;
    for i:=0 to 15 do Crossreference[i]:=-1;

    for i:=1 to MidiTextIndexIn-1 do
      begin
      CrossreferencePresent[MidiTextsTrack[i]]:=true;
      if TrackSelection<0 then
        begin
        TrackSelection:=MidiTextsTrack[i]-1;
        end;
      end;

    //// Burde den starte fra 1 med index?
    TabControl.Tabs.Clear;
    TabIndex:=0;
    for i:=0 to 15 do
      begin
      if CrossreferencePresent[i] then
        begin
        Crossreference[CrossreferenceIndex]:=i;
        if MidiTrackNames[i+1]<>'' then
          TabControl.Tabs.Add(MidiTrackNames[i+1])
        else
          begin
          TabControl.Tabs.Add('Channel '+IntToStr(i));
          TabToChannel[TabIndex]:=i;
          inc(TabIndex);
          end;
        inc(CrossreferenceIndex);
        end
      else
        begin

        end;
      end;

    TabControl.TabIndex:=0;
    TabIndex:=0;

    for i:=0 to 15 do linenumber[i]:=1;
    for i:=0 to 15 do linestring[i]:='';

    for i:=1 to MidiTextIndexIn-1 do
      begin
      track:=MidiTextsTrack[i]-1;
      MidiTextIndexLine[i]:=linenumber[track];
      if MaxLines<linenumber[track] then MaxLines:=linenumber[track];
      MidiTextIndexPos[i]:=FormText.ImageText.Canvas.TextExtent(linestring[track]).cx;
        begin
        s:='';
        if Length(MidiTexts[i])>0 then s:=MidiTexts[i][Length(MidiTexts[i])];
        // The xml-text for quote is converted to qoute
        p:=Pos('&quot;',MidiTexts[i]);
        if p>0 then
          MidiTexts[i]:=copy(MidiTexts[i],1,p-1)+'"'+copy(MidiTexts[i],p+6,Length(MidiTexts)-p-6);

        linestring[track]:=linestring[track]+MidiTexts[i]+' ';

        if  (Length(MidiTexts[i])>1) and 
            ((((MidiTexts[i][Length(MidiTexts[i])]='.'){ and (Length(MidiTexts[i])>1)}) or
            (MidiTexts[i][Length(MidiTexts[i])]=',') or
            (MidiTexts[i][Length(MidiTexts[i])]='!')) or (MidiTexts[i][Length(MidiTexts[i])]=';') or
            (MidiTexts[i][Length(MidiTexts[i])]='?') or
           (FormText.ImageText.Canvas.TextExtent(linestring[track]+MidiTexts[i]).cx>
             (ClientWidth*95) div 100)) then
          begin
          if MidiXmlFile then
  {$ifdef Fpc}
            TextLine[track,linenumber[track]]:=linestring[track]
  {$else}
            TextLine[track,linenumber[track]]:=linestring[track]
  {$endif}
          else
            begin
  {$ifdef Darwin}
            TextLine[track,linenumber[track]]:=AnsiToUnicodeString(linestring[track]);
  {$else}
            TextLine[track,linenumber[track]]:=linestring[track];
  {$endif}
            end;
          linestring[track]:='';
          if Strip(TextLine[track,linenumber[track]])='.' then
            TextLine[track,linenumber[track]]:=''
          else
          inc(linenumber[track]);
          if MaxLines<linenumber[track] then MaxLines:=linenumber[track];
          end;
        end;
      end;
    // Remaining not written lines
    for i:=0 to 15 do
      begin
      if linestring[i]<>'' then
        begin
        if MidiXmlFile then
          TextLine[i,linenumber[i]]:=linestring[i]
        else
{$ifdef Darwin}
          TextLine[i,linenumber[i]]:=AnsiToUnicodeString(linestring[i]);
{$else}
          TextLine[i,linenumber[i]]:=linestring[i];
{$endif}
        inc(linenumber[i]);
        // Extra empty line to terminate
        TextLine[i,linenumber[i]]:='';
        inc(linenumber[i]);

        if MaxLines<linenumber[i] then MaxLines:=linenumber[i];
        end;
      end;

    // To position cursor at next line
    for i:=0 to 15 do
      begin
      MidiTextIndexPos[MidiTextIndexIn]:=1;
      MidiTextIndexLine[MidiTextIndexIn]:=linenumber[i];
      end;

    ImageText.Height:=LineHeight*(MaxLines+1);
    ImageText.Width:=ClientWidth-2;

    TextsAreInitialised:=true;
    MustPaint:=true;
    end;
  end;


//---------------------------------------------------------------------------
//
//     Procedure:     OnResize
//
//     Purpose:       Set new size of status bar and let OnChange do the rest
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.OnResize(Sender: TObject);

  begin
  MustResize:=true;
  end;
//---------------------------------------------------------------------------
//
//     Procedure:     ResizeText
//
//     Purpose:       ////Set new size of status bar and let OnChange do the rest
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.ResizeText(Sender: TObject);

  begin
  StatusBar.Panels.Items[0].Width:=ClientWidth div 4;
  StatusBar.Panels.Items[1].Width:=ClientWidth div 8;
  StatusBar.Panels.Items[2].Width:=ClientWidth div 4;
  StatusBar.Panels.Items[3].Width:=ClientWidth div 4;
  NumberOfLines:=(ClientHeight div LineHeight)-3;
  if NumberOfLines<0 then NumberOfLines:=0;
  TabControl.TabIndex:=TabIndex;
  OnChange(Sender);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:     OnChange
//
//     Purpose:       To change the current tab, i.e. show new track
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.OnChange(Sender: TObject);

var i: integer;     // For loop (for all lines)
    rect: TRect;    // Clear canvas

  begin

  if TextsAreInitialised and (TabIndex<>TabControl.TabIndex) then
    begin
    if TabControl.TabIndex>=0 then TabIndex:=TabControl.TabIndex;
    Rect.Left:=0;
    Rect.Top:=0;
    Rect.Right:=ImageText.Width-1;
    Rect.Bottom:=ImageText.Height-1;
    ImageText.Canvas.FillRect(Rect);

    for i:=0 to MaxLines do
    ImageText.Canvas.TextOut(Indentation,LineHeight*i,TextLine[CrossReference[TabIndex]-1,i]);

    ArrowX:=Indentation-8;
    ArrowY:=LineHeight;
    ArrowOut(ArrowX,ArrowY);
    LastArrowX:=ArrowX;
    LastArrowY:=ArrowY;
    end;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:     OnCreate
//
//     Purpose:       Do initialisation
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.OnCreate(Sender: TObject);

  begin
  TabsAreInitialised:=false;
  PageSize:=1;
  CrossreferenceIndex:=0;
  TrackSelection:=-1;
  LastHandledIndex:=0;
  Font.Size:=11;
  Font.Name:='Sans Serif';
  ImageText.Canvas.Font.Size:=10;
  ImageText.Canvas.Font.Name:='Sans Serif';
  LastTextNumber:=-1;
  MaxLines:=-1;
  ShownLines:=0;
  ArrowX:=MidiTextIndexPos[1];
  ArrowY:=MidiTextIndexLine[1]*LineHeight+LineHeight-1;
  LineHeight:=22;
  MustResize:=true;
  ArrowX:=0;
  ArrowY:=LineHeight-1;
  LastArrowX:=0;
  LastArrowY:=0;
  OnPaint(Sender);
  end;

//---------------------------------------------------------------------------
//
//     Procedure:     OnActivate
//
//     Purpose:       Redraw window - not used any more
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.OnActivate(Sender: TObject);

    begin
  ////
  end;

//---------------------------------------------------------------------------
//
//     Procedure:     OnChangeScrollBar
//
//     Purpose:       Redraw the window (using the scrollbar position)
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.OnChangeScrollBar(Sender: TObject);

  begin
  if (SystemState=MidiStarting) or (SystemState=MidiUp) then
    begin
    OnPaint(Sender);
    end;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:     OnMouseWheel
//
//     Purpose:       Move the scrollbar in direction of the mouse wheel
//
//     Parameters:    Sender = standard windows
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  begin
  if WheelDelta>0 then
    ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position-1
  else if WheelDelta<0 then
    ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position+1
  end;

//---------------------------------------------------------------------------
//
//     Procedure:     CopyTextToFile
//
//     Purpose:       Save the song text
//
//     Parameters:    none
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure TFormText.CopyTextToFile;

var i,j: integer;
    s: string;
    p: integer;

  begin

  MemoText.Lines.Clear;
  for j:=0 to 15 do
    begin
    MemoText.Lines.Add('Track: '+IntToStr(j));
    for i:=0 to MaxLines do
      begin
      s:=TextLine[j,i];
      if s<>'' then
        begin
          repeat
          p:=Pos('- ',s);
          if p>0 then s:=copy(s,1,p-1)+copy(s,p+2,Length(s)-p-1);
          until p=0;
        MemoText.Lines.Add(s);
        end;
      end;
  MemoText.CopyToClipboard;
{$ifdef FPC}
  MemoText.Lines.SaveToFile(Utf8ToAnsi(FileName+'.txt'));
{$else}
  MemoText.Lines.SaveToFile(FileName+'.txt');
{$endif}
    end;
  end;

end.
